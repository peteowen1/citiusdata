# Export the athletics-calendar data for inthegame.blog/athletics.
#
# Ticket 04 put athletics at its own site root, mirroring afl/ and football/,
# with /multisport/ kept as the cross-sport games archive. This publishes what
# that section reads. The R2 prefix is `athletics/`, separate from the `games/`
# prefix the Commonwealth Games pages use, so neither can overwrite the other.
#
# Re-runnable end to end. A later run supersedes an earlier one.
#
# STALENESS IS THE DEFAULT FAILURE MODE, not an edge case: there is no CI for
# this and it is run by hand (citiusdata#2), with six meets in five weeks. Every
# artefact therefore carries `generated_at` and the site renders an "as at"
# stamp off it. Do not remove that column to tidy the schema.

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(arrow); library(jsonlite)
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))

D      <- file.path(VERSE, "citiusdata", "data")
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

BLOG   <- file.path(VERSE, "citiusdata", "blog")
BUCKET <- "inthegame-data"
PREFIX <- "athletics"
NOW    <- Sys.time()
dir.create(BLOG, recursive = TRUE, showWarnings = FALSE)

# --- calendar -----------------------------------------------------------------
# Hand-maintained, because competition_catalogue.parquet is built from HARVESTED
# HISTORY and structurally cannot contain a meet that has not happened yet.
cal <- fread(file.path(D, "athletics_calendar.csv"))
cal[, `:=`(date_start = as.Date(date_start), date_end = as.Date(date_end),
           prediction_cutoff = as.Date(prediction_cutoff))]
# Every check below is an all(...) or an anyDuplicated(), and both are vacuously
# satisfied by a zero-row table - so an empty or mis-parsed calendar would sail
# through the entire block. Assert the rows exist first.
stopifnot(
  "the calendar parsed to zero rows" = nrow(cal) > 0,
  "meet_id must be unique" = !anyDuplicated(cal$meet_id),
  "dates must be ordered within a meet" = all(cal$date_end >= cal$date_start),
  "a prediction cutoff must precede its meet" = all(cal$prediction_cutoff < cal$date_start),
  "state must be one of upcoming/live/scored" =
    all(cal$state %in% c("upcoming", "live", "scored")))
cli::cli_alert_success("Calendar: {nrow(cal)} meet{?s}, {cal[state=='upcoming', .N]} upcoming.")

# --- Birmingham card ----------------------------------------------------------
# PRESENT-OR-SKIPPED, not assumed. This block used to read the card
# unconditionally, which made every meet's publish depend on Birmingham's
# artefacts sitting on the same machine. That is invisible on the laptop, where
# they always do, and fatal anywhere else: forecasting Budapest on a runner
# died here with "cannot open the connection" after passing every check of its
# own (2026-09-07).
#
# Shipping the August card as a CI input was the wrong fix, and its own gate
# said so: sanity_birmingham_card.R rejected it, correctly, because the
# deployed calibration was promoted on 4 September and a card built against a
# superseded config should not be republished. A freshly built one passes.
#
# So build Birmingham when this run asks for it and its card is here, skip it
# otherwise, and carry its manifest block forward either way (see the manifest
# section) — a partial publish must never unpublish a meet from the site.

# --- which meets this run publishes -------------------------------------------
# Optional meet ids on the command line. With none, every meet whose card is on
# disk is rebuilt, which is what the laptop has always done. With one or more,
# only those are built and every other meet is left exactly as published.
#
# WHY PRESENCE ALONE IS NOT ENOUGH. A card that cannot pass its own sanity
# script aborts this whole run, so one un-republishable meet blocks every other
# meet's publish. birmingham2026 is in that state today: it was built in August
# and the calibration was promoted on 4 September, so its gate correctly refuses
# to republish it. Without a selector that single fact stops Brussels and
# Budapest from reaching the site — on a runner AND on the laptop, where the
# stale card also sits in data/.
#
# File presence answers "could this be built", never "was this asked for". The
# two only ever agreed by accident, on a machine that happened to hold every
# card and no bad ones.
SEL <- commandArgs(trailingOnly = TRUE)
SEL <- SEL[nzchar(SEL)]
if (length(SEL)) {
  unknown <- setdiff(SEL, cal$meet_id)
  # A typo would otherwise select nothing, build nothing, and still write and
  # upload a manifest: a no-op publish reported as a success.
  if (length(unknown)) {
    cli::cli_abort(c("Unknown meet id(s): {.val {unknown}}",
                     i = "Known: {.val {cal$meet_id}}"))
  }
  cli::cli_alert_info("Publishing only: {.val {SEL}}")
}
wanted <- function(mid) length(SEL) == 0L || mid %in% SEL

# Shared by every meet's card, so defined BEFORE the Birmingham block rather
# than inside it. Both used to live in there, which meant skipping Birmingham
# took the finals-only loop down with it ("object 'KEEP' not found") — the same
# hidden coupling this change exists to remove, one level further in.
KEEP <- c("event_id", "discipline", "sex", "athlete_id", "athlete", "nation",
          "nation_code",
          "p_gold", "p_medal", "p_final", "p_reach_r2", "p_reach_r3",
          "ability", "sigma", "ability_se", "n_rounds", "field_modelled",
          # cutoff_requested and history_max_date added 2026-09-07: `cutoff` is
          # now clamped to the last date actually in the history, so on its own
          # it no longer tells you whether it was clamped. These two carry the
          # date that was asked for and the data boundary, which is what lets a
          # page say "locked from data before X" honestly. Without them in KEEP
          # the columns are written to data/ and then dropped here, which is
          # what happened when the clamp first shipped.
          "generated_at", "cutoff", "cutoff_requested", "history_max_date",
          "config", "counts_source",
          "combined_rows_excluded",
          "field_type", "field_source", "field_entrants", "field_unmodelled")

orient <- as.data.table(citius_events())[, .(event_id, orientation, family)]

# --- personal / season bests ----------------------------------------------
# athlete_pbs.parquet / athlete_sbs.parquet (assemble_athlete_profiles.R,
# harvested from each athlete's own World Athletics profile) key on
# (athlete_id, discipline), and `discipline` there is WA's own free-text
# name -- confirmed to match citius_events()' discipline naming exactly
# (both come from the same source), so no code-mapping table is needed; the
# join is directly onto the card's own existing (athlete_id, discipline)
# columns. Purely a display value -- these never feed the model, only shown
# beside the predicted mark for context.
#
# A rare athlete with >1 row per discipline (indoor vs outdoor, or an older
# mark re-surfaced in the source) is collapsed to the single most-recent one
# BEFORE merging, so the join can never fan out a card row -- fan-out here
# would silently duplicate every other column on that row too, not just add
# an extra best-mark row.
BESTS_PB <- file.path(D, "athlete_pbs.parquet")
BESTS_SB <- file.path(D, "athlete_sbs.parquet")

.load_bests <- function(path, prefix) {
  if (!file.exists(path)) {
    cli::cli_alert_warning("{basename(path)} not found -- {prefix}_mark/{prefix}_date will be NA on every row.")
    return(data.table(athlete_id = character(0), discipline = character(0)))
  }
  dt <- setDT(read_parquet(path, col_select = c("athlete_id", "discipline", "date", "mark")))
  dt[, athlete_id := as.character(athlete_id)]
  # na.last = TRUE is load-bearing, not a style choice: setorder()'s default
  # (na.last = FALSE) puts NA dates FIRST regardless of the -date descending
  # modifier -- verified directly, a group of {NA, 2024-01-01, 2023-01-01}
  # sorts NA to row 1. .SD[1L] below would then silently keep the NA-dated
  # row and discard the real most-recent mark, with no error anywhere. Zero
  # NA dates in either source parquet today (checked), but nothing prevents
  # World Athletics from sending one for an older/partial profile.
  setorder(dt, athlete_id, discipline, -date, na.last = TRUE)
  dt <- dt[, .SD[1L], by = .(athlete_id, discipline)]
  setnames(dt, c("date", "mark"), paste0(prefix, c("_date", "_mark")))
  dt[]
}
PB_BESTS <- .load_bests(BESTS_PB, "pb")
SB_BESTS <- .load_bests(BESTS_SB, "sb")
cli::cli_alert_info("Bests loaded: {nrow(PB_BESTS)} PB rows, {nrow(SB_BESTS)} SB rows.")

#' Attach personal-best / season-best mark + date to a card, by (athlete_id,
#' discipline). Additive only -- never changes row count (both source tables
#' are pre-collapsed to one row per key above) or any existing column.
attach_bests <- function(dt) {
  dt <- merge(dt, PB_BESTS, by = c("athlete_id", "discipline"), all.x = TRUE)
  dt <- merge(dt, SB_BESTS, by = c("athlete_id", "discipline"), all.x = TRUE)
  dt
}

BHAM_CARD  <- file.path(D, "birmingham2026_pretournament.rds")
BHAM_BUILD <- wanted("birmingham2026") && file.exists(BHAM_CARD)
if (!BHAM_BUILD) {
  cli::cli_alert_info(
    "birmingham2026: not built this run — skipping its artefacts; its manifest block is carried forward.")
}

if (BHAM_BUILD) {
pred <- setDT(readRDS(BHAM_CARD))
st   <- fread(file.path(D, "birmingham2026_round_structure.csv"))

# The card is only publishable if its own sanity script passes. Run it here
# rather than trusting that someone remembered: an unchecked card is exactly
# what sanity_glasgow_card.R exists to prevent, and this script is the last
# thing between the numbers and the public.
sanity <- file.path(VERSE, "citiusdata", "scripts", "sanity_birmingham_card.R")
rc <- system2("Rscript", shQuote(sanity), stdout = FALSE, stderr = FALSE)
if (!identical(rc, 0L)) {
  cli::cli_abort(c("sanity_birmingham_card.R FAILED (exit {rc}) - nothing published.",
                   i = "Run it directly to see which check failed."))
}
cli::cli_alert_success("Sanity checks passed; the card is publishable.")

# Round labels the page shows, joined so the site does not reimplement them.
lab <- st[, .(event_id, round_index, round, races, advance, fastest_losers,
              counts_source)]

# Trim to what a page actually needs. Everything else is weight on the wire.
#
# `field_type`/`field_source` and the field_entrants/field_unmodelled pair are
# in this list for a reason that is LATENT for Birmingham and load-bearing for
# the Diamond League cards. Birmingham's field is an official entry list, so its
# provenance caveat has never mattered and the columns were simply never carried.
# The Brussels/Budapest cards are built on a field that is NOT an official entry
# list (World Athletics has published none), and predict_diamond_league_final.R
# stamps `field_type = "third_party_qualifier_list_unofficial"` plus a
# `field_source` note on every row to say so. Without these here, the first
# person to wire a DL card into this export would silently drop that caveat at
# exactly the moment it matters most -- the same shape as the staleness column
# this file's own header warns not to tidy away. Found in review 2026-08-31,
# BEFORE a DL card was wired in, so it never actually shipped mis-stamped.
#
# This project's convention is that caveats live in the published data, not
# hardcoded in the page (so a wrong claim is fixed in one place) -- which only
# works if the caveat survives this select.
#
# `nation_code` is here for the same class of reason, found the same way. The
# DL cards' entry list gives nationality as free text, mixing bare codes
# ("USA") with 19-character full names ("Trinidad and Tobago"), and the site
# renders it into a badge sized for three characters. add_nation_codes.R
# fetches authoritative codes and patches the card -- but until this list
# carried the column, that whole step ended here: the card had nation_code at
# 100% coverage and the exported artefact had none of it, on a correctly
# ordered run. An enrichment step with no path to a reader. Found in review
# 2026-08-31, again before anything shipped.
#
# intersect() below means a column absent for a given meet just doesn't appear:
# Birmingham gains field_type = "official_entry_list" and skips the rest.
card <- pred[, intersect(KEEP, names(pred)), with = FALSE]
card[, meet_id := "birmingham2026"]

# --- predicted mark -----------------------------------------------------------
# Formatted HERE rather than in the page: the page would otherwise need the
# registry's orientation and the seconds/metres/points distinction, and a second
# implementation is a second thing to get wrong.
#
# The formatter now lives in the package as predicted_mark(), because the blog
# shows a predicted mark in TWO places — this per-event card and the evergreen
# athlete ratings table — and the same athlete printing two different times on
# two pages reads as the site being wrong. One implementation, both exports.
# Its round-trip against Duplantis 6.03 m, Mahuchikh 1.98 m, Skotheim 8,813 pts,
# a 2:08:29 marathon and a 1:56.3 800m is asserted in the package tests.
#
# This is a TYPICAL mark, not a peak. The model forecasts a recency-weighted
# average and a championship final is closer to an athlete's best day, so these
# read slightly slow by design. The page says so.
card <- merge(card, orient, by = "event_id", all.x = TRUE)
card[, c("pred_mark", "mark_unit") := predicted_mark(ability, orientation)]
card[, c("orientation", "family") := NULL]
stopifnot("every predicted mark must format" = !any(is.na(card$pred_mark)))
cli::cli_alert_success("Predicted marks formatted for all {nrow(card)} rows.")

card <- attach_bests(card)
cli::cli_alert_info("birmingham2026: PB on {sum(!is.na(card$pb_mark))}/{nrow(card)} rows, SB on {sum(!is.na(card$sb_mark))}/{nrow(card)}.")

# Ranking within event, so the page never has to sort to find a favourite.
setorder(card, event_id, -p_gold)
card[, rank_gold := seq_len(.N), by = event_id]

# --- events -------------------------------------------------------------------
ev <- as.data.table(citius_events())[event_id %in% unique(card$event_id),
        .(event_id, discipline, sex, family, orientation)]
ev <- merge(ev, card[, .(field = .N, favourite = athlete[1],
                         p_favourite = p_gold[1]), by = event_id],
            by = "event_id", all.x = TRUE)
ev <- merge(ev, st[, .(n_rounds = max(round_index)), by = event_id],
            by = "event_id", all.x = TRUE)

for (x in list(cal, card, lab, ev)) if (is.data.table(x)) x[, generated_at := NOW]

# Nation projection, built from the joint per-simulation podiums rather than by
# summing marginals — see predict_birmingham2026.R for why that distinction
# carries 81% of the mass on this field.
nat_f <- file.path(D, "birmingham2026_nations.parquet")
if (!file.exists(nat_f)) {
  cli::cli_abort("birmingham2026_nations.parquet missing - re-run predict_birmingham2026.R.")
}
nations <- setDT(as.data.frame(read_parquet(nat_f)))

# citiusdata#9. predict_birmingham2026.R writes the card's source rds and this
# nations table in the SAME run, seconds apart. If their stamps are far apart,
# this export is about to publish a nations table from a different, older
# simulation alongside a fresh card -- two views of one run that disagree,
# under a single "as at". Nothing downstream could catch it: the only existing
# check is that the file exists.
card_stamp <- max(pred$generated_at)
nat_stamp  <- max(nations$generated_at)
prov_gap   <- as.numeric(difftime(nat_stamp, card_stamp, units = "mins"))
if (!is.finite(prov_gap) || abs(prov_gap) > 10) {
  cli::cli_abort(c(
    "nations.parquet is not from the same predict run as the card - nothing published.",
    i = "card {format(card_stamp)}, nations {format(nat_stamp)}, {round(abs(prov_gap))} min apart.",
    i = "Re-run predict_birmingham2026.R so both come from one simulation."))
}

# Stamped like every other artefact. `generated_at` here means WHEN THIS WAS
# PUBLISHED -- the convention the other four already follow, and what the site
# renders its "as at" from. nations was the only one left out, so the section
# dated one table differently from the rest for no reason a reader could act on.
#
# The provenance gate above is what makes stamping it safe. On its own, writing
# NOW over this column would do the opposite of what it looks like: it would
# erase the only evidence that the table came from an older run, hiding exactly
# the staleness the column exists to reveal.
nations[, generated_at := NOW]

artefacts <- list(
  "calendar.parquet"                    = cal,
  "birmingham2026-predictions.parquet"  = card,
  "birmingham2026-rounds.parquet"       = lab,
  "birmingham2026-events.parquet"       = ev,
  "birmingham2026-nations.parquet"      = nations)

} else {
  # The calendar is not Birmingham's, and every run publishes it: it is what
  # the section's index reads to list meets at all.
  artefacts <- list("calendar.parquet" = cal)
}

# --- actual results, for meets that have started -------------------------------
# Independent of whether this run rebuilt a meet's PREDICTION card above:
# results come straight from the harvested corpus (championship_results.rds),
# keyed by wa_competition_id, and publish whenever the harvester has anything
# for that meet. A meet mid-competition publishes a PARTIAL results file
# (whatever rounds have actually run), which is a real and useful state, not
# an error -- unlike the prediction cards, where a missing file is a hard
# stop, a meet with zero harvested rows (upcoming, or not yet harvested) is
# the normal state for most rows in `cal` on most days, so this is skipped
# quietly per meet rather than aborting the run.
#
# inthegame-blog#athletics event.qmd reads <meet>-results.parquet and shows
# the FINAL round's place/mark beside the pre-meet prediction; heats/semis are
# published too (never know when a future page wants them) but not yet read.
CH <- setDT(readRDS(file.path(D, "championship_results.rds")))
RESULTS_META <- tryCatch(
  setDT(as.data.frame(read_parquet(file.path(D, "athlete_meta.parquet"),
                                    col_select = c("athlete_id", "country")))),
  error = function(e) {
    cli::cli_warn("athlete_meta.parquet unavailable -- results will publish with no nation.")
    NULL
  }
)
if (!is.null(RESULTS_META)) RESULTS_META[, athlete_id := as.character(athlete_id)]

for (i in seq_len(nrow(cal))) {
  mid <- cal$meet_id[i]
  # Deliberately NOT gated on wanted(): results are independent of which
  # meet's PREDICTION card this run was asked to rebuild (`SEL` above) --
  # a run scoped to one meet's card should still refresh every other meet's
  # results, since nothing about that scoping says "and don't touch results".
  comp_id <- suppressWarnings(as.integer(cal$wa_competition_id[i]))
  if (is.na(comp_id)) next
  sub <- CH[competition_id == comp_id & !is.na(event_id)]
  if (!nrow(sub)) { cli::cli_alert_info("{mid}: no harvested results yet -- results file skipped."); next }

  # Same "final" definition export_blog_data.R uses for the Commonwealth Games
  # pages, so the two never disagree about which round was the medal race.
  # Semifinals match "final" as a substring, which is why the semi exclusion
  # has to come second.
  res <- sub[, .(event_id, athlete_id = as.character(athlete_id), athlete = athlete_name,
                 place, mark, mark_string, wind, round,
                 is_final = grepl("final", round, ignore.case = TRUE) &
                            !grepl("semi", round, ignore.case = TRUE))]
  if (!is.null(RESULTS_META)) {
    res <- merge(res, RESULTS_META, by = "athlete_id", all.x = TRUE)
  } else {
    res[, country := NA_character_]
  }
  setnames(res, "country", "nation")
  res[, `:=`(meet_id = mid, generated_at = NOW)]

  artefacts[[sprintf("%s-results.parquet", mid)]] <- res
  n_final <- res[is_final == TRUE & !is.na(place) & place > 0, .N]
  cli::cli_alert_success(
    "{mid}: {nrow(res)} result row{?s} across {uniqueN(res$event_id)} event{?s} ({n_final} placed finalist{?s}).")
}

# --- Diamond League / finals-only cards ---------------------------------------
# Birmingham's block above assumes a multi-round feed entry list: a round
# structure csv, a nations parquet, derived heat counts. A Diamond League final
# has none of that -- one race per event, no heats, no qualifying, no
# advancement. So these cards publish a SUBSET of Birmingham's artefacts rather
# than fabricating empty rounds data to fit a shape the meet does not have.
#
# THE SITE IS THE CONTRACT. athletics/meet.qmd and athletics/event.qmd are both
# query-param driven and otherwise meet-agnostic; between them they fetch three
# objects per meet, so three is what a card must publish:
#   <meet>-predictions.parquet   the card (meet.qmd + event.qmd)
#   <meet>-events.parquet        one row per event (meet.qmd's table)
#   <meet>-rounds.parquet        see below -- required even with no rounds
#
# The rounds object is NOT optional and NOT padding. event.qmd deliberately
# separates "this event genuinely has one round" from "the rounds file did not
# load", and on a null fetch prints "Round detail is unavailable right now" --
# which for a one-day meet would report an outage that is not happening. One
# honest row per event (round_index 1, "Final", one race, nothing advancing)
# states the true shape instead. `counts_source` is deliberately NOT "derived":
# that value is what triggers event.qmd's Technical-Delegates note explaining
# how heat counts were guessed, and there are no heats here to explain.
# budapest2026 is the same finals-only shape and publishes the same three
# objects, despite a materially better field source (World Athletics' own
# qualification standings, IDs pre-resolved) than Brussels' third-party
# compilation. The shape of the meet, not the provenance of its field, is what
# decides which artefacts a card needs -- so both belong in this one loop, and
# the difference in provenance is carried by field_type/field_source per row.
# Mark calibration, read rather than asserted. form_display_marks.R measures how
# often each displayed mark is actually beaten and writes it beside the parquet;
# the caveat sentence is built from that number so the page cannot drift from
# what was measured. A missing file is a hard stop, not a silent default: a page
# that quietly loses its calibration caveat is worse than one that fails to build.
#
# Defined here rather than beside the manifest because the finals-only blocks
# below quote CAVEAT_PEAK too, and they are built inside the loop.
CALIB_F <- file.path(D, sprintf("form_display_%s_calib.json", TAG))
if (!file.exists(CALIB_F))
  stop("form_display_final_calib.json is missing -- run form_display_marks.R before exporting")
CALIB <- fromJSON(CALIB_F)
CAVEAT_PEAK <- sprintf(
  paste("The \"good day\" mark is beaten in %s (%.1f%% of finals, measured out of sample).",
        "It is built as a 90th percentile, but one spread is shared across athletes",
        "whose race-to-race variation differs, so it is optimistic by a few points."),
  CALIB$peak_label, CALIB$goodday_beaten_pct)
cat(sprintf("mark calibration: typical beaten %.2f%%, good day %.2f%% (%s)\n",
            CALIB$typical_beaten_pct, CALIB$goodday_beaten_pct, CALIB$peak_label))

# Retrospective backfill (2026-09-11): lausanne2026/silesia2026/zurich2026
# added -- same Diamond-League finals-only shape as brussels2026/budapest2026,
# just with a field derived from harvested results instead of a captured
# pre-meet entry list (see predict_diamond_league_final.R's FIELD table).
DL_MEETS <- c("brussels2026", "budapest2026", "lausanne2026", "silesia2026", "zurich2026")

# Manifest blocks for the finals-only meets, filled in as each is built.
#
# These did not exist until 2026-09-07 and their absence was silent. The
# athletics index decides whether a meet is forecast SOLELY from the manifest
# (`_hasCard` in athletics/index.qmd) — the parquets it would then read are
# never consulted for that question. So a Diamond League card could upload
# perfectly, three artefacts and all, and the section would still print "not
# forecast" beside it for ever. Publishing the data is not publishing the meet.
dl_blocks <- list()

for (mid in DL_MEETS) {
  if (!wanted(mid)) { cli::cli_alert_info("{mid}: not selected this run, skipping."); next }
  cf <- file.path(D, sprintf("%s_pretournament.rds", mid))
  if (!file.exists(cf)) { cli::cli_alert_info("{mid}: no card built yet, skipping."); next }

  # Same gate Birmingham gets: the card is only publishable if its own sanity
  # script passes, run here rather than trusting that someone remembered.
  dl_sanity <- file.path(VERSE, "citiusdata", "scripts", "sanity_diamond_league_card.R")
  rc_dl <- system2("Rscript", c(shQuote(dl_sanity), shQuote(mid)),
                   stdout = FALSE, stderr = FALSE)
  if (!identical(rc_dl, 0L)) {
    cli::cli_abort(c("sanity_diamond_league_card.R FAILED for {mid} (exit {rc_dl}) - nothing published.",
                     i = "Run it directly to see which check failed."))
  }
  cli::cli_alert_success("{mid}: sanity checks passed; the card is publishable.")

  dp <- setDT(readRDS(cf))
  card_stamp <- max(dp$generated_at)  # captured before KEEP/generated_at overwrite below

  # p_final is STRUCTURAL here, not estimated: in a straight final, being in the
  # field IS being in the final. event.qmd already knows this -- it prints a
  # structural 1 as "100%" while refusing to print 100% for any estimate -- but
  # it needs the column to exist to say so. predict_diamond_league_final.R does
  # not emit it (there is no round to reach), so it is set here, where the
  # reason it equals 1 is a property of the meet shape rather than a model
  # output being rounded up.
  dp[, p_final := 1]

  dcard <- dp[, intersect(KEEP, names(dp)), with = FALSE]
  dcard[, meet_id := mid]

  dcard <- merge(dcard, orient, by = "event_id", all.x = TRUE)
  dcard[, c("pred_mark", "mark_unit") := predicted_mark(ability, orientation)]
  dcard[, c("orientation", "family") := NULL]
  stopifnot("every predicted mark must format" = !any(is.na(dcard$pred_mark)))

  dcard <- attach_bests(dcard)
  cli::cli_alert_info("{mid}: PB on {sum(!is.na(dcard$pb_mark))}/{nrow(dcard)} rows, SB on {sum(!is.na(dcard$sb_mark))}/{nrow(dcard)}.")

  setorder(dcard, event_id, -p_gold)
  dcard[, rank_gold := seq_len(.N), by = event_id]

  # One row per event, same columns Birmingham's events table carries. n_rounds
  # comes off the card rather than a round structure file, because for these
  # meets the card is the only thing that knows it.
  dev <- as.data.table(citius_events())[event_id %in% unique(dcard$event_id),
           .(event_id, discipline, sex, family, orientation)]
  dev <- merge(dev, dcard[, .(field = .N, favourite = athlete[1],
                              p_favourite = p_gold[1],
                              n_rounds = n_rounds[1]), by = event_id],
               by = "event_id", all.x = TRUE)

  dlab <- dcard[, .(round_index = 1L, round = "Final", races = 1L,
                    advance = NA_integer_, fastest_losers = NA_integer_,
                    counts_source = "single_final"), by = event_id]

  for (x in list(dcard, dev, dlab)) if (is.data.table(x)) x[, generated_at := NOW]

  artefacts[[sprintf("%s-predictions.parquet", mid)]] <- dcard
  artefacts[[sprintf("%s-rounds.parquet", mid)]]      <- dlab
  artefacts[[sprintf("%s-events.parquet", mid)]]      <- dev

  # Nation projection, if predict_diamond_league_final.R wrote one. Optional,
  # not required (`sanity_diamond_league_card.R` above does not check it):
  # older cards -- Brussels, whose forecast is now historical and will not be
  # re-run -- predate this and have none, and a re-export of a meet's other
  # artefacts must not abort for that. Requested in inthegame-blog#680;
  # nations.qmd was 404ing on {meet_id}-nations.parquet for every DL-shaped
  # meet. Same joint-per-simulation-podium reasoning and the same staleness
  # gate as Birmingham's nations table just above.
  nat_f_dl <- file.path(D, sprintf("%s_nations.parquet", mid))
  if (file.exists(nat_f_dl)) {
    nat_dl <- setDT(as.data.frame(read_parquet(nat_f_dl)))
    nat_stamp_dl <- max(nat_dl$generated_at)
    gap_dl <- as.numeric(difftime(nat_stamp_dl, card_stamp, units = "mins"))
    if (!is.finite(gap_dl) || abs(gap_dl) > 10) {
      cli::cli_alert_warning(
        "{mid}: {.file {basename(nat_f_dl)}} is {round(abs(gap_dl))} min from the card's own run -- not published this time. Re-run predict_diamond_league_final.R for {mid} so both come from one simulation.")
    } else {
      nat_dl[, generated_at := NOW]
      artefacts[[sprintf("%s-nations.parquet", mid)]] <- nat_dl
      cli::cli_alert_success("{mid}: nation projection ({nrow(nat_dl)} nation{?s}) will publish.")
    }
  } else {
    cli::cli_alert_info("{mid}: no {.file {basename(nat_f_dl)}} -- nations page will 404 for this meet.")
  }

  # counts_source is "single_final", not "derived": "derived" is what triggers
  # event.qmd's note explaining how heat counts were guessed, and a one-day
  # final has no heats to explain. The caveats are this shape's, not
  # Birmingham's — no draw, no advancement, no round-level byes exist here, so
  # repeating those three would be describing a meet that is not happening.
  fld_type <- as.character(unique(dcard$field_type)[1])
  dl_blocks[[mid]] <- list(
    events_modelled = uniqueN(dcard$event_id),
    # meet.qmd's "What this card covers" cell guards on BOTH events_modelled
    # and events_in_programme together, and only this one was ever set -- the
    # cell rendered nothing for every DL-shaped meet. Unlike Birmingham there
    # is no technical-regulations document to read a firm programme size from
    # (these fields come from a third-party compiler or a best-effort WA
    # fetch, not an official confirmed program), so this is honestly
    # events_modelled itself rather than a guessed larger number -- true, and
    # says "everything fetchable was modelled" rather than implying a gap that
    # isn't sourced. Found via inthegame-blog#680.
    events_in_programme = uniqueN(dcard$event_id),
    athletes = uniqueN(dcard$athlete_id),
    cutoff = as.character(unique(dcard$cutoff)[1]),
    counts_source = "single_final",
    field_type = fld_type,
    field_source = as.character(unique(dcard$field_source)[1]),
    caveats = c(
      "One straight final per event: every entrant is in the final by definition, so a 100% final probability is the shape of the meet, not a model output.",
      "The field is the declared start list as at the cutoff. Late withdrawals and additions are not modelled.",
      # A PROVISIONAL field has to say so ON THE PAGE. The fact was already
      # carried, precisely, in `field_source` - and `field_source` is a data
      # column the meet page never renders, so a reader saw a card with no hint
      # that its start list was known to be superseded. Something true in a
      # column nobody displays is not something the reader has been told.
      if (grepl("provisional", fld_type, ignore.case = TRUE))
        paste("The field is PROVISIONAL: it is the qualification standings as at the cutoff,",
              "and later results can still displace entrants. Treat the names as likely, not settled.")
      else NULL,
      "Predicted marks are a typical performance, not a peak.",
      CAVEAT_PEAK))

  cli::cli_alert_success(
    "{mid}: {nrow(dcard)} athlete-event{?s} across {uniqueN(dcard$event_id)} event{?s}.")
}

for (nm in names(artefacts)) {
  write_parquet(artefacts[[nm]], file.path(BLOG, nm))
  cli::cli_alert_success("{nm}: {nrow(artefacts[[nm]])} row{?s}")
}

# The manifest carries the freshness stamp the section trusts, so it is written
# and uploaded LAST and only if every data artefact landed. Publishing it
# unconditionally puts a brand-new "as at just now" over a card that failed to
# upload and is a run behind -- fresh-looking and wrong.
# --- per-event notes ----------------------------------------------------------
# Where the model says something a reader who follows the sport will find
# surprising, say so ON THAT EVENT rather than leaving them to assume we have not
# noticed. Published as data, keyed by event_id, so the page renders it and the
# claim lives in one place.
#
# Written by hand, deliberately: an automatic "thin evidence" banner on all 17
# flagged athlete-events would be noise. These are the ones where the model
# actively contradicts the form book.
EVENT_NOTES <- list(
  "AT-800Metres-W" = paste(
    "We make Femke Bol favourite over Keely Hodgkinson, and we are less sure of",
    "that than the number looks. Hodgkinson beat Bol into second at the London",
    "Diamond League on 18 July (1:56.21 to 1:56.46) and has the faster season",
    "best, 1:54.33 against 1:55.60. Bol has moved to the 800m recently and we",
    "hold six of her races against Hodgkinson's 105 — and with that little",
    "evidence the model reads her as unusually consistent, which flatters her",
    "chances. Treat this as our most disputable call of the meet."))

# A meet's manifest block is what the site reads to know a card exists at all;
# athletics/index.qmd shows "not forecast" without one. So a run that did not
# rebuild a meet must CARRY ITS BLOCK FORWARD rather than omit it — omitting it
# would silently unpublish a meet that is still live on the site, which is a
# worse outcome than the coupling this change removes.
#
# The previous manifest comes from the public bucket, not from BLOG/: a runner's
# BLOG/ is empty, and the published copy is the thing whose claims we are
# preserving. A fetch failure is fatal by design — carrying nothing forward
# while believing we did is how a meet would disappear quietly.
#
# The rule is per meet, not per Birmingham: any meet this run did not rebuild
# keeps whatever the published manifest says about it. Birmingham is the one
# that must be there — it is live on the site today — so its absence from a
# fetched manifest is fatal. A finals-only meet that has never published has
# no block to lose, and demanding one would block the first Brussels or
# Budapest publish for ever.
PUBLISHED_MANIFEST <- paste0(
  "https://pub-ee4bf5b599a047f9ac2b9facc1587008.r2.dev/", PREFIX, "/athletics-manifest.json")

skipped <- c(if (!BHAM_BUILD) "birmingham2026",
             setdiff(DL_MEETS, names(dl_blocks)))
prev <- NULL
if (length(skipped)) {
  prev <- tryCatch(jsonlite::fromJSON(PUBLISHED_MANIFEST, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(prev)) {
    cli::cli_abort(c(
      "Did not rebuild {.val {skipped}} and could not read the published manifest.",
      i = "Publishing now could drop a meet that is currently live.",
      i = "Source: {.url {PUBLISHED_MANIFEST}}"))
  }
  if (!BHAM_BUILD && is.null(prev$birmingham)) {
    cli::cli_abort(c(
      "Skipped birmingham2026 but the published manifest has no block for it.",
      i = "Publishing without it would remove a meet that is currently live."))
  }
  for (m in skipped) {
    have <- !is.null(prev[[m]]) || (identical(m, "birmingham2026") && !is.null(prev$birmingham))
    if (have) cli::cli_alert_info("{m}: manifest block carried forward.")
    else      cli::cli_alert_info("{m}: not rebuilt and never published — nothing to carry forward.")
  }
}

# Whatever the published manifest said about a finals-only meet we did not
# rebuild, said again verbatim. NULL entries drop out, so a meet that has
# never published stays absent rather than appearing as an empty block, which
# `_hasCard` would read as "forecast" and the meet page would then fail to
# fill.
for (m in setdiff(DL_MEETS, names(dl_blocks))) {
  if (!is.null(prev) && !is.null(prev[[m]])) dl_blocks[[m]] <- prev[[m]]
}

manifest <- list(
  generated_at = format(NOW, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  config = DEPLOYED$stamp,
  meets = nrow(cal),
  event_notes = EVENT_NOTES,
  mark_calibration = CALIB,
  birmingham = if (!BHAM_BUILD) prev$birmingham else list(
    events_modelled = uniqueN(card$event_id),
    events_in_programme = 44L,
    unmodelled = "Marathon Race Walk (men's and women's) - no registry event_id",
    athletes = uniqueN(card$athlete_id),
    cutoff = as.character(unique(card$cutoff)[1]),
    counts_source = "derived",
    caveats = c(
      "Heat counts are derived, not official: Technical Delegates publish them with the start lists.",
      "Advancement assumes a seeded draw. The published draw cannot be ingested.",
      "Round-level no-marks and byes are not modelled.",
      "Predicted marks are a typical performance, not a peak: a championship final is closer to an athlete's best day, so they read slightly slow.",
      CAVEAT_PEAK,
      # This line said the OPPOSITE until 2026-08-06 -- "wider uncertainty,
      # which raises their win probability" -- which was an assumption, and
      # measuring it refuted it. Within an event, sigma correlates -0.245 with
      # race count: few races produces a SMALLER spread, so such an athlete
      # looks more consistent than the evidence can support. Raising their
      # uncertainty in fact LOWERS their win probability. See NEXT-STEPS.
      "An athlete with few races in an event can look more consistent than the evidence supports, which can overstate their chances.")))

# Keyed by full meet_id (brussels2026), which is what athletics/index.qmd looks
# for first; Birmingham above keeps its year-stripped key because that is what
# is already published and meet.qmd falls back to it.
# The length guard is not defensive padding: `names(list())` is NULL and
# `order(NULL)` is an error, not an empty result, so a Birmingham-only run —
# the one case where no finals-only meet is built OR carried forward — died
# here after every artefact had been written. Caught by running the Birmingham
# branch on CI, which the Budapest runs never exercised.
if (length(dl_blocks)) manifest <- c(manifest, dl_blocks[order(names(dl_blocks))])

write_json(manifest, file.path(BLOG, "athletics-manifest.json"),
           auto_unbox = TRUE, pretty = TRUE, na = "null")

upload <- function(f) {
  key <- sprintf("%s/%s/%s", BUCKET, PREFIX, f)
  # shQuote is not optional: the cache-control value contains a space and
  # system2() does no quoting on Windows, so it would arrive as two arguments.
  args <- c("r2", "object", "put", shQuote(key), "--file", shQuote(file.path(BLOG, f)),
            "--cache-control", shQuote("public, max-age=300"), "--remote")
  st <- suppressWarnings(system2("wrangler", args, stdout = TRUE, stderr = TRUE))
  ok <- is.null(attr(st, "status")) || attr(st, "status") == 0
  if (ok) cli::cli_alert_success("uploaded {key}")
  else cli::cli_alert_danger("FAILED {key}: {paste(tail(st, 3), collapse = ' ')}")
  ok
}

if (nzchar(Sys.getenv("CITIUS_SKIP_UPLOAD"))) {
  cli::cli_alert_info("CITIUS_SKIP_UPLOAD set - wrote to {.file {BLOG}} only.")
} else {
  ok <- vapply(names(artefacts), upload, logical(1))
  if (!all(ok)) {
    cli::cli_abort(c("{sum(!ok)} data upload{?s} failed - manifest NOT uploaded.",
                     i = "R2 still serves the previous run's manifest, so the section
                          stays self-consistent. Re-run once the cause is fixed."))
  }
  if (!upload("athletics-manifest.json")) cli::cli_abort("Manifest upload failed.")
}
