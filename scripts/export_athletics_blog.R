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
pred <- setDT(readRDS(file.path(D, "birmingham2026_pretournament.rds")))
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
# intersect() below means a column absent for a given meet just doesn't appear:
# Birmingham gains field_type = "official_entry_list" and skips the rest.
KEEP <- c("event_id", "discipline", "sex", "athlete_id", "athlete", "nation",
          "p_gold", "p_medal", "p_final", "p_reach_r2", "p_reach_r3",
          "ability", "sigma", "ability_se", "n_rounds", "field_modelled",
          "generated_at", "cutoff", "config", "counts_source",
          "combined_rows_excluded",
          "field_type", "field_source", "field_entrants", "field_unmodelled")
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
orient <- as.data.table(citius_events())[, .(event_id, orientation, family)]
card <- merge(card, orient, by = "event_id", all.x = TRUE)
card[, c("pred_mark", "mark_unit") := predicted_mark(ability, orientation)]
card[, c("orientation", "family") := NULL]
stopifnot("every predicted mark must format" = !any(is.na(card$pred_mark)))
cli::cli_alert_success("Predicted marks formatted for all {nrow(card)} rows.")

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
DL_MEETS <- c("brussels2026")   # budapest2026 joins this once its card is built

for (mid in DL_MEETS) {
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

# Mark calibration, read rather than asserted. form_display_marks.R measures how
# often each displayed mark is actually beaten and writes it beside the parquet;
# the caveat sentence is built from that number so the page cannot drift from
# what was measured. A missing file is a hard stop, not a silent default: a page
# that quietly loses its calibration caveat is worse than one that fails to build.
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

manifest <- list(
  generated_at = format(NOW, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  config = DEPLOYED$stamp,
  meets = nrow(cal),
  event_notes = EVENT_NOTES,
  mark_calibration = CALIB,
  birmingham = list(
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
