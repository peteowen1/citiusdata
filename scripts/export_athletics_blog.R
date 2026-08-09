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

VERSE <- "C:/dev/citiusverse"
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(arrow); library(jsonlite)
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))

D      <- file.path(VERSE, "citiusdata", "data")
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
stopifnot(
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
KEEP <- c("event_id", "discipline", "sex", "athlete_id", "athlete", "nation",
          "p_gold", "p_medal", "p_final", "p_reach_r2", "p_reach_r3",
          "ability", "sigma", "ability_se", "n_rounds", "field_modelled",
          "generated_at", "cutoff", "config", "counts_source",
          "combined_rows_excluded")
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

manifest <- list(
  generated_at = format(NOW, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  config = DEPLOYED$stamp,
  meets = nrow(cal),
  event_notes = EVENT_NOTES,
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
