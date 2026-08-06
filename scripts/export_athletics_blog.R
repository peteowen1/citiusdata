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
# perf = orientation * log(mark), and orientation is +/-1, so the inverse is
# exp(ability * orientation). Formatted HERE rather than in the page: the page
# would otherwise need the registry's orientation and the seconds/metres/points
# distinction, and a second implementation is a second thing to get wrong.
#
# Verified against known athletes before shipping: Duplantis 6.03 m, Mahuchikh
# 1.98 m, Skotheim 8,813 pts, a 2:08:29 marathon, a 1:56.3 800m.
#
# This is a TYPICAL mark, not a peak. The model forecasts a recency-weighted
# average and a championship final is closer to an athlete's best day, so these
# read slightly slow by design. The page says so.
orient <- as.data.table(citius_events())[, .(event_id, orientation, family)]
card <- merge(card, orient, by = "event_id", all.x = TRUE)
card[, pred_value := exp(ability * orientation)]
fmt_mark <- function(v, o) {
  if (!is.finite(v)) return(NA_character_)
  if (o < 0) {
    if (v < 60)   return(sprintf("%.2f", v))
    if (v < 3600) return(sprintf("%d:%05.2f", as.integer(v %/% 60), v %% 60))
    return(sprintf("%d:%02d:%02d", as.integer(v %/% 3600),
                   as.integer((v %% 3600) %/% 60), as.integer(round(v %% 60))))
  }
  if (v > 1000) return(format(round(v), big.mark = ","))
  sprintf("%.2f", v)
}
card[, pred_mark := mapply(fmt_mark, pred_value, orientation)]
card[, mark_unit := fifelse(orientation < 0, "", fifelse(pred_value > 1000, "pts", "m"))]
card[, c("orientation", "family", "pred_value") := NULL]
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

artefacts <- list(
  "calendar.parquet"                    = cal,
  "birmingham2026-predictions.parquet"  = card,
  "birmingham2026-rounds.parquet"       = lab,
  "birmingham2026-events.parquet"       = ev)

for (nm in names(artefacts)) {
  write_parquet(artefacts[[nm]], file.path(BLOG, nm))
  cli::cli_alert_success("{nm}: {nrow(artefacts[[nm]])} row{?s}")
}

# The manifest carries the freshness stamp the section trusts, so it is written
# and uploaded LAST and only if every data artefact landed. Publishing it
# unconditionally puts a brand-new "as at just now" over a card that failed to
# upload and is a run behind -- fresh-looking and wrong.
manifest <- list(
  generated_at = format(NOW, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  config = DEPLOYED$stamp,
  meets = nrow(cal),
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
