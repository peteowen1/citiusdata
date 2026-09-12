# derive_athlete_ids_from_results.R
#
# Backfill helper (2026-09-11): for a Diamond League meet with no pre-meet
# entries file (lausanne2026/silesia2026/zurich2026 -- the model was never
# run at the time, so nobody captured a start list), the actual field is
# already known from the harvested results (championship_results.rds), with
# authoritative athlete_id and event_id -- no name-fuzzy-matching needed at
# all, which makes this MORE accurate than the standard
# resolve_diamond_league_athletes.R path (that script exists specifically
# because its input, a third-party entries list, has name+country only).
#
# Writes <meet>_athlete_ids.csv in the exact shape
# resolve_diamond_league_athletes.R produces, so predict_diamond_league_final.R
# (which only reads that file) needs no changes.
#
# Usage: Rscript derive_athlete_ids_from_results.R <meet_id> <competition_id> [exclude_athlete_ids_csv]
#   exclude_athlete_ids_csv: optional comma-separated athlete_ids to drop --
#   for entrants predict_diamond_league_final.R's own entrant-accounting
#   flags as "unexplained" (has history/ability but its internal accounting
#   doesn't place them in the final card, for a reason its own comment says
#   to "eyeball" rather than treat as a hard error).

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) cli::cli_abort("Usage: derive_athlete_ids_from_results.R <meet_id> <competition_id> [exclude_athlete_ids_csv]")
MEET <- args[[1]]
COMP_ID <- args[[2]]
DROP <- if (length(args) >= 3 && nzchar(args[[3]])) strsplit(args[[3]], ",")[[1]] else character(0)

ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
# Diamond Discipline ONLY -- a Diamond League meet day also runs Promotional/
# National/U20/U18 undercard races at the same venue under the same
# competition_id (confirmed on Lausanne: unfiltered pulled in junior 300m
# heats etc., which predict_diamond_league_final.R correctly refused to
# publish with "6 entrants unaccounted for" since they have no place in an
# elite-ability model). Filtering to event_name == "Diamond Discipline"
# reproduces the calendar's own documented event count exactly (14 for
# Lausanne, matching athletics_calendar.csv's "14 disciplines" note).
sub <- ch[competition_id == COMP_ID & event_name == "Diamond Discipline"]
if (!nrow(sub)) cli::cli_abort("No championship_results Diamond Discipline rows for competition_id {COMP_ID} ({MEET}).")

# One row per (athlete_id, event_id) -- a heat + final for the same athlete
# in the same event must not double-count as two field entries.
ids <- unique(sub[, .(athlete_id, event_id, athlete = athlete_name, sex = sex_code)])

meta_path <- file.path(D, "athlete_meta.parquet")
if (file.exists(meta_path)) {
  meta <- setDT(arrow::read_parquet(meta_path, col_select = c("athlete_id", "country")))
  ids <- merge(ids, meta, by = "athlete_id", all.x = TRUE)
} else {
  cli::cli_warn("athlete_meta.parquet not found -- country left blank.")
  ids[, country := NA_character_]
}

n_no_country <- ids[is.na(country) | !nzchar(country), .N]
if (n_no_country) cli::cli_alert_warning("{n_no_country} athlete(s) with no country on file.")

out <- ids[, .(
  row = .I,
  athlete, country,
  event = event_id,      # already canonical; no free-text event label to keep
  event_id,
  match_tier = "from_results",   # not name-matched: athlete_id came directly
                                  # from the harvested result row
  aid = athlete_id,
  n_ev = NA_integer_, n_all = NA_integer_, n_cand = 1L,
  athlete_id
)]

if (length(DROP)) {
  dropped <- out[athlete_id %in% DROP]
  out <- out[!athlete_id %in% DROP]
  cli::cli_alert_info("Dropped {nrow(dropped)} manually-excluded entrant(s): {paste(DROP, collapse=', ')}")
  # Audit trail: `data/` is gitignored and this is a one-off shell arg, so
  # without a sidecar file the exclusion has no durable record anywhere a
  # future reader could recover it -- unlike every OTHER excluded-entrant
  # class here (no_history_in_event, unexplained), which already lands in
  # <meet>_unmodelled_entrants.csv via predict_diamond_league_final.R's own
  # entrant accounting. This is that same idea, one step earlier.
  excl_path <- file.path(D, paste0(MEET, "_manually_excluded.csv"))
  fwrite(dropped[, .(athlete_id, athlete, event_id, country)], excl_path)
  cli::cli_alert_info("Manual exclusions recorded at {.file {excl_path}}")
}

out_path <- file.path(D, paste0(MEET, "_athlete_ids.csv"))
fwrite(out, out_path)
cli::cli_alert_success("{nrow(out)} field entries across {uniqueN(out$event_id)} events written to {.file {out_path}}")
