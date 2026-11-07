# Frozen reference fields for the medal-chance rating (citius#5).
#
# DECIDED 2026-10-27 (Pete, citiusverse DECISIONS c988bf3): the medal-chance
# rating is measured against a FIXED field, not "the top 8 right now", so a
# 2019 rating and a 2026 rating mean the same thing.
#   - Olympic events: the Paris 2024 Olympic final.
#   - Events not held at Paris: the Tokyo 2025 World Championships final.
#   - Each finalist's ability is frozen as of the day BEFORE their final, using
#     the deployed ability (deployed_ability(), the same call the ratings page
#     makes), and the table carries ability_se so estimation error stays on.
#   - Re-freeze after LA 2028 as a NEW labelled version; never edit this one.
#
# Output: data/medal_reference_fields.parquet, one row per finalist per event,
# with reference_label, frozen_with (the deployed calibration), frozen_on and the
# corpus end date / row count on every row. Pass rating_medal_chance() only the
# ability columns (athlete_id, event_id, ability, sigma, ability_se, plus any
# ability_peak the rated table also has): extra columns are dropped with a
# warning. Field sizes differ (8 on the track, 12 in the field, 80+ on the
# road), so medal chances are comparable within an event, not across events.
#
# Two corpus traps this handles (found 2026-11-07):
#   - Decathlon and heptathlon results are filed under the single-event ids
#     (AT-100Metres-M etc.) with round "Combined - Group"; counted as finals
#     they made the men's 100m "final" 30 athletes. Only Final / F rounds count.
#   - Finals harvested by both routes appear as "Final" (competition) and "F"
#     (career); finalists are de-duplicated by athlete.
#
# Cost: deployed_ability() runs on the full corpus once per distinct final date
# (~20 dates). CITIUS_REF_EVENTS=<id,id> limits the run to those events, for
# testing; the abilities then differ slightly from a full run because
# neighbour_combine borrows from related events, so a limited run never writes
# the real output file.

suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages({ library(data.table); library(arrow) })
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
OUT <- here::here("citiusdata", "data")

REFS <- list(
  list(key = "paris2024", label = "Paris 2024 Olympic final", comp = "^The XXXIII Olympic Games$",
       from = as.Date("2024-07-30"), to = as.Date("2024-08-12")),
  list(key = "tokyo2025", label = "Tokyo 2025 World Championships final",
       comp = "World Athletics Championships", from = as.Date("2025-09-12"), to = as.Date("2025-09-22"))
)
LIMIT <- strsplit(Sys.getenv("CITIUS_REF_EVENTS"), ",", fixed = TRUE)[[1]]
LIMIT <- LIMIT[nzchar(LIMIT)]
FINAL_F <- file.path(OUT, "medal_reference_fields.parquet")
# Checked BEFORE ~100 min of compute, not after (review 2026-11-08).
if (!length(LIMIT) && file.exists(FINAL_F)) {
  cli::cli_abort(c("{.file {FINAL_F}} exists. A frozen reference is never rewritten.",
                   i = "A re-freeze is a new labelled version: give it a new file name."))
}
# Paris 2024 held 42 individual athletics events (21 men's, 21 women's,
# including both combined events, the marathons and the 20 km walks). Fewer
# means a final was missed (another round label, a missing comp_name) and the
# event would have fallen through to Tokyo or vanished without an error.
N_PARIS_EXPECTED <- 42L
# One file per final date, reused on a restart, so a run killed late keeps
# what it did. Cleared by hand once the real output is written.
SCRATCH <- file.path(OUT, if (length(LIMIT)) "medal_ref_scratch_TEST" else "medal_ref_scratch")
dir.create(SCRATCH, showWarnings = FALSE)

corpus <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
CORPUS_ROWS <- nrow(corpus); CORPUS_MAX <- max(corpus$date, na.rm = TRUE)
cli::cli_alert_info("corpus: {format(CORPUS_ROWS, big.mark = ',')} rows, {min(corpus$date, na.rm = TRUE)} to {CORPUS_MAX}")

# Any other "final"-like round label at either Games is reported: the filter
# below counts only Final / F, so a variant ("Final A", ...) would be missed.
for (r in REFS) {
  lab <- corpus[date >= r$from & date <= r$to & grepl(r$comp, comp_name) &
                  grepl("final", round, ignore.case = TRUE) & !grepl("semi", round, ignore.case = TRUE),
                .N, by = round]
  cli::cli_alert_info("{r$label}: final-like round labels {paste0(lab$round, ' (', lab$N, ')', collapse = ', ')}")
  other <- setdiff(lab$round, c("Final", "F"))
  if (length(other)) cli::cli_alert_warning("{r$label}: final-like labels NOT counted: {.val {other}}")
}

# --- finalists ---------------------------------------------------------------
finals <- rbindlist(lapply(REFS, function(r) {
  x <- corpus[date >= r$from & date <= r$to & grepl(r$comp, comp_name) &
                round %in% c("Final", "F") & !is.na(event_id) & !is.na(athlete_id)]
  if (!nrow(x)) cli::cli_abort("No final rows found for {r$label}: check comp_name and dates.")
  x <- unique(x[, .(athlete_id = as.character(athlete_id), event_id, final_date = date)])
  # One final per event: a later date for the same event would be a second
  # race, and taking it silently would mix two fields.
  nd <- x[, uniqueN(final_date), by = event_id][V1 > 1]
  if (nrow(nd)) cli::cli_abort("{r$label}: events with finals on more than one date: {.val {nd$event_id}}")
  x[, `:=`(reference = r$key, reference_label = r$label)]
  unique(x, by = c("event_id", "athlete_id"))
}))
# Paris wins where an event was held at both.
finals <- finals[!(reference == "tokyo2025" & event_id %in% finals[reference == "paris2024", unique(event_id)])]
if (length(LIMIT)) finals <- finals[event_id %in% LIMIT]
if (!nrow(finals)) cli::cli_abort("No finalists left (CITIUS_REF_EVENTS = {.val {LIMIT}}?)")
size <- finals[, .N, by = .(reference, event_id)]
cli::cli_alert_info("{nrow(size)} event{?s}: {sum(size$reference == 'paris2024')} Paris, {sum(size$reference == 'tokyo2025')} Tokyo; finalists per event {min(size$N)}-{max(size$N)}")
if (!length(LIMIT)) {
  n_paris <- size[reference == "paris2024", .N]
  if (n_paris != N_PARIS_EXPECTED) {
    cli::cli_abort("Paris 2024: {n_paris} events with a final, expected {N_PARIS_EXPECTED}. A final was missed or an extra event matched: {.val {sort(size[reference == 'paris2024', event_id])}}")
  }
  cli::cli_alert_info("Tokyo-only events: {.val {size[reference == 'tokyo2025', event_id]}}")
}
small <- size[N < 4]
if (nrow(small)) cli::cli_abort("Finals with under 4 athletes (rating_medal_chance needs 4): {.val {small$event_id}}")

# --- abilities frozen the day before each final --------------------------------
calibration <- deployed_calibration(OUT)
clean_all <- flag_implausible(corpus)
clean_all <- clean_all[!is.na(event_id) & !is.na(perf)]
rm(corpus); invisible(gc())
dates <- sort(unique(finals$final_date))
cli::cli_alert_info("{length(dates)} final date{?s} to freeze at")
frozen <- rbindlist(lapply(seq_along(dates), function(i) {
  d <- dates[i]
  t0 <- Sys.time()
  ev <- finals[final_date == d, unique(event_id)]
  sf <- file.path(SCRATCH, paste0(format(d), ".parquet"))
  if (file.exists(sf)) {
    out <- setDT(read_parquet(sf))
    # A scratch file from a different finalist list (another LIMIT, a fix to
    # the filter) must not be reused.
    if (!setequal(paste(out$event_id, out$athlete_id), finals[final_date == d, paste(event_id, athlete_id)])) {
      cli::cli_abort("{.file {sf}} holds a different finalist list; delete {.file {SCRATCH}} and rerun.")
    }
    cli::cli_alert_info("{i}/{length(dates)} {d}: reused {.file {basename(sf)}}")
    return(out)
  }
  ab <- deployed_ability(clean_all[date < d], as_of = d - 1L, calibration = calibration)
  ab[, athlete_id := as.character(athlete_id)]
  out <- merge(finals[final_date == d], ab[event_id %in% ev], by = c("athlete_id", "event_id"), all.x = TRUE)
  rm(ab); invisible(gc())
  write_parquet(out, paste0(sf, ".tmp")); file.rename(paste0(sf, ".tmp"), sf)
  cli::cli_alert_success("{i}/{length(dates)} {d}: {length(ev)} event{?s}, {nrow(out)} finalists, {sum(is.na(out$ability))} with no ability ({round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 1)} min)")
  out
}), fill = TRUE)

# --- checks --------------------------------------------------------------------
# A finalist with no ability (no prior legal mark in that event) cannot be part
# of the field. Say how many, per event, and refuse an event left under 4.
miss <- frozen[is.na(ability) | is.na(sigma) | is.na(ability_se), .N, by = event_id]
if (nrow(miss)) cli::cli_alert_warning("finalists with no ability, dropped: {paste0(miss$event_id, ' ', miss$N, collapse = '; ')}")
frozen <- frozen[!is.na(ability) & !is.na(sigma) & !is.na(ability_se)]
left <- frozen[, .N, by = event_id][N < 4]
if (nrow(left)) cli::cli_abort("Events under 4 rated finalists after dropping: {.val {left$event_id}}")
stopifnot(!anyDuplicated(frozen[, .(event_id, athlete_id)]))
frozen[, frozen_with := DEPLOYED$calibration]
frozen[, frozen_on := Sys.Date()]
frozen[, `:=`(corpus_max_date = CORPUS_MAX, corpus_rows = CORPUS_ROWS)]
data.table::setcolorder(frozen, c("reference", "reference_label", "event_id", "athlete_id", "final_date",
                                  "ability", "sigma", "ability_se"))
print(frozen[, .(finalists = .N, ability_min = round(min(ability), 4), ability_max = round(max(ability), 4),
                 se_median = round(median(ability_se), 4)), by = .(reference, event_id)][order(reference, event_id)])

if (length(LIMIT)) {
  f <- file.path(tempdir(), "medal_reference_fields_TEST.parquet")
  write_parquet(frozen, f)
  cli::cli_alert_info("CITIUS_REF_EVENTS set: wrote a TEST copy only, {.file {f}}")
} else {
  f <- FINAL_F
  if (file.exists(f)) cli::cli_abort("{.file {f}} appeared during the run; not overwriting it.")
  write_parquet(frozen, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f)
  cli::cli_alert_success("wrote {.file {f}}: {nrow(frozen)} rows, {uniqueN(frozen$event_id)} events. Delete {.file {SCRATCH}} once checked.")
}
