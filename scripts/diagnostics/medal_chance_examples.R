# Example rows for walking the medal-chance rating (citius#5) with Pete BEFORE
# it is published. Reads the frozen reference fields built by
# build_medal_reference.R, rates today's top 10 in a handful of events the same
# way the ratings page builds abilities (deployed_ability() on the corpus, active
# in the last 730 days), and writes one CSV to look through. Publishes nothing.
#
# Run from C:/dev/citiusverse:  Rscript citiusdata/scripts/diagnostics/medal_chance_examples.R

suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages({ library(data.table); library(arrow) })
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
OUT <- here::here("citiusdata", "data")

EVENTS <- c("AT-100Metres-M", "AT-100Metres-W", "AT-400MetresHurdles-M", "AT-1500Metres-W",
            "AT-HighJump-W", "AT-JavelinThrow-M", "AT-Marathon-M", "AT-35KilometresRaceWalk-M")
TOP <- 10L

ref <- setDT(read_parquet(file.path(OUT, "medal_reference_fields.parquet")))
calibration <- deployed_calibration(OUT)
clean_all <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))
clean_all <- clean_all[!is.na(event_id) & !is.na(perf)]
AS_OF <- max(clean_all$date, na.rm = TRUE)
# The whole corpus, not just EVENTS: neighbour_combine borrows from related
# events, so a filtered history would give different abilities from the page's.
ab <- deployed_ability(clean_all, as_of = AS_OF, calibration = calibration)
ab[, athlete_id := as.character(athlete_id)]
ab <- ab[event_id %in% EVENTS & !is.na(last_date) & as.numeric(AS_OF - last_date) <= 730]

nm <- unique(setDT(readRDS(file.path(OUT, "championship_results.rds")))[!is.na(athlete_name),
          .(athlete_id = as.character(athlete_id), athlete_name)])[, .(athlete_name = athlete_name[1]), by = athlete_id]
ev <- citius_events()[, .(event_id, orientation)]

rows <- rbindlist(lapply(EVENTS, function(e) {
  r <- ref[event_id == e]
  if (!nrow(r)) { cli::cli_alert_warning("{e}: no reference field"); return(NULL) }
  a <- ab[event_id == e][order(-ability)][seq_len(min(TOP, .N))]
  keep <- intersect(names(a), c("athlete_id", "event_id", "ability", "sigma", "ability_se", "ability_peak"))
  refc <- r[, intersect(names(r), keep), with = FALSE]
  p <- rating_medal_chance(a[, keep, with = FALSE], e, calibration = calibration, reference = refc,
                           n_sims = 5000L, context = list(meet_tier = "M1", round_class = "final"), seed = 1L)
  o <- ev[event_id == e, orientation]
  bronze_ref <- sort(r$ability, decreasing = TRUE)[3]
  a[, .(event_id, rank = seq_len(.N), athlete_id, ability = round(ability, 4),
        typical_mark = round(perf_to_mark(ability, o), 2),
        ability_se = round(ability_se, 4), last_date,
        medal_chance = round(p, 3),
        reference = r$reference_label[1], ref_finalists = nrow(r),
        ref_3rd_best_mark = round(perf_to_mark(bronze_ref, o), 2))]
}))
rows <- merge(rows, nm, by = "athlete_id", all.x = TRUE)
setcolorder(rows, c("event_id", "rank", "athlete_name"))
setorder(rows, event_id, rank)
f <- file.path(OUT, "medal_chance_examples_20261108.csv")
fwrite(rows, f)
print(rows[, .(event_id, rank, athlete_name, typical_mark, medal_chance, ref_3rd_best_mark)], nrows = 200)
cli::cli_alert_success("wrote {.file {f}} ({nrow(rows)} rows, as of {AS_OF})")
