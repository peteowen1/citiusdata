# Rebuild athletics calibration and backtest from the full competition harvest.
#
# History now comes from competition results rather than per-athlete fetches.
# That is a better source in every respect: whole fields (so shared race effects
# are identifiable), no-marks retained (so foul rates are measurable), and
# coverage of 85k athletes rather than 643 — coverage being what capped the
# earlier backtest at 78%.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache")
N_SIMS <- 10000L
MAX_BACKTEST_COMPS <- as.integer(Sys.getenv("CITIUS_BACKTEST_COMPS", "200"))

champs <- rbindlist(lapply(list.files(CACHE, full.names = TRUE), readRDS),
                    use.names = TRUE, fill = TRUE)
champs <- champs[!is.na(date)]
saveRDS(champs, file.path(OUT, "championship_results.rds"))
cli::cli_alert_success(
  "{format(nrow(champs), big.mark=',')} results | {uniqueN(champs$competition_id)} meets | {format(uniqueN(champs$athlete_id), big.mark=',')} athletes"
)

# --- calibration -------------------------------------------------------------
clean <- flag_implausible(champs)
calibration <- calibrate(clean, min_races = 30L)
half_life <- fit_half_life(clean[!is.na(perf) & !is.na(event_id)])
aging <- fit_aging_curve(clean[!is.na(perf) & !is.na(event_id)])

saveRDS(calibration, file.path(OUT, "calibration.rds"))
saveRDS(half_life, file.path(OUT, "half_life.rds"))
saveRDS(aging, file.path(OUT, "aging.rds"))

cli::cli_h2("Calibration")
cat("tail_df:", calibration$tail_df, "| events calibrated:",
    sum(calibration$events$calibrated, na.rm = TRUE), "\n")
cat("\nhalf-lives:\n"); print(half_life)
cat("\nround:\n"); print(calibration$round)
cat("\ntier:\n"); print(calibration$tier)
cat("\naging peaks:\n"); print(aging$peaks)

# --- backtest ----------------------------------------------------------------
# Ability is re-estimated per competition from strictly prior results, so the
# meet being predicted cannot inform its own forecast. That is the expensive
# part, hence the cap: competitions are sampled across tiers and years rather
# than taking the most recent, so the sample is not all one era.
finals <- clean[!is.na(event_id) & !is.na(place) & !is.na(perf) &
                  grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]

pool <- unique(finals[, .(competition_id, comp_start, comp_tier)])
pool <- pool[!is.na(comp_start) & comp_start >= as.Date("2015-01-01")]
setorder(pool, comp_start)
if (nrow(pool) > MAX_BACKTEST_COMPS) {
  pool <- pool[round(seq(1, .N, length.out = MAX_BACKTEST_COMPS))]
}
cli::cli_alert_info("Backtesting {nrow(pool)} meet{?s}.")

all_pred <- list(); all_out <- list()
for (i in seq_len(nrow(pool))) {
  cid <- pool$competition_id[i]
  cut_date <- pool$comp_start[i]
  block <- finals[competition_id == cid]

  past <- clean[date < cut_date & !is.na(perf) & !is.na(event_id)]
  if (nrow(past) < 5000L) next
  ability <- estimate_ability(past, as_of = cut_date, half_life = half_life,
                              calibration = calibration)

  for (ev in unique(block$event_id)) {
    field <- unique(block[event_id == ev], by = "athlete_id")
    entrants <- ability[event_id == ev &
                          athlete_id %in% as.character(field$athlete_id)]
    if (nrow(entrants) < 4L) next

    sim <- simulate_event(entrants, n_sims = N_SIMS,
                          calibration = calibration, seed = 11L)
    mp <- medal_probs(sim)
    key <- paste(cid, ev)
    mp[, race_id := key]
    all_pred[[length(all_pred) + 1L]] <- mp
    all_out[[length(all_out) + 1L]] <- data.table(
      race_id = key, athlete_id = mp$athlete_id,
      hit = mp$athlete_id %in% as.character(field[place == 1L]$athlete_id),
      hit_medal = mp$athlete_id %in% as.character(field[place <= 3L]$athlete_id))
  }
  if (i %% 25 == 0) cli::cli_alert("  backtest {i}/{nrow(pool)}")
}

pred <- rbindlist(all_pred, fill = TRUE)
outc <- rbindlist(all_out, fill = TRUE)
cov <- outc[, .(wp = any(hit)), by = race_id]
keep <- cov[wp == TRUE]$race_id
cli::cli_alert_info(
  "{nrow(cov)} race{?s}; winner in field for {length(keep)} ({round(100*length(keep)/nrow(cov))}%)."
)

gold <- score_predictions(pred[race_id %in% keep], outc[race_id %in% keep], "p_gold")
medal <- score_predictions(pred[race_id %in% keep],
                           outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")

cli::cli_h2("Backtest (winner-in-field)")
cat(sprintf("gold  brier %.4f vs %.4f  skill %+.3f  (%d races)\n",
            gold$overall$brier, gold$overall$brier_baseline,
            gold$overall$brier_skill, gold$overall$n_races))
cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
            medal$overall$brier, medal$overall$brier_baseline, medal$overall$brier_skill))
cat("\nreliability:\n"); print(gold$reliability[n >= 20])
br <- gold$by_race
cat(sprintf("\nraces beating baseline: %d of %d (%.0f%%)\n",
            sum(br$skill > 0), nrow(br), 100 * mean(br$skill > 0)))

saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc),
        file.path(OUT, "backtest.rds"))
