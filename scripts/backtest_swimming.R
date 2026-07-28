# Swimming calibration and out-of-sample backtest.
#
# Mirrors backtest_championships.R: ability is re-estimated per competition from
# swims dated strictly before it began, and entrants are the athletes who
# actually contested the final. Swimming has a stable athlete key (World
# Aquatics PersonId), so cross-meet linking is exact rather than name-matched.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
N_SIMS <- 20000L

# Input is switchable so the multi-source corpus can be compared against the
# single-source history with nothing else changed. Any other difference between
# the two runs would be uninterpretable.
HISTORY <- Sys.getenv("CITIUS_SWIM_HISTORY", "swimming_history.rds")
history <- readRDS(file.path(OUT, HISTORY))
cli::cli_alert_info("Input: {HISTORY}")
history <- history[!is.na(event_id) & !is.na(athlete_id)]
cli::cli_alert_info(
  "{nrow(history)} swim{?s}, {uniqueN(history$athlete_id)} athlete{?s}, {uniqueN(history$competition_id)} competition{?s}."
)

# --- calibration -------------------------------------------------------------
clean <- flag_implausible(history)
calibration <- calibrate(clean, min_races = 10L)
# Swimming uses 180 days, NOT the 730 tuned for athletics. Validated on 895
# races: 180 beats 730 on every measure (gold skill 0.253 vs 0.234, mean
# reliability gap 0.035 vs 0.046, 70% vs 67% of races beating baseline).
#
# The half-life is not a global constant or even a sport constant - it tracks
# how often athletes in a discipline actually compete. Swimming's World Cup
# circuit means a 180-day window still holds ample evidence, so shortening it
# buys recency for free. Athletics is sparser: at 180 days most athletes fall
# below w_total = 1, ability_se balloons to ~2x sigma, and favourites get
# under-rated. Do not copy a tuned value across sports.
half_life_fitted <- fit_half_life(clean[!is.na(perf)])
half_life <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "180"))
cat("\nfitted (next-result MAE) vs used (ranking-tuned):\n")
print(half_life_fitted); cat("using:", half_life, "days\n")

saveRDS(calibration, file.path(OUT, "calibration_swimming.rds"))
saveRDS(half_life, file.path(OUT, "half_life_swimming.rds"))

cli::cli_h2("Swimming calibration")
cat("tail_df:", calibration$tail_df, "\n")
print(calibration$events[calibrated == TRUE][order(-n_races)][1:10,
  .(event_id, sigma_within = round(sigma_within, 4),
    condition_sd = round(condition_sd, 4), cond_share = round(cond_share, 2),
    n_races)])
cat("\nfitted half-lives:\n"); print(half_life)
cat("\nround context:\n"); print(calibration$round)

# --- backtest ----------------------------------------------------------------
# World Aquatics labels the round "Finals" (plural); "Semifinals" must not match.
finals <- clean[!is.na(place) & !is.na(perf) &
                  grepl("^finals?$", round, ignore.case = TRUE)]
cli::cli_alert_info("{uniqueN(finals$competition_id)} competition{?s} with finals.")

all_pred <- list(); all_out <- list()
for (cid in unique(finals$competition_id)) {
  block <- finals[competition_id == cid]
  cut_date <- min(block$comp_start, na.rm = TRUE)

  past <- clean[comp_start < cut_date & !is.na(perf)]
  if (nrow(past) < 500L) next
  ability <- estimate_ability(past, as_of = cut_date, half_life = half_life,
                              calibration = calibration)

  # Keyed by competition+event, and that is CORRECT for swimming even though
  # athletics needs race_key. The rule is how PLACINGS are assigned, and the two
  # sports genuinely differ:
  #
  #   swimming  115 of 116 multi-section groups rank GLOBALLY (one winner across
  #             all heats -- a distance "timed final" is swum in heats but
  #             placed on time overall)
  #   athletics 2,462 of 2,545 rank PER SECTION (each gala section has its own
  #             place 1)
  #
  # Keying swimming per race_key gives 4.5% multi-winner races and 11.1% with NO
  # winner, because it splits a timed final into heats. Per competition+event it
  # is 0.3% and 0.0%. Do not copy the athletics fix across.
  #
  # Note the asymmetry: race_key is still the right unit for the SHARED SHOCK in
  # decompose_races(), since those heats were swum separately in their own
  # conditions. Scoring unit and conditions unit are not the same thing.
  for (ev in unique(block$event_id)) {
    field <- unique(block[event_id == ev], by = "athlete_id")
    entrants <- ability[event_id == ev & athlete_id %in% field$athlete_id]
    if (nrow(entrants) < 4L) next

    sim <- simulate_event(entrants, n_sims = N_SIMS,
                          calibration = calibration, seed = 11L)
    mp <- medal_probs(sim)
    key <- paste(cid, ev)
    mp[, race_id := key]
    all_pred[[length(all_pred) + 1L]] <- mp

    all_out[[length(all_out) + 1L]] <- data.table(
      race_id = key, athlete_id = mp$athlete_id,
      hit = mp$athlete_id %in% field[place == 1L]$athlete_id,
      hit_medal = mp$athlete_id %in% field[place <= 3L]$athlete_id)
  }
}

pred <- rbindlist(all_pred, fill = TRUE)
outc <- rbindlist(all_out, fill = TRUE)

cov <- outc[, .(winner_present = any(hit)), by = race_id]
keep <- cov[winner_present == TRUE]$race_id
cli::cli_alert_info("{nrow(cov)} event{?s}; winner in field for {length(keep)}.")

gold <- score_predictions(pred[race_id %in% keep], outc[race_id %in% keep], "p_gold")
medal <- score_predictions(pred[race_id %in% keep],
                           outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")

cli::cli_h2("Swimming backtest (winner-in-field)")
cat(sprintf("gold  brier %.4f vs %.4f  skill %+.3f\n",
            gold$overall$brier, gold$overall$brier_baseline, gold$overall$brier_skill))
cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
            medal$overall$brier, medal$overall$brier_baseline, medal$overall$brier_skill))
print(gold$reliability[n >= 10])

saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc),
        file.path(OUT, "backtest_swimming.rds"))
