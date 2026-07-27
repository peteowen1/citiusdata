# Out-of-sample backtest against completed championships.
#
# The point is to answer "do these probabilities mean anything", which nothing
# else in the package does. Estimators recovering planted parameters proves the
# maths; only scoring against real outcomes proves the forecast.
#
# Two rules make this a genuine test rather than a flattering one:
#
#  1. **Strict temporal cut.** Ability is estimated using only performances
#     dated before the championship began. Any leakage of the result being
#     predicted turns this into an exercise in reading the answer.
#  2. **Real fields.** Entrants are the athletes who actually contested the
#     final, taken from the results themselves. This isolates model quality from
#     field-projection error, which is a separate problem.
#
# Skill is measured against a uniform-within-event baseline. Beating it means
# the model has learned who is fast; failing to means it has not, whatever the
# absolute Brier score looks like.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
N_SIMS <- 20000L

history     <- readRDS(file.path(OUT, "athletics_history.rds"))
calibration <- readRDS(file.path(OUT, "calibration.rds"))
half_life   <- readRDS(file.path(OUT, "half_life.rds"))
champs      <- readRDS(file.path(OUT, "championship_results.rds"))

finals_all <- champs[!is.na(event_id) & !is.na(place) & !is.na(comp_start) &
                       grepl("final", round, ignore.case = TRUE) &
                       !grepl("semi", round, ignore.case = TRUE)]
cli::cli_alert_info("{uniqueN(finals_all$competition_id)} competition{?s} with finals.")

all_pred <- list(); all_out <- list()

# Ability is re-estimated once per competition, from performances strictly
# before that competition started. Re-estimating per competition (rather than
# once globally) is the whole point: a single global fit would let every
# championship see every other championship's results.
for (cid in unique(finals_all$competition_id)) {
  block <- finals_all[competition_id == cid]
  cut_date <- min(block$comp_start, na.rm = TRUE)
  ch_name <- data.table::first(block$comp_name)

  past <- history[date < cut_date & !is.na(perf) & !is.na(event_id)]
  if (nrow(past) < 500L) next
  ability <- estimate_ability(past, as_of = cut_date, half_life = half_life,
                              calibration = calibration)

  for (ev in unique(block$event_id)) {
    field <- block[event_id == ev]
    if (anyDuplicated(field$athlete_id)) field <- unique(field, by = "athlete_id")
    entrants <- ability[event_id == ev &
                          athlete_id %in% as.character(field$athlete_id)]
    if (nrow(entrants) < 4L) next          # too little pre-race history to test

    sim <- simulate_event(entrants, n_sims = N_SIMS,
                          calibration = calibration, seed = 11L)
    mp <- medal_probs(sim)
    key <- paste(cid, ev)
    mp[, `:=`(race_id = key, championship = ch_name, cut_date = cut_date)]
    all_pred[[length(all_pred) + 1L]] <- mp

    won <- field[place == 1L]$athlete_id
    medalled <- field[place <= 3L]$athlete_id
    all_out[[length(all_out) + 1L]] <- data.table(
      race_id = key, championship = ch_name,
      athlete_id = mp$athlete_id,
      hit = mp$athlete_id %in% as.character(won),
      hit_medal = mp$athlete_id %in% as.character(medalled))
  }
}

pred <- rbindlist(all_pred, fill = TRUE)
outc <- rbindlist(all_out, fill = TRUE)

gold <- score_predictions(pred, outc, prob_col = "p_gold")
medal <- score_predictions(pred, outc[, .(race_id, athlete_id, hit = hit_medal)],
                           prob_col = "p_medal")

cli::cli_h2("Gold")
str(gold$overall)
print(gold$reliability)
cli::cli_h2("Medal")
str(medal$overall)

saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc),
        file.path(OUT, "backtest.rds"))
