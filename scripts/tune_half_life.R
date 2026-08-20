# Refit the recency half-life against the objective we actually care about.
#
# fit_half_life() minimises MAE on predicting an athlete's *next single result*,
# which favours recency and returned ~180 days. But a half-life tuned for point
# prediction is not the one that best ranks a field: at 180 days most athletes
# end up with w_total below 1, so ability_se (= sigma / sqrt(w_total + kappa))
# runs at roughly twice sigma and dominates the simulation. The backtest showed
# the consequence directly — favourites given 0.392 won 0.459.
#
# This scores each candidate half-life on Brier skill over real finals, which is
# the loss the model is judged on. Still fully empirical; only the objective
# changes.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))

OUT <- here::here("citiusdata", "data")
CANDIDATES <- as.numeric(strsplit(Sys.getenv("CITIUS_HL_GRID", "180,365,730,1460,2920"), ",")[[1]])
N_MEETS <- .env_int("CITIUS_HL_MEETS", "30")
N_SIMS <- 4000L

# The 365 this script selected is now DEPLOYED$half_life, and DEPLOYED also
# carries per-family overrides (road = 1095, walk = 730) that this sweep never
# tested. The sweep itself is still sound -- every candidate shares one
# calibration, so a stale one biases all arms alike and the ARGMAX survives --
# but the absolute skill numbers do not describe the deployed model, and a rerun
# should use deployed_calibration(OUT) before any of them is requoted.
cli::cli_alert_warning(
  "Sweep runs on calibration.rds (2026-07-28), not the deployed calibration.")

champs      <- readRDS(file.path(OUT, "championship_results.rds"))
calibration <- readRDS(file.path(OUT, "calibration.rds"))

clean <- flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]
finals <- clean[!is.na(place) &
                  grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]

pool <- unique(finals[, .(competition_id, comp_start)])[!is.na(comp_start) &
                                                          comp_start >= as.Date("2018-01-01")]
setorder(pool, comp_start)
pool <- pool[round(seq(1, .N, length.out = min(N_MEETS, .N)))]
cli::cli_alert_info("Tuning over {nrow(pool)} meet{?s} x {length(CANDIDATES)} candidate{?s}.")

score_one <- function(hl) {
  preds <- list(); outs <- list()
  for (i in seq_len(nrow(pool))) {
    cid <- pool$competition_id[i]; cut_date <- pool$comp_start[i]
    block <- finals[competition_id == cid]
    # Cap history depth: beyond a decade adds nothing and costs a lot.
    past <- clean[date < cut_date & date >= cut_date - 3650]
    if (nrow(past) < 5000L) next
    ability <- estimate_ability(past, as_of = cut_date, half_life = hl,
                                calibration = calibration)
    for (ev in unique(block$event_id)) {
      field <- unique(block[event_id == ev], by = "athlete_id")
      ent <- ability[event_id == ev & athlete_id %in% as.character(field$athlete_id)]
      if (nrow(ent) < 4L) next
      mp <- medal_probs(simulate_event(ent, n_sims = N_SIMS,
                                       calibration = calibration, seed = 11L))
      key <- paste(cid, ev)
      mp[, race_id := key]
      preds[[length(preds) + 1L]] <- mp
      outs[[length(outs) + 1L]] <- data.table(
        race_id = key, athlete_id = mp$athlete_id,
        hit = mp$athlete_id %in% as.character(field[place == 1L]$athlete_id))
    }
  }
  if (!length(preds)) return(NULL)
  p <- rbindlist(preds, fill = TRUE); o <- rbindlist(outs, fill = TRUE)
  keep <- o[, .(wp = any(hit)), by = race_id][wp == TRUE]$race_id
  s <- score_predictions(p[race_id %in% keep], o[race_id %in% keep], "p_gold")
  fav <- merge(p[race_id %in% keep], o[race_id %in% keep],
               by = c("race_id", "athlete_id"))[, .SD[which.max(p_gold)], by = race_id]
  data.table(half_life = hl, skill = s$overall$brier_skill,
             brier = s$overall$brier, races = s$overall$n_races,
             fav_pred = mean(fav$p_gold), fav_won = mean(fav$hit))
}

res <- rbindlist(lapply(CANDIDATES, function(hl) {
  r <- score_one(hl)
  if (!is.null(r)) cli::cli_alert("  hl={hl}: skill {round(r$skill,3)}, fav {round(r$fav_pred,3)} vs {round(r$fav_won,3)}")
  r
}), fill = TRUE)

res[, calib_gap := fav_won - fav_pred]
setorder(res, -skill)
cli::cli_h2("Half-life tuned on ranking skill")
print(res)
saveRDS(res, file.path(OUT, "half_life_tuning.rds"))
