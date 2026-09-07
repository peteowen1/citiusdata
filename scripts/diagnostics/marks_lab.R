# MARKS LAB: score predicted marks per event, with no simulation.
#
# WHY. 18 of 54 events beat the last-5 baseline on marks and that is the
# binding constraint on the launch gate. A full arm costs ~2.5h, of which the
# simulation is ~4% and the per-meet ability refit is ~96%. But `median_mark`
# is analytically `ability` (every additive term in simulate_event()'s draw is
# independent, zero-mean and symmetric, which backtest_athletics.R's MARKS_ONLY
# path already relies on), so a marks-only question needs no simulation at all,
# and a monthly as-of refit costs minutes instead of hours.
#
# WHAT IT DOES. For a window of T1_elite finals: refit ability at the start of
# each month from marks strictly before it, predict each entrant's mark, and
# score MAE per event against the last-5 baseline computed on the same rows.
#
# THE BASELINE IS THE FRAMEWORK'S, ANALYTICALLY. score_goal_by_event.R rolls
# the athlete's last 5 prior oriented performances through the simulator at the
# measured sigma_within; the median of that draw is its location, so the mean
# of those five performances is the same number without the Monte Carlo. The
# `verify` step below checks this whole pipeline against a real arm's stored
# median_mark on the same rows before any conclusion is drawn from it.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_lab.R'
# Env:
#   CITIUS_LAB_FROM / _TO   test window (default 2024-01-01 / 2026-09-01)
#   CITIUS_LAB_HL           half-life override, "family=days,..." (default: DEPLOYED)
#   CITIUS_LAB_HL_GLOBAL    global half-life (default DEPLOYED$half_life)
#   CITIUS_LAB_CAL          calibration (default DEPLOYED$calibration)
#   CITIUS_LAB_TAG          suffix for the output csv
#   CITIUS_LAB_VERIFY       arm to check against (default backtest_strip_fullsim.rds)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT  <- here::here("citiusdata", "data")
FROM <- as.Date(Sys.getenv("CITIUS_LAB_FROM", "2024-01-01"))
TO   <- as.Date(Sys.getenv("CITIUS_LAB_TO", "2026-09-01"))
CAL  <- Sys.getenv("CITIUS_LAB_CAL", DEPLOYED$calibration)
TAG  <- Sys.getenv("CITIUS_LAB_TAG", "")
VERIFY <- Sys.getenv("CITIUS_LAB_VERIFY", "backtest_strip_fullsim.rds")
HL_GLOBAL <- as.numeric(Sys.getenv("CITIUS_LAB_HL_GLOBAL", DEPLOYED$half_life))
hl_map <- DEPLOYED$hl_family
if (nzchar(Sys.getenv("CITIUS_LAB_HL", ""))) {
  kv <- strsplit(trimws(strsplit(Sys.getenv("CITIUS_LAB_HL"), ",")[[1]]), "=")
  for (p in kv) hl_map[[trimws(p[1])]] <- as.numeric(p[2])
}
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
cal <- readRDS(file.path(OUT, CAL))
say("window %s..%s | calibration %s | global half-life %g | family %s",
    format(FROM), format(TO), CAL, HL_GLOBAL,
    paste(names(hl_map), unlist(hl_map), sep = "=", collapse = ","))

# --- the test set: T1_elite finals, as the goal metric defines them ----------
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
ch[, competition_id := as.character(competition_id)]
test <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0 & !is.na(event_id),
           .(race_key, athlete_id, event_id, date = as.Date(date), competition_id, round, mark)]
test <- unique(test, by = c("race_key", "athlete_id"))
ct <- as.data.table(open_dataset(file.path(OUT, "competition_catalogue.parquet")) |>
                      dplyr::select(competition_id, meet_tier) |> dplyr::collect())
ct[, competition_id := as.character(competition_id)]
test <- merge(test, unique(ct[!is.na(meet_tier)], by = "competition_id"), by = "competition_id")
test <- test[meet_tier == "T1_elite" & date >= FROM & date < TO]
test <- test[grepl("final", tolower(round)) & !grepl("semi|quarter", tolower(round))]
reg <- as.data.table(citius_events())[, .(event_id, orientation, family, discipline = event_id, sex)]
test <- merge(test, reg[, .(event_id, orientation, family, sex)], by = "event_id")
test[, act := orientation * log(mark)]
test <- test[is.finite(act)]
say("test set: %s rows, %s races, %d events, %s meets",
    format(nrow(test), big.mark = ","), format(uniqueN(test$race_key), big.mark = ","),
    uniqueN(test$event_id), format(uniqueN(test$competition_id), big.mark = ","))
stopifnot(nrow(test) > 1000)

# --- history, once ----------------------------------------------------------
store <- file.path(OUT, "athletics_corpus_store")
cols <- intersect(c("athlete_id", "event_id", "date", "perf", "age", "round", "tier", "meet_tier",
                    "competition_id", "race_key", "wind", "momentum", "indoor", "venue_country"),
                  names(open_dataset(store)))
EVS <- unique(test$event_id)
h <- as.data.table(read_results_store(store, events = EVS, from = FROM - 365 * 12, to = TO, columns = cols))
h <- flag_implausible(h)[is.finite(perf)]
h[, athlete_id := as.character(athlete_id)]; h[, date := as.Date(date)]
say("history: %s rows", format(nrow(h), big.mark = ","))

# --- model marks: ability refit at each month start --------------------------
test[, month := as.Date(format(date, "%Y-%m-01"))]
cuts <- sort(unique(test$month))
say("refitting ability at %d month starts (only = the test athletes)", length(cuts))
reg_f <- as.data.table(citius_events()[, c("event_id", "family")])
pred <- rbindlist(lapply(cuts, function(cut) {
  past <- h[date < cut]
  pf <- merge(past, reg_f, by = "event_id", all.x = TRUE)
  pf[is.na(family), family := ""]
  # `only =` is the package's own fast path: the population priors (prior_mu,
  # sigma_between, the robust-sigma scale k) are computed cheaply over EVERY
  # athlete and the expensive per-athlete body runs just for the ones named.
  # The result is identical for those athletes, asserted by a package test.
  # Without it this refit spent its time estimating athletes no test race
  # contains. backtest_athletics.R has always passed it; this lab did not.
  want <- unique(test[month == cut]$athlete_id)
  ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
    fam <- g$family[1]
    hl <- if (fam %in% names(hl_map)) hl_map[[fam]] else HL_GLOBAL
    estimate_ability(g[, !"family"], as_of = cut, half_life = hl, calibration = cal,
                     adjust_race = isTRUE(DEPLOYED$adjust_race), only = want)
  }), fill = TRUE)
  ab <- deployed_debias(ab)
  ab[, athlete_id := as.character(athlete_id)]
  m <- merge(test[month == cut], ab[, .(athlete_id, event_id, ability, shrinkage, w_total)],
             by = c("athlete_id", "event_id"))
  m[, .(race_key, athlete_id, event_id, family, sex, date, act, pred = ability, shrinkage, w_total)]
}), fill = TRUE)
say("predicted %s of %s test rows (%.1f%%)", format(nrow(pred), big.mark = ","),
    format(nrow(test), big.mark = ","), 100 * nrow(pred) / nrow(test))

# --- the last-5 baseline, on the same rows ----------------------------------
hh <- h[, .(athlete_id, event_id, date, perf)]
setorder(hh, athlete_id, event_id, date)
pk <- unique(pred[, .(athlete_id, event_id, date)])
jj <- hh[pk, on = .(athlete_id, event_id), allow.cartesian = TRUE][date < i.date]
setorder(jj, athlete_id, event_id, i.date, -date)
jj[, rk := seq_len(.N), by = .(athlete_id, event_id, i.date)]
b5 <- jj[rk <= 5, .(base = mean(perf), n_prior = .N), by = .(athlete_id, event_id, date = i.date)]
pred <- merge(pred, b5, by = c("athlete_id", "event_id", "date"), all.x = TRUE)
pred <- pred[is.finite(base) & n_prior >= 3L]
say("scoreable with a last-5 baseline (3+ prior marks): %s rows", format(nrow(pred), big.mark = ","))

# --- score -------------------------------------------------------------------
pred[, `:=`(ae_model = 100 * abs(pred - act), ae_base = 100 * abs(base - act))]
ev <- pred[, .(races = uniqueN(race_key), n = .N,
               mae_model = mean(ae_model), mae_base = mean(ae_base),
               se = sd(ae_model - ae_base) / sqrt(.N)), by = .(event_id, family, sex)]
ev[, `:=`(gap = 100 * (mae_model - mae_base) / mae_base,
          t = (mae_model - mae_base) / se, beat = mae_model < mae_base)]
sc <- ev[races >= 10]
setorder(sc, gap)
cat(sprintf("\n=== MARKS: %d of %d scoreable events beat last-5 (pooled MAE model %.3f vs base %.3f) ===\n",
            sum(sc$beat), nrow(sc), weighted.mean(sc$mae_model, sc$n), weighted.mean(sc$mae_base, sc$n)))
print(sc[, .(event_id, family, races, n, model = round(mae_model, 3), last5 = round(mae_base, 3),
             gap = round(gap, 1), t = round(t, 1))], nrows = 60)
cat("\nby family:\n")
print(sc[, .(events = .N, beat = sum(beat), model = round(weighted.mean(mae_model, n), 3),
             last5 = round(weighted.mean(mae_base, n), 3),
             gap = round(100 * (weighted.mean(mae_model, n) - weighted.mean(mae_base, n)) / weighted.mean(mae_base, n), 1)),
         by = family][order(-events)])
fwrite(sc, file.path(OUT, paste0("marks_lab", TAG, ".csv")))

# --- verify against a real arm on the same rows ------------------------------
if (nzchar(VERIFY) && file.exists(file.path(OUT, VERIFY))) {
  b <- readRDS(file.path(OUT, VERIFY))
  ap <- merge(as.data.table(b$predictions)[, .(race_key = race_id, athlete_id = as.character(athlete_id), arm_mark = median_mark)],
              as.data.table(b$outcomes)[, .(race_key = race_id, athlete_id = as.character(athlete_id))],
              by = c("race_key", "athlete_id"))
  v <- merge(pred[, .(race_key, athlete_id, event_id, act, pred)], ap, by = c("race_key", "athlete_id"))
  v <- merge(v, reg[, .(event_id, orientation)], by = "event_id")
  v[, arm_pred := orientation * log(arm_mark)]
  cat(sprintf("\n=== VERIFY against %s on %s shared rows ===\n", VERIFY, format(nrow(v), big.mark = ",")))
  cat(sprintf("lab vs arm predicted mark: mean diff %+.4f%%, sd %.4f, correlation %.4f\n",
              100 * mean(v$pred - v$arm_pred), 100 * sd(v$pred - v$arm_pred), cor(v$pred, v$arm_pred)))
  cat(sprintf("lab MAE %.3f vs arm MAE %.3f on those rows\n",
              100 * mean(abs(v$pred - v$act)), 100 * mean(abs(v$arm_pred - v$act))))
  cat("A small mean diff and MAE within ~0.05 means the lab reproduces the arm; a\n")
  cat("large one means the lab is measuring something else and its sweep is worthless.\n")
}
say("wrote marks_lab%s.csv", TAG)
