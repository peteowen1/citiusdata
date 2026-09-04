# Walk ONE race end to end: why does the model's predicted mark lose to a plain
# mean of the athlete's last five, in the events where it loses worst?
#
# Mandated by CLAUDE.md: "Diagnosing a model? Walk ONE example row end-to-end
# BEFORE fitting more variants." Aggregate diagnostics can be true and
# reassuring while the specification underneath is broken -- four model variants
# and dozens of RMSEs once missed a league tag decided by a coin-flip tiebreak.
#
# The population: Discus Throw M is the worst event in the 2026-09-05 goal
# scorecard (marks MAE 3.256% model vs 2.393% last-5, +0.863pp), and the whole
# throw/jump/sprint/hurdles family fails the same way, so this is the shared
# defect rather than one event's quirk.
#
# Usage:
#   Rscript citiusdata/scripts/diagnostics/walk_one_marks_row.R
#   CITIUS_WALK_EVENT=AT-DiscusThrow-M CITIUS_WALK_N=3 Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
ARM   <- Sys.getenv("CITIUS_WALK_ARM", "backtest_wac_trt_0904.rds")
EVENT <- Sys.getenv("CITIUS_WALK_EVENT", "AT-DiscusThrow-M")
NSHOW <- .env_int("CITIUS_WALK_N", "2")

b <- readRDS(file.path(OUT, ARM))
p <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                      a_mark = median_mark, shrinkage, w_total)]
o <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)]
d <- merge(p, o, by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date,
            competition_id, place, athlete_name)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- d[event_id == EVENT & date >= as.Date("2020-01-01")]
stopifnot("no rows for that event in the test window" = nrow(d) > 0)

reg <- as.data.table(citius_events())[event_id == EVENT]
ori <- reg$orientation[1]

# The same last-5 the scorer and score_arm.R use: mean of the athlete's last
# five oriented performances strictly before the race.
hist <- deployed_history(OUT, events = EVENT,
                         from = min(d$date) - 3650, to = max(d$date))
setDT(hist); hist[, athlete_id := as.character(athlete_id)]
hist <- hist[!is.na(perf) & !is.na(date)]

walk <- function(rid) {
  r <- d[race_id == rid][order(place)]
  cat(strrep("=", 78), "\n")
  cat(sprintf("RACE %s | %s | %s | %d finishers\n", rid, EVENT,
              format(r$date[1]), nrow(r)))
  cat(strrep("=", 78), "\n\n")
  out <- rbindlist(lapply(seq_len(nrow(r)), function(i) {
    a <- r[i]
    h <- hist[athlete_id == a$athlete_id & date < a$date][order(date)]
    last5 <- utils::tail(h$perf, 5)
    l5_perf <- if (length(last5)) mean(last5) else NA_real_
    l5_mark <- if (is.na(l5_perf)) NA_real_ else exp(l5_perf / ori)
    data.table(
      place = a$place,
      athlete = substr(a$athlete_name, 1, 22),
      actual = round(a$actual, 2),
      model = round(a$a_mark, 2),
      last5 = round(l5_mark, 2),
      model_err = round(100 * abs(a$a_mark - a$actual) / a$actual, 2),
      last5_err = round(100 * abs(l5_mark - a$actual) / a$actual, 2),
      n_prior = nrow(h),
      shrink = round(a$shrinkage, 3),
      w_total = round(a$w_total, 2),
      # What the athlete had actually been throwing lately, in marks, so the
      # numbers above can be sanity-checked by eye rather than trusted.
      recent = paste(round(utils::tail(exp(h$perf / ori), 5), 2), collapse = " "))
  }))
  print(out)
  cat(sprintf("\n  race mean |err|: model %.2f%%  last-5 %.2f%%   (model %s)\n\n",
              mean(out$model_err, na.rm = TRUE), mean(out$last5_err, na.rm = TRUE),
              if (mean(out$model_err, na.rm = TRUE) < mean(out$last5_err, na.rm = TRUE))
                "better" else "WORSE"))
  out
}

# Pick the races where the model loses hardest -- the defect is clearest there,
# and a race where it happens to win would teach nothing about the failure.
per_race <- d[, .(model = mean(100 * abs(a_mark - actual) / actual)), by = race_id]
setorder(per_race, -model)
res <- rbindlist(lapply(utils::head(per_race$race_id, NSHOW), walk))

cli::cli_h2("Is the model's error a LEVEL shift or a SPREAD problem?")
# A level shift means every prediction in the race is biased the same way and a
# single offset fixes it. A spread problem means the model is compressing or
# stretching the field and no offset helps. These need opposite fixes, and the
# MAE alone cannot tell them apart -- which is why chasing MAE has not moved it.
res[, `:=`(signed_model = NULL)]
d[, sm := 100 * (a_mark - actual) / actual]
d[, sl := 100 * (exp(0) - 1)]
lev <- d[, .(bias_model = round(mean(sm), 3),
             mae_model = round(mean(abs(sm)), 3),
             centred_mae_model = round(mean(abs(sm - mean(sm))), 3)), by = race_id]
cat(sprintf("\nacross %d races of %s since 2020:\n", nrow(lev), EVENT))
cat(sprintf("  mean signed bias : %+.3f%%  (a pure level shift would show up here)\n",
            mean(lev$bias_model)))
cat(sprintf("  mean |err|       : %.3f%%\n", mean(lev$mae_model)))
cat(sprintf("  mean |err| after removing each race's own mean bias: %.3f%%\n",
            mean(lev$centred_mae_model)))
cat(sprintf("\n  => %.0f%% of the model's mark error is a per-race LEVEL shift,\n",
            100 * (1 - mean(lev$centred_mae_model) / mean(lev$mae_model))))
cat("     the rest is getting the athletes wrong relative to each other.\n")
