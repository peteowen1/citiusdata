# Does the model's predicted mark beat naive SB / PB / last-5 guesses?
#
# Mirrors score_arm.R's leakage-safe construction exactly: history rolled to
# `date - 1L`, so every baseline sees only results STRICTLY BEFORE the race
# being predicted. perf = orientation * log(mark), higher is better, so PB/SB
# are cummax not cummin.
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  PB must be biased OPTIMISTIC (mean signed error > 0). A personal best
#       is a maximum; if it comes out unbiased the leakage guard has failed.
#   A2  SB must be biased optimistic too, but by LESS than PB.
#   A3  last-5 mean must be near-unbiased -- it is a central estimate.
#   A4  SB must be NA for an athlete's first race of a season. If nothing is
#       NA, the within-season restriction is not being applied.
#   A5  every predictor scored on the IDENTICAL row set, or the comparison is
#       not a comparison.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")

b <- readRDS(file.path(OUT, "backtest_ctrl_now.rds"))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation)], by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, class, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- history, exactly as score_arm.R builds it -----------------------------
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)

g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
hist[, pbrun := cummax(perf), by = g]                      # running personal best
hist[, yr := year(date)]
hist[, sbrun := cummax(perf), by = .(athlete_id, event_id, yr)]  # running season's best

q <- d[, .(athlete_id, event_id, date = date - 1L, race_id, yr = year(date))]

# last-5 and PB: roll on (athlete, event, date)
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5, pbrun, lastperf = perf)]
m[, l5 := (cs - cs5) / pmin(k, 5)]

# SB: roll WITHIN the same calendar year, so no prior result that season -> NA
setkeyv(hist, c("athlete_id", "event_id", "yr", "date"))
s <- hist[q, on = .(athlete_id, event_id, yr, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, sbrun)]

d <- merge(d, m[, .(race_id, athlete_id, l5, pb = pbrun, last1 = lastperf)], by = c("race_id", "athlete_id"))
d <- merge(d, s[, .(race_id, athlete_id, sb = sbrun)], by = c("race_id", "athlete_id"))
d <- d[date >= HOLDOUT]

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]

cat("\n==== ANCHOR CHECKS ====\n")
cat(sprintf("A4  SB undefined (first race of season): %d of %d rows (%.1f%%)\n",
            sum(is.na(d$sb)), nrow(d), 100 * mean(is.na(d$sb))))
cat(sprintf("    last-5 NA %d | PB NA %d\n", sum(is.na(d$l5)), sum(is.na(d$pb))))

# A5: identical row set -- restrict to rows where ALL predictors are defined
full <- d[!is.na(sb) & !is.na(l5) & !is.na(pb) & !is.na(last1) & !is.na(a_perf) & !is.na(act_perf)]
cat(sprintf("A5  complete-case rows scored by every predictor: %s of %s\n",
            format(nrow(full), big.mark = ","), format(nrow(d), big.mark = ",")))

err <- function(p) 100 * (p - full$act_perf)     # % of a mark, model convention
E <- list(model = err(full$a_perf), last5 = err(full$l5), sb = err(full$sb), pb = err(full$pb), last1 = err(full$last1))

cat("\nA1-A3  mean signed error (%, +ve = predicted better than actual):\n")
for (nm in names(E)) cat(sprintf("    %-6s %+7.3f\n", nm, mean(E[[nm]])))

rep_pop <- function(dd, label) {
  nr <- uniqueN(dd$race_id); if (nr < 25) return(invisible(NULL))
  idx <- which(full$race_id %in% dd$race_id)
  if (!length(idx)) return(invisible(NULL))
  e <- lapply(E, function(v) v[idx])
  ec <- lapply(e, function(v) v - mean(v, na.rm = TRUE))
  cat(sprintf("\n%s  |  %d races, %s predictions\n", label, uniqueN(full$race_id[idx]),
              format(length(idx), big.mark = ",")))
  cat(sprintf("  %-8s %9s %9s %9s %9s\n", "", "MAE", "RMSE", "MAE ctr", "RMSE ctr"))
  for (nm in names(e)) {
    cat(sprintf("  %-8s %9.4f %9.4f %9.4f %9.4f\n", nm,
                mean(abs(e[[nm]])), sqrt(mean(e[[nm]]^2)),
                mean(abs(ec[[nm]])), sqrt(mean(ec[[nm]]^2))))
  }
  # paired tests: model vs each naive predictor, on centred MAE (the standing metric)
  for (nm in c("last5", "last1", "sb", "pb")) {
    t <- t.test(abs(ec[[nm]]), abs(ec$model), paired = TRUE)
    rel <- 100 * (mean(abs(ec$model)) - mean(abs(ec[[nm]]))) / mean(abs(ec[[nm]]))
    cat(sprintf("    model vs %-5s  centred MAE %+6.2f%%  %s  p=%.3g\n", nm, rel,
                ifelse(rel < 0, "MODEL", "naive"), t$p.value))
  }
}

cli::cli_h1("Predicted mark vs naive baselines (deployed model, holdout {HOLDOUT})")
rep_pop(full[class %in% c("olympics", "world_champs", "commonwealth")], "PRIMARY: majors")
rep_pop(full[meet_tier == "T1_elite"], "DECISIONS: T1 elite")
rep_pop(full[meet_tier == "T2_strong"], "T2 strong")
rep_pop(full, "CONTEXT: all scored finals")
