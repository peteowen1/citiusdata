# Systematic calibration/bias/metrics sweep across cuts, on the CURRENT
# deployed calibration (backtest_ctrl_now.rds: calibration_corpus_csigma_coast,
# 120 meets sampled evenly 2016-2026, T1+T2+T3, finals only).
#
# Metrics, matching this repo's established conventions rather than inventing
# new ones:
#   bias_pct    signed, RAW (not centred) mean((actual-predicted)/predicted),
#               oriented so +ve = actual beat prediction. This is a LOCATION
#               metric -- deliberately not centred, unlike marks MAE ctr
#               elsewhere in this repo, which centres specifically to remove
#               location and isolate spread. Centring here would hide the
#               exact thing being measured.
#   rmse_pct    root-mean-square of the same oriented relative error --
#               SPREAD, sensitive to outliers rather than robust to them.
#   concordance per-race Spearman correlation between predicted order (by
#               median_mark) and actual `place`, then averaged across races --
#               same rank-agreement convention as check_combined_ranking.R /
#               check_seeded_best.R elsewhere in citiusdata/scripts, applied
#               within-race instead of pooled-across-the-board.
#   gold/medal Brier, gold/medal logloss -- score_arm.R's own metrics,
#               recomputed here so every metric in this report comes from one
#               script with one population definition.
#
# Population: merged (multi-heat-collapsed) races EXCLUDED throughout, same
# rule form_ratings.R and score_arm.R both apply -- a merged race hands out up
# to 20+ golds under one race_id and corrupts hit/medal-based metrics.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

b <- readRDS(file.path(D, "backtest_ctrl_now.rds"))
say("arm: %s | run_at: %s", b$meta$calibration, as.character(b$meta$run_at))

pred <- as.data.table(b$predictions)
pred[, athlete_id := as.character(athlete_id)]
outc <- as.data.table(b$outcomes)
outc[, athlete_id := as.character(athlete_id)]
d <- merge(pred, outc, by = c("race_id", "athlete_id"))
say("raw predictions: %s | merged (collapsed-heat) races excluded: %s",
    format(nrow(d), big.mark = ","), format(sum(d$merged), big.mark = ","))
d <- d[merged == FALSE]

ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
ch[, `:=`(competition_id = as.character(competition_id), athlete_id = as.character(athlete_id))]
act <- unique(ch[!is.na(race_key) & !is.na(mark) & !is.na(place),
                 .(race_id = race_key, athlete_id, actual = mark, actual_place = place,
                   event_id, date, competition_id)])
d <- merge(d, act, by = c("race_id", "athlete_id"))

ev <- as.data.table(citius_events())[, .(event_id, discipline, sex, orientation, family)]
d <- merge(d, ev, by = "event_id")
d[, year := data.table::year(date)]

cat_tbl <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier, class)], by = "competition_id", all.x = TRUE)

d[, bias_pct := orientation * (actual - median_mark) / median_mark * 100]
say("final analysis population: %s predictions, %s races, %s to %s",
    format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","), min(d$year), max(d$year))

# --- concordance: per-race Spearman(predicted order, actual place) ---------
conc_by_race <- d[, if (.N >= 4 && data.table::uniqueN(median_mark) > 1)
                    .(rho = suppressWarnings(stats::cor(-orientation * median_mark, actual_place,
                                                        method = "spearman")), n = .N)
                  else .(rho = NA_real_, n = .N), by = race_id]

concordance_for <- function(race_ids) {
  r <- conc_by_race[race_id %in% race_ids & !is.na(rho)]
  if (!nrow(r)) return(NA_real_)
  weighted.mean(r$rho, w = r$n)
}

metrics_for <- function(dt) {
  if (!nrow(dt)) return(NULL)
  data.table(
    n = nrow(dt), races = uniqueN(dt$race_id),
    bias_pct = mean(dt$bias_pct), rmse_pct = sqrt(mean(dt$bias_pct^2)),
    concordance = concordance_for(unique(dt$race_id)),
    gold_brier = mean((dt$p_gold - dt$hit)^2),
    gold_logloss = mean(-(dt$hit * log(pmax(dt$p_gold, 1e-9)) +
                          (1 - dt$hit) * log(pmax(1 - dt$p_gold, 1e-9)))),
    medal_brier = mean((dt$p_medal - dt$hit_medal)^2),
    medal_logloss = mean(-(dt$hit_medal * log(pmax(dt$p_medal, 1e-9)) +
                           (1 - dt$hit_medal) * log(pmax(1 - dt$p_medal, 1e-9))))
  )
}

cat("\n================ BY TIER ================\n")
print(rbindlist(lapply(split(d, d$meet_tier), metrics_for), idcol = "meet_tier"))

cat("\n================ BY FAMILY (T1_elite only) ================\n")
d_t1 <- d[meet_tier == "T1_elite"]
print(rbindlist(lapply(split(d_t1, d_t1$family), metrics_for), idcol = "family"))

cat("\n================ BY FAMILY (all tiers pooled) ================\n")
print(rbindlist(lapply(split(d, d$family), metrics_for), idcol = "family"))

cat("\n================ BY YEAR (all tiers pooled, since T1_elite has year gaps) ================\n")
by_year <- rbindlist(lapply(split(d, d$year), metrics_for), idcol = "year")
setorder(by_year, year)
print(by_year)

cat("\n================ BY DISCIPLINE+SEX (T1_elite, min 50 preds) ================\n")
disc <- rbindlist(lapply(split(d_t1, paste(d_t1$discipline, d_t1$sex)), metrics_for), idcol = "discipline_sex")
print(disc[n >= 50][order(-abs(bias_pct))][1:min(20, .N)])

saveRDS(d, file.path(D, "calibration_sweep_data.rds"))
say("\nwrote calibration_sweep_data.rds (%s rows)", format(nrow(d), big.mark = ","))
