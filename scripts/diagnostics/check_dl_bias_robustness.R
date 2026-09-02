# Stress-test the DL year/family bias finding against three concrete
# confounds before trusting it: (1) wind -- the prediction is wind-NEUTRAL
# (calibration$wind adjusts HISTORY, but nothing un-does the real wind on the
# specific final being scored) while the actual mark is whatever wind actually
# blew; (2) outliers -- flag_implausible() was never applied to the outcome
# side of the original check, so an injury/DNF-type mark could skew a family
# mean; (3) coasting/sandbagging -- an athlete predicted out of medal
# contention may not race maximally, which would show as overprediction
# concentrated at low predicted probability, not a genuine miscalibration.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

d <- readRDS(file.path(D, "dl_calibration_by_year.rds"))
cal <- readRDS(file.path(D, "calibration_corpus_csigma_coast.rds"))
b <- readRDS(file.path(D, "backtest_ctrl_now.rds"))

# --- 1. OUTLIERS: was flag_implausible() skipped on the outcome side? -------
ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
ch_flagged <- flag_implausible(copy(ch))
ch_flagged[, `:=`(competition_id = as.character(competition_id),
                  athlete_id = as.character(athlete_id))]
bad <- unique(ch_flagged[implausible == TRUE, .(race_id = race_key, athlete_id)])
d[, flagged := FALSE]
d[bad, flagged := TRUE, on = c("race_id", "athlete_id")]
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("Implausible (Hampel-flagged) actual marks among DL predictions: %d of %d (%.2f%%)",
    sum(d$flagged), nrow(d), 100 * mean(d$flagged))

fam_clean <- d[flagged == FALSE, .(n = .N, mean_bias_pct = mean(bias_pct)), by = family]
fam_dirty <- d[, .(n = .N, mean_bias_pct = mean(bias_pct)), by = family]
cmp <- merge(fam_dirty, fam_clean, by = "family", suffixes = c("_all", "_clean"))
cat("\n=== family bias: all rows vs implausible-marks-excluded ===\n")
print(cmp[order(family)])

# Distance-drift check survives outlier removal?
dist_year_clean <- d[family == "distance" & flagged == FALSE,
                     .(n = .N, mean_bias_pct = mean(bias_pct)), by = year]
setorder(dist_year_clean, year)
cat("\n=== distance family, outlier-excluded, by year ===\n")
print(dist_year_clean)

# --- 2. WIND: un-do the REAL wind on the actual mark, same coefficient the --
# model already applies to history, and see if the family bias moves. -------
wind_src <- unique(ch[!is.na(race_key), .(race_id = race_key,
                     athlete_id = as.character(athlete_id), wind, event_id)])
d2 <- merge(d, wind_src, by = c("race_id", "athlete_id", "event_id"), all.x = TRUE)
if (!is.null(cal$wind) && nrow(cal$wind)) {
  d2 <- merge(d2, as.data.table(cal$wind)[, .(event_id, wind_beta = beta)], by = "event_id", all.x = TRUE)
} else {
  d2[, wind_beta := NA_real_]
}
d2[is.na(wind_beta), wind_beta := 0]
d2[is.na(wind), wind := 0]
# citius/R/marks.R: perf = orientation * log(mark), and ability.R subtracts
# wind_beta*wind on the PERF scale. Inverting: mark_adj = mark *
# exp(-orientation * wind_beta * wind) -- NOT a flat subtraction, which would
# silently be wrong by a factor of `actual` itself (this was caught and fixed
# before running: the first draft used `actual - orientation*wind_beta*wind`,
# mixing the log-scale coefficient into raw mark units).
d2[, actual_neutral := actual * exp(-orientation * wind_beta * wind)]
d2[, bias_pct_wind_adj := orientation * (actual_neutral - median_mark) / median_mark * 100]

say("\nRows with a nonzero recorded wind reading: %d of %d (%.1f%%)",
    sum(d2$wind != 0, na.rm = TRUE), nrow(d2), 100 * mean(d2$wind != 0, na.rm = TRUE))
cat("\n=== family bias: raw vs wind-adjusted actual ===\n")
print(d2[, .(n = .N, mean_bias_pct = mean(bias_pct), mean_bias_wind_adj = mean(bias_pct_wind_adj)),
         by = family][order(family)])

cat("\n=== distance family by year: raw vs wind-adjusted ===\n")
print(d2[family == "distance", .(n = .N, mean_bias_pct = mean(bias_pct),
                                 mean_bias_wind_adj = mean(bias_pct_wind_adj)), by = year][order(year)])

# --- 3. COASTING/SANDBAGGING: does the negative bias concentrate among -----
# athletes predicted to have no realistic medal chance? --------------------
pred_full <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), p_gold, p_medal)]
d3 <- merge(d, pred_full, by = c("race_id", "athlete_id"))
d3[, contention := fifelse(p_medal >= 0.15, "live medal shot (p_medal>=15%)",
                    fifelse(p_medal >= 0.03, "outside shot (3-15%)", "no realistic shot (<3%)"))]
cat("\n=== bias by predicted contention level (tests coasting/sandbagging) ===\n")
print(d3[, .(n = .N, mean_bias_pct = mean(bias_pct)), by = contention][order(-mean_bias_pct)])

cat("\n=== same, split by family (throw vs distance, the two extremes) ===\n")
print(d3[family %in% c("throw", "distance"),
         .(n = .N, mean_bias_pct = mean(bias_pct)), by = .(family, contention)][order(family, -mean_bias_pct)])

# Correlation, not just three buckets -- does bias trend with p_medal itself?
ct <- cor.test(d3$bias_pct, d3$p_medal)
say("\ncorrelation(bias_pct, p_medal) = %.4f (p = %.4g) -- positive would mean\nfavourites OUTperform predictions relatively more, i.e. longshots coast",
    ct$estimate, ct$p.value)
