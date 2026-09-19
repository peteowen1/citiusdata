# A post-hoc fix for finals under-confidence, measured out of sample.
#
# The career model's favourite wins 48.4% of M1 finals at a mean p_gold of
# 0.430 (career_finals_calibration.R). Before touching sigma inside the
# simulator (an arm that costs hours), test the cheapest honest remedy: one
# parameter, temperature scaling on the log-odds of every athlete's simulated
# probability, p' = sigmoid(a + b * logit(p)), fitted on the OLDER half of the
# meets and scored on the NEWER half. Then renormalise within each race so the
# win probabilities still sum to one. If Brier and log-loss fall out of sample
# and the curve straightens, this can ship in the export today; the sigma arm
# remains the structural fix.
#
# Brier and log-loss: LOWER is better. gap = actual - predicted, per bucket.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
CACHE <- Sys.getenv("CITIUS_CAL_CACHE", "backtest_cache_tierclass3_ctrl")
fs <- setdiff(list.files(file.path(OUT, CACHE), pattern = "\\.rds$"), "_arm.rds")
rows <- rbindlist(lapply(fs, function(f) {
  o <- readRDS(file.path(OUT, CACHE, f)); if (!length(o)) return(NULL)
  rbindlist(lapply(o, function(r) {
    if (is.null(r$pred) || is.null(r$outc)) return(NULL)
    p <- as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), p_gold, p_medal)]
    oc <- as.data.table(r$outc)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)]
    m <- merge(p, oc, by = c("race_id", "athlete_id")); m[, meet := sub("[.]rds$", "", f)]; m
  }), fill = TRUE)
}), fill = TRUE)
rows <- rows[is.finite(p_gold) & p_gold > 0 & p_gold < 1]
# meet ids are WA competition ids, roughly chronological; split by rank
meets <- sort(unique(rows$meet)); cut <- meets[ceiling(length(meets) / 2)]
rows[, half := fifelse(meet <= cut, "fit", "test")]
cat(sprintf("[%s] %s rows, %d races; fit half %d meets, test half %d meets\n", CACHE, format(nrow(rows), big.mark = ","), uniqueN(rows$race_id),
            uniqueN(rows[half == "fit"]$meet), uniqueN(rows[half == "test"]$meet)))

logit <- function(p) log(p / (1 - p)); sigm <- function(z) 1 / (1 + exp(-z))
score <- function(p, y) c(brier = mean((p - y)^2), logloss = -mean(y * log(pmax(p, 1e-9)) + (1 - y) * log(pmax(1 - p, 1e-9))))
fit_ts <- function(p, y) { z <- logit(p); f <- glm(y ~ z, family = binomial()); coef(f) }
apply_ts <- function(p, ab) sigm(ab[1] + ab[2] * logit(p))
renorm <- function(d, col) d[, (col) := get(col) / sum(get(col)), by = race_id]

for (what in c("gold", "medal")) {
  pcol <- paste0("p_", what); ycol <- if (what == "gold") "hit" else "hit_medal"
  ab <- fit_ts(rows[half == "fit"][[pcol]], rows[half == "fit"][[ycol]])
  te <- rows[half == "test"]
  te[, p_raw := get(pcol)]
  te[, p_ts := apply_ts(p_raw, ab)]
  if (what == "gold") renorm(te, "p_ts") else te[, p_ts := p_ts * 3 / sum(p_ts), by = race_id][, p_ts := pmin(p_ts, 0.999)]
  s0 <- score(te$p_raw, te[[ycol]]); s1 <- score(te$p_ts, te[[ycol]])
  cat(sprintf("\n== %s: temperature a = %.3f, b = %.3f (b > 1 sharpens). TEST half, %s rows:\n   Brier %.4f -> %.4f (%+.1f%%), log-loss %.4f -> %.4f (%+.1f%%)\n",
              toupper(what), ab[1], ab[2], format(nrow(te), big.mark = ","), s0["brier"], s1["brier"], 100*(s1["brier"]/s0["brier"]-1), s0["logloss"], s1["logloss"], 100*(s1["logloss"]/s0["logloss"]-1)))
  fav <- te[order(race_id, -p_raw)][, .SD[1L], by = race_id]
  cat(sprintf("   favourites (%d races): predicted %.3f -> %.3f, actual %.3f\n", nrow(fav), mean(fav$p_raw), mean(fav$p_ts), mean(fav[[ycol]])))
  edges <- if (what == "gold") c(0, .05, .1, .2, .3, .4, .5, .6, .8, 1) else c(0, .1, .2, .3, .5, .7, .85, 1)
  te[, bin := cut(p_raw, edges, include.lowest = TRUE)]
  print(te[, .(n = .N, raw = round(mean(p_raw), 3), scaled = round(mean(p_ts), 3), actual = round(mean(get(ycol)), 3)), by = bin][order(bin)][, `:=`(gap_raw = actual - raw, gap_scaled = actual - scaled)][])
  if (what == "gold") ab_gold <- ab else ab_medal <- ab
}
out <- data.table(target = c("gold", "medal"), a = c(ab_gold[1], ab_medal[1]), b = c(ab_gold[2], ab_medal[2]), fitted_on = CACHE, fit_meets = uniqueN(rows[half == "fit"]$meet))
# NOT persisted: measured 2026-09-19, b = 1.04 for gold (Brier -0.1%), the miss is
# concentrated in the top favourites, not a slope -- a global temperature cannot
# fix it and the medal renormalisation made things worse. Kept as the record.
if (nzchar(Sys.getenv("CITIUS_PERSIST_TEMPERATURE"))) arrow::write_parquet(out, file.path(OUT, "conditions_params", "prob_temperature.parquet"))
cat("\n(temperature table not persisted -- see the comment above)\n")
