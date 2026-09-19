# Is the CAREER model (the cards) under-confident in championship finals?
# Reads one backtest cache (default the deployed control arm, 120 M1 meets):
# for every scored race, the favourite's simulated p_gold and whether they
# won, bucketed by p_gold -- predicted-vs-actual on a proportion, which has no
# attenuation problem (the point check_champ_peaking.R makes for the form
# engine). Same for medals: the top-3 by p_medal vs actual podium. A curve
# that sits ABOVE the diagonal (actual > predicted) is under-confidence: the
# spread is too wide for a final.
#
# CITIUS_CAL_CACHE names the cache dir under citiusdata/data.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
CACHE <- Sys.getenv("CITIUS_CAL_CACHE", "bt_cache_adjm_ctrl")
fs <- setdiff(list.files(file.path(OUT, CACHE), pattern = "\\.rds$"), "_arm.rds")
rows <- rbindlist(lapply(fs, function(f) {
  o <- readRDS(file.path(OUT, CACHE, f)); if (!length(o)) return(NULL)
  rbindlist(lapply(o, function(r) {
    if (is.null(r$pred) || is.null(r$outc)) return(NULL)
    p <- as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), p_gold, p_medal)]
    oc <- as.data.table(r$outc)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)]
    merge(p, oc, by = c("race_id", "athlete_id"))
  }), fill = TRUE)
}), fill = TRUE)
rows <- rows[is.finite(p_gold)]
cat(sprintf("[%s] %s athlete-race rows, %s races\n", CACHE, format(nrow(rows), big.mark = ","), format(uniqueN(rows$race_id), big.mark = ",")))
rows[, event_id := sub("^[^|]*\\|", "", race_id)]

# --- every athlete, every race: calibration by predicted win probability ------
cal <- function(d, p, y, edges) {
  d <- copy(d); d[, bin := cut(get(p), edges, include.lowest = TRUE)]
  d[, .(n = .N, predicted = round(mean(get(p)), 3), actual = round(mean(get(y)), 3)), by = bin][order(bin)][, gap := actual - predicted][]
}
cat("\nWIN: all athlete-races, by p_gold bucket. actual - predicted: positive = under-confident (they win more than we say)\n")
print(cal(rows, "p_gold", "hit", c(0, .02, .05, .1, .2, .3, .4, .5, .6, .8, 1)))
cat("\nMEDAL: by p_medal bucket\n")
print(cal(rows, "p_medal", "hit_medal", c(0, .05, .1, .2, .3, .5, .7, .85, 1)))

# --- the favourite only ------------------------------------------------------
fav <- rows[order(race_id, -p_gold)][, .SD[1L], by = race_id]
cat(sprintf("\nFAVOURITES (%d races): mean p_gold %.3f, actual win rate %.3f  -> gap %+.3f (positive = under-confident)\n",
            nrow(fav), mean(fav$p_gold), mean(fav$hit), mean(fav$hit) - mean(fav$p_gold)))
print(cal(fav, "p_gold", "hit", c(0, .2, .3, .4, .5, .6, .7, 1)))
# Brier and log-loss on the favourite, for the spread test later (lower is better)
brier <- mean((fav$p_gold - fav$hit)^2); ll <- -mean(fav$hit * log(pmax(fav$p_gold, 1e-6)) + (1 - fav$hit) * log(pmax(1 - fav$p_gold, 1e-6)))
cat(sprintf("favourite Brier %.4f, log-loss %.4f\n", brier, ll))
# strong favourites, by family
fam <- as.data.table(citius::citius_events())[, .(event_id, family)]
fav <- merge(fav, fam, by = "event_id", all.x = TRUE)
cat("\nfavourites with p_gold >= 0.4, by family (gap positive = under-confident):\n")
print(fav[p_gold >= 0.4, .(races = .N, predicted = round(mean(p_gold), 3), actual = round(mean(hit), 3), gap = round(mean(hit) - mean(p_gold), 3)), by = family][order(-gap)])
