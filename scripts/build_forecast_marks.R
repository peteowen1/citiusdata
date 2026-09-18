# forecast_marks.parquet: one row per performance the FORM engine forecast,
# walk-forward — what it expected BEFORE the race, what happened, and why.
#
# The engine (form_ratings.R, SEQ_HIST=1) already persists this per row in
# seqv3_history_<tag>.parquet: r_pre is the pre-race rating (the forecast, in
# perf-log units), perf the mark it saw (wind/altitude/indoor-adjusted when
# SEQ_ADJ=1), shock the field-shared surprise it removed and surprise the
# athlete's own residual — perf - r_pre == surprise + shock exactly. This
# script joins that to adjusted_marks.parquet for the raw mark and the
# condition components, converts everything to marks, and writes one flat
# table to query by athlete, meet, event or date.
#
#   forecast_perf  = r_pre                     forecast_mark  = mark(r_pre)
#   actual perf    = perf_raw                  mark           = as run
#   adj_perf       = perf_raw - wind - venue - indoor - engine shock
#   error          = adj_perf - forecast_perf  (= surprise; + = beat forecast)
#
# Its within-race error variance is the sigma_e the LIVE race-shock shrinkage
# should use (citius::race_shock); see conditions_params/*.json var_resid.
#
# FORM_TAG picks the engine run (default final); ADJ_FILE the marks file.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({ library(arrow); library(data.table) })
D <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
hist_f <- file.path(D, sprintf("seqv3_history_%s.parquet", TAG))
adj_f  <- file.path(D, Sys.getenv("ADJ_FILE", "adjusted_marks.parquet"))
stopifnot(file.exists(hist_f), file.exists(adj_f))

h <- setDT(read_parquet(hist_f))
h[, athlete_id := as.character(athlete_id)]
cat(sprintf("engine history [%s]: %s rows, %s to %s, %d events, forecast on %.1f%% (seen rows)\n",
            TAG, format(nrow(h), big.mark=","), min(h$date), max(h$date), uniqueN(h$event_id), 100*mean(h$seen)))

a <- setDT(read_parquet(adj_f, col_select = c("race_key","athlete_id","event_id","mark","perf","wind","alt_m","indoor",
                                              "wind_adj","venue_adj","indoor_adj","comp_name","venue_city","place","family","sex")))
a[, athlete_id := as.character(athlete_id)]
# multi-attempt field events share a key; keep the best mark per key so the join cannot fan out
a <- a[order(-perf)][, .SD[1L], by = .(race_key, athlete_id, event_id)]

n0 <- nrow(h)
f <- merge(h, a, by = c("race_key", "athlete_id", "event_id"), all.x = TRUE, sort = FALSE, suffixes = c("", "_adj"))
stopifnot("join fanned out" = nrow(f) == n0)
cat(sprintf("matched to adjusted_marks: %.1f%% of engine rows\n", 100*mean(is.finite(f$mark))))

reg <- as.data.table(citius_events())[, .(event_id, orientation)]
f <- merge(f, reg, by = "event_id", all.x = TRUE, sort = FALSE)
f[, forecast_perf := r_pre]
f[, adj_perf := perf - shock]                       # engine perf is already condition-adjusted
f[, error := adj_perf - forecast_perf]
f[, forecast_mark := perf_to_mark(forecast_perf, orientation)]
f[, adj_mark := perf_to_mark(adj_perf, orientation)]
f[, race_shock := shock]
setnames(f, "place.x", "place", skip_absent = TRUE)
out <- f[, .(race_key, athlete_id, event_id, family, sex, date, comp_name, venue_city, place,
             forecast_mark, mark, adj_mark, forecast_perf, adj_perf, error,
             wind, alt_m, indoor, wind_adj, venue_adj, indoor_adj, race_shock,
             n_eff, v_pre, k, seen)]

cat("\nforecast error by family (perf-log units x100 = % of mark; sd lower is better, mean near 0 = unbiased):\n")
print(out[seen == TRUE & is.finite(error), .(rows = .N, mean_pct = round(100*mean(error), 3), sd_pct = round(100*sd(error), 3)), by = family][order(sd_pct)])
cat(sprintf("\nwithin-race error variance (sigma_e^2 for live shrinkage), pooled: %.3e\n",
            out[seen == TRUE & is.finite(error), var(error - mean(error)), by = race_key][, mean(V1, na.rm = TRUE)]))

OUT_PATH <- file.path(D, sprintf("forecast_marks_%s.parquet", TAG))
write_parquet(out, OUT_PATH)
cat(sprintf("wrote %s: %s rows x %d cols\n", basename(OUT_PATH), format(nrow(out), big.mark=","), ncol(out)))
