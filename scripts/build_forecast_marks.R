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

# ROUND-AWARE FORECAST (2026-09-19). The engine's r_pre is a typical-mark
# forecast; athletes cruise in heats and the strongest cruise most
# (diagnostics/heats_cruise_factor.R: heats MAE 1.703 -> 1.597 with a
# round x family x strength offset fitted on 2020-24, and it beats last-5
# on heats, 1.503 vs 1.590). The offsets and each event's strength-quintile
# edges live in conditions_params/round_offsets*.parquet; a missing cell
# means no offset. forecast_perf stays the raw engine forecast; the round-
# aware one is what a page or a scorer should compare a heat against.
f[, round := fifelse(rc %chin% c("heat", "semi", "final"), rc, "other")]
ro_f <- file.path(D, "conditions_params", "round_offsets.parquet")
qb_f <- file.path(D, "conditions_params", "round_offsets_quintiles.parquet")
f[, cruise := 0]
if (file.exists(ro_f) && file.exists(qb_f)) {
  ro <- setDT(read_parquet(ro_f)); qb <- setDT(read_parquet(qb_f))
  f <- merge(f, qb, by = "event_id", all.x = TRUE, sort = FALSE)
  f[, strength_q := 1L + (r_pre > q1) + (r_pre > q2) + (r_pre > q3) + (r_pre > q4)]
  f[is.na(strength_q), strength_q := 3L]
  f[, c("q1", "q2", "q3", "q4") := NULL]
  f <- merge(f, ro[, .(family, round, strength_q, cruise_cell = cruise)], by = c("family", "round", "strength_q"), all.x = TRUE, sort = FALSE)
  f[is.finite(cruise_cell), cruise := cruise_cell]; f[, cruise_cell := NULL]
  cat(sprintf("round offsets applied on %.1f%% of rows (heats %.1f%%)\n", 100*mean(f$cruise != 0), 100*mean(f[round == "heat"]$cruise != 0)))
} else cat("round_offsets not found -- forecast_round equals the raw forecast\n")
f[, forecast_round_perf := forecast_perf + cruise]
f[, forecast_round_mark := perf_to_mark(forecast_round_perf, orientation)]
f[, error_round := adj_perf - forecast_round_perf]

out <- f[, .(race_key, athlete_id, event_id, family, sex, date, comp_name, venue_city, place, round,
             forecast_mark, forecast_round_mark, mark, adj_mark, forecast_perf, forecast_round_perf, adj_perf, error, error_round,
             wind, alt_m, indoor, wind_adj, venue_adj, indoor_adj, race_shock,
             n_eff, v_pre, k, seen)]

cat("\nforecast error by family (perf-log units x100 = % of mark; sd lower is better, mean near 0 = unbiased):\n")
print(out[seen == TRUE & is.finite(error), .(rows = .N, mean_pct = round(100*mean(error), 3), sd_pct = round(100*sd(error), 3)), by = family][order(sd_pct)])
cat("\nMAE by round, raw forecast vs round-aware (% of mark, lower is better; 2025-26 only, the offsets' out-of-sample window):\n")
print(out[seen == TRUE & is.finite(error) & date >= as.Date("2025-01-01"),
          .(rows = .N, mae_raw = round(100*mean(abs(error)), 3), mae_round = round(100*mean(abs(error_round)), 3)), by = round][order(round)])
cat(sprintf("\nwithin-race error variance (sigma_e^2 for live shrinkage), pooled: %.3e\n",
            out[seen == TRUE & is.finite(error), var(error - mean(error)), by = race_key][, mean(V1, na.rm = TRUE)]))

OUT_PATH <- file.path(D, sprintf("forecast_marks_%s.parquet", TAG))
write_parquet(out, OUT_PATH)
cat(sprintf("wrote %s: %s rows x %d cols\n", basename(OUT_PATH), format(nrow(out), big.mark=","), ncol(out)))
