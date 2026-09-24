# Does the form engine's pre-race forecast beat "average of the athlete's last
# five marks" on marks error? Uses the engine's own walk-forward history
# (seqv3_history_<tag>.parquet): r_pre is the forecast, perf the mark it
# scored (condition-adjusted when SEQ_ADJ=1), so the last-5 baseline is built
# from the same perf series, in date order, strictly before each race.
#
# MAE in % of mark (perf-log units x100); lower is better. Reported overall,
# by family, by race_code (rc) and for the 2025-26 windows. FORM_TAG picks
# the run (default final). MIN_PRIOR is how many earlier marks the baseline
# needs (default 5; 3 admits more rows and favours the baseline less).
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({ library(arrow); library(data.table) })
D <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final"); MIN_PRIOR <- as.integer(Sys.getenv("MIN_PRIOR", "5"))
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key", "date", "event_id", "athlete_id", "r_pre", "perf", "rc", "seen")))
reg <- as.data.table(citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
setorder(h, athlete_id, event_id, date, race_key)
# rolling mean of the previous k marks, k <= 5, none from the same day
h[, prior_n := seq_len(.N) - 1L, by = .(athlete_id, event_id)]
h[, last5 := {
  x <- perf; out <- rep(NA_real_, .N)
  for (i in seq_along(x)) if (i > 1L) { j <- max(1L, i - 5L); out[i] <- mean(x[j:(i - 1L)]) }
  out }, by = .(athlete_id, event_id)]
h[, same_day_prior := date == shift(date), by = .(athlete_id, event_id)]
s <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf) & prior_n >= MIN_PRIOR & !(same_day_prior %in% TRUE)]
s[, `:=`(e_model = abs(perf - r_pre), e_last5 = abs(perf - last5))]
cat(sprintf("[%s] rows with a forecast AND >= %d prior marks: %s (of %s seen)\n\n", TAG, MIN_PRIOR,
            format(nrow(s), big.mark = ","), format(sum(h$seen), big.mark = ",")))
fmt <- function(d, by) {
  o <- d[, .(rows = .N, model = round(100 * mean(e_model), 3), last5 = round(100 * mean(e_last5), 3)), by = by]
  o[, model_vs_last5_pct := round(100 * (model / last5 - 1), 1)]
  o[order(model_vs_last5_pct)]
}
cat("MAE % of mark, lower is better; model_vs_last5_pct negative = the model wins\n")
cat("\n== all rows, by family ==\n"); print(fmt(s, "family"))
cat("\n== 2025-26 (tune + confirm), by family ==\n"); print(fmt(s[date >= as.Date("2025-01-01")], "family"))
cat("\n== 2025-26, by race_code (rc), rows >= 2000 ==\n"); r <- fmt(s[date >= as.Date("2025-01-01")], "rc"); print(r[rows >= 2000])
cat("\n== 2025-26, by event: the events where last-5 still wins ==\n")
e <- fmt(s[date >= as.Date("2025-01-01")], "event_id"); print(e[model_vs_last5_pct > 0 & rows >= 300])
cat(sprintf("\nevents (>= 300 rows) where the model wins: %d of %d\n", e[rows >= 300 & model_vs_last5_pct < 0, .N], e[rows >= 300, .N]))
p <- s[date >= as.Date("2025-01-01"), .(model = 100 * mean(e_model), last5 = 100 * mean(e_last5))]
cat(sprintf("\nPOOLED 2025-26: model %.3f vs last-5 %.3f (%+.1f%%), paired t = %.1f\n", p$model, p$last5, 100 * (p$model / p$last5 - 1),
            s[date >= as.Date("2025-01-01"), t.test(e_model - e_last5)$statistic]))
