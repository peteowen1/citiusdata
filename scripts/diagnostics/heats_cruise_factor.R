# Heats are the one cut where last-five still beats the engine (+6.3% MAE,
# marks_mae_vs_last5.R). Hypothesis: the forecast targets a typical mark and
# athletes cruise in heats, so the forecast is systematically too fast (or
# far) there. Test the cheapest fix -- an additive round x family offset on
# the forecast, fitted on 2020-24, scored on 2025-26 -- before touching the
# engine. If it lands, it is a post-hoc calibration of r_pre by round.
#
# MAE in % of mark, lower is better. FORM_TAG picks the engine run.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({ library(arrow); library(data.table) })
D <- here::here("citiusdata", "data"); TAG <- Sys.getenv("FORM_TAG", "final")
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key", "date", "event_id", "athlete_id", "r_pre", "perf", "shock", "rc", "seen", "n_eff")))
reg <- as.data.table(citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
s <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf)]
# TARGET: "adj" (default) fits the cruise net of the race shock -- the target
# forecast_marks scores against (adj_perf - r_pre); "raw" is perf - r_pre. The
# first persisted offsets were fitted raw and made heats WORSE when applied to
# the shock-removed table (1.448 -> 1.493): a shared slow heat is shock, not
# cruise, and was being removed twice.
TARGET <- Sys.getenv("TARGET", "adj")
s[, err := if (TARGET == "adj") perf - shock - r_pre else perf - r_pre]   # + = better than forecast
s[, round := fifelse(rc %chin% c("heat", "semi", "final"), rc, "other")]
fit <- s[date < as.Date("2025-01-01")]; test <- s[date >= as.Date("2025-01-01")]

cat("Mean error by round x family on the FIT window (2020-24), % of mark; negative = athletes run slower than forecast:\n")
off <- fit[, .(n = .N, mean_err = mean(err), sd_err = sd(err)), by = .(family, round)]
print(dcast(off[, .(family, round, v = round(100 * mean_err, 2))], family ~ round, value.var = "v"))

# apply the fitted offset to the test window
test <- merge(test, off[, .(family, round, cruise = mean_err)], by = c("family", "round"), all.x = TRUE)
test[is.na(cruise), cruise := 0]
test[, `:=`(e0 = abs(err), e1 = abs(err - cruise))]
cat("\nTEST 2025-26: MAE before and after the round x family offset\n")
byr <- test[, .(rows = .N, mae_now = round(100 * mean(e0), 3), mae_offset = round(100 * mean(e1), 3)), by = round]
byr[, change_pct := round(100 * (mae_offset / mae_now - 1), 1)]; print(byr[order(round)])
byf <- test[round == "heat", .(rows = .N, mae_now = round(100 * mean(e0), 3), mae_offset = round(100 * mean(e1), 3)), by = family]
byf[, change_pct := round(100 * (mae_offset / mae_now - 1), 1)]
cat("\nheats only, by family:\n"); print(byf[order(change_pct)])
tt <- test[round == "heat", t.test(e1 - e0)]
cat(sprintf("\nheats: paired |error| offset - now: %+.4f%% (t = %.1f, p = %.3g), n = %s\n",
            100 * tt$estimate, tt$statistic, tt$p.value, format(test[round == "heat", .N], big.mark = ",")))
# is the cruise effect about the athlete's standing? strong athletes cruise more
cat("\nheats: mean error by forecast-strength quintile within event (1 = weakest forecast, 5 = strongest), fit window:\n")
fit[round == "heat", q := cut(frank(r_pre) / .N, c(0, .2, .4, .6, .8, 1), labels = 1:5, include.lowest = TRUE), by = event_id]
print(fit[round == "heat" & !is.na(q), .(rows = .N, mean_err_pct = round(100 * mean(err), 2)), by = q][order(q)])

# --- round x family x strength quintile ------------------------------------
# strength = the athlete's forecast rank within the event on the fit window;
# quintile boundaries are taken from the fit window and applied to the test
# window by r_pre, so nothing from 2025-26 leaks into the offsets
qb <- fit[, .(q1 = quantile(r_pre, .2), q2 = quantile(r_pre, .4), q3 = quantile(r_pre, .6), q4 = quantile(r_pre, .8)), by = event_id]
qcut <- function(d) { d <- merge(d, qb, by = "event_id", all.x = TRUE); d[, q := 1L + (r_pre > q1) + (r_pre > q2) + (r_pre > q3) + (r_pre > q4)]; d[, c("q1","q2","q3","q4") := NULL]; d }
fit2 <- qcut(copy(fit)); test2 <- qcut(copy(test))
off2 <- fit2[, .(n = .N, cruise2 = mean(err)), by = .(family, round, q)]
test2 <- merge(test2, off2[n >= 200, .(family, round, q, cruise2)], by = c("family", "round", "q"), all.x = TRUE)
test2[is.na(cruise2), cruise2 := cruise]
test2[, e2 := abs(err - cruise2)]
cat("\nTEST 2025-26 heats: MAE now / round x family / round x family x strength\n")
print(test2[round == "heat", .(rows = .N, mae_now = round(100*mean(e0), 3), mae_rf = round(100*mean(e1), 3), mae_rfs = round(100*mean(e2), 3))])
tt2 <- test2[round == "heat", t.test(e2 - e1)]
cat(sprintf("heats: strength term on top of round x family: %+.4f%% (t = %.1f)\n", 100 * tt2$estimate, tt2$statistic))
# against the last-5 baseline on the same heats rows (>= 5 prior marks)
setorder(h, athlete_id, event_id, date, race_key)
h[, prior_n := seq_len(.N) - 1L, by = .(athlete_id, event_id)]
h[, last5 := { x <- perf; out <- rep(NA_real_, .N); for (i in seq_along(x)) if (i > 1L) { j <- max(1L, i - 5L); out[i] <- mean(x[j:(i-1L)]) }; out }, by = .(athlete_id, event_id)]
l5 <- h[, .(race_key, athlete_id, event_id, last5, prior_n)]
t5 <- merge(test2[round == "heat"], l5, by = c("race_key", "athlete_id", "event_id"))[prior_n >= 5 & is.finite(last5)]
t5[, e5 := abs(perf - last5)]
cat(sprintf("\nheats with >= 5 prior marks (n = %s): MAE model %.3f | + round x family %.3f | + strength %.3f | last-5 %.3f\n",
            format(nrow(t5), big.mark = ","), 100*mean(t5$e0), 100*mean(t5$e1), 100*mean(t5$e2), 100*mean(t5$e5)))
tt3 <- t5[, t.test(e2 - e5)]
cat(sprintf("heats: strength-adjusted model vs last-5: %+.4f%% (t = %.1f, p = %.3g)\n", 100 * tt3$estimate, tt3$statistic, tt3$p.value))

# --- persist: the offsets a scorer or page applies to a heat/semi forecast ---
# fitted on the FIT window only. cruise is in perf-log units (negative = the
# athlete runs slower than their typical-mark forecast in that round);
# forecast_for_round = r_pre + cruise. Quintile edges per event ship with it.
out <- off2[n >= 200, .(family, round, strength_q = q, n, cruise = round(cruise2, 6))]
write_parquet(out, file.path(D, "conditions_params", "round_offsets.parquet"))
write_parquet(qb, file.path(D, "conditions_params", "round_offsets_quintiles.parquet"))
jsonlite::write_json(list(fitted_on = "2020-2024", scored_on = "2025-2026", heats_mae_pct = list(now = round(100*mean(test2[round=="heat"]$e0), 3),
  with_offsets = round(100*mean(test2[round=="heat"]$e2), 3)), offsets = out, quintiles = qb),
  file.path(D, "conditions_params", "round_offsets.json"), auto_unbox = TRUE, digits = 6)
cat(sprintf("\nwrote conditions_params/round_offsets.{parquet,json}: %d (family, round, strength) cells, quintile edges for %d events\n", nrow(out), nrow(qb)))
