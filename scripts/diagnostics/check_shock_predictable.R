# Can the race shock be PREDICTED from the field, before the race?
#
# The shock is currently learned from the race itself, so it cannot inform a
# forecast. But if an elite field reliably runs fast, then E[S | field] is
# estimable in advance and a race-specific predicted mark becomes possible:
#   predicted = R + offset + E[S | field]
# which is what turns "her typical race is 1:58" into "she will run 1:54 here".
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
p <- h[seen == TRUE & rc == "final" & is.finite(perf) & is.finite(r_pre)]
p[, resid := perf - r_pre]
# Per race: the realised shock, and a field-quality measure known BEFORE the
# race (mean carried-in rating, standardised within event).
p[, r_z := (r_pre - mean(r_pre)) / stats::sd(r_pre), by = event_id]
race <- p[, .(shock = mean(resid), field_q = mean(r_z), n = .N,
              date = date[1], event_id = event_id[1]), by = race_key][n >= 5]
fit <- race[date < as.Date("2025-01-01")]
val <- race[year(date) == 2026]
cat(sprintf("races: %s fit, %s check\n\n", format(nrow(fit), big.mark=","),
            format(nrow(val), big.mark=",")))
m <- stats::lm(shock ~ field_q, data = fit)
cat(sprintf("shock = %+.4f%% %+.4f%% * field_quality   (R2 %.3f, n %s)\n",
            100*coef(m)[1], 100*coef(m)[2], summary(m)$r.squared,
            format(nrow(fit), big.mark=",")))
cat("\nrealised shock by field-quality band:\n")
fit[, qb := cut(field_q, stats::quantile(field_q, seq(0,1,.2)), include.lowest = TRUE,
                labels = c("weakest 20%","20-40%","40-60%","60-80%","strongest 20%"))]
val[, qb := cut(field_q, stats::quantile(fit$field_q, seq(0,1,.2)), include.lowest = TRUE,
                labels = c("weakest 20%","20-40%","40-60%","60-80%","strongest 20%"))]
a <- fit[, .(n_fit = .N, shock_fit = round(100*mean(shock), 3)), by = qb]
b <- val[!is.na(qb), .(n_26 = .N, shock_26 = round(100*mean(shock), 3)), by = qb]
print(merge(a, b, by = "qb", all = TRUE)[order(qb)])
cat("\nout-of-sample check: does the fitted line predict 2026 shocks?\n")
val[, pred := stats::predict(m, val)]
cat(sprintf("  correlation(predicted, realised) = %.3f over %s races\n",
            stats::cor(val$pred, val$shock), format(nrow(val), big.mark=",")))
cat(sprintf("  mean |error| %.3f%% vs %.3f%% using a constant\n",
            100*mean(abs(val$shock - val$pred)),
            100*mean(abs(val$shock - mean(fit$shock)))))
