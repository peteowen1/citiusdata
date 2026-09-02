# The stated uncertainty is about twice too tight. This is why, and the fix.
#
# THE SYMPTOM. z = (perf - r_pre) / sqrt(v_pre) should have sd 1 if the model's
# own variance is honest. Measured, it runs 1.74 to 2.45 across every evidence
# band (check_thin_calibration.R). Every interval the model publishes is roughly
# half as wide as it should be, and that is true for deep records as well as thin
# ones, so it is not an evidence-depth problem.
#
# THE MECHANISM, which is visible in one line of the engine. form_ratings.R:1667
# updates the variance state as
#
#   V <- v_pre + kv * (surprise^2 - v_pre)
#
# so `v` is an EWMA of SURPRISE, the residual AFTER the shared race shock has
# been removed. It is therefore the athlete's own race-to-race variability, which
# is exactly the right quantity for the Kalman-style learning it drives.
#
# But the thing we predict is a RAW mark, and
#
#   perf - r_pre = surprise + shock
#
# so predicting one with the variance of the other omits the race-conditions term
# entirely. On a future race the shock is unknown by definition, so it belongs in
# the predictive variance even though it must stay out of the learning variance.
# Two different quantities that have been sharing one column.
#
# WHAT THIS SCRIPT DOES. Verifies the decomposition holds in the stored history,
# measures the shock variance, and checks whether v_pre + var(shock) actually
# lands sd(z) on 1. It changes nothing - if the arithmetic does not work the
# answer is that the mechanism is wrong, and that is worth knowing before editing
# an engine.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0]
stopifnot("history is empty" = nrow(h) > 100000,
          "history has no shock column" = "shock" %chin% names(h),
          "history has no surprise column" = "surprise" %chin% names(h))
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id")
h[, resid := perf - r_pre]
cat(sprintf("%s: %s scored races\n", TAG, format(nrow(h), big.mark = ",")))

# --- 1. does the decomposition actually hold? ---------------------------------
# Asserted rather than assumed. If resid is not shock + surprise then the whole
# argument above is wrong and nothing after this matters.
hs <- h[is.finite(shock) & is.finite(surprise)]
hs[, gap := resid - (shock + surprise)]
cat(sprintf("\n=== 1. resid == shock + surprise? ===\n"))
cat(sprintf("rows with both terms: %s of %s\n",
            format(nrow(hs), big.mark = ","), format(nrow(h), big.mark = ",")))
cat(sprintf("max |resid - (shock + surprise)| = %.3g\n", max(abs(hs$gap))))
cat(sprintf("share within 1e-8: %.2f%%\n", 100 * mean(abs(hs$gap) < 1e-8)))

# --- 2. how big is the shock, and where? --------------------------------------
# Per RACE, not per row: the shock is one number shared by the field, so counting
# it once per athlete would weight big fields more and misstate its variance.
cat("\n=== 2. the race-conditions term, one value per race ===\n")
rs <- unique(hs[, .(race_key, event_id, family, shock)])
sh <- rs[, .(races = .N, sd_shock_pct = round(100 * stats::sd(shock), 2)), by = family]
setorder(sh, -sd_shock_pct)
print(sh)
cat(sprintf("\npooled shock sd: %.3f%% over %s races\n",
            100 * stats::sd(rs$shock), format(nrow(rs), big.mark = ",")))

# --- 3. does adding it fix the calibration? -----------------------------------
# The shock variance is taken PER EVENT, because conditions vary far more in a
# road race than in a shot put circle, and a pooled value would over-correct one
# and under-correct the other.
vs <- rs[, .(v_shock = stats::var(shock), n_races = .N), by = event_id]
vs_pooled <- stats::var(rs$shock)
# an event with too few races cannot estimate its own shock variance
vs[n_races < 30, v_shock := vs_pooled]
h <- merge(h, vs[, .(event_id, v_shock)], by = "event_id", all.x = TRUE)
h[!is.finite(v_shock), v_shock := vs_pooled]
h[, band := cut(n_eff, c(-Inf, 1, 2, 3, 5, 8, 15, Inf),
                labels = c("<1", "1-2", "2-3", "3-5", "5-8", "8-15", "15+"))]
h[, resid_c := resid - stats::median(resid), by = event_id]
cal <- h[, .(races = .N,
             sd_z_now  = round(stats::sd(resid_c / sqrt(v_pre)), 3),
             sd_z_fixed = round(stats::sd(resid_c / sqrt(v_pre + v_shock)), 3)),
         by = band]
setorder(cal, band)
cat("\n=== 3. sd(z) before and after adding the race-conditions term ===\n")
print(cal)
cat(sprintf("\npooled: %.3f -> %.3f (target 1.000)\n",
            h[, stats::sd(resid_c / sqrt(v_pre))],
            h[, stats::sd(resid_c / sqrt(v_pre + v_shock))]))

cat("\n=== by family, after the fix ===\n")
fam <- h[, .(races = .N,
             sd_z_now = round(stats::sd(resid_c / sqrt(v_pre)), 3),
             sd_z_fixed = round(stats::sd(resid_c / sqrt(v_pre + v_shock)), 3)),
         by = family]
setorder(fam, -races)
print(fam)

cat("\nA value still above 1 after the fix means something ELSE is also missing -\n")
cat("most likely that v itself is an EWMA and lags a changing athlete. Report it\n")
cat("rather than tuning a fudge factor to force 1.000.\n")

f <- file.path(D, "predictive_variance.json")
writeLines(jsonlite::toJSON(list(tag = TAG, shock_sd_pooled = 100 * stats::sd(rs$shock),
                                 by_band = cal, by_family = fam,
                                 v_shock_by_event = vs),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
