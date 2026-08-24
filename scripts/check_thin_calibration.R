# Is a thin-evidence rating WRONG, or just uncertain?
#
# 312 rows in the published top tens rest on fewer than three effective races,
# and that has been treated as a defect since it was noticed. Evidence shrinkage
# was the attempted fix and it was refuted hard: pulling thin athletes toward the
# event mean costs 1.03 pp of out-of-sample weighted concordance, 6.5x the noise
# floor. A correction that large in the wrong direction is evidence about the
# thing being corrected - if thin ratings were systematically too high, shrinking
# them would have helped, and it did the opposite.
#
# So separate two questions that "thin evidence" runs together:
#
#   BIAS   - does an athlete with little evidence run WORSE than their rating
#            says? If yes the rating is inflated and something must change. If
#            the residual is centred, the level is honest.
#   SPREAD - is the model's own uncertainty (v_pre) right for them? A rating can
#            be unbiased and still be reported far too confidently, and that is a
#            different defect with a different fix.
#
# The test is a straight calibration check on the engine's own history: r_pre is
# the rating carried INTO a race and n_eff the evidence behind it, both strictly
# pre-race, so `perf - r_pre` is an honest out-of-sample residual.
#
# CENTRED PER EVENT, because a rating converges to an athlete's typical race
# while these rows include finals, and that level difference is the per-event
# offset the display already applies. Leaving it in would show a bias in every
# band and say nothing about evidence depth.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre) & is.finite(n_eff)]
stopifnot("history is empty" = nrow(h) > 100000)
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id")
h[, resid := perf - r_pre]
h[, resid_c := resid - stats::median(resid), by = event_id]
h[, band := cut(n_eff, c(-Inf, 1, 2, 3, 5, 8, 15, Inf),
                labels = c("<1", "1-2", "2-3", "3-5", "5-8", "8-15", "15+"))]
cat(sprintf("%s: %s scored races, %s athlete-events\n", TAG,
            format(nrow(h), big.mark = ","),
            format(uniqueN(paste(h$athlete_id, h$event_id)), big.mark = ",")))

# --- 1. BIAS ------------------------------------------------------------------
# In perf space a POSITIVE residual is a better performance than the rating
# predicted, for every family, because perf already carries the orientation.
cat("\n=== 1. BIAS: does a thin rating over-predict? ===\n")
b <- h[, .(races = .N,
           median_resid_pct = round(100 * (exp(stats::median(resid_c)) - 1), 3),
           mean_resid_pct   = round(100 * (exp(mean(resid_c)) - 1), 3),
           beat_rating_pct  = round(100 * mean(resid_c > 0), 1)), by = band]
setorder(b, band)
print(b)
cat("\nbeat_rating_pct should sit near 50 in EVERY band. Below 50 for thin bands\n")
cat("would mean thin ratings are too high - the assumption shrinkage was built on.\n")

# --- 2. SPREAD ----------------------------------------------------------------
# v_pre is the model's own variance for that prediction. If it is right, the
# standardised residual z = resid / sqrt(v_pre) has sd 1 in every band. sd well
# above 1 means the model is MORE confident than it should be.
cat("\n=== 2. SPREAD: is the model's own uncertainty right? ===\n")
z <- h[is.finite(v_pre) & v_pre > 0]
s <- z[, .(races = .N,
           actual_sd_pct = round(100 * stats::sd(resid_c), 2),
           sd_of_z = round(stats::sd(resid_c / sqrt(v_pre)), 3),
           claimed_sd_pct = round(100 * mean(sqrt(v_pre)), 2)), by = band]
setorder(s, band)
print(s)
cat("\nsd_of_z = 1.0 means the stated uncertainty is right. Above 1 means the\n")
cat("model is OVER-CONFIDENT by that factor - the interval is too narrow.\n")

# --- 3. what this means for the published top tens ----------------------------
cat("\n=== 3. the athletes actually on the page ===\n")
d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
d[, athlete_id := as.character(athlete_id)]
# The display table is written already ordered and carries its own `rk`. An
# earlier version re-sorted on R_rank, which is INTERNAL to form_display_marks.R
# and not a column of the output - so use what is actually there.
stopifnot("display table has no rk column" = "rk" %chin% names(d))
top <- d[rk <= 10]
cat(sprintf("published top tens: %d rows, %d with n_eff < 3 (%.1f%%)\n",
            nrow(top), sum(top$n_eff < 3), 100 * mean(top$n_eff < 3)))
h[, key := paste(athlete_id, event_id)]
top[, key := paste(athlete_id, event_id)]
# their NEXT races after the rating was formed are the honest test, but the
# history ends where the rating does, so use their whole rated record instead and
# say so rather than implying a forward test.
tb <- h[key %chin% top$key, .(races = .N,
        beat_rating_pct = round(100 * mean(resid_c > 0), 1),
        sd_of_z = round(stats::sd(resid_c / sqrt(v_pre), na.rm = TRUE), 3)),
        by = .(thin = n_eff < 3)]
print(tb)

f <- file.path(D, "thin_calibration.json")
writeLines(jsonlite::toJSON(list(tag = TAG, bias = b, spread = s, top_ten = tb),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
cat("\nIf bias is flat and only the spread is wrong, thin evidence is not a\n")
cat("ranking defect at all - it is an UNCERTAINTY reporting defect, and the fix\n")
cat("belongs in what the page shows about confidence, not in the ordering.\n")
