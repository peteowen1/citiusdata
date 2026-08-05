# Did the season adjustment actually fire?
#
# Run this BEFORE reading the scorecard. The dangerous outcome of this A/B is not
# "season does not help" -- it is a dead heat produced by the offsets never being
# applied, which is indistinguishable from a null result on the scorecard and
# reads as a clean negative. That exact failure has a name here: a column
# narrowed away upstream silently disables an adjustment and the A/B reports no
# difference. `venue_country` and `indoor` are both new to `keep_cols`, so this
# run is precisely the case where it could happen.
#
# A real null shows many predictions moving by small amounts. A dead adjustment
# shows ZERO predictions moving.
suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"

off <- readRDS(file.path(D, "backtest_season_off.rds"))
on  <- readRDS(file.path(D, "backtest_season_on.rds"))

a <- as.data.table(off$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                        g_off = p_gold, m_off = median_mark)]
b <- as.data.table(on$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                       g_on = p_gold, m_on = median_mark)]
d <- merge(a, b, by = c("race_id", "athlete_id"))
cat(sprintf("matched predictions: %s across %s races\n",
            format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ",")))

moved_g <- d[abs(g_on - g_off) > 1e-9, .N]
moved_m <- d[is.finite(m_on) & is.finite(m_off) & abs(m_on - m_off) > 1e-9, .N]
cat(sprintf("gold probabilities that moved : %s (%.1f%%)\n",
            format(moved_g, big.mark = ","), 100 * moved_g / nrow(d)))
cat(sprintf("median marks that moved       : %s (%.1f%%)\n",
            format(moved_m, big.mark = ","), 100 * moved_m / nrow(d)))

if (moved_g == 0L && moved_m == 0L) {
  cat("\nDEAD HEAT WITH ZERO MOVEMENT.\n")
  cat("The season/indoor offsets did NOT reach estimate_ability(). Do not read\n")
  cat("the scorecard as a null result. Check, in order:\n")
  cat("  1. `venue_country` and `indoor` survive `keep_cols` in backtest_athletics.R\n")
  cat("  2. calibration_season_on.rds carries non-empty $season and $indoor\n")
  cat("  3. the two arms actually used different CITIUS_BT_CALIBRATION values\n")
  quit(status = 1)
}

cat(sprintf("\nmark shift: median %+.4f%%, mean |shift| %.4f%%, max |shift| %.3f%%\n",
            100 * median(log(d$m_on / d$m_off), na.rm = TRUE),
            100 * mean(abs(log(d$m_on / d$m_off)), na.rm = TRUE),
            100 * max(abs(log(d$m_on / d$m_off)), na.rm = TRUE)))
cat(sprintf("gold prob shift: mean |delta| %.5f, max |delta| %.4f\n",
            mean(abs(d$g_on - d$g_off)), max(abs(d$g_on - d$g_off))))

# The arms must also have been built from DIFFERENT calibrations. Same-file runs
# would move nothing and land in the branch above, but a mislabelled pair could
# move things for the wrong reason.
cat("\ncalibration recorded by each arm:\n")
cat("  off:", off$meta$calibration %||% "(unrecorded)",
    "md5", substr(off$meta$calibration_md5 %||% "?", 1, 12), "\n")
cat("  on :", on$meta$calibration %||% "(unrecorded)",
    "md5", substr(on$meta$calibration_md5 %||% "?", 1, 12), "\n")
# The md5 is the check that matters. Two different FILENAMES can hold identical
# content -- which is what a mis-built pair looks like -- and that would move
# nothing while appearing correctly configured.
if (identical(off$meta$calibration, on$meta$calibration)) {
  cat("\nBOTH ARMS RECORD THE SAME CALIBRATION FILE - the comparison is invalid.\n")
  quit(status = 1)
}
if (!is.null(off$meta$calibration_md5) &&
    identical(off$meta$calibration_md5, on$meta$calibration_md5)) {
  cat("\nDIFFERENT FILENAMES, IDENTICAL CONTENT - the pair was built wrong.\n")
  quit(status = 1)
}
cat("\nOK: the adjustment fired and the arms used different calibrations.\n")
