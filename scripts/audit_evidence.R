# Does the model pay athletes for uncertainty?
#
# THE STANDING DETECTOR. Run it against every backtest artefact. Unlike a data
# sweep it does not need to know what went wrong: it asks whether thinly-raced
# athletes are credited with more win probability than they go on to win, which
# is the symptom of ANY defect that inflates their spread -- a corrupt mark, a
# non-robust estimator, a shrinkage target that is too small, a missing filter.
#
# Baseline measured 2026-07-31 on `backtest_csigma.rds`, 44,607 predictions:
#
#   w_total   predicted   observed   ratio
#   < 1        0.0509      0.0412     0.81
#   1-2        0.0497      0.0426     0.86
#   2-5        0.0563      0.0475     0.84
#   5-10       0.0834      0.0820     0.98
#   10+        0.1127      0.1162     1.03
#
# Thin athletes over-credited by ~16-19%, well-observed ones slightly
# under-credited. Among athletes given a real chance (p_gold > 0.10), the
# w_total < 1 band was predicted at 0.217 and won 0.177 -- a 4-point gap in the
# band that decides races.
#
# Those numbers had been sitting in every backtest artefact for weeks, because
# nothing ever bucketed the reliability table by EVIDENCE. A calibration curve
# pooled over all athletes hides it: the two errors cancel.
#
# Usage:  CITIUS_AUDIT_BT=backtest_crob.rds Rscript scripts/audit_evidence.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

BT <- Sys.getenv("CITIUS_AUDIT_BT", "backtest_csigma.rds")
bt <- readRDS(file.path(OUT, BT))
p <- as.data.table(bt$predictions); o <- as.data.table(bt$outcomes)
if (!"w_total" %in% names(p)) {
  cli::cli_abort("{.file {BT}} carries no {.field w_total}; re-run the backtest.")
}
d <- merge(p[, .(race_id, athlete_id, p_gold, p_medal, w_total, shrinkage)],
           o[, .(race_id, athlete_id, hit, hit_medal)],
           by = c("race_id", "athlete_id"))
cli::cli_alert_info("{BT}: {format(nrow(d), big.mark = ',')} prediction{?s} over {uniqueN(d$race_id)} race{?s}.")

d[, band := cut(w_total, c(-Inf, 1, 2, 5, 10, Inf),
                labels = c("<1", "1-2", "2-5", "5-10", "10+"))]

show <- function(dt, pcol, hcol, label) {
  t <- dt[, .(n = .N,
              predicted = round(mean(get(pcol)), 4),
              observed  = round(mean(get(hcol)), 4),
              ratio     = round(mean(get(hcol)) / mean(get(pcol)), 3)), by = band]
  setorder(t, band)
  cat("\n", label, "\n", sep = "")
  print(t)
  invisible(t)
}
cat("\n=== ALL PREDICTIONS ===")
g <- show(d, "p_gold", "hit", "gold")
show(d, "p_medal", "hit_medal", "medal")

cat("\n=== WHERE IT DECIDES RACES (p_gold > 0.10) ===")
show(d[p_gold > 0.10], "p_gold", "hit", "gold")

cat("\n=== PROBABILITY MASS ON THIN ATHLETES ===\n")
cat(sprintf("gold probability held by w_total < 2: %.1f%%\n",
            100 * sum(d[w_total < 2]$p_gold) / sum(d$p_gold)))
cat(sprintf("golds actually won by them          : %.1f%%\n",
            100 * sum(d[w_total < 2]$hit) / sum(d$hit)))

THIN_BANDS <- c("<1", "1-2", "2-5")
thin <- g[band %in% THIN_BANDS]
# A vacuous state must never print PASS. min(numeric(0)) is Inf and an all-NaN
# ratio column both slide straight past `worst < 0.90` -- the unverifiable rows
# count as their own failing assertion instead.
#
# Presence is checked by LABEL, not `n == 0`: a grouped aggregation only emits
# rows for bands that exist in the data, so an unrepresented band is ABSENT
# from `thin`, never present with n = 0 -- an `any(n == 0)` guard here can
# literally never fire (review 2026-08-14).
if (!nrow(d) || !all(THIN_BANDS %in% thin$band) || any(!is.finite(thin$ratio))) {
  cat("\n--------------------------------------------------------------\n")
  cat("FAIL: unverifiable -- the merged table is empty, a thin-evidence band\n")
  cat(sprintf("is missing entirely (have: %s), or a ratio is non-finite.\n",
              paste(intersect(THIN_BANDS, thin$band), collapse = ", ")))
  cat("A PASS here would be vacuous.\n")
  cat("--------------------------------------------------------------\n")
  quit(save = "no", status = 1)
}
worst <- min(thin$ratio, na.rm = TRUE)
cat("\n--------------------------------------------------------------\n")
cat(sprintf("worst thin-band ratio: %.3f  (1.0 = calibrated)\n", worst))
if (worst < 0.90) {
  cat("FAIL: thin-evidence athletes are being paid for uncertainty.\n")
  cat("--------------------------------------------------------------\n")
  # The substantive verdict must reach automation the same way the vacuous one
  # does -- printing FAIL and exiting 0 is a green run to anything that gates
  # on status.
  quit(save = "no", status = 1)
}
cat("PASS: no material evidence-dependent miscalibration.\n")
cat("--------------------------------------------------------------\n")
