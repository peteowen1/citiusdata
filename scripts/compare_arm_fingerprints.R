# Assert two backtest arms differ ONLY where they are supposed to.
#
# WHY. An A/B is only a measurement of the mechanism if everything else is held
# equal, and "everything else" here is 52 resolved settings -- calibration file,
# aging file, half-life, sigma mode, tier filter, history md5, and so on. A
# stale env var leaking in from a previous arm, or a data file changing under a
# resumed run, moves the result without moving the mechanism, and the number
# still looks like an effect. backtest_athletics.R already writes its resolved
# settings to `_arm.rds` in each cache; this reads both and diffs them.
#
# The md5 fields are the important ones: they catch the two arms having been run
# against DIFFERENT vintages of the corpus or calibration, which is exactly what
# happens when one arm runs before a rebuild and the other after.
#
# Usage:
#   Rscript compare_arm_fingerprints.R <ctrl_cache_dir> <arm_cache_dir> [expected_diff_fields...]

suppressMessages(library(data.table))
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 2) stop("need two cache directories")
ctrl_d <- a[1]; arm_d <- a[2]
expected <- if (length(a) > 2) a[-(1:2)] else c(
  "neighbour_combine", "neighbour_combine_events",
  "neighbour_link_days", "neighbour_link_athletes")

rd <- function(d) {
  f <- file.path(d, "_arm.rds")
  if (!file.exists(f)) stop(sprintf("no _arm.rds in %s -- did that arm run?", d))
  readRDS(f)
}
ctrl <- rd(ctrl_d); arm <- rd(arm_d)

keys <- union(names(ctrl), names(arm))
one <- function(x) if (is.null(x) || !length(x)) NA_character_ else paste(as.character(x), collapse = ",")
cmp <- data.table(field = keys,
                  ctrl = vapply(keys, function(k) one(ctrl[[k]]), character(1)),
                  arm  = vapply(keys, function(k) one(arm[[k]]),  character(1)))
cmp[, differs := !(ctrl == arm | (is.na(ctrl) & is.na(arm)))]
cmp[is.na(differs), differs := TRUE]

diffs <- cmp[differs == TRUE]
cat("fields that differ between the two arms:\n")
if (nrow(diffs)) print(diffs[, .(field, ctrl, arm)]) else cat("  (none)\n")

unexpected <- setdiff(diffs$field, expected)
missing    <- setdiff(expected, diffs$field)

if (length(missing))
  cat(sprintf("\nNOTE: expected to differ but identical: %s\n", paste(missing, collapse = ", ")))

if (length(unexpected)) {
  cat(sprintf("\nFAIL: %d field(s) differ that should have been held equal: %s\n",
              length(unexpected), paste(unexpected, collapse = ", ")))
  cat("The two arms are not comparable -- this measures those differences, not the mechanism.\n")
  quit(status = 1)
}
if (!length(diffs)) {
  cat("\nFAIL: the two arms are IDENTICAL -- the arm's mechanism never switched on.\n")
  quit(status = 1)
}
cat("\nFINGERPRINTS OK: the arms differ only in the mechanism under test.\n")
