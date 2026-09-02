# DO WE ALREADY HOLD BACKTEST ARMS THAT DIFFER IN THE CONDITION SHOCK?
#
# p_medal underpredicts longshots by 3.2 to 3.6 standard errors. Three
# mechanisms would each produce that: a performance tail too thin, a per-athlete
# sigma too small for weaker athletes, or a shared condition shock too narrow.
#
# The shock is the one worth testing first, and it is also the one most likely
# to be dismissed for the wrong reason. The verse rule says a shock shared by a
# whole field cannot change finishing order - true, but only when every athlete
# has the SAME sensitivity to it. The engine estimates a per-athlete sensitivity
# s_i, and a shared shock times a spread of sensitivities DOES reorder a field.
# That is why the calibration must be quoted as the product sd(s_i) x
# condition_sd, and it is why a too-narrow shock would suppress exactly the
# upsets that let an outsider medal, while leaving head-to-head concordance
# between two well-known athletes almost untouched. That asymmetric signature is
# what distinguishes it from the other two mechanisms.
#
# Before running anything expensive: a couple of dozen backtest arms are already
# on disk. If any of them used a calibration with a different condition_sd, the
# comparison is free. Read their recorded meta rather than their file names -
# arm names in this project do not reliably record what ran.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")

fs <- list.files(OUT, pattern = "^backtest.*\\.rds$", full.names = TRUE)
stopifnot("no backtest arms on disk" = length(fs) > 0)
cat(sprintf("%d backtest arm(s) on disk\n\n", length(fs)))

rows <- rbindlist(lapply(fs, function(f) {
  b <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(b) || !"meta" %chin% names(b)) return(NULL)
  m <- b$meta
  g <- function(k) {
    v <- m[[k]]
    if (is.null(v) || !length(v)) return(NA_character_)
    as.character(v)[1]
  }
  data.table(arm = sub("\\.rds$", "", basename(f)),
             calibration = g("calibration"),
             cal_md5 = substr(g("calibration_md5"), 1, 8),
             n_sims = g("n_sims"),
             races = g("races_scored"),
             run_at = g("run_at"),
             preds = if (is.data.frame(b$predictions)) nrow(b$predictions) else NA_integer_)
}), fill = TRUE)
stopifnot("no arm carried a readable meta block" = nrow(rows) > 0)

setorder(rows, calibration, arm)
print(rows[, .(arm, calibration, cal_md5, races, preds)])

cat("\n=== distinct calibrations in use ===\n")
print(rows[, .(arms = .N, example = arm[1]), by = .(calibration, cal_md5)][order(-arms)])

# What condition_sd does each distinct calibration actually carry? That is the
# number under test, and it lives in the calibration file, not in the backtest.
cat("\n=== condition_sd inside each calibration file ===\n")
cals <- unique(stats::na.omit(rows$calibration))
for (cf in cals) {
  pth <- file.path(OUT, cf)
  if (!file.exists(pth)) { cat(sprintf("%-44s MISSING from disk\n", cf)); next }
  cal <- tryCatch(readRDS(pth), error = function(e) NULL)
  if (is.null(cal)) { cat(sprintf("%-44s unreadable\n", cf)); next }
  cs <- cal$condition_sd
  if (is.null(cs)) {
    cat(sprintf("%-44s no condition_sd element (has: %s)\n", cf,
                paste(utils::head(names(cal), 8), collapse = ", ")))
  } else {
    v <- if (is.data.frame(cs)) cs[[intersect(c("condition_sd","value","sd"),
                                              names(cs))[1]]] else as.numeric(cs)
    v <- v[is.finite(v)]
    cat(sprintf("%-44s n=%d  median %.4f  range %.4f to %.4f\n",
                cf, length(v), stats::median(v), min(v), max(v)))
  }
}
cat("\nIf two arms share everything except a calibration whose condition_sd\n")
cat("differs, their p_medal curves answer the question with no new compute.\n")
cat("If every arm shares one calibration, the test needs a fresh run.\n")
