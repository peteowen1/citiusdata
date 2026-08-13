# Paired A/B between two backtest arms, scored on the meets they BOTH hold.
#
# The backtest caches one file per meet, so an arm that has run 100 meets can be
# compared against a completed baseline immediately -- no baseline compute, and
# no waiting for a full run. A 100-meet subset carries ~950 races; the csigma
# win registered t = -13.6 on 3,621, which scales to t ~ 7 here, so anything
# worth adopting is detectable. A marginal result at this size earns a full run
# rather than a decision.
#
# It is a genuine paired test on identical races, not an offline proxy -- which
# matters, because three effects refuted overnight on 2026-07-30 had each passed
# an offline validation first.
#
# Usage:
#   CITIUS_QC_A=backtest_cache_csigma CITIUS_QC_B=backtest_cache_casym \
#     Rscript scripts/quick_compare.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

A <- Sys.getenv("CITIUS_QC_A", "backtest_cache_csigma")
B <- Sys.getenv("CITIUS_QC_B", "backtest_cache_casym")

load_cache <- function(dir) {
  fs <- list.files(file.path(OUT, dir), pattern = "[.]rds$", full.names = TRUE)
  if (!length(fs)) cli::cli_abort("No cache files in {.file {dir}}.")
  cids <- sub("[.]rds$", "", basename(fs))
  blobs <- lapply(fs, readRDS)
  names(blobs) <- cids
  blobs[vapply(blobs, function(b) is.list(b) && length(b) > 0L, logical(1))]
}
a <- load_cache(A); b <- load_cache(B)
common <- intersect(names(a), names(b))
cli::cli_alert_info("{A}: {length(a)} meet{?s} | {B}: {length(b)} | common: {length(common)}")
if (!length(common)) cli::cli_abort("No meets in common.")

flat <- function(blobs, cids) {
  x <- unlist(blobs[cids], recursive = FALSE)
  x <- Filter(function(z) is.list(z) && !is.null(z$pred), x)
  list(pred = rbindlist(lapply(x, `[[`, "pred"), fill = TRUE),
       outc = rbindlist(lapply(x, `[[`, "outc"), fill = TRUE))
}
fa <- flat(a, common); fb <- flat(b, common)

# Score only races whose winner is in the field, and only races BOTH arms hold.
races <- intersect(unique(fa$outc[, .(race_id, w = any(hit)), by = race_id][w == TRUE]$race_id),
                   unique(fb$outc[, .(race_id, w = any(hit)), by = race_id][w == TRUE]$race_id))
cli::cli_alert_info("{length(races)} scoreable race{?s} in common.")

brier_by_race <- function(f, col, hitcol) {
  p <- f$pred[race_id %in% races, .(race_id, athlete_id, p = get(col))]
  o <- f$outc[race_id %in% races, .(race_id, athlete_id, hit = get(hitcol))]
  m <- merge(p, o, by = c("race_id", "athlete_id"))
  m[, .(brier = mean((p - hit)^2)), by = race_id]
}

report <- function(col, hitcol, label) {
  ba <- brier_by_race(fa, col, hitcol); bb <- brier_by_race(fb, col, hitcol)
  m <- merge(ba, bb, by = "race_id", suffixes = c("_a", "_b"))
  d <- m$brier_b - m$brier_a
  tt <- stats::t.test(m$brier_b, m$brier_a, paired = TRUE)
  rel <- 100 * (mean(m$brier_b) - mean(m$brier_a)) / mean(m$brier_a)
  cat(sprintf("\n%-6s A %.5f  B %.5f  | diff %+.5f (%+.2f%%)  t = %+.2f  p = %.3g  n = %d\n",
              label, mean(m$brier_a), mean(m$brier_b), mean(d), rel,
              -tt$statistic, tt$p.value, nrow(m)))
  cat(sprintf("       B better in %d of %d races (%.0f%%)\n",
              sum(d < 0), nrow(m), 100 * mean(d < 0)))
  invisible(rel)
}
cat("\n=== PLACINGS (negative diff = B better) ===")
g <- report("p_gold", "hit", "gold")
md <- report("p_medal", "hit_medal", "medal")

# Marks must NOT move: this is pre-registered as a placings-only change, so a
# significant shift in MAE means the arm changed something it should not have.
cat("\n=== MARKS (must stay flat) ===\n")
champs <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
champs[, athlete_id := as.character(athlete_id)]
act <- champs[!is.na(mark) & !is.na(race_key), .(race_id = race_key, athlete_id, actual = mark)]
mk <- function(f) {
  if (!"median_mark" %in% names(f$pred)) return(NULL)
  p <- f$pred[race_id %in% races, .(race_id, athlete_id = as.character(athlete_id), median_mark)]
  m <- merge(p, act, by = c("race_id", "athlete_id"))
  m[is.finite(median_mark) & is.finite(actual) & actual > 0,
    .(race_id, athlete_id, ape = abs(median_mark - actual) / actual)]
}
ma <- mk(fa); mb <- mk(fb)
if (is.null(ma) || is.null(mb)) {
  cat("predictions carry no median_mark; marks not scorable from cache\n")
} else {
  m <- merge(ma, mb, by = c("race_id", "athlete_id"), suffixes = c("_a", "_b"))
  tt <- stats::t.test(m$ape_b, m$ape_a, paired = TRUE)
  cat(sprintf("MAE   A %.4f%%  B %.4f%%  | diff %+.4f pp  t = %+.2f  p = %.3g  n = %s\n",
              100 * mean(m$ape_a), 100 * mean(m$ape_b),
              100 * (mean(m$ape_b) - mean(m$ape_a)), tt$statistic, tt$p.value,
              format(nrow(m), big.mark = ",")))
}

cat("\n--- pre-registered thresholds: gold Brier <= -1% relative, marks not significantly worse ---\n")
cat(sprintf("gold %+.2f%%  ->  %s\n", g, if (g <= -1) "MEETS threshold" else "does NOT meet threshold"))
