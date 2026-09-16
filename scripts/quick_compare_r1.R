# Paired A/B between two backtest arms, restricted to R1 races -- races whose
# OWN race_code is in the empirically top-performing bucket (OW/GW/GL/DF/A),
# not the meet's catalogue tier. See docs/reference/tier-terminology.md and
# modelling-traps.md's "R1 vs M1": a T1_elite/M1 meeting still contains plenty
# of lower-code support races (14% of meetings mix tiers internally), so
# scoring the whole M1 population dilutes a race-level effect with races the
# mechanism under test barely touches.
#
# Same paired-race, common-cache logic as quick_compare.R (see that file for
# the rationale on cache reuse and t-stat scaling); this only adds the R1
# filter before scoring, applied on top of whatever meets/races the two
# arms already hold in common.
#
# Usage:
#   CITIUS_QC_A=backtest_cache_ctrl CITIUS_QC_B=backtest_cache_trt \
#     Rscript scripts/quick_compare_r1.R
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
cli::cli_alert_info("{length(races)} scoreable race{?s} in common (before the R1 filter).")

# --- R1 FILTER: restrict to races whose OWN race_code is top-tier ------------
R1_CODES <- c("OW", "GW", "GL", "DF", "A")
champs <- tryCatch(
  # Only the four columns used below: race_key/race_code for the R1 filter,
  # athlete_id/mark for the marks block. Without `columns=` this is SELECT *
  # -- 33 columns x 5M rows for four of them. Add here if you read another.
  with_citius_db_connection(function(conn) load_championship_results(
    conn, columns = c("race_key", "race_code", "athlete_id", "mark")), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(champs) || !nrow(champs)) champs <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
race_code_of <- unique(champs[!is.na(race_key), .(race_key, race_code)], by = "race_key")
r1_race_ids <- race_code_of[race_code %in% R1_CODES]$race_key
races_r1 <- intersect(races, r1_race_ids)
cli::cli_alert_info("{length(races_r1)} of {length(races)} scoreable races are R1 ({.val {R1_CODES}}) -- {round(100*length(races_r1)/length(races), 1)}%.")
if (!length(races_r1)) cli::cli_abort("No R1 races in common -- nothing to score.")
races <- races_r1

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
cat("\n=== R1 PLACINGS (negative diff = B better) ===")
g <- report("p_gold", "hit", "gold")
md <- report("p_medal", "hit_medal", "medal")

# Marks must NOT move: the tier_class remap is pre-registered as a
# placings/weighting change via result_weight() and race_shock, so a
# significant shift in MAE means the arm changed something beyond that.
cat("\n=== R1 MARKS (must stay flat) ===\n")
act <- champs[!is.na(mark) & !is.na(race_key), .(race_id = race_key, athlete_id, actual = mark)]
act[, athlete_id := as.character(athlete_id)]
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
