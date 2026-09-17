# Paired marks A/B between two backtest arms, marks FIRST.
#
# WHY NOT quick_compare.R. That one is built for placings-primary arms: it
# reports gold/medal Brier first and treats marks as a "must stay flat" guard,
# so on a CITIUS_BT_MARKS_ONLY arm it dies in the placings t-test before it ever
# reaches the marks block (p_gold is NA by construction). For a mechanism that
# can only move marks -- a whole-field shock like altitude, which cannot reorder
# a field -- the two are the wrong way round.
#
# THE SPLIT THAT MATTERS IS ALTITUDE, NOT THE POOL. An altitude term changes a
# prediction only where alt_m > 0, and most M1 meets are near sea level, so a
# pooled MAE dilutes the effect toward zero and would read as "no effect" for a
# term that is working exactly as designed. Reporting the pooled number alone
# would be the "use the metric with power" failure in its most direct form.
# Bands, and per family, are the test.
#
# Paired per (race_id, athlete_id) on the races BOTH arms hold, so the two arms
# are compared on identical rows and no meet-mix difference can leak in.
#
# Usage:
#   CITIUS_MA_A=bt_cache_alt_ctrl CITIUS_MA_B=bt_cache_alt_on \
#     Rscript citiusdata/scripts/diagnostics/compare_marks_arms.R
#
# Run compare_arm_fingerprints.R first. This script does not verify that the two
# arms differ only in the mechanism; it assumes someone has.

suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")

A <- Sys.getenv("CITIUS_MA_A", "bt_cache_alt_ctrl")
B <- Sys.getenv("CITIUS_MA_B", "bt_cache_alt_on")

load_arm <- function(dir) {
  d <- file.path(OUT, dir)
  fs <- setdiff(list.files(d, pattern = "\\.rds$"), "_arm.rds")
  out <- rbindlist(lapply(fs, function(f) {
    o <- readRDS(file.path(d, f))
    if (!length(o)) return(NULL)
    rbindlist(lapply(o, function(r) {
      if (is.null(r$pred) || !"median_mark" %in% names(r$pred)) return(NULL)
      as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), median_mark)]
    }), fill = TRUE)
  }), fill = TRUE)
  unique(out, by = c("race_id", "athlete_id"))
}

pa <- load_arm(A); pb <- load_arm(B)
cat(sprintf("%s: %s rows | %s: %s rows\n", A, format(nrow(pa), big.mark = ","),
            B, format(nrow(pb), big.mark = ",")))

# Actual marks, event and altitude from the PARQUET STORE, not DuckDB.
# `alt_m` exists only in the stores -- build_stores.R joins venue_elevation on
# the way out, and citius.duckdb's championship_results has no such column. That
# is also the more faithful source: the store is what the backtest itself read,
# so this compares against the exact alt_m the model saw rather than a
# re-derived one.
suppressMessages(library(arrow))
STORE <- Sys.getenv("CITIUS_MA_STORE", "athletics_corpus_store")
champs <- as.data.table(
  open_dataset(file.path(OUT, STORE)) |>
    dplyr::select(race_key, athlete_id, mark, event_id, alt_m) |>
    dplyr::collect())
champs[, athlete_id := as.character(athlete_id)]
act <- champs[!is.na(mark) & !is.na(race_key) & mark > 0,
              .(race_id = race_key, athlete_id, actual = mark, event_id, alt_m)]
act <- unique(act, by = c("race_id", "athlete_id"))

fam <- as.data.table(citius_events())[, .(event_id, family)]
act <- merge(act, fam, by = "event_id", all.x = TRUE)

ape <- function(p) {
  m <- merge(p, act, by = c("race_id", "athlete_id"))
  m[is.finite(median_mark) & is.finite(actual) & actual > 0][
    , ape := abs(median_mark - actual) / actual][]
}
ma <- ape(pa); mb <- ape(pb)
m <- merge(ma[, .(race_id, athlete_id, ape_a = ape, family, alt_m)],
           mb[, .(race_id, athlete_id, ape_b = ape)],
           by = c("race_id", "athlete_id"))
if (!nrow(m)) stop("no paired rows -- do the two arms share any meets yet?")

cat(sprintf("\npaired rows: %s across %s races\n",
            format(nrow(m), big.mark = ","), format(uniqueN(m$race_id), big.mark = ",")))
cat(sprintf("rows with a known altitude: %.1f%%; rows above 1000 m: %s\n",
            100 * mean(is.finite(m$alt_m)), format(m[is.finite(alt_m) & alt_m > 1000, .N], big.mark = ",")))

rep_block <- function(d, label) {
  if (nrow(d) < 30) return(data.table(cut = label, n = nrow(d), mae_a = NA_real_,
                                      mae_b = NA_real_, diff_pp = NA_real_, t = NA_real_, p = NA_real_))
  tt <- tryCatch(stats::t.test(d$ape_b, d$ape_a, paired = TRUE), error = function(e) NULL)
  data.table(cut = label, n = nrow(d),
             mae_a = 100 * mean(d$ape_a), mae_b = 100 * mean(d$ape_b),
             diff_pp = 100 * (mean(d$ape_b) - mean(d$ape_a)),
             t = if (is.null(tt)) NA_real_ else unname(tt$statistic),
             p = if (is.null(tt)) NA_real_ else tt$p.value)
}

cat("\n=== BY ALTITUDE BAND — this is where an altitude term must show ===\n")
cat("MAE is mean absolute percentage error on the mark; LOWER is better.\n")
cat("diff_pp = B minus A in percentage points; NEGATIVE means the altitude arm is better.\n")
m[, band := cut(fifelse(is.finite(alt_m), alt_m, -1),
                c(-Inf, -0.5, 200, 800, 1500, 2200, Inf),
                labels = c("unknown", "<200m", "200-800m", "800-1500m", "1500-2200m", ">2200m"))]
print(rbindlist(lapply(levels(m$band), function(b) rep_block(m[band == b], b)))[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])

cat("\n=== BY FAMILY, rows above 800 m only (where the term is material) ===\n")
hi <- m[is.finite(alt_m) & alt_m > 800]
if (nrow(hi) < 30) {
  cat(sprintf("only %d paired rows above 800 m -- not enough to split by family yet.\n", nrow(hi)))
} else {
  print(rbindlist(lapply(sort(unique(hi$family)), function(f) rep_block(hi[family == f], f)))[
    , lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
}

cat("\n=== POOLED (expected to be near flat: most rows are sea level) ===\n")
print(rep_block(m, "all rows")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
cat("\nA pooled null is NOT evidence against the term. Read the bands.\n")
