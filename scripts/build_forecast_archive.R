# Recover every forecast we already computed and threw away.
#
# WHY THIS EXISTS. Asking "which config is better on M1?" currently costs a
# fresh backtest arm -- roughly an hour -- even though we have ALREADY computed
# the answer. `backtest_athletics.R` caches per meet, and each cached race
# carries the full per-athlete prediction:
#
#     athlete_id, p_gold, p_medal, p_top8, median_rank, median_mark,
#     shrinkage, w_total
#
# Across 57 arm caches that is well over a million predictions sitting on disk,
# scattered, unqueryable, unlabelled by config, and deleted by anyone tidying.
# This flattens them into one parquet keyed by config, so the question becomes
# a filter instead of a re-run.
#
# WHAT IT CANNOT RECOVER, and says so rather than faking it: the per-position
# distribution (pos_1..pos_8). The backtest keeps `median_rank` and three
# summary probabilities, not the full rank vector, so "what were his odds of
# finishing 4th" is NOT answerable from the caches -- only from
# build_forecast_store.R, which re-simulates and keeps it. Rows from here carry
# source = "backtest_cache" and NA for those columns; rows from there carry
# source = "simulated_full". A reader must be able to tell which, because an
# NA that means "never recorded" and an NA that means "genuinely zero" are
# different facts.
#
# Config identity comes from each cache's own `_arm.rds` fingerprint -- the
# same 52-field record model_registry.R reads -- so a row can always be traced
# back to the exact model vintage that produced it.
#
# Usage:  Rscript citiusdata/scripts/build_forecast_archive.R [cache_dir ...]
#         (no args = every backtest_cache_* / bt_cache_* on disk)

VERSE <- here::here()
suppressMessages({library(data.table); library(arrow)})
D   <- file.path(VERSE, "citiusdata", "data")
OUT <- file.path(D, "forecasts")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

args <- commandArgs(trailingOnly = TRUE)
dirs <- if (length(args)) file.path(D, args) else {
  dd <- list.dirs(D, recursive = FALSE)
  dd[grepl("(backtest_cache_|bt_cache_)", basename(dd))]
}
dirs <- dirs[dir.exists(dirs)]
if (!length(dirs)) cli::cli_abort("No cache directories found.")
say("scanning %d cache director%s", length(dirs), if (length(dirs) == 1) "y" else "ies")

.chr <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1]

one_cache <- function(dir) {
  fp_f <- file.path(dir, "_arm.rds")
  fp <- if (file.exists(fp_f)) readRDS(fp_f) else NULL
  blobs <- setdiff(list.files(dir, pattern = "\\.rds$", full.names = TRUE), fp_f)
  if (!length(blobs)) return(NULL)

  rows <- lapply(blobs, function(bf) {
    b <- tryCatch(readRDS(bf), error = function(e) NULL)
    if (!is.list(b) || !length(b)) return(NULL)
    # One blob is a list of races; each race is list(pred, outc).
    pr <- lapply(b, function(r) {
      if (!is.list(r) || is.null(r$pred) || !nrow(r$pred)) return(NULL)
      p <- as.data.table(r$pred)
      # Attach the truth where the cache kept it, so a config comparison and a
      # scored check can come from the same table rather than two joins.
      if (!is.null(r$outc) && nrow(r$outc)) {
        o <- as.data.table(r$outc)[, .(race_id, athlete_id,
                                       hit = as.logical(hit),
                                       hit_medal = as.logical(hit_medal))]
        p <- merge(p, o, by = intersect(c("race_id", "athlete_id"), names(p)),
                   all.x = TRUE)
      }
      p
    })
    rbindlist(Filter(Negate(is.null), pr), fill = TRUE)
  })
  out <- rbindlist(Filter(function(x) !is.null(x) && nrow(x), rows), fill = TRUE)
  if (!nrow(out)) return(NULL)

  out[, `:=`(
    arm             = sub("^(backtest_cache_|bt_cache_)", "", basename(dir)),
    cache           = basename(dir),
    source          = "backtest_cache",
    calibration     = .chr(fp$calibration),
    calibration_md5 = .chr(fp$calibration_md5),
    tier_filter     = .chr(fp$tier_filter),
    n_sims          = if (is.null(fp$n_sims)) NA_integer_ else as.integer(fp$n_sims),
    adjust_race     = if (is.null(fp$adjust_race)) NA else as.logical(fp$adjust_race)
  )]
  # race_id is `<competition_id>|<wa_event_id>|<round>|<race_number>`; the
  # competition is the only part we need to join meet_tier, and parsing it
  # beats re-deriving it.
  out[, competition_id := tstrsplit(as.character(race_id), "\\|", keep = 1L)[[1]]]
  out[]
}

all <- rbindlist(lapply(seq_along(dirs), function(i) {
  r <- one_cache(dirs[i])
  if (!is.null(r)) say("  %-46s %s rows", basename(dirs[i]), format(nrow(r), big.mark = ","))
  r
}), fill = TRUE)

if (!nrow(all)) cli::cli_abort("No predictions recovered.")

# Columns the forward store has and the caches never recorded. Named and left
# NA rather than omitted, so the schemas line up and the gap is visible.
for (cn in paste0("pos_", 1:8)) if (!cn %in% names(all)) all[, (cn) := NA_real_]

cat_f <- file.path(D, "competition_catalogue.parquet")
if (file.exists(cat_f)) {
  ct <- as.data.table(read_parquet(cat_f))[, .(competition_id = as.character(competition_id),
                                               meet_tier, comp_name, first_date)]
  all <- merge(all, ct, by = "competition_id", all.x = TRUE)
  cov <- 100 * mean(!is.na(all$meet_tier))
  say("meet_tier attached to %.1f%% of rows", cov)
  if (cov < 50) cli::cli_warn("meet_tier coverage is only {round(cov,1)}% -- check the competition_id join type.")
} else {
  cli::cli_warn("No competition_catalogue.parquet; meet_tier not attached.")
}

f <- file.path(OUT, "forecast_archive.parquet")
write_parquet(all, f)
say("wrote %s: %s rows, %d arms, %s races, %s athletes",
    basename(f), format(nrow(all), big.mark = ","), uniqueN(all$arm),
    format(uniqueN(all$race_id), big.mark = ","),
    format(uniqueN(all$athlete_id), big.mark = ","))

say("by meet_tier:")
print(all[, .(rows = .N, races = uniqueN(race_id), arms = uniqueN(arm)), by = meet_tier][order(-rows)])

# PER-ARM MEET COUNTS, AND A FLAG FOR THE PARTIAL ONES.
#
# This archive is structurally immune to the "newest file wins" trap -- it reads
# EVERY cache and tags every row with its arm, so nothing is pooled and there is
# no selection step to get wrong. (The auspol session hit that trap the hard way
# on 2026-09-17: a genuinely newer file existed for two of its pairs only
# because a rerun had been restricted to fewer pairs, so "newest overall"
# silently orphaned the pairs it did not cover.)
#
# The hole it DOES have is one level along: a partially-run arm lands here as a
# perfectly legitimate arm, and nothing says it holds 8 meets while the arm
# someone wants to compare it against holds 120. Three such caches existed side
# by side today (8, 40 and 120 meets) during optimisation work. A comparison
# across them would be wrong and would look fine.
#
# So print the meet count per arm and name the short ones. The bar is relative
# to the median arm, not absolute, because a deliberately small pool is a valid
# thing to run -- what matters is that it is not silently mixed with a big one.
.arm_n <- all[, .(meets = uniqueN(competition_id), races = uniqueN(race_id),
                  rows = .N), by = arm][order(meets)]
.med <- stats::median(.arm_n$meets)
say("\nper-arm coverage (median arm has %d meets):", .med)
print(.arm_n)
.short <- .arm_n[meets < 0.5 * .med]
if (nrow(.short)) {
  cli::cli_warn(c(
    "{nrow(.short)} arm{?s} hold{?s/} fewer than half the median arm's meets.",
    "!" = "{paste(.short$arm, .short$meets, sep = ': ', collapse = '; ')}",
    "i" = "These are PARTIAL runs. Comparing one against a full arm compares different meet sets, which no fingerprint check catches because the history is identical either way."))
}
