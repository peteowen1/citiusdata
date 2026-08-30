# Convert the result corpora to partitioned parquet stores.
#
# Measured on 8.6M rows with the query the backtest actually issues:
#
#   .rds                          46.1s per query   ~10.6 h across 825 meets
#   parquet                        8.7s              120 min
#   parquet partitioned by event   0.39s               5 min
#
# 118x, because every query in this pipeline filters on event_id, so partition
# pruning skips all but a handful of files without opening them. Costs ~2x disk.
#
# The .rds files are KEPT. They are the harvest output and the stores are a
# derived read layer; rebuilding a store is cheap, re-harvesting is not.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

# The catalogue's meet_tier is per-COMPETITION and anchor-guarded (see
# build_competition_catalogue.R), unlike the feed's `tier`, which is
# per-RESULT and non-monotonic once fitted (mid corrects harder than low --
# .scratch/athletics-calendar/issues/03-diamond-league-tier-defect.md). Loaded
# once here; only athletics stores join it (join_tier = TRUE below).
CAT_TBL <- {
  f <- file.path(OUT, "competition_catalogue.parquet")
  if (file.exists(f)) {
    ct <- setDT(arrow::read_parquet(f))[, .(competition_id, meet_tier)]
    ct[, competition_id := as.character(competition_id)]
    ct
  } else NULL
}

build <- function(src, dest, label, join_tier = FALSE) {
  f <- file.path(OUT, src)
  if (!file.exists(f)) { cli::cli_alert_warning("{label}: {src} not found, skipping."); return(invisible()) }
  d <- setDT(readRDS(f))
  # flag_implausible() MUST run before partitioning, not after reading.
  #
  # It is a GLOBAL operation: the Hampel filter takes a median and MAD per event
  # across the whole corpus. Applied to a per-meet slice it would compute those
  # thresholds from a handful of rows and flag completely different marks --
  # silently, and differently for every meet.
  #
  # Storing raw data and cleaning on read produced 22 extra rows and materially
  # different abilities (max shrinkage difference 0.96) against the .rds path.
  d <- flag_implausible(d)

  if (join_tier) {
    if (is.null(CAT_TBL)) {
      cli::cli_abort("{label}: join_tier = TRUE but competition_catalogue.parquet is missing.")
    }
    # The documented trap (ticket 03): competition_id round-trips through
    # parquet as character while the harvest/corpus holds it as integer. A
    # silent type mismatch here leaves every meet_tier NA -- which looks
    # exactly like "the fix did nothing" rather than an error. Coerce and
    # ASSERT coverage rather than trust the join.
    d[, competition_id := as.character(competition_id)]
    d <- merge(d, CAT_TBL, by = "competition_id", all.x = TRUE)
    cov <- 100 * mean(!is.na(d$meet_tier))
    cli::cli_alert_info("  {label}: meet_tier attached to {round(cov, 1)}% of rows.")
    if (cov <= 50) {
      cli::cli_abort("{label}: meet_tier coverage {round(cov, 1)}% -- join is broken, refusing to ship it silently.")
    }
  }

  # Store the columns the models read, sorted by partition then date. Both
  # matter, and both were measured:
  #
  #   34 cols, unsorted   329 MB   1.620s per query
  #   11 cols, sorted       9 MB   0.090s per query
  #
  # Column count is the obvious one -- pushdown still opens and decodes what is
  # there. The 15x bloat from adding a single boolean column is the surprise:
  # flag_implausible() does grouped work that REORDERS rows, and parquet's
  # dictionary and run-length encoding depend on row order. Sorted data
  # compresses; shuffled data does not.
  # `venue_country` is carried for `fit_season_effect()`, which splits the
  # seasonal phase by hemisphere. Without it every mark classifies northern and
  # southern-hemisphere athletes get a calendar that is six months out of phase
  # -- the same silent-default failure as wind being dropped on corpus
  # promotion. It is low-cardinality (~200 codes) and the rows are already
  # sorted by event and date, so dictionary encoding keeps the cost small.
  keep <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "round",
            "tier", "meet_tier", "competition_id", "comp_start", "place", "race_key",
            "sex", "discipline", "wind", "indoor", "comp_name", "venue_country")
  present <- intersect(keep, names(d))
  dropped <- setdiff(names(d), present)
  d <- d[, ..present]
  data.table::setorderv(d, intersect(c("event_id", "date"), names(d)))
  if (length(dropped)) {
    cli::cli_alert_info("  {label}: keeping {length(present)} column{?s}, dropping {length(dropped)}.")
  }
  t0 <- Sys.time()
  write_results_store(d, file.path(OUT, dest))
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  files <- list.files(file.path(OUT, dest), recursive = TRUE, full.names = TRUE)
  cli::cli_alert_success(
    "{label}: {format(nrow(d), big.mark = ',')} rows -> {length(files)} partition{?s} in {round(el)}s ({round(sum(file.size(files))/1e6)} MB)"
  )

  # Verify the round trip rather than assume it. A store that silently drops
  # rows would be discovered as a mysteriously improved backtest.
  back <- read_results_store(file.path(OUT, dest))
  if (nrow(back) != nrow(d)) {
    cli::cli_abort("{label}: round trip lost rows ({nrow(d)} -> {nrow(back)}).")
  }
  na_before <- sum(is.na(d$event_id)); na_after <- sum(is.na(back$event_id))
  if (na_before != na_after) {
    cli::cli_abort("{label}: unmatched-event rows changed ({na_before} -> {na_after}).")
  }
  cli::cli_alert_info("  round trip verified: {format(nrow(back), big.mark = ',')} rows, {na_after} unmatched.")
  invisible()
}

# join_tier stays OFF for both athletics stores below. It was switched on
#2026-08-29 for a meet_tier fix that was then properly tested and REJECTED
# (.scratch/athletics-calendar/issues/03-diamond-league-tier-defect.md
# addendum): T1 elite regressed +3.15% (p=3e-15) on full-history confirmation,
# against DEPLOYED's feed-tier-fitted calibration. Turning join_tier back on
# here without ALSO promoting a meet_tier-fitted calibration would silently
# feed meet_tier labels through offsets fitted on the feed's tier -- a
# mismatch, not a fix. The machinery is kept (tested, correct) for whenever a
# same-source calibration is adopted; until then it must stay off. Reverted
# 2026-08-30.
build("championship_results.rds", "athletics_store", "Athletics competitions", join_tier = FALSE)
# The UNIFIED corpus -- 4.99M rows against the competition harvest's 308k. Without
# a store the backtest filters it in memory once per meet, 900 times, which is
# what made the corpus arm 16x slower than the harvest arms rather than equal.
if (file.exists(file.path(OUT, "athletics_corpus.rds"))) {
  build("athletics_corpus.rds", "athletics_corpus_store", "Athletics corpus", join_tier = FALSE)
}
build("swimming_history_full.rds", "swimming_store", "Swimming")
if (file.exists(file.path(OUT, "swimming_corpus.rds"))) {
  build("swimming_corpus.rds", "swimming_corpus_store", "Swimming corpus")
}
if (file.exists(file.path(OUT, "athletics_history.rds"))) {
  build("athletics_history.rds", "athletics_careers_store", "Athletics careers")
}
if (file.exists(file.path(OUT, "swim_athlete_history.rds"))) {
  build("swim_athlete_history.rds", "swimming_careers_store", "Swimming careers")
}
