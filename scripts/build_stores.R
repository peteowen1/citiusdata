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
# The .rds files are KEPT as a compat export for scripts not yet migrated
# off them. As of 2026-08-30 the athletics stores below source from
# citius.duckdb, not readRDS() -- the .rds files are no longer this
# script's input for athletics, only a byproduct other scripts still read.
# Swimming is unchanged, still RDS-sourced; it hasn't been migrated.
# Rebuilding a store is cheap either way, re-harvesting is not.

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

build <- function(src, dest, label, join_tier = FALSE, data = NULL) {
  if (is.null(data)) {
    f <- file.path(OUT, src)
    if (!file.exists(f)) { cli::cli_alert_warning("{label}: {src} not found, skipping."); return(invisible()) }
    d <- setDT(readRDS(f))
  } else {
    # Pre-loaded (from citius.duckdb -- see the athletics calls below).
    # `src` is kept only as a label for the "not found" branch above; a
    # DuckDB-sourced call never takes it.
    d <- data.table::copy(setDT(data))
    if (!nrow(d)) { cli::cli_alert_warning("{label}: 0 rows in citius.duckdb, skipping."); return(invisible()) }
  }
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

# join_tier is ON for both athletics stores below, as of 2026-09-04.
#
# HISTORY, because the flag has been flipped twice and the reason matters more
# than the value. It was switched on 2026-08-29 for a meet_tier fix that full
# history then REJECTED (T1 elite +3.15% worse, p=3e-15) and reverted off on
# 2026-08-30 -- correctly, because DEPLOYED's calibration was fitted on the
# feed's `tier`, so feeding meet_tier labels through it is a mismatch, not a
# fix. Re-run on the rebuilt catalogue 2026-09-04, the same arm reversed to
# -2.68% BETTER on marks MAE (p=1.9e-283), gold logloss also better, medal
# logloss a tie, and Pete promoted it.
#
# THE PAIRING IS THE WHOLE POINT: this flag and DEPLOYED$calibration must move
# together. ON here requires a meet_tier-fitted calibration
# (calibration_corpus_wac_coast_0904.rds); OFF requires a feed-tier-fitted one.
# Either one alone silently applies offsets fitted on one label set to a
# different label set, which no test fails and no guard catches -- it just
# quietly predicts worse. If you revert one, revert the other in the same
# commit.
#
# KNOWN OPEN RISK, recorded rather than buried: the 2026-09-04 reversal was
# measured on a population that is NOT size-matched to the run it overturned
# (53,311 predictions on a T1-only 394-meet pool vs 259 T1 races in a
# T1+T2+T3 120-meet pool), so "the catalogue fixes caused the reversal" is
# likely but UNPROVEN -- a differently-composed population is a live
# alternative explanation. The test that would settle it (pin this meet list,
# swap only the catalogue vintage) was recommended and deliberately skipped in
# favour of shipping. See the 2026-09-04 addendum in
# .scratch/athletics-calendar/issues/03-diamond-league-tier-defect.md.
#
# SOURCE: citius.duckdb, not readRDS(), as of 2026-08-30. The two athletics
# RDS files remain a compat export for the 40+ scripts that still read them
# directly (retiring those is separate, larger, out of scope here) -- but
# this store-building step, which is what deployed_history() actually reads
# through, no longer depends on them. Read-only connection: this script only
# loads, store_championship_results()/store_athletics_corpus() elsewhere own
# the writes.
#
# TWO SAFETY NETS, both required, neither optional:
#
# (1) The load itself can fail outright -- citius.duckdb missing entirely
#     (a fresh checkout only gets the .rds files from the release; the DB is
#     gitignored and never published), a table not yet populated, or locked
#     by a concurrent writer. The old readRDS() branch degraded to a warning
#     on a missing file; a hard abort here would be a regression, and would
#     also block the unrelated swimming builds later in this same script.
#     Falls back to `data = NULL` (the original readRDS() path) per table.
#
# (2) Even a SUCCESSFUL load can be silently stale: merge_referenced.R and
#     build_athletics_corpus.R now write to citius.duckdb alongside the RDS
#     files (2026-08-30), but that write can itself fail (network, lock,
#     bug) while the RDS write still succeeds -- both scripts warn on that
#     but do not abort, by design, so the RDS file stays authoritative. A
#     silently-behind DuckDB would otherwise build a store from data that
#     looks perfectly well-formed and is simply missing recent meets. Falls
#     back to `data = NULL` for whichever table is behind, not both.
.athletics <- tryCatch(
  with_citius_db_connection(function(conn) {
    list(championship_results = load_championship_results(conn),
         athletics_corpus     = load_athletics_corpus(conn))
  }, read_only = TRUE),
  error = function(e) {
    cli::cli_warn(c(
      "citius.duckdb unavailable: {conditionMessage(e)}",
      "i" = "Falling back to readRDS() for both athletics stores."
    ))
    list(championship_results = NULL, athletics_corpus = NULL)
  }
)

.stale_check <- function(duck, rds_file, label) {
  if (is.null(duck)) return(NULL)
  f <- file.path(OUT, rds_file)
  if (!file.exists(f)) return(duck)  # nothing to compare against; trust DuckDB
  rds <- setDT(readRDS(f))
  behind_rows <- nrow(duck) < nrow(rds)
  behind_comps <- "competition_id" %in% names(duck) && "competition_id" %in% names(rds) &&
    uniqueN(duck$competition_id) < uniqueN(rds$competition_id)
  if (behind_rows || behind_comps) {
    cli::cli_warn(c(
      "citius.duckdb's {label} is BEHIND {rds_file}: {nrow(duck)} vs {nrow(rds)} rows{if (behind_comps) paste0(', ', uniqueN(duck$competition_id), ' vs ', uniqueN(rds$competition_id), ' competitions') else ''}.",
      "i" = "Falling back to the .rds file for this store -- check why the DuckDB write-through didn't keep up (merge_referenced.R / build_athletics_corpus.R)."
    ))
    return(NULL)
  }
  # The opposite direction is not just "fine, ignore it": a T3 merge script
  # writes DuckDB before its atomic .rds rename, so a crash between the two
  # leaves DuckDB genuinely ahead. This store then builds correctly off the
  # newer DuckDB data, but the 40+ scripts that still readRDS() the .rds
  # directly (see this file's own header) stay on the older rows with no
  # error anywhere -- warn so that gap doesn't go unnoticed, without
  # discarding the newer, still-good DuckDB data by falling back.
  ahead_rows <- nrow(duck) > nrow(rds)
  ahead_comps <- "competition_id" %in% names(duck) && "competition_id" %in% names(rds) &&
    uniqueN(duck$competition_id) > uniqueN(rds$competition_id)
  if (ahead_rows || ahead_comps) {
    cli::cli_warn(c(
      "citius.duckdb's {label} is AHEAD of {rds_file}: {nrow(duck)} vs {nrow(rds)} rows.",
      "i" = "This store will be built from the newer DuckDB data, but scripts that readRDS() {rds_file} directly won't see it -- finish the interrupted merge's .rds write."
    ))
  }
  duck
}
.athletics$championship_results <- .stale_check(.athletics$championship_results,
  "championship_results.rds", "championship_results")
.athletics$athletics_corpus <- .stale_check(.athletics$athletics_corpus,
  "athletics_corpus.rds", "athletics_corpus")

build("championship_results.rds", "athletics_store", "Athletics competitions",
      join_tier = TRUE, data = .athletics$championship_results)
# The UNIFIED corpus -- 4.99M rows against the competition harvest's 308k. Without
# a store the backtest filters it in memory once per meet, 900 times, which is
# what made the corpus arm 16x slower than the harvest arms rather than equal.
build("athletics_corpus.rds", "athletics_corpus_store", "Athletics corpus",
      join_tier = TRUE, data = .athletics$athletics_corpus)
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
