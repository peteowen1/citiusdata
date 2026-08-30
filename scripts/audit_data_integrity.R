# Standing data-integrity audit: duplicates, missing data, corruption,
# mislabeling, and cross-store consistency between the RDS files and
# citius.duckdb.
#
# WHY THIS EXISTS. Two incidents in the same week (2026-08-29/30): a merge
# used a dedup key that collapsed legitimate multi-attempt rows, and a stale
# catalogue mislabeled current Diamond League meets into the wrong tier. Both
# were caught by hand, after the fact. This makes the checks that would have
# caught them routine and automatic instead.
#
# WHAT THIS DOES NOT GUARANTEE. "Zero duplicates found" does not mean the
# corpus is complete -- a competition harvested with a corrupted feed page can
# still be internally consistent. "No implausible marks" does not mean every
# mark is correct, only that nothing sits far enough from its event's median
# to be caught by a robust outlier test. This audit finds the failure modes
# already seen in this repo's history, not every possible one. Read the
# PASS/FLAG/FAIL lines, don't just check the exit code.
#
# DOES NOT OVERLAP with:
#   audit_history.R    -- deeper implausible-mark sweeps (per-source cleanliness,
#                          athlete-relative contamination). This script only
#                          reports flag_implausible()'s headline rate as a
#                          sanity check against a known baseline; run
#                          audit_history.R for the full sweep.
#   audit_coverage.R    -- compares harvested data against the LIVE feed (what
#                          exists that we don't have). This script only checks
#                          INTERNAL consistency (does a T1 meet already in our
#                          own catalogue have zero results in our own corpus).
#   audit_anchors.R     -- model-quality anchor (does the rating track recent
#                          form). Unrelated: this script never touches ability
#                          estimates.
#   build_competition_catalogue.R's own anchor block -- ad hoc, per-class
#                          spot checks (Olympics, Diamond League, age-group).
#                          This script's mislabeling check is the same PATTERN
#                          applied systematically to every class, not just the
#                          ones someone thought to name.
#
# Usage:  Rscript scripts/audit_data_integrity.R
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
library(arrow)
OUT <- here::here("citiusdata", "data")

anchor <- function(label, ok, detail = "") {
  status <- if (isTRUE(ok)) "PASS" else if (identical(ok, "FLAG")) "FLAG" else "**FAIL**"
  cat(sprintf("  %-58s %s%s\n", label, status, if (nzchar(detail)) paste0("  ", detail) else ""))
  ok
}
verdicts <- list()

cat("\n=================================================================\n")
cat("DATA INTEGRITY AUDIT --", format(Sys.time()), "\n")
cat("=================================================================\n")

ch  <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
cor <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cli::cli_alert_info("championship_results: {format(nrow(ch), big.mark=',')} rows | athletics_corpus: {format(nrow(cor), big.mark=',')} rows | catalogue: {format(nrow(cat_tbl), big.mark=',')} competitions")

# ---- 1. DUPLICATES -----------------------------------------------------------
# competition_id + race_key + athlete_id legitimately repeats for a field
# athlete's multiple attempts (no attempt-order column exists in the schema to
# disambiguate directly) -- that is exactly the distinction the 2026-08-29
# incident's dedup key got backwards. A group is a TRUE duplicate only if
# every column is identical, not just the key.
cat("\n=== 1. DUPLICATES ===\n")
key_cols <- c("competition_id", "race_key", "athlete_id")
ch[, grp_n := .N, by = key_cols]
multi <- ch[grp_n > 1]
if (nrow(multi)) {
  full_dup <- multi[, .N, by = c(names(ch))][N > 1]
  n_exact_dup_rows <- if (nrow(full_dup)) sum(full_dup$N - 1) else 0L
  n_legit_multiattempt_groups <- multi[, .(is_dup = uniqueN(.SD) < .N), by = key_cols,
                                        .SDcols = setdiff(names(ch), c(key_cols, "grp_n"))]
  n_multiattempt_groups <- uniqueN(multi[, ..key_cols])
} else {
  n_exact_dup_rows <- 0L
  n_multiattempt_groups <- 0L
}
anchor("no exact full-row duplicates", n_exact_dup_rows == 0,
       sprintf("%s exact-duplicate rows found", format(n_exact_dup_rows, big.mark = ",")))
cat(sprintf("  (%s athlete-race groups have >1 row -- expected for field events with multiple attempts,\n   distinguished from true duplicates by differing mark/value_raw, not flagged here)\n",
            format(n_multiattempt_groups, big.mark = ",")))
ch[, grp_n := NULL]

# ---- 2a. SCHEMA COMPLETENESS -------------------------------------------------
cat("\n=== 2a. SCHEMA COMPLETENESS (championship_results) ===\n")
expected <- CITIUS_DB_SCHEMA$championship_results
missing_cols <- setdiff(expected, names(ch))
anchor("every expected column present", length(missing_cols) == 0,
       if (length(missing_cols)) paste("missing:", paste(missing_cols, collapse = ", ")) else "")
present <- intersect(expected, names(ch))
null_rates <- ch[, lapply(.SD, function(x) round(100 * mean(is.na(x)), 2)), .SDcols = present]
cat("  null rate per column:\n")
for (col in present) cat(sprintf("    %-22s %5.2f%%\n", col, null_rates[[col]]))

# ---- 2b. COVERAGE COMPLETENESS -----------------------------------------------
# A T1 meet already in our own catalogue with zero rows in our own corpus is a
# silent internal gap -- distinct from audit_coverage.R's feed-vs-us check.
cat("\n=== 2b. COVERAGE COMPLETENESS: T1 meets since 2016 with ZERO results ===\n")
t1_recent <- cat_tbl[meet_tier == "T1_elite" & year >= 2016]
have_results <- unique(ch$competition_id)
t1_gap <- t1_recent[!competition_id %in% have_results]
anchor("every T1_elite meet since 2016 has >=1 result row", nrow(t1_gap) == 0,
       sprintf("%d of %d T1 meets missing", nrow(t1_gap), nrow(t1_recent)))
if (nrow(t1_gap)) {
  cat("  gaps:\n")
  print(head(t1_gap[order(-year), .(year, comp_name, class)], 20))
}

# ---- 3. IMPLAUSIBLE MARKS ----------------------------------------------------
# Headline rate only -- see audit_history.R for the full two-sweep check.
cat("\n=== 3. IMPLAUSIBLE MARKS (headline rate; see audit_history.R for the full sweep) ===\n")
flagged <- flag_implausible(ch[!is.na(event_id) & !is.na(perf)])
rate <- 100 * mean(flagged$implausible)
BASELINE <- 0.17  # this session's export log, 12,618 of ~7.2M corpus rows
anchor("implausible-mark rate is in the expected range (0.05%-0.5%)",
       rate >= 0.05 && rate <= 0.5,
       sprintf("%.3f%% flagged (session baseline on the larger corpus: %.2f%%)", rate, BASELINE))

# ---- 4. TIER/STRENGTH MISLABELING -------------------------------------------
# Systematic version of build_competition_catalogue.R's ad hoc anchors: for
# EVERY class (not just the ones someone thought to name), does the assigned
# tier match what measured strength says the class should be?
cat("\n=== 4. TIER vs STRENGTH, by class (systematic) ===\n")
by_class <- cat_tbl[!is.na(strength), .(
  n = .N,
  median_strength = round(median(strength), 1),
  p10 = round(quantile(strength, .1), 1),
  p90 = round(quantile(strength, .9), 1),
  modal_tier = names(sort(table(meet_tier), decreasing = TRUE))[1]
), by = class][order(-median_strength)]
by_class[, off_tier_pct := sapply(class, function(cl) {
  sub <- cat_tbl[class == cl & !is.na(strength)]
  round(100 * mean(sub$meet_tier != names(sort(table(sub$meet_tier), decreasing = TRUE))[1]), 1)
})]
print(by_class)
bad_classes <- by_class[off_tier_pct > 15 & n >= 10]
anchor("no class has >15% of its meets split across tiers (n>=10)", nrow(bad_classes) == 0,
       if (nrow(bad_classes)) paste(sprintf("%s (%.0f%% off, n=%d)", bad_classes$class,
             bad_classes$off_tier_pct, bad_classes$n), collapse = "; ") else "")
# Negative check, same reasoning as build_competition_catalogue.R's own: a
# class whose median strength clearly belongs in a different tier band than
# its modal assignment is a labeling bug, not noise.
TIER_BAND <- function(s) fcase(s >= 75, "T1_elite", s >= 40, "T2_strong", default = "T3_development")
by_class[, strength_implied_tier := TIER_BAND(median_strength)]
mismatch <- by_class[strength_implied_tier != modal_tier & n >= 10]
anchor("no class's median strength implies a different tier than its modal assignment (n>=10)",
       nrow(mismatch) == 0,
       if (nrow(mismatch)) paste(sprintf("%s: strength->%s but modal=%s", mismatch$class,
             mismatch$strength_implied_tier, mismatch$modal_tier), collapse = "; ") else "")

# ---- 5. CROSS-STORE CONSISTENCY (RDS vs citius.duckdb) -----------------------
cat("\n=== 5. CROSS-STORE CONSISTENCY: RDS vs citius.duckdb ===\n")
check_store <- function(rds_dt, table_name) {
  with_citius_db_connection(function(conn) {
    if (!citius_table_exists(conn, table_name)) {
      anchor(sprintf("%s: table exists in citius.duckdb", table_name), FALSE, "table not found")
      return(invisible(NULL))
    }
    db_n <- DBI::dbGetQuery(conn, sprintf("SELECT COUNT(*) n FROM %s", table_name))$n
    anchor(sprintf("%s: row count matches (RDS %s vs DuckDB %s)", table_name,
                   format(nrow(rds_dt), big.mark=","), format(db_n, big.mark=",")),
           db_n == nrow(rds_dt))

    if ("competition_id" %in% names(rds_dt)) {
      db_comps <- DBI::dbGetQuery(conn, sprintf("SELECT DISTINCT competition_id FROM %s", table_name))$competition_id
      sym_diff <- length(union(setdiff(unique(rds_dt$competition_id), db_comps),
                                setdiff(db_comps, unique(rds_dt$competition_id))))
      anchor(sprintf("%s: competition_id set matches", table_name), sym_diff == 0,
             sprintf("%d competitions differ", sym_diff))
    }
    if ("race_key" %in% names(rds_dt)) {
      db_races <- DBI::dbGetQuery(conn, sprintf("SELECT DISTINCT race_key FROM %s", table_name))$race_key
      sym_diff <- length(union(setdiff(unique(rds_dt$race_key), db_races),
                                setdiff(db_races, unique(rds_dt$race_key))))
      anchor(sprintf("%s: race_key set matches", table_name), sym_diff == 0,
             sprintf("%d race_keys differ", sym_diff))
    }
    # Value-level check, not just keys: sample-checksum non-key columns so a
    # value-level corruption (a rounding/precision change, a wrong-column
    # read) that preserves row/key counts doesn't pass silently -- the exact
    # gap the bootstrap script's reviewer flagged in this same session.
    val_cols <- intersect(c("mark", "place", "date"), names(rds_dt))
    jk <- key_cols[key_cols %in% names(rds_dt)]
    if (length(val_cols) && nrow(rds_dt) > 0) {
      set.seed(1)
      # NA join keys are excluded from the value-sample check: SQL's `t.x =
      # s.x` is NULL (never TRUE) when either side is NULL, so an NA-keyed row
      # (career-history rows with no race-level key, by design in
      # athletics_corpus) never matches and reads as a false "0 rows in
      # DuckDB" mismatch -- found while building this script (2026-08-30):
      # 818 of 1988 sampled athletics_corpus keys "mismatched" this way, all
      # of them competition_id/race_key == NA, none a real value difference.
      # Reported separately below rather than silently dropped.
      na_mask <- Reduce(`|`, lapply(jk, function(k) is.na(rds_dt[[k]])))
      eligible <- rds_dt[!na_mask]
      n_na_key <- sum(na_mask)
      # Sample KEYS, then pull every row sharing that key -- not just the one
      # row `sample.int` happened to land on. A key can legitimately map to
      # >1 row (multi-attempt field events), and the DuckDB side below joins
      # on key, so it always returns every sibling row for a sampled key. If
      # this side only kept the originally-sampled row, a key with 2 real
      # rows where only 1 got sampled would compare {one value} against
      # DuckDB's {two values} and report a false mismatch. Found 2026-08-30:
      # 14 of 2000 sampled athletics_corpus keys "mismatched" this way, every
      # one a real multi-attempt pair present correctly on both sides -- not
      # a DuckDB import bug, an asymmetric sample.
      samp_idx <- sample.int(nrow(eligible), min(2000, nrow(eligible)))
      samp_keys <- unique(eligible[samp_idx, jk, with = FALSE])
      samp <- eligible[samp_keys, on = jk, c(jk, val_cols), with = FALSE, nomatch = NULL]
      # Pull the same rows back from duckdb via a temp registration + join,
      # cheaper than N queries. Register the DEDUPLICATED samp_keys, not
      # samp's own key columns -- samp has one row per SIBLING (multiple rows
      # per multi-attempt key), so registering it directly would put N
      # identical key rows into the temp table, and a plain SQL join between
      # two N-row-per-key sides returns N*N rows, not N -- reintroducing a
      # false mismatch (db_vals length N^2 vs rds_vals length N) for exactly
      # the multi-attempt keys this fix exists to handle correctly. Found in
      # review (2026-08-30), same session as the fix itself.
      duckdb::duckdb_register(conn, "audit_sample_tmp", samp_keys)
      on.exit(tryCatch(duckdb::duckdb_unregister(conn, "audit_sample_tmp"), error = function(e) NULL), add = TRUE)
      join_cond <- paste(sprintf("t.%s = s.%s", jk, jk), collapse = " AND ")
      db_samp <- setDT(DBI::dbGetQuery(conn, sprintf(
        "SELECT t.%s FROM %s t JOIN audit_sample_tmp s ON %s",
        paste(c(jk, val_cols), collapse = ", t."), table_name, join_cond)))
      # A key can legitimately map to >1 row (multi-attempt); compare on the
      # sorted multiset of values per key rather than assuming 1:1.
      merged <- merge(
        samp[, .(rds_vals = list(sort(mark))), by = jk],
        db_samp[, .(db_vals = list(sort(mark))), by = jk],
        by = jk, all = TRUE)
      value_mismatch <- merged[, sum(mapply(function(a, b) !isTRUE(all.equal(a, b)), rds_vals, db_vals))]
      anchor(sprintf("%s: sampled mark values match (n=%d keys, %s excluded for NA key)",
                     table_name, nrow(merged), format(n_na_key, big.mark = ",")),
             value_mismatch == 0, sprintf("%d key(s) differ", value_mismatch))
    }
  })
}
check_store(ch, "championship_results")
check_store(cor, "athletics_corpus")

# ---- 6. KNOWN-ATHLETE SANITY CHECK -------------------------------------------
# No automated pass/fail -- per citius/CLAUDE.md's own recommendation, a human
# eyeballs these. Chosen for long, well-documented careers spanning multiple
# Olympic cycles, so a gap or a wrong mark is easy to spot on sight.
cat("\n=== 6. KNOWN-ATHLETE SANITY CHECK (eyeball this) ===\n")
KNOWN <- c("Usain Bolt", "Allyson Felix", "Mondo Duplantis", "Eliud Kipchoge", "Sydney McLaughlin")
for (nm in KNOWN) {
  rows <- ch[!is.na(athlete_name) & athlete_name == nm]
  if (!nrow(rows)) { cat(sprintf("\n  %-20s -- NOT FOUND in championship_results\n", nm)); next }
  cat(sprintf("\n  %s -- %d results, %s to %s\n", nm, nrow(rows),
              min(rows$date, na.rm = TRUE), max(rows$date, na.rm = TRUE)))
  print(rows[order(date), .(date, comp_name, event_name, round, place, mark_string)][
    seq(1, .N, length.out = min(8, .N))])
}

# ---- VERDICT ------------------------------------------------------------------
cat("\n=================================================================\n")
cat("Run audit_history.R (deeper implausible-mark sweep) and audit_coverage.R\n")
cat("(feed-vs-harvested gap) alongside this for the full picture.\n")
cat("=================================================================\n")
