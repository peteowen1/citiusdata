# Merge a BATCH of harvest_wa_results.R outputs into championship_results.rds
# in one pass, then run the full downstream chain.
#
# WHY THIS EXISTS. append_meet_to_championship_results.R does one meet at a
# time and reloads/resaves the whole ~4.7M-row championship_results.rds per
# call -- fine for one meet, catastrophic for the 4,723-meet missing-meets
# harvest (2026-09-16/17). This does the same tested transform
# (.map_harvest_to_championship(), shared via _map_harvest_to_championship.R)
# and the same coverage assertions, but combines every meet FIRST and appends
# ONCE.
#
# Usage:
#   Rscript citiusdata/scripts/merge_missing_meets_batch.R <ids_file>
#   <ids_file>: one competition_id per line (LF or CRLF, either is fine).
#   CITIUS_MERGE_DRYRUN=1 to validate and report without writing anything.
#   CITIUS_MERGE_SKIP_CHAIN=1 to append + sync DuckDB only, skip the
#     corpus/catalogue/store rebuild (e.g. to batch several merges).

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

args <- commandArgs(trailingOnly = TRUE)
IDS_FILE <- if (length(args)) args[[1]] else cli::cli_abort("Give a file of competition_ids, one per line.")
DRY <- nzchar(Sys.getenv("CITIUS_MERGE_DRYRUN", ""))
SKIP_CHAIN <- nzchar(Sys.getenv("CITIUS_MERGE_SKIP_CHAIN", ""))

source(file.path(VERSE, "citiusdata", "scripts", "_map_harvest_to_championship.R"))

ids <- trimws(readLines(file.path(D, IDS_FILE)))
ids <- ids[nzchar(ids)]
say("target: %s competition_ids", format(length(ids), big.mark = ","))

# --- what championship_results.rds already holds ----------------------------
CH_F <- file.path(D, "championship_results.rds")
ch <- readRDS(CH_F)
say("existing championship_results.rds: %s rows / %s comps",
    format(nrow(ch), big.mark = ","), format(uniqueN(ch$competition_id), big.mark = ","))

already <- intersect(as.integer(ids), unique(ch$competition_id))
if (length(already)) {
  say("dropping %d id%s already in championship_results.rds", length(already), if (length(already)==1) "" else "s")
  ids <- as.character(setdiff(as.integer(ids), already))
}

# --- load every raw harvest file, map it, combine ----------------------------
missing_files <- character(0)
read_fail <- character(0)
mapped <- vector("list", length(ids))
for (i in seq_along(ids)) {
  cid <- ids[i]
  f <- file.path(D, paste0(cid, "_raw_results.rds"))
  if (!file.exists(f)) { missing_files <- c(missing_files, cid); next }
  h <- tryCatch(as.data.table(readRDS(f)), error = function(e) NULL)
  if (is.null(h) || !nrow(h)) { next }  # a genuinely empty meet is not an error
  m <- tryCatch(.map_harvest_to_championship(h, cid, D),
                error = function(e) { cli::cli_warn("mapping failed for {cid}: {conditionMessage(e)}"); NULL })
  if (is.null(m)) { read_fail <- c(read_fail, cid); next }
  mapped[[i]] <- m
}
if (length(missing_files)) say("WARNING: %d id%s had no raw file on disk (skipped): %s",
    length(missing_files), if (length(missing_files)==1) "" else "s",
    paste(head(missing_files, 10), collapse = ", "))
if (length(read_fail)) cli::cli_abort("{length(read_fail)} meet{?s} failed the harvest->championship mapping -- fix before merging: {.val {read_fail}}")

new <- rbindlist(Filter(Negate(is.null), mapped), fill = TRUE, use.names = TRUE)
say("mapped %s rows across %s meets", format(nrow(new), big.mark = ","), uniqueN(new$competition_id))

# --- validate columns match exactly, not just "enough of them" --------------
need <- names(ch)
missing_cols <- setdiff(need, names(new))
if (length(missing_cols)) cli::cli_abort(
  "Batch is missing column{?s} {.field {missing_cols}} that championship_results.rds needs -- fix the mapping, do not fabricate them silently.")
extra <- setdiff(names(new), need)
if (length(extra)) { say("dropping columns not in championship_results.rds's schema: %s", paste(extra, collapse=", ")); new <- new[, ..need] } else { new <- new[, ..need] }

cov <- vapply(new, function(x) mean(!is.na(x)), numeric(1))
say("column fill rates for the new rows (0%% on a column that is normally populated is a mapping bug):")
print(round(sort(cov), 3))
# Same exempt list as append_meet_to_championship_results.R -- these are
# genuinely absent from the direct-harvest API path, not a mapping bug.
.exempt_zero <- c("comp_name", "comp_tier", "discipline_code", "value_raw", "birthdate_year_only")
zero <- names(cov)[cov == 0 & !(names(cov) %in% .exempt_zero)]
if (length(zero)) cli::cli_abort(
  "column{?s} 100%% empty after mapping: {.field {zero}} -- fix the mapping rather than writing an empty column.")

# Compare against the EXISTING corpus's own fill rate for each column, so a
# column that is merely SPARSE (not 0%) but far below its historical norm
# also gets caught -- "assert coverage, not presence" applies at any rate,
# not just zero.
cov_old <- vapply(ch[, ..need], function(x) mean(!is.na(x)), numeric(1))
drift <- data.table(column = names(cov), new_fill = round(cov, 3), existing_fill = round(cov_old[names(cov)], 3))
drift[, gap := existing_fill - new_fill]
say("columns where the new batch's fill rate is >20pp below the existing corpus's own norm:")
print(drift[gap > 0.20][order(-gap)])

# 0-sentinel and NA competition_id, same guard as merge_referenced.R.
stopifnot("competition_id contains NA" = !anyNA(new$competition_id))
stopifnot("competition_id contains 0" = !any(new$competition_id == 0, na.rm = TRUE))
stopifnot("competition_id overlaps the existing store" = !any(new$competition_id %in% ch$competition_id))

if (DRY) { say("DRY RUN -- nothing written. %s rows would be appended.", format(nrow(new), big.mark=",")); quit(status = 0) }

# --- backup, append, save ----------------------------------------------------
backup <- file.path(D, "championship_results_premerge_missingmeets.rds")
if (file.exists(backup)) {
  say("rollback already exists at championship_results_premerge_missingmeets.rds; keeping it")
} else {
  saveRDS(ch, backup)
  say("backed up pre-merge championship_results.rds to championship_results_premerge_missingmeets.rds")
}

before <- nrow(ch)
ch2 <- rbind(ch, new, fill = TRUE)
stopifnot(nrow(ch2) == before + nrow(new))
saveRDS(ch2, CH_F)
say("appended: %s -> %s rows (%s comps -> %s comps)",
    format(before, big.mark = ","), format(nrow(ch2), big.mark = ","),
    format(uniqueN(ch$competition_id), big.mark = ","), format(uniqueN(ch2$competition_id), big.mark = ","))

# --- verify by re-reading -----------------------------------------------------
back <- readRDS(CH_F)
if (nrow(back) != nrow(ch2)) cli::cli_abort("wrote {nrow(ch2)} rows but read back {nrow(back)} -- the append did not land cleanly.")
back_new <- back[competition_id %in% as.integer(unique(new$competition_id))]
if (nrow(back_new) != nrow(new)) cli::cli_abort(
  "wrote {nrow(new)} new rows but read back {nrow(back_new)} for the merged competitions.")
say("verified: round trip clean.")

# --- citius.duckdb MUST move with the RDS file (see merge_referenced.R) -----
tryCatch({
  citius::with_citius_db_connection(function(conn) {
    citius::store_championship_results(conn, new, mode = "merge")
  })
  say("citius.duckdb updated to match.")
}, error = function(e) {
  cli::cli_warn(c(
    "Failed to update citius.duckdb: {conditionMessage(e)}",
    "!" = "championship_results.rds is correct; DuckDB is now BEHIND it.",
    "i" = "build_stores.R falls back to RDS when DuckDB is stale, but fix this before relying on that."
  ))
})

if (SKIP_CHAIN) { say("CITIUS_MERGE_SKIP_CHAIN set -- stopping here."); quit(status = 0) }

# --- rebuild chain: corpus -> catalogue -> stores, order is load-bearing -----
say("=== build_athletics_corpus.R")
rc <- system2("Rscript", shQuote(file.path(VERSE, "citiusdata", "scripts", "build_athletics_corpus.R")))
if (!identical(rc, 0L)) cli::cli_abort("build_athletics_corpus.R exited {rc}; chain stopped. championship_results.rds is updated but athletics_corpus.rds is NOT.")

say("=== run_catalogue_chain.ps1 (9 steps)")
rc <- system2("powershell", c("-File", shQuote(file.path(VERSE, "citiusdata", "scripts", "run_catalogue_chain.ps1"))))
if (!identical(rc, 0L)) cli::cli_abort("run_catalogue_chain.ps1 exited {rc}; chain stopped before build_stores.R. New meets have NO meet_tier yet.")

say("=== build_stores.R")
rc <- system2("Rscript", shQuote(file.path(VERSE, "citiusdata", "scripts", "build_stores.R")))
if (!identical(rc, 0L)) cli::cli_abort("build_stores.R exited {rc}.")

say("chain complete. %s new competitions merged, corpus/catalogue/stores rebuilt.", uniqueN(new$competition_id))
