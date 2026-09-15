# Bring citius.duckdb's championship_results up to date with the .rds.
#
# WHY THIS EXISTS. championship_results lives in two places: the authoritative
# `championship_results.rds` that the append scripts write, and the DuckDB copy
# that several builders read FIRST (build_competition_catalogue.R calls
# with_citius_db_connection(load_championship_results) and only falls back to
# the .rds if that fails). After the 2026-09-15 backfill appended 203,900 rows
# to the .rds, DuckDB was 3,184 competitions behind -- so the catalogue rebuilt
# from pre-backfill data while every check passed. This is the exact hazard
# docs/reference/storage-conventions.md warns about: a half-migrated core table
# is worse than an un-migrated one, because the two copies drift in silence.
#
# Run this after ANY append to championship_results.rds, before the catalogue
# chain. It merges only the competitions DuckDB is missing, so
# .citius_store_merge()'s row-count assertion is a genuine check rather than a
# tautology -- 4,544,586 + 203,900 must land on the .rds's own 4,748,486 or the
# whole transaction rolls back.
#
# Usage:
#   Rscript citiusdata/scripts/sync_duckdb_championship_results.R      # dry run
#   CITIUS_SYNC_GO=1 Rscript ...                                       # write

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D  <- file.path(VERSE, "citiusdata", "data")
GO <- nzchar(Sys.getenv("CITIUS_SYNC_GO", ""))
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

ch <- readRDS(file.path(D, "championship_results.rds"))
setDT(ch)
say("RDS: %s rows x %d cols, %s competitions",
    format(nrow(ch), big.mark = ","), ncol(ch),
    format(uniqueN(ch$competition_id), big.mark = ","))

conn <- get_citius_db_connection(read_only = TRUE)
n_db   <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM championship_results")$n
have   <- DBI::dbGetQuery(conn, "SELECT DISTINCT competition_id FROM championship_results")$competition_id
dbcols <- DBI::dbGetQuery(conn, "SELECT column_name FROM information_schema.columns
                                 WHERE table_name = 'championship_results'
                                 ORDER BY ordinal_position")$column_name
DBI::dbDisconnect(conn, shutdown = TRUE)
say("DuckDB: %s rows x %d cols, %s competitions",
    format(n_db, big.mark = ","), length(dbcols), format(length(have), big.mark = ","))

# The merge is only safe as a top-up if DuckDB is a strict SUBSET. A row DuckDB
# holds and the .rds does not means the two have genuinely diverged rather than
# one being behind, and topping up would leave that row orphaned with nothing
# to reconcile it against -- that wants a deliberate decision, not this script.
extra <- setdiff(as.character(have), as.character(ch$competition_id))
if (length(extra)) cli_abort(c(
  "DuckDB holds {length(extra)} competition{?s} the .rds does not, so it is not simply behind.",
  x = "{.val {head(extra, 8)}}",
  i = "The two copies have diverged. Decide which is authoritative before syncing."))

if (!setequal(names(ch), dbcols)) cli_abort(c(
  "Column sets differ between the .rds and DuckDB.",
  i = "rds only: {.field {setdiff(names(ch), dbcols)}}; db only: {.field {setdiff(dbcols, names(ch))}}"))

miss <- setdiff(as.character(ch$competition_id), as.character(have))
if (!length(miss)) { say("DuckDB is already current; nothing to do."); quit(status = 0) }

new <- ch[as.character(competition_id) %in% miss]
setcolorder(new, dbcols)
say("to merge: %s competitions, %s rows, %s .. %s",
    format(length(miss), big.mark = ","), format(nrow(new), big.mark = ","),
    as.character(min(new$date, na.rm = TRUE)), as.character(max(new$date, na.rm = TRUE)))

# Coverage, not presence: a column that arrives 100% empty here would be a
# mapping fault upstream, and would land silently because the schema matches.
cov <- vapply(new, function(x) mean(!is.na(x)), numeric(1))
empty <- names(cov)[cov == 0]
if (length(empty)) say("NOTE: %d column(s) 100%% empty in this batch: %s",
                       length(empty), paste(empty, collapse = ", "))

target <- nrow(ch)
if (!GO) {
  say("DRY RUN -- nothing written. CITIUS_SYNC_GO=1 to merge %s rows (%s -> %s).",
      format(nrow(new), big.mark = ","), format(n_db, big.mark = ","),
      format(target, big.mark = ","))
  quit(status = 0)
}

added <- with_citius_db_connection(function(cn)
  store_championship_results(cn, new, mode = "merge"))

after <- with_citius_db_connection(
  function(cn) DBI::dbGetQuery(cn, "SELECT COUNT(*) n FROM championship_results")$n,
  read_only = TRUE)
say("DuckDB now %s rows (added %s)", format(after, big.mark = ","), format(added, big.mark = ","))
if (after != target) cli_abort(
  "DuckDB is at {after} rows but the .rds has {target}; the two are still out of step.")
say("SYNCED: DuckDB and championship_results.rds agree at %s rows.", format(after, big.mark = ","))
