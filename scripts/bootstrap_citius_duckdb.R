# One-off: import the three current RDS stores into citius.duckdb.
#
# Read-only on the RDS files -- this only writes citius.duckdb. Run once to
# bootstrap the new store; Phase 2+ writer scripts take over from here.
#
# Usage: Rscript scripts/bootstrap_citius_duckdb.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

D <- here::here("citiusdata", "data")
conn <- get_citius_db_connection()
on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

parity_check <- function(table_name, rds_path, key_cols) {
  rds <- setDT(readRDS(rds_path))
  db  <- setDT(DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s", table_name)))

  ok <- TRUE
  if (nrow(rds) != nrow(db)) {
    cli::cli_alert_danger("{table_name}: row count mismatch -- rds {nrow(rds)}, db {nrow(db)}")
    ok <- FALSE
  }
  if (!setequal(names(rds), names(db))) {
    cli::cli_alert_danger("{table_name}: column set mismatch -- rds only: {setdiff(names(rds), names(db))}; db only: {setdiff(names(db), names(rds))}")
    ok <- FALSE
  }
  for (k in key_cols) {
    if (k %in% names(rds) && k %in% names(db)) {
      d <- setdiff(unique(rds[[k]]), unique(db[[k]]))
      b <- setdiff(unique(db[[k]]), unique(rds[[k]]))
      if (length(d) || length(b)) {
        cli::cli_alert_danger("{table_name}: {k} set mismatch -- rds only: {length(d)}, db only: {length(b)}")
        ok <- FALSE
      }
    }
  }
  if (ok) cli::cli_alert_success("{table_name}: PASS ({nrow(db)} rows, columns and keys match)")
  ok
}

cli::cli_h2("championship_results")
ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
store_championship_results(conn, ch, mode = "replace")
r1 <- parity_check("championship_results", file.path(D, "championship_results.rds"),
                   c("competition_id", "athlete_id"))

cli::cli_h2("athletics_corpus")
co <- setDT(readRDS(file.path(D, "athletics_corpus.rds")))
store_athletics_corpus(conn, co, mode = "replace")
r2 <- parity_check("athletics_corpus", file.path(D, "athletics_corpus.rds"),
                   c("competition_id", "athlete_id"))

cli::cli_h2("athletics_history")
hi <- setDT(readRDS(file.path(D, "athletics_history.rds")))
store_athletics_history(conn, hi, mode = "replace")
r3 <- parity_check("athletics_history", file.path(D, "athletics_history.rds"),
                   c("competition_id", "athlete_id"))

if (!all(r1, r2, r3)) {
  cli::cli_abort("Bootstrap parity check FAILED -- see above. citius.duckdb does not yet match the RDS files.")
}
cli::cli_alert_success("Bootstrap complete: citius.duckdb matches all three RDS files.")
