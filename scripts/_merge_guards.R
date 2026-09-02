# Shared safety helpers for scripts that swap championship_results.rds live
# (merge_referenced.R, merge_t3_full_checkpoint.R, merge_t3_pilot_2026.R).
# Factored out 2026-09-02 after a review found the two T3 merge scripts had
# no concurrent-run guard at all -- merge_referenced.R's own header comment
# explains why that matters (a backtest reads championship_results.rds
# lazily across its run, so replacing the file mid-flight gives the arm two
# different corpora and a history_md5 that describes neither; the same class
# of mistake as editing backtest_athletics.R while it ran, which cost a
# completed 380-meet arm on 2026-07-30).

#' Refuse to proceed if another Rscript process may be reading the corpus.
#'
#' A single `Rscript foo.R` on Windows produces TWO processes both named
#' Rscript -- a launcher and the worker R runs in -- so the caller's own pair
#' must be excluded by matching on its own script name in the command line,
#' not by process-tree walking (Windows recycles PIDs; the two processes of
#' one run have come back with unrelated ParentProcessIds).
#'
#' @param self_pattern The calling script's own filename, e.g.
#'   "merge_t3_pilot_2026.R" -- used to exclude its own launcher/worker pair.
citius_merge_guard <- function(self_pattern) {
  say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")
  ps <- paste0(
    "@(Get-CimInstance Win32_Process -Filter \"Name='Rscript.exe'\" | ",
    "Where-Object { $_.CommandLine -notlike '*", self_pattern, "*' }).Count")
  running <- system2("powershell.exe", c("-NoProfile", "-Command", shQuote(ps)),
                     stdout = TRUE, stderr = FALSE)
  running <- suppressWarnings(as.integer(tail(running[nzchar(running)], 1)))
  say("other R processes detected: ", if (is.na(running)) "unknown" else running)
  if (!is.na(running) && running > 0L) {
    cli::cli_alert_warning("{running} Rscript processes are running.")
    if (!nzchar(Sys.getenv("CITIUS_MERGE_FORCE"))) {
      cli::cli_abort(c("x" = "Refusing to swap the corpus while another R process may be reading it.",
                       "i" = "Wait for it, or set {.envvar CITIUS_MERGE_FORCE=1} if you are certain."))
    }
  }
}

#' Write an RDS file via tmp-file + rename so a crash mid-write never leaves
#' a truncated championship_results.rds -- the same file every reader trusts
#' as the source of truth.
citius_atomic_write <- function(obj, path) {
  tmp <- paste0(path, ".tmp-write")
  saveRDS(obj, tmp)
  ok <- file.rename(tmp, path)
  if (!ok) cli::cli_abort("atomic rename failed for {path}")
}
