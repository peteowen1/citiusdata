# Shared safety helpers for harvest and merge scripts.
#
# citius_merge_guard() / citius_atomic_write() -- for scripts that swap
# championship_results.rds live (merge_referenced.R, merge_t3_full_checkpoint.R,
# merge_t3_pilot_2026.R). Factored out 2026-09-02 after a review found the two
# T3 merge scripts had no concurrent-run guard at all -- merge_referenced.R's
# own header comment explains why that matters (a backtest reads
# championship_results.rds lazily across its run, so replacing the file
# mid-flight gives the arm two different corpora and a history_md5 that
# describes neither; the same class of mistake as editing backtest_athletics.R
# while it ran, which cost a completed 380-meet arm on 2026-07-30).
#
# citius_empty_response_is_fault() -- for per-competition-cache harvesters
# (harvest_referenced.R, harvest_reharvest_targets.R, harvest_t3_full_2026.R,
# harvest_t3_pilot_2026.R). Hoisted 2026-09-04, see the function's own comment.

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

#' Is an EMPTY fetch response a genuine fact, or a transient fault?
#'
#' Hoisted 2026-09-04 from `harvest_reharvest_targets.R`, where this exact
#' check was written after a real incident: on 2026-08-20 a re-fetch of
#' 7196499 (Jamaican Championships 2023) returned nothing and overwrote 473
#' good rows, and a scan then found 47 competitions cached as empty while the
#' corpus held 20,201 rows for them -- among them the 2025 World Indoor
#' Championships and the Kenyan and South African nationals. The fix landed in
#' that one script and was never ported to `harvest_referenced.R`, which had
#' the same `list.files(CACHE)`-as-"done" pattern and the expected count
#' already in scope, unused -- and was then copy-pasted, still unguarded, into
#' two brand-new scripts (`harvest_t3_full_2026.R`, `harvest_t3_pilot_2026.R`).
#'
#' An empty response is only a FACT if nothing else contradicts it. If the
#' corpus already credits this competition with `expect` rows, an empty
#' response is a fault, not an answer, and must not be cached as one --
#' `list.files(CACHE)` treats "a .rds file exists" as "done" regardless of
#' content, so caching a fault here means it is never retried again.
#'
#' @param expect Rows the corpus already holds for this competition (NA if
#'   unknown -- treated as "cannot judge, not a fault").
#' @return TRUE if this is a fault (do not cache; retry later), FALSE if it is
#'   safe to cache as a genuine empty result.
citius_empty_response_is_fault <- function(expect) {
  isTRUE(!is.na(expect) && expect > 0)
}
