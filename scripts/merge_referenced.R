# Merge the referenced-competition harvest into the championship store.
#
# harvest_referenced.R ends by printing the remaining steps as prose -- "rbind
# into championship_results.rds, then build_athletics_corpus.R, build_stores.R,
# build_competition_catalogue.R, rebaseline_chain.R". Prose is not a procedure:
# it has to be retyped correctly at 2am, the order matters because every step
# reads the one before, and getting it wrong produces a corpus that looks fine
# and is subtly stale.
#
# This is that instruction as code. It merges, then runs the chain in order.
#
# Usage:  Rscript scripts/merge_referenced.R
#         CITIUS_MERGE_ONLY=1 Rscript ...     (merge, skip the rebuild chain)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

new_f <- file.path(OUT, "championship_results_referenced.rds")
ch_f  <- file.path(OUT, "championship_results.rds")
if (!file.exists(new_f)) cli::cli_abort("No {.file championship_results_referenced.rds}; run harvest_referenced.R first.")

# REFUSE TO MERGE UNDER A RUNNING ARM. A backtest reads championship_results.rds
# lazily across its run, so replacing the file mid-flight gives the arm two
# different corpora and a history_md5 that describes neither. This is the same
# class of mistake as editing backtest_athletics.R while it ran, which cost a
# completed 380-meet arm on 2026-07-30.
# COUNTING Rscript PROCESSES FROM INSIDE ONE NEEDS THE SELF-TREE EXCLUDED.
#
# A single `Rscript foo.R` on Windows produces TWO processes both named
# Rscript -- a launcher and the worker R runs in. The first version of this
# guard counted them and compared against 1, so it fired on every clean run and
# refused a merge with nothing else in the machine. It failed safe, but a guard
# that always trips gets disabled, which is worse than not having it.
#
# So: exclude this process, its parent (the launcher), and any child of it.
# Anything left is genuinely someone else's R.
# Identify our own by COMMAND LINE, not by process tree. Both the launcher and
# the worker carry the same command line, so matching on the script name
# excludes exactly our pair. Parent-PID walking was tried and is not reliable
# here: Windows recycles PIDs, and the two Rscript.exe processes of one run came
# back with unrelated ParentProcessIds.
ps <- paste0(
  "@(Get-CimInstance Win32_Process -Filter \"Name='Rscript.exe'\" | ",
  "Where-Object { $_.CommandLine -notlike '*merge_referenced.R*' }).Count")
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

ch  <- setDT(readRDS(ch_f))
new <- setDT(readRDS(new_f))
say("existing ", format(nrow(ch), big.mark = ","), " rows / ",
    format(uniqueN(ch$competition_id), big.mark = ","), " comps")
say("harvested ", format(nrow(new), big.mark = ","), " rows / ",
    format(uniqueN(new$competition_id), big.mark = ","), " comps")

# The harvest already skips competition_ids present in the store, but it is
# resumable across runs and other harvests (majors, gap) write the same store,
# so overlap is possible. Dropping whole competitions rather than de-duplicating
# rows keeps each competition sourced from exactly one harvest -- a half-merged
# field would corrupt the shared race effect, which is the entire reason these
# competitions are worth fetching.
dup <- intersect(unique(new$competition_id), unique(ch$competition_id))
if (length(dup)) {
  say("dropping ", length(dup), " competition(s) already in the store")
  new <- new[!competition_id %in% dup]
}

before <- nrow(ch)
ch <- rbind(ch, new, fill = TRUE)
stopifnot(nrow(ch) == before + nrow(new))

# The 0-sentinel bug again: a phantom competition_id 0 once collected 2.5M rows
# and made every meet-level statistic meaningless. Cheap to assert, expensive to
# miss.
stopifnot(!any(ch$competition_id == 0, na.rm = TRUE))

if (!nrow(new)) {
  cli::cli_alert_warning("Nothing new to merge; leaving the store untouched.")
  quit(save = "no")
}

# NEVER OVERWRITE AN EXISTING ROLLBACK. Re-running the merge would otherwise
# copy the ALREADY-MERGED store over the backup, so the second run silently
# destroys the only route back to the pre-harvest state -- exactly when you most
# want it, because a re-run usually means something went wrong the first time.
# The dedup above makes a second run a no-op for the data, which is precisely
# what would make this easy to miss.
backup <- file.path(OUT, "championship_results_premerge.rds")
if (file.exists(backup)) {
  say("rollback already exists at championship_results_premerge.rds; keeping it")
} else {
  saveRDS(readRDS(ch_f), backup)
}
saveRDS(ch, ch_f)
say("merged -> ", format(nrow(ch), big.mark = ","), " rows / ",
    format(uniqueN(ch$competition_id), big.mark = ","), " comps",
    "  (previous store kept at championship_results_premerge.rds)")

# citius.duckdb MUST move with the RDS file, not just get rebuilt from it
# later. build_stores.R now sources the shipping Arrow store from DuckDB
# (2026-08-30) -- if this script updated only the RDS, DuckDB would silently
# fall behind on the very next merge, and build_stores.R would build the
# store every downstream prediction reads from off stale data with no error
# anywhere in the chain. `new` is the SAME already-deduped rows just written
# above; store_championship_results() does its own idempotent
# competition-level dedup against whatever DuckDB already holds, so this is
# safe to re-run.
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

if (nzchar(Sys.getenv("CITIUS_MERGE_ONLY"))) {
  say("CITIUS_MERGE_ONLY set; skipping the rebuild chain.")
  quit(save = "no")
}

# Order is not negotiable: the corpus feeds the stores, the stores feed the
# catalogue, and the calibration chain reads the corpus. Each step is run in a
# fresh process so that a failure is loud and localised rather than leaving a
# half-updated environment to poison the next step.
chain <- c("build_athletics_corpus.R", "build_stores.R",
           "build_competition_catalogue.R", "rebaseline_chain.R")
for (s in chain) {
  say("=== ", s)
  code <- system2("Rscript", shQuote(here::here("citiusdata", "scripts", s)))
  if (!identical(code, 0L)) cli::cli_abort("{.file {s}} exited {code}; chain stopped.")
}
say("chain complete. Re-run the reference arm before scoring anything.")
