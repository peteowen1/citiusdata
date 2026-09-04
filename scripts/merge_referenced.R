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
#         CITIUS_MERGE_INPUT=championship_results_majors.rds Rscript ...
#           (merge a different harvest's output -- e.g. harvest_missing_majors.R's
#           -- instead of the referenced-competition harvest's default)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

new_f_name <- Sys.getenv("CITIUS_MERGE_INPUT", "championship_results_referenced.rds")
new_f <- file.path(OUT, new_f_name)
ch_f  <- file.path(OUT, "championship_results.rds")
if (!file.exists(new_f)) cli::cli_abort("No {.file {new_f_name}}; run harvest_referenced.R first (or set CITIUS_MERGE_INPUT to point at a different harvest's output).")

citius_merge_guard("merge_referenced.R")

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
citius_atomic_write(ch, ch_f)
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
