# T3_development completeness pilot: 2026-only, results >= 20 at last catalogue
# snapshot. 1,262 competitions, chosen as a bounded test of full T3 backfill
# before committing to the full ~20,000-competition gap. See NEXT-STEPS.md /
# session notes 2026-08-30 for the decision.
#
# Same resumable-cache pattern as harvest_referenced.R: each competition caches
# to its own file, skipped on a later pass, failures counted and reported.
#
# Usage: Rscript scripts/harvest_t3_pilot_2026.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_t3pilot")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

pilot <- readRDS(file.path(OUT, "..", "t3_pilot_scope_tmp.rds"))
todo_ids <- as.character(pilot$competition_id)
done <- sub("[.]rds$", "", list.files(CACHE, pattern = "[.]rds$"))
todo <- setdiff(todo_ids, done)
cli::cli_h2("{length(todo_ids)} pilot competitions | {length(done)} cached | {length(todo)} to fetch")

ok <- 0L; empty <- 0L; failed <- 0L; t0 <- Sys.time()
for (i in seq_along(todo)) {
  cid <- as.integer(todo[i])
  r <- tryCatch(setDT(athletics_competition_results(cid)),
                error = function(e) { failed <<- failed + 1L; NULL })
  if (is.null(r)) next
  if (!nrow(r)) { empty <- empty + 1L; saveRDS(data.table(), file.path(CACHE, paste0(cid, ".rds"))); next }
  saveRDS(r, file.path(CACHE, paste0(cid, ".rds"))); ok <- ok + 1L
  if (i %% 50 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cli::cli_alert_info("{i}/{length(todo)} | ok {ok} empty {empty} failed {failed} | {round(el,1)} min | {round(i/el,1)}/min")
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cli::cli_alert_success("fetched {ok}, empty {empty}, failed {failed} in {round(el,1)} min")
