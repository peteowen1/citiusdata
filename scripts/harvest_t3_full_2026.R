# Full T3_development backfill: 18,748 competitions (the real remaining gap
# after the 2026-only pilot merged, re-derived 2026-08-31 -- union of
# competition_catalogue.parquet + the two archived pre-corpus-rebuild
# snapshots, T3_development rows only, diffed against the CURRENT
# championship_results.rds). Real multi-day background operation.
#
# Same resumable-cache pattern as harvest_t3_pilot_2026.R, plus a heartbeat
# file so a FUTURE check can tell "still working" from "died silently"
# without watching it live -- the pilot's first resume attempt died with zero
# trace and it took a live poll to notice. Updated every 25 competitions.
#
# THE FIRST RUN (2026-08-31 11:26-20:15) collapsed to a crawl after ~1 hour --
# throughput ~6.8/min for the first 100 competitions, then ~3.5/hour for the
# next ~7.75 hours, spent almost entirely in flat 60s retry backoffs (httr2
# respecting a server Retry-After header -- a deliberate rate-limit signal,
# not random server struggle; confirmed by inspecting the retry config in
# citius/R/utils.R and the fact that the limit had already reset by the time
# this was diagnosed minutes after stopping the process).
#
# ROOT CAUSE, confirmed by a live instrumented test (2026-08-31, 5 real T3
# competition ids): athletics_competition_results()'s default `days = 1:12`
# fetches up to 12 separate day-page requests per competition. T3 meets are
# local/club/regional -- 1-2 non-empty days out of 12 checked, every time
# tested. 10-11 of every 12 requests per competition were pure waste, and
# THAT volume (not raw competition count) is what tripped the rate limit
# after the first ~100 competitions' worth of burst traffic. Fixed by
# passing `days = 1:3` below -- a 4x cut in request volume with margin above
# the observed 1-2 day maximum, not a guess. Worth keeping regardless of the
# paragraph below -- it's a real, separate efficiency fix.
#
# THE RETEST (2026-08-31, after the days=1:3 fix, relaunched cleanly and
# tracked live for 15-16 min by two independent methods): stalled flat at the
# SAME cache count for 13 minutes, then crawled to ~1.5/min -- worse than the
# original run's own first 15 minutes. The days=1:3 fix did NOT clear this.
# Revised diagnosis: the initial "it's fine now" read (a handful of isolated
# diagnostic requests succeeding minutes after stopping the first run) was a
# false negative -- trivial volume was never going to retrigger a penalty
# that a few spaced-out requests can't test either way. The 13-minute dead
# stall at the START of the retest, before any meaningful new volume had been
# sent, points at a longer-lived IP-level rate-limit/penalty from the first
# run's sustained hammering, not a volume-per-competition problem the days=1:3
# fix could address alone.
#
# STATUS 2026-08-31: PAUSED. Pete's call -- not worth chasing an unknown
# cooldown window against an external system today, given T1/T2 are
# essentially complete and T3 is documented elsewhere as low-value for the
# model. 334 competitions were cached before stopping and have been merged
# (see merge_t3_full_checkpoint.R) -- that data is not wasted. Resuming this
# needs either a much longer untested cooldown before the next attempt, or a
# fundamentally more conservative sustained rate accepted as slow from
# minute one rather than fast-then-collapsing. Do not just re-run this file
# expecting a different result without addressing the pacing.
#
# Usage: Rscript scripts/harvest_t3_full_2026.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_t3full")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
HEARTBEAT <- file.path(OUT, "t3_full_heartbeat_tmp.txt")

target_ids <- as.character(readRDS(file.path(OUT, "t3_full_target_ids_tmp.rds")))
done <- sub("[.]rds$", "", list.files(CACHE, pattern = "[.]rds$"))
todo <- setdiff(target_ids, done)
cli::cli_h2("{length(target_ids)} T3 full-backfill competitions | {length(done)} cached | {length(todo)} to fetch")
writeLines(sprintf("START %s | %d/%d cached", format(Sys.time()), length(done), length(target_ids)), HEARTBEAT)

ok <- 0L; empty <- 0L; failed <- 0L; t0 <- Sys.time()
for (i in seq_along(todo)) {
  cid <- as.integer(todo[i])
  r <- tryCatch(setDT(athletics_competition_results(cid, days = 1:3)),
                error = function(e) { failed <<- failed + 1L; NULL })
  if (is.null(r)) next
  if (!nrow(r)) { empty <- empty + 1L; saveRDS(data.table(), file.path(CACHE, paste0(cid, ".rds"))); next }
  saveRDS(r, file.path(CACHE, paste0(cid, ".rds"))); ok <- ok + 1L
  if (i %% 25 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    total_done <- length(done) + ok + empty
    writeLines(sprintf("HEARTBEAT %s | %d/%d fetched this run (ok %d empty %d failed %d) | %d/%d total cached | %.1f min | %.1f/min",
                        format(Sys.time()), i, length(todo), ok, empty, failed,
                        total_done, length(target_ids), el, i / el), HEARTBEAT)
    cli::cli_alert_info("{i}/{length(todo)} | ok {ok} empty {empty} failed {failed} | {round(el,1)} min | {round(i/el,1)}/min")
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
writeLines(sprintf("DONE %s | fetched %d empty %d failed %d in %.1f min", format(Sys.time()), ok, empty, failed, el), HEARTBEAT)
cli::cli_alert_success("fetched {ok}, empty {empty}, failed {failed} in {round(el,1)} min")
