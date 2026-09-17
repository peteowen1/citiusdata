# Harvest many meets in ONE R session, discovering the WA endpoint once.
#
# WHY. The 4,949-meet run on 2026-09-16/17 took 5h48m, and almost none of it
# was fetching results. Measured per meet, 2026-09-17:
#
#     library load                   0.24s
#     WA endpoint discovery          2.93s
#     day probe (1 request)          0.94s
#     ------------------------------------
#     fixed cost before any fetch    4.10s      (observed total ~4.2s/meet)
#
# harvest_missing_by_tier.ps1 spawns a FRESH Rscript per meet, so all of that
# is paid 4,949 times. Endpoint discovery alone is ~4.0 hours of the 5h48m.
# This pays it once.
#
# WHAT IT DOES NOT CHANGE. It calls harvest_wa_results.R per meet rather than
# reimplementing it, so the output contract (`<id>_raw_results.rds` plus the
# `_raw_startlist`/`_raw_summary` siblings), the day probe, the parse-rate
# assertions and every other guard in that script are byte-identical to a
# standalone run. The only difference is which endpoint object it uses.
#
# NOT parallel yet, deliberately. Serial-with-reuse is the big, safe win
# (~5h48m -> ~1.8h); workers are the second step and the repo has already
# measured that at 7.6x on the career route (0.11s/request at 6 vs 0.84s
# serial, latency bound). Get the free 4 hours before adding concurrency to a
# rate-limited public feed.
#
# Usage:
#   Rscript citiusdata/scripts/harvest_meets_batch.R <ids_file>
#     <ids_file>  one competition_id per line, relative to citiusdata/data
#   CITIUS_BATCH_LIMIT=50   stop after N meets (smoke test)
#   CITIUS_BATCH_REDISCOVER=300  re-discover the endpoint every N meets
#                                (default 300; WA rotates the edge)

VERSE <- here::here()
suppressMessages({library(httr2); library(data.table); library(cli)})
D       <- file.path(VERSE, "citiusdata", "data")
SCRIPTS <- file.path(VERSE, "citiusdata", "scripts")
HARVEST <- file.path(SCRIPTS, "harvest_wa_results.R")

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) cli_abort("Give a file of competition_ids, one per line.")
ids <- trimws(readLines(file.path(D, args[[1]])))
ids <- ids[nzchar(ids)]
LIMIT <- suppressWarnings(as.integer(Sys.getenv("CITIUS_BATCH_LIMIT", "")))
if (!is.na(LIMIT) && LIMIT > 0) ids <- head(ids, LIMIT)
REDISCOVER <- as.integer(Sys.getenv("CITIUS_BATCH_REDISCOVER", "300"))

# Already on disk = already done. Same resumability the PowerShell runner had,
# kept because a 4,949-meet job will be interrupted at least once.
done <- file.exists(file.path(D, paste0(ids, "_raw_results.rds")))
if (any(done)) cli_alert_info("{sum(done)} of {length(ids)} already harvested; skipping those.")
todo <- ids[!done]
if (!length(todo)) { cli_alert_success("Nothing to do."); quit(status = 0) }

.discover <- function() {
  o <- capture.output(v <- source(file.path(SCRIPTS, "discover_wa_endpoint.R"))$value)
  if (is.na(v$status) || v$status != 200L)
    cli_abort("Endpoint discovery returned HTTP {v$status}.")
  v
}

t_start <- Sys.time()
cli_h1("Batch harvest: {length(todo)} meet{?s}")
ep <- .discover()
cli_alert_success("Endpoint {.val {ep$edge}} discovered once for the whole batch.")

ok <- 0L; fail <- 0L; failed_ids <- character(0)
for (i in seq_along(todo)) {
  cid <- todo[i]

  # THE EDGE ROTATES. It moved three times in four days in September 2026
  # (4881 -> 4883 -> 4888), so a batch long enough to matter can outlive its
  # own endpoint. Re-discovering periodically costs 2.93s per REDISCOVER meets
  # instead of per meet, and turns a mid-run rotation from "every remaining
  # meet fails" into "one interval is slow".
  if (i > 1L && REDISCOVER > 0L && (i - 1L) %% REDISCOVER == 0L) {
    ep <- tryCatch(.discover(), error = function(e) { cli_alert_warning(
      "Re-discovery failed ({conditionMessage(e)}); keeping the current endpoint."); ep })
  }

  Sys.setenv(CITIUS_COMP = cid, CITIUS_MEET = cid)
  t0 <- Sys.time()
  # local() so each meet's top-level objects cannot leak into the next -- the
  # harvester assigns GQL, res, SIDE, DAY_DATES and more at top level, and a
  # leftover from a failed meet silently contaminating the next one is exactly
  # the class of bug this repo keeps finding. `ep` is passed in deliberately.
  res <- tryCatch({
    local({ ep <- ep; source(HARVEST, local = TRUE) }); TRUE
  }, error = function(e) { cli_alert_danger("{cid}: {conditionMessage(e)}"); FALSE })
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (isTRUE(res)) ok <- ok + 1L else { fail <- fail + 1L; failed_ids <- c(failed_ids, cid) }
  if (i %% 25L == 0L || i == length(todo)) {
    el <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
    rate <- el / i
    cli_alert_info(paste0("{i}/{length(todo)}  ok={ok} fail={fail}  ",
                          "{round(el,1)}m elapsed, {round(rate*60,1)}s/meet, ",
                          "~{round(rate*(length(todo)-i),1)}m left"))
  }
}

el <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cli_h2("Done")
cli_alert_success("{ok} ok, {fail} failed, of {length(todo)} in {round(el,1)} min ({round(el*60/max(1,length(todo)),2)}s/meet)")
if (length(failed_ids)) {
  f <- file.path(D, "harvest_batch_failed_ids.txt")
  writeLines(failed_ids, f)
  cli_alert_warning("Failed ids written to {.file {basename(f)}} -- re-run the batch against that file.")
}
