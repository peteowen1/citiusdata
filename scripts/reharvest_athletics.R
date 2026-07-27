# Full athletics re-harvest under the current parser.
#
# Writes to a SUFFIXED cache and output file rather than over the live ones, so
# a backtest or prediction run reading championship_results.rds is never pulled
# out from under. Promote by renaming once validated.
#
#   CITIUS_HARVEST_TAG   suffix for cache dir and output (default "v3")
#   CITIUS_MAX_COMPS     competitions per run (resumable; default all)
#
# Re-harvests are expensive and rare, so capture everything the feed offers even
# if unused today — adding a field later means fetching all of it again. See
# scripts/audit_feed_coverage.R, which exists to answer that before each sweep.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("CITIUS_HARVEST_TAG", "v3")
CACHE <- file.path(OUT, paste0("ath_comp_cache_", TAG))
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

comps <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))
todo <- comps[!file.exists(file.path(CACHE, paste0(competition_id, ".rds")))]
cli::cli_alert_info("{nrow(todo)} of {nrow(comps)} competition{?s} remaining ({TAG}).")

# Page only the days the meet ran. 603 of 1,120 meets last a single day, so a
# blanket 1:12 wasted 84% of requests. NOT an early stop on the first empty day:
# day pages are not contiguous (competition 7134069 has events on days 1, 9, 11
# and 12), so stopping at a gap would silently lose most of a championship.
todo[, dur := as.integer(end - start) + 1L]
todo[is.na(dur) | dur < 1L, dur := 12L]
todo[, dur := pmin(dur + 1L, 12L)]

n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_COMPS", "1200")))
for (i in seq_len(n)) {
  cid <- todo$competition_id[i]
  r <- tryCatch(competition_results(cid, days = seq_len(todo$dur[i])),
                error = function(e) NULL)
  if (!is.null(r) && nrow(r)) {
    r[, `:=`(comp_name = todo$name[i], comp_start = todo$start[i],
             comp_tier = todo$tier[i])]
  } else r <- data.table()
  saveRDS(r, file.path(CACHE, paste0(cid, ".rds")))
  if (i %% 25 == 0) { cat(sprintf("  %d/%d\n", i, n)); flush.console() }
}

champs <- rbindlist(lapply(list.files(CACHE, full.names = TRUE), readRDS),
                    use.names = TRUE, fill = TRUE)
if (nrow(champs)) {
  champs <- champs[!is.na(date)]
  saveRDS(champs, file.path(OUT, paste0("championship_results_", TAG, ".rds")))
  cat(sprintf("\n%s results | %d meets | %s races | %s athletes\n",
              format(nrow(champs), big.mark = ","), uniqueN(champs$competition_id),
              format(uniqueN(champs$race_key), big.mark = ","),
              format(uniqueN(champs$athlete_id), big.mark = ",")))
  cat(sprintf("unmatched events: %.1f%%\n", 100 * mean(is.na(champs$event_id))))
  dup <- champs[!is.na(place) & place > 0L,
                .(dups = sum(duplicated(place))), by = race_key]
  cat(sprintf("races with duplicate placings: %s\n",
              format(sum(dup$dups > 0), big.mark = ",")))
  for (col in c("discipline_code", "event_name", "comp_day", "birthdate_year_only")) {
    if (col %in% names(champs)) {
      cat(sprintf("  %-20s %.1f%% populated\n", col, 100 * mean(!is.na(champs[[col]]))))
    }
  }
}
