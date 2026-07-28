# Assemble the swimming cache, restoring competition_id and race_key.
#
# The first full sweep wrote comp_name and comp_start but not competition_id or
# race_key. race_key is what calibrate() groups on for the shared race effect
# and competition_id is the backtest's scoring key, so a corpus without them
# loads cleanly and calibrates silently wrong -- it reported "0 meets" and
# nothing else complained.
#
# No re-fetch is needed: each cache FILENAME is the competition id, so the
# missing column can be reconstructed from the cache itself.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "swim_cache_full")
files <- list.files(CACHE, full.names = TRUE)
cli::cli_alert_info("Assembling {length(files)} cache file{?s}.")

read_one <- function(f) {
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  setDT(x)
  cid <- as.integer(sub("\\.rds$", "", basename(f)))
  if (!"competition_id" %in% names(x) || all(is.na(x$competition_id))) {
    x[, competition_id := cid]
  }
  if (!"race_key" %in% names(x) || all(is.na(x$race_key))) {
    # Same construction as the original harvest: a heat within an event within a
    # competition is one race.
    x[, race_key := paste(competition_id, event_id,
                          if ("heat_name" %in% names(x)) heat_name else NA_character_,
                          sep = "|")]
  }
  x
}

parts <- lapply(split(files, ceiling(seq_along(files) / 200)), function(ch)
  rbindlist(lapply(ch, read_one), use.names = TRUE, fill = TRUE))
all <- rbindlist(parts, use.names = TRUE, fill = TRUE)
rm(parts); invisible(gc())

stopifnot(
  "competition_id must be populated" = mean(!is.na(all$competition_id)) > 0.99,
  "race_key must be populated" = mean(!is.na(all$race_key)) > 0.99
)
saveRDS(all, file.path(OUT, "swimming_history_full.rds"))

cat(sprintf("\n%s swims | %s meets | %s races | %s athletes\n",
            format(nrow(all), big.mark = ","), uniqueN(all$competition_id),
            format(uniqueN(all$race_key), big.mark = ","),
            format(uniqueN(all$athlete_id), big.mark = ",")))
cat(sprintf("date range: %s to %s\n", min(all$date, na.rm = TRUE), max(all$date, na.rm = TRUE)))
print(all[, .(swims = .N, meets = uniqueN(competition_id)), by = .(level, course)][order(-swims)])
cat("\nfield size per race (a pool is 8-10 lanes):\n")
print(summary(all[!is.na(race_key), .N, by = race_key]$N))
