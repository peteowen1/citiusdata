# Assemble the swim career cache into a single corpus.
#
# harvest_swimming_careers.R pointed at this script before it existed -- an audit
# of all three scrapers against the session's learnings caught it.
#
# Assembly is separate from harvesting for the same reason as the athletics
# sweep: rbindlist-ing tens of thousands of files after every batch costs
# minutes and a large memory peak, and the cache is the source of truth.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "swim_athlete_cache")
files <- list.files(CACHE, full.names = TRUE)
cli::cli_alert_info("Assembling {format(length(files), big.mark = ',')} cache file{?s}.")

# Chunked so the peak is one chunk plus the accumulator rather than two full
# copies. This ecosystem has already OOM-killed a pipeline through exactly that
# kind of accumulation.
parts <- lapply(split(files, ceiling(seq_along(files) / 2000)), function(ch)
  rbindlist(lapply(ch, function(f) tryCatch(readRDS(f), error = function(e) NULL)),
            use.names = TRUE, fill = TRUE))
all <- rbindlist(parts, use.names = TRUE, fill = TRUE)
rm(parts); invisible(gc())
if (!nrow(all)) { cli::cli_alert_danger("Nothing to assemble."); quit(save = "no") }

saveRDS(all, file.path(OUT, "swim_athlete_history.rds"))
cat(sprintf("\n%s swims | %s athletes | %s meets | %s races\n",
            format(nrow(all), big.mark = ","),
            format(uniqueN(all$athlete_id), big.mark = ","),
            format(uniqueN(all$comp_name), big.mark = ","),
            format(uniqueN(all$race_key), big.mark = ",")))
cat(sprintf("date range: %s to %s\n", min(all$date, na.rm = TRUE), max(all$date, na.rm = TRUE)))

cat("\n=== the number this harvest exists to move ===\n")
old <- setDT(readRDS(file.path(OUT, "swimming_history_full.rds")))
oe <- old[!is.na(event_id), .N, by = .(athlete_id, event_id)]
ne <- all[!is.na(event_id), .N, by = .(athlete_id, event_id)]
cat(sprintf("  results per athlete-event, competition route: median %.0f, mean %.1f\n",
            median(oe$N), mean(oe$N)))
cat(sprintf("  results per athlete-event, career route     : median %.0f, mean %.1f\n",
            median(ne$N), mean(ne$N)))
cat("  Shrinkage is driven by this. A median of 1 means every athlete regresses\n")
cat("  to the event mean, which is the 'probability spread too evenly' symptom.\n")

cat("\n=== competition types, unavailable from the competition route ===\n")
if ("comp_type" %in% names(all)) print(all[, .N, by = comp_type][order(-N)])

cat("\n=== unmatched (expect relays) ===\n")
print(head(all[is.na(event_id), .N, by = discipline][order(-N)], 6))
cat(sprintf("\nunmatched: %.1f%%\n", 100 * mean(is.na(all$event_id))))
