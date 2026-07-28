# Assemble the Swim England ranking cache into one corpus.
#
# Separate from harvesting for the same reason as the other two sweeps:
# rbindlist-ing thousands of files after every batch costs minutes and a large
# memory peak, and the cache -- not the assembled file -- is the source of
# truth. A previous harvester here merged straight into an assembled corpus and
# silently deleted 3,305 rows; never do that again.
#
# Usage:  Rscript scripts/assemble_swimengland.R
VERSE <- "C:/dev/citiusverse"
suppressMessages({library(citius); library(data.table)})
OUT <- file.path(VERSE, "citiusdata", "data")
CACHE <- file.path(OUT, "se_rankings_cache")
say <- function(...) cat(sprintf(...), "\n", sep = "")

files <- list.files(CACHE, full.names = TRUE)
if (!length(files)) { say("cache is empty -- run harvest_swimengland_rankings.R"); quit(save = "no") }
say("assembling %s cache file%s", format(length(files), big.mark = ","),
    if (length(files) == 1) "" else "s")

# Chunked so the peak is one chunk plus the accumulator, not two full copies.
parts <- lapply(split(files, ceiling(seq_along(files) / 2000)), function(ch)
  rbindlist(lapply(ch, function(f) tryCatch(readRDS(f), error = function(e) NULL)),
            use.names = TRUE, fill = TRUE))
all <- rbindlist(parts, use.names = TRUE, fill = TRUE)
rm(parts); invisible(gc())
if (!nrow(all)) { say("nothing assembled"); quit(save = "no") }

# The same swimmer appears once per (event, pool, sex, year) per nationality
# sweep, and the nationality sweeps overlap -- X is the superset, J/G/I are
# swept separately because X may not include the Crown Dependencies. Dedupe on
# the natural key so the overlap does not double-count.
before <- nrow(all)
all <- unique(all, by = c("tiref", "discipline", "course", "sex", "season", "mark_string"))
say("rows %s -> %s after dedupe (%.1f%% overlap between nationality sweeps)",
    format(before, big.mark = ","), format(nrow(all), big.mark = ","),
    100 * (1 - nrow(all) / before))

all[, event_id := match_event(discipline, sex)]
all[, mark := parse_mark(mark_string)]
say("event matched: %.1f%% | marks parsed: %.1f%%",
    100 * mean(!is.na(all$event_id)), 100 * mean(!is.na(all$mark)))
un <- all[is.na(event_id), .N, by = discipline][order(-N)]
if (nrow(un)) { say("\nunmatched disciplines (expect the short-course-only IMs):"); print(head(un, 8)) }

say("\n%s ranked times | %s swimmers | %s meets | %s..%s",
    format(nrow(all), big.mark = ","), format(uniqueN(all$tiref), big.mark = ","),
    format(uniqueN(all$comp_name), big.mark = ","),
    min(all$date, na.rm = TRUE), max(all$date, na.rm = TRUE))
say("course split: %s", paste(sprintf("%s %s", names(table(all$course)),
                                      format(table(all$course), big.mark = ",")), collapse = " | "))
say("season bests per swimmer: median %s, max %s",
    median(all[, .N, by = tiref]$N), max(all[, .N, by = tiref]$N))

saveRDS(all, file.path(OUT, "swimengland_rankings.rds"))
arrow::write_parquet(all, file.path(OUT, "swimengland_rankings.parquet"))
say("\nwrote swimengland_rankings.{rds,parquet}")
say("NOTE: every row is is_best = TRUE. These are ranked lists, truncated at the")
say("good end, so they support ability estimation but NOT variance estimation.")
