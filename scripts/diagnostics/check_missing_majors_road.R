# Can we identify the World Marathon Majors without scraping anything?
#
# The catalogue already tiers `road_race` by measured strength, and the file's
# own reasoning says knowledge beats estimation where knowledge exists - which is
# exactly the case for London, Berlin, Boston, Chicago, New York and Tokyo. The
# blocker is that those competitions have NO CATALOGUE ROW, so there is nothing
# to tier.
#
# The corpus store has comp_name NA for them. But the ATHLETE cache carries a
# `competition` name on every result row, and we already hold 87,125 of those
# files. If the majors are nameable from there, this is a local join rather than
# 6,093 more requests.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]

f <- list.files(file.path(D, "ath_athlete_cache"), full.names = TRUE)
cat(sprintf("scanning %s athlete cache files for competition names...\n",
            format(length(f), big.mark = ",")))
nm <- rbindlist(lapply(f, function(p) {
  x <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(x) || !all(c("competition_id","competition") %in% names(x))) return(NULL)
  y <- as.data.table(x)[, .(competition_id = as.character(competition_id),
                            competition, date, event_id,
                            venue_city = if ("venue_city" %in% names(x)) venue_city else NA_character_)]
  unique(y[!is.na(competition) & nzchar(competition)])
}), fill = TRUE)
nm <- unique(nm, by = c("competition_id", "event_id"))
cat(sprintf("named competition-events recovered: %s across %s competitions\n",
            format(nrow(nm), big.mark = ","), format(uniqueN(nm$competition_id), big.mark = ",")))

# which of those are road/marathon AND absent from the catalogue?
road <- nm[grepl("Marathon|Kilometres Road|Road Run|Half", event_id, ignore.case = TRUE) |
           grepl("^AT-(Marathon|HalfMarathon|[0-9]+KilometresRoad)", event_id)]
miss <- road[!competition_id %chin% cat0$competition_id]
cat(sprintf("\nroad competition-events NOT in the catalogue: %s (%s competitions)\n",
            format(nrow(miss), big.mark = ","), format(uniqueN(miss$competition_id), big.mark = ",")))

MAJORS <- "London|Berlin|Boston|Chicago|New York|Tokyo|Sydney|Valencia|Amsterdam|Rotterdam|Paris|Frankfurt|Dubai|Seville|Osaka|Nagoya|Houston|Xiamen|Hangzhou"
cat("\n=== named majors sitting OUTSIDE the catalogue ===\n")
mj <- miss[grepl(MAJORS, competition, ignore.case = TRUE)]
agg <- mj[, .(events = uniqueN(event_id), first = min(date), last = max(date)),
          by = .(competition, competition_id)]
agg <- agg[, .(competitions = .N, events = sum(events),
               first = min(first), last = max(last)), by = competition]
setorder(agg, -competitions)
print(utils::head(agg, 25))
cat(sprintf("\n%d distinct competition NAMES, %s competition ids, matching the majors list\n",
            nrow(agg), format(uniqueN(mj$competition_id), big.mark = ",")))
cat("\n=== the biggest uncatalogued road competitions by name, majors or not ===\n")
big <- miss[, .(ids = uniqueN(competition_id), rows = .N), by = competition][order(-rows)]
print(utils::head(big, 25))
