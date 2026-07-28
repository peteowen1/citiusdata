# Full World Aquatics pool-swimming harvest.
#
# The previous filter kept only Olympics / Worlds / World Cup: 36 competitions
# of 2,272 available since 2015. That is why 37% of Glasgow finalists had no
# history to be rated from -- national-level swimmers by construction cannot
# appear in a set that contains only the three biggest meets on earth.
#
# Principle: HARVEST BROADLY, FILTER AT MODEL TIME. Re-harvesting costs hours of
# rate-limited requests; filtering a column costs nothing. Everything below is
# kept and TAGGED rather than excluded, so a later decision to use or ignore
# short course, masters or age-group meets needs no new sweep.
#
# Excluded only: the five other sports World Aquatics governs, which return no
# pool swimming disciplines at all.
#
#   CITIUS_SWIM_FROM   earliest competition date (default 2010-01-01)
#   CITIUS_MAX_COMPS   competitions per run; resumable, so run until it says 0

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "swim_cache_full")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
FROM <- as.Date(Sys.getenv("CITIUS_SWIM_FROM", "2010-01-01"))

pages <- rbindlist(lapply(0:40, function(p) {
  tryCatch(aquatics_competitions(page = p, page_size = 100, sort = "dateFrom,desc"),
           error = function(e) NULL)
}), use.names = TRUE, fill = TRUE)
pages <- unique(setDT(pages), by = "competition_id")
saveRDS(pages, file.path(OUT, "swim_competitions_all.rds"))
cli::cli_alert_info("{nrow(pages)} competition{?s} listed by World Aquatics.")

# The only exclusions: sports that are not pool swimming. Masters, junior, youth
# and short course are all KEPT and tagged -- a masters swimmer is a real
# athlete, an age-group result is real evidence about a young swimmer, and a
# short-course time is a real performance in a different event.
OTHER_SPORTS <- "Water Polo|Diving|Artistic|Open Water|High Diving"
pool <- pages[!grepl(OTHER_SPORTS, official_name, ignore.case = TRUE) &
                !is.na(date_from) & date_from <= Sys.Date() & date_from >= FROM]
# Course matters physically: a 25m pool has twice the turns and push-offs, so a
# short-course time is a DIFFERENT event, not a faster version of the same one.
# Tagged here; the models decide what to do with it.
pool[, course := fifelse(grepl("\\(25m\\)|short course", official_name, ignore.case = TRUE),
                         "SC", "LC")]
pool[, level := fifelse(grepl("Masters", official_name, ignore.case = TRUE), "masters",
                 fifelse(grepl("Junior|Youth|Age Group", official_name, ignore.case = TRUE), "age-group",
                 fifelse(grepl("Olympic|World Aquatics Championships|World Cup", official_name,
                               ignore.case = TRUE), "global", "national")))]
cli::cli_alert_info("{nrow(pool)} pool competition{?s} since {format(FROM)}.")
print(pool[, .N, by = .(level, course)][order(-N)])

todo <- pool[!file.exists(file.path(CACHE, paste0(competition_id, ".rds")))]
cli::cli_alert_info("{nrow(todo)} remaining to harvest.")
n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_COMPS", "3000")))

for (i in seq_len(n)) {
  cid <- todo$competition_id[i]
  f <- file.path(CACHE, paste0(cid, ".rds"))
  disc <- tryCatch(aquatics_disciplines(cid), error = function(e) NULL)
  if (is.null(disc) || !nrow(disc)) { saveRDS(data.table(), f); next }
  out <- rbindlist(lapply(disc$discipline_id, function(did) {
    tryCatch(aquatics_results(did), error = function(e) NULL)
  }), use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    out[, `:=`(comp_name = todo$official_name[i], comp_start = todo$date_from[i],
               course = todo$course[i], level = todo$level[i],
               venue_city = todo$city[i], venue_country = todo$country[i])]
  }
  saveRDS(out, f)
  if (i %% 25 == 0) { cat(sprintf("  %d/%d\n", i, n)); flush.console() }
}

files <- list.files(CACHE, full.names = TRUE)
all <- rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
if (nrow(all)) {
  saveRDS(all, file.path(OUT, "swimming_history_full.rds"))
  cat(sprintf("\n%s swims | %s meets | %s athletes\n",
              format(nrow(all), big.mark = ","), uniqueN(all$competition_id),
              format(uniqueN(all$athlete_id), big.mark = ",")))
  if ("level" %in% names(all)) print(all[, .(swims = .N, meets = uniqueN(competition_id)),
                                         by = .(level, course)][order(-swims)])
}
cli::cli_alert_info("{nrow(todo) - n} competition{?s} still to fetch - run again.")
