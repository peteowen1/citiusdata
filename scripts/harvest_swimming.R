# Harvest World Aquatics swimming results at scale.
#
# Three request levels: competitions -> disciplines -> results. The middle level
# is what makes this slow, so competitions are filtered to majors before any
# discipline call is made.
#
# Long course only. 25m results are excluded rather than pooled: a short-course
# time is a different event physically (more turns, more push-off) and mixing
# them would inflate within-athlete variance with a systematic offset rather
# than genuine form variation.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

pages <- rbindlist(lapply(0:30, function(p) {
  tryCatch(aquatics_competitions(page = p, page_size = 100, sort = "dateFrom,desc"),
           error = function(e) NULL)
}), use.names = TRUE, fill = TRUE)
pages <- unique(pages, by = "competition_id")

# World Aquatics governs six sports and names them all "World Cup", so a naive
# name filter sweeps in Water Polo, Diving, Artistic Swimming, High Diving and
# Open Water. Those legitimately return zero *pool swimming* disciplines, which
# looks like a harvest failure but is the filter's fault. Exclude them by name.
OTHER_SPORTS <- "Water Polo|Diving|Artistic|Open Water|Masters|Junior|Youth|25m"
major <- pages[date_from <= Sys.Date() & date_from >= as.Date("2015-01-01") &
                 grepl("Olympic Games|World Aquatics Championships|Swimming World Cup|World Swimming Championships",
                       name, ignore.case = TRUE) &
                 !grepl(OTHER_SPORTS, name, ignore.case = TRUE)]
cli::cli_alert_info("Harvesting {nrow(major)} competition{?s}.")

# Each competition is cached to its own file as soon as it lands. This is a
# ~2000-request sweep across three API levels; accumulating in memory and saving
# only at the end means any interruption discards the lot, and a rerun starts
# from zero. Per-competition caching makes the harvest resumable and idempotent.
CACHE <- file.path(OUT, "swim_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(major))) {
  cid <- major$competition_id[i]
  f <- file.path(CACHE, paste0(cid, ".rds"))
  if (file.exists(f)) next                      # already harvested

  disc <- tryCatch(aquatics_disciplines(cid), error = function(e) NULL)
  if (is.null(disc) || !nrow(disc)) {
    saveRDS(data.table(), f)                    # record the miss, don't retry it
    next
  }

  out <- rbindlist(lapply(disc$discipline_id, function(did) {
    tryCatch(aquatics_results(did), error = function(e) NULL)
  }), use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    out[, `:=`(competition_id = cid, comp_name = major$name[i],
               comp_start = major$date_from[i])]
    cli::cli_alert_success("[{i}/{nrow(major)}] {major$name[i]}: {nrow(out)} swim{?s}.")
  }
  saveRDS(out, f)
}

results <- rbindlist(lapply(list.files(CACHE, full.names = TRUE), readRDS),
                     use.names = TRUE, fill = TRUE)

if (!nrow(results)) {
  cli::cli_alert_danger("No results harvested.")
} else {
  # race_key groups a heat; needed for calibrate() to identify shared effects.
  results[, race_key := paste(competition_id, event_id, heat_name, sep = "|")]
  saveRDS(results, file.path(OUT, "swimming_history.rds"))
  cli::cli_alert_success(
    "{nrow(results)} swim{?s} | {uniqueN(results$athlete_id)} athlete{?s} | {uniqueN(results$event_id)} event{?s}."
  )
}
