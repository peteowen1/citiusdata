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

# MEASURED, not assumed: World Aquatics serves results only for events it
# sanctions itself. Probing 10 unharvested global events found 3 with results;
# probing 10 national championships found 0. All 43 meets already held are
# global events. National meets are on this calendar because it is a calendar --
# their results live with the national federation.
#
# So the pool is the global calendar, not the whole listing. Sweeping all 3,310
# would spend hours proving that 2,835 national meets hold nothing.
#
# This also caps what swimming can know: the 37% of Glasgow finalists with no
# history are national-level swimmers whose results are not in this API at all.
# No amount of harvesting reaches them; that needs a different source.
OTHER_SPORTS <- "Water Polo|Diving|Artistic|Open Water|High Diving"
GLOBAL <- "Olympic|World Aquatics Championships|World Cup|World Swimming Championships|FINA"
pool <- pages[!grepl(OTHER_SPORTS, official_name, ignore.case = TRUE) &
                grepl(GLOBAL, official_name, ignore.case = TRUE) &
                !is.na(date_from) & date_from <= Sys.Date() & date_from >= FROM]
# Oldest first: recent meets are listed before their results are loaded, so a
# newest-first sweep spends its first hours on empty files. Ordering this way
# means an interrupted run still leaves usable data behind.
data.table::setorder(pool, date_from)
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
n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_COMPS", "5000")))
remaining_after <- max(0L, nrow(todo) - n)
todo <- todo[seq_len(n)]

# Measured like the athletics feed: LATENCY bound, not throttle bound. Per
# request 0.283s at a 0.25s throttle and 0.274s at 0.08s -- flat, so the throttle
# is not what costs. Concurrency is: 1 worker 0.275s/req, 3 workers 0.152s,
# 6 workers 0.117s, zero failures throughout.
#
# 75% of listed competitions hold results, averaging 38 disciplines each, so this
# is ~91,600 requests. Six workers takes it from 7 hours to 3.
WORKERS <- as.integer(Sys.getenv("CITIUS_WORKERS", "6"))
cli::cli_alert_info("Harvesting {n} competition{?s} on {WORKERS} worker{?s}.")

fetch_comp <- function(j, cache) {
  f <- file.path(cache, paste0(j$cid, ".rds"))
  disc <- tryCatch(aquatics_disciplines(j$cid), error = function(e) NULL)
  # An empty file records a competition that holds nothing, so a rerun does not
  # pay to rediscover it. 25% of listed meets are empty -- mostly ones too recent
  # to have been loaded.
  if (is.null(disc) || !nrow(disc)) { saveRDS(data.table::data.table(), f); return(0L) }
  out <- data.table::rbindlist(lapply(disc$discipline_id, function(did) {
    tryCatch(aquatics_results(did), error = function(e) NULL)
  }), use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    # competition_id and race_key are NOT optional metadata. race_key is what
    # calibrate() groups on to identify the shared race effect, and
    # competition_id is the backtest's scoring key -- omitting them produces a
    # corpus that loads fine, calibrates silently wrong, and reports "0 meets".
    # That is exactly what the first version of this script did.
    out[, `:=`(competition_id = j$cid, comp_name = j$name, comp_start = j$start,
               course = j$course, level = j$level,
               venue_city = j$city, venue_country = j$country)]
    out[, race_key := paste(competition_id, event_id, heat_name, sep = "|")]
  }
  saveRDS(out, f)
  nrow(out)
}

jobs <- lapply(seq_len(n), function(i) list(
  cid = todo$competition_id[i], name = todo$official_name[i],
  start = todo$date_from[i], course = todo$course[i], level = todo$level[i],
  city = todo$city[i], country = todo$country[i]))

t0 <- Sys.time()
if (WORKERS <= 1L) {
  for (j in jobs) fetch_comp(j, CACHE)
} else {
  cl <- parallel::makeCluster(WORKERS)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
    library(data.table); NULL
  })
  # Every value the worker needs travels INSIDE the job. Referencing `todo` from
  # the worker looks natural, fails silently because the data.table is not in
  # that environment, and reports success having written nothing -- which is
  # exactly what happened on the first parallel attempt at the athletics sweep.
  parallel::clusterExport(cl, c("fetch_comp", "CACHE"), envir = environment())
  chunks <- split(seq_len(n), ceiling(seq_len(n) / 100))
  for (k in seq_along(chunks)) {
    idx <- chunks[[k]]
    got <- parallel::parLapply(cl, jobs[idx], function(j) fetch_comp(j, CACHE))
    # A chunk where NOTHING succeeded is a failure, not a quiet success. The
    # first parallel sweep in this project reported success having written zero
    # files, because the worker referenced a variable that was never exported;
    # parLapply swallowed every error. Caught only by checking the file count.
    #
    # Empty competitions are legitimate here (a listed meet whose results are not
    # loaded), so this guards on a whole chunk failing rather than on any single
    # empty result.
    if (!any(unlist(got) > 0) && k > 1L) {
      cli::cli_alert_danger("Chunk {k} returned no swims for any competition - stopping.")
      break
    }
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    done <- max(idx)
    cat(sprintf("  %d/%d  (%.1f/min, ~%.0f min left, %s swims this chunk)\n",
                done, n, done / el, (n - done) / (done / el),
                format(sum(unlist(got)), big.mark = ",")))
    flush.console()
  }
}

cli::cli_alert_info("{remaining_after} competition{?s} still to fetch - run again.")

# Assembly is OFF by default, as for the athletics sweep: a complete harvest is
# millions of swims across ~3,100 files, and rbindlist-ing all of them after
# every batch costs minutes and a large memory peak for no benefit. The cache is
# the source of truth.
if (!identical(Sys.getenv("CITIUS_ASSEMBLE", "0"), "1")) {
  cli::cli_alert_info("Cache holds {length(list.files(CACHE))} competition file{?s}. Set CITIUS_ASSEMBLE=1 to build swimming_history_full.rds.")
} else {
  files <- list.files(CACHE, full.names = TRUE)
  cli::cli_alert_info("Assembling {length(files)} file{?s}...")
  parts <- lapply(split(files, ceiling(seq_along(files) / 500)), function(ch)
    rbindlist(lapply(ch, readRDS), use.names = TRUE, fill = TRUE))
  all <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  rm(parts); invisible(gc())
  if (nrow(all)) {
    saveRDS(all, file.path(OUT, "swimming_history_full.rds"))
    cat(sprintf("\n%s swims | %s meets | %s athletes\n",
                format(nrow(all), big.mark = ","), uniqueN(all$competition_id),
                format(uniqueN(all$athlete_id), big.mark = ",")))
    if ("level" %in% names(all)) print(all[, .(swims = .N, meets = uniqueN(competition_id)),
                                           by = .(level, course)][order(-swims)])
  }
}
