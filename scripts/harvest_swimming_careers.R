# Harvest complete swim careers from World Aquatics.
#
# The competition route serves only sanctioned events -- ten probed national
# championships returned results for none of them -- so it cannot see a swimmer
# who has never made a global final. That was 37% of Glasgow's finalists, and
# harvesting the ENTIRE global calendar (43 -> 220 meets, 4.2x the swims) moved
# their coverage by three percentage points. The ceiling was not the harvest, it
# was the route.
#
# The ATHLETE route is different: for Hosszu Katinka it returns 1,893 swims
# across 160 meets against 1,191 across 28, and the extra meets are exactly the
# missing category -- Hungarian National Championships, Sette Colli, Mare
# Nostrum, CANA Africa Junior. Same asymmetry as athletics, where the athlete
# endpoint took results per athlete-event from a median of 1 to 7.
#
#   CITIUS_MAX_SWIMMERS  per run (resumable)
#   CITIUS_WORKERS       concurrency (measured sweet spot 6)

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "swim_athlete_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

sw <- setDT(readRDS(file.path(OUT, "swimming_history_full.rds")))
sw <- sw[!is.na(athlete_id)]

# Sex is not in the athlete response and match_event() needs it: "Women's 200m
# Medley" is unambiguous, "200m Freestyle" is not. Take it from what we hold,
# and drop athletes whose recorded sex conflicts rather than guess -- a wrong
# sex files an entire career under the wrong events.
known <- sw[!is.na(sex), .(sex = first(sex), n_sex = uniqueN(sex), n_held = .N),
            by = athlete_id]
known[n_sex > 1L, sex := NA_character_]
noSex <- sw[!athlete_id %in% known$athlete_id, .(sex = NA_character_, n_sex = 0L,
                                                 n_held = .N), by = athlete_id]
pool <- rbind(known, noSex, fill = TRUE)

# Finalists first: their estimates decide races, so an interrupted sweep still
# leaves the useful half done.
fin <- unique(sw[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
                   !grepl("semi", round, ignore.case = TRUE)]$athlete_id)
pool[, priority := fifelse(athlete_id %in% fin, 1L, fifelse(n_held <= 3L, 2L, 3L))]
setorder(pool, priority, n_held)
cli::cli_alert_info(
  "{format(nrow(pool), big.mark = ',')} swimmer{?s}: {sum(pool$priority == 1L)} finalist{?s}, {sum(is.na(pool$sex))} without a usable sex."
)

done <- sub("\\.rds$", "", list.files(CACHE))
todo <- pool[!athlete_id %in% done]
cli::cli_alert_info("{format(nrow(todo), big.mark = ',')} remaining.")
n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_SWIMMERS", "200000")))
remaining_after <- max(0L, nrow(todo) - n)
todo <- todo[seq_len(n)]
WORKERS <- as.integer(Sys.getenv("CITIUS_WORKERS", "6"))
cli::cli_alert_info("Fetching {n} swimmer{?s} on {WORKERS} worker{?s}.")

fetch_one <- function(j, cache) {
  r <- tryCatch(aquatics_athlete_results(j$id, sex = j$sex), error = function(e) NULL)
  saveRDS(if (is.null(r)) data.table::data.table() else r,
          file.path(cache, paste0(j$id, ".rds")))
  if (is.null(r)) 0L else nrow(r)
}
# Every value the worker needs travels inside the job: referencing `todo` from a
# worker fails silently and reports success having written nothing.
jobs <- lapply(seq_len(n), function(i) list(
  id = todo$athlete_id[i],
  sex = if (!is.na(todo$sex[i])) todo$sex[i] else NULL))

t0 <- Sys.time()
if (WORKERS <= 1L) {
  for (j in jobs) fetch_one(j, CACHE)
} else {
  cl <- parallel::makeCluster(WORKERS)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
    library(data.table); NULL
  })
  parallel::clusterExport(cl, c("fetch_one", "CACHE"), envir = environment())
  chunks <- split(seq_len(n), ceiling(seq_len(n) / 500))
  for (k in seq_along(chunks)) {
    idx <- chunks[[k]]
    got <- parallel::parLapply(cl, jobs[idx], function(j) fetch_one(j, CACHE))
    if (!any(unlist(got) > 0)) {
      cli::cli_alert_danger("Chunk {k} returned nothing for any swimmer - stopping.")
      break
    }
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    done_n <- max(idx)
    cat(sprintf("  %d/%d  (%.0f/min, ~%.0f min left, %s swims this chunk)\n",
                done_n, n, done_n / el, (n - done_n) / (done_n / el),
                format(sum(unlist(got)), big.mark = ",")))
    flush.console()
  }
}
cli::cli_alert_info("{remaining_after} swimmer{?s} still to fetch - run again.")
cli::cli_alert_info("Cache holds {length(list.files(CACHE))} file{?s}. Assemble with assemble_swimming_careers.R.")
