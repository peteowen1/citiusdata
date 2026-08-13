# Harvest complete athlete careers from World Athletics.
#
# We hold a MEDIAN OF 6% of each athlete's career. Sampled profiles carry 50-271
# results where our competition harvest holds 3-17. Competition discovery was
# built from ~50 hand-written name queries, so any meet not matching a keyword
# is invisible -- and most meets do not match.
#
# This is very probably the root of the open calibration problem: 73% of
# finalists have <=2 results in their event, so w_total is tiny, shrinkage is
# heavy, and every thinly-observed athlete regresses toward the event mean. A
# model whose probabilities are "spread too evenly" is what thin evidence looks
# like from the outside.
#
# The athlete endpoint returns a whole career in ONE request, so this is far
# cheaper per result than competition discovery: ~87k requests to go from 6% to
# effectively complete.
#
# Ordered by relevance rather than by id: athletes who reach FINALS are the ones
# whose ability estimates actually decide a forecast, so they are fetched first
# and an interrupted sweep still leaves the useful half done.
#
#   CITIUS_MAX_ATHLETES  per run (resumable; run until it reports 0 remaining)

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_athlete_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, aid := suppressWarnings(as.integer(athlete_id))]
ch <- ch[!is.na(aid)]

# Priority 1: anyone who has contested a final -- their estimate decides races.
# Priority 2: anyone we hold few results for, since they gain the most.
# Priority 3: everyone else.
fin <- unique(ch[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
                   !grepl("semi", round, ignore.case = TRUE)]$aid)
# athletics_athlete_results() fetches the PROFILE first when sex or birthdate is missing,
# which doubles the request count for a sweep this size. We already hold both
# for every one of these athletes, so passing them turns 2 requests per athlete
# into 1 -- measured at 51 athletes/min, that is the difference between a 28
# hour sweep and a 14 hour one.
# Verified before relying on it: across 59 sampled athletes the feed's sex and
# the profile's NEVER disagreed (37 of 37 where both exist), and the profile is
# MISSING sex for 37% -- so passing it also recovers results that would
# otherwise get no event_id at all (15.5% unusable -> 11.5%).
#
# 604 athletes (0.69%) carry conflicting sex_code within our own data. Those are
# left NULL so they take the profile path rather than being fetched under a
# guessed sex, which would file an entire career under the wrong events.
known <- ch[!is.na(sex_code) | !is.na(birthdate),
            .(sex = data.table::first(stats::na.omit(sex_code)),
              birthdate = data.table::first(stats::na.omit(birthdate)),
              n_sex = data.table::uniqueN(stats::na.omit(sex_code))), by = aid]
known[n_sex > 1L, sex := NA_character_]
held <- ch[, .(n_held = .N), by = aid]
held <- merge(held, known, by = "aid", all.x = TRUE)
held[, priority := fifelse(aid %in% fin, 1L, fifelse(n_held <= 3L, 2L, 3L))]
setorder(held, priority, n_held)
cli::cli_alert_info(
  "{format(nrow(held), big.mark = ',')} athlete{?s}: {sum(held$priority == 1L)} finalist{?s}, {sum(held$priority == 2L)} thinly held, {sum(held$priority == 3L)} other."
)

done <- sub("\\.rds$", "", list.files(CACHE))
todo <- held[!as.character(aid) %in% done]
cli::cli_alert_info("{format(nrow(todo), big.mark = ',')} remaining ({round(100 * nrow(todo) / nrow(held))}%).")

n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_ATHLETES", "200000")))
# Measured: this feed is LATENCY bound, not throttle bound -- per-request time is
# flat from a 0.25s throttle down to 0.03s (0.85s vs 0.76s). So the only lever is
# concurrency, and it works: 1 worker 0.81s/req, 3 workers 0.37s, 6 workers
# 0.22s, 10 workers 0.19s. Zero failures at every level.
#
# Six is the sweet spot. Ten buys 0.5x more for 67% more connections against a
# free community API, which is not a trade worth making.
WORKERS <- as.integer(Sys.getenv("CITIUS_WORKERS", "6"))
remaining_after <- max(0L, nrow(todo) - n)   # captured BEFORE todo is truncated
todo <- todo[seq_len(n)]
cli::cli_alert_info("Fetching {n} athlete{?s} on {WORKERS} worker{?s}.")

fetch_one <- function(id, sx, bd, cache) {
  ok <- TRUE
  r <- tryCatch(athletics_athlete_results(id, sex = sx, birthdate = bd),
                error = function(e) { ok <<- FALSE; NULL })
  # A fetch ERROR does not write a file at all, so the athlete is retried next
  # run instead of being cached as a permanent miss -- caching an empty result
  # for a request that never actually completed poisons the cache the same way
  # an empty file poisons a "no results" case, except this one was never true.
  if (!ok) return(list(n = 0L, failed = TRUE))
  # A genuinely empty result IS cached, so a rerun does not retry it forever.
  # Each worker writes its own files, so no coordination is needed.
  saveRDS(if (is.null(r)) data.table::data.table() else r,
          file.path(cache, paste0(id, ".rds")))
  list(n = if (is.null(r)) 0L else nrow(r), failed = FALSE)
}

n_failed <- 0L
t0 <- Sys.time()
if (WORKERS <= 1L) {
  for (i in seq_len(n)) {
    res <- fetch_one(todo$aid[i], if (!is.na(todo$sex[i])) todo$sex[i] else NULL,
                     if (!is.na(todo$birthdate[i])) todo$birthdate[i] else NULL, CACHE)
    if (isTRUE(res$failed)) n_failed <- n_failed + 1L
  }
} else {
  cl <- parallel::makeCluster(WORKERS)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  # PSOCK workers need library()/load_all on the search path; requireNamespace
  # alone leaves the functions unavailable unqualified.
  parallel::clusterEvalQ(cl, {
    suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
    library(data.table); NULL
  })
  parallel::clusterExport(cl, c("fetch_one", "CACHE"), envir = environment())
  # Each task carries its OWN values. Referencing `todo` inside the worker looks
  # natural and fails silently: the data.table is not in the worker's
  # environment, every task errors, parLapply swallows it and the run reports
  # success having written nothing. Caught only because the cache count did not
  # move.
  jobs <- lapply(seq_len(n), function(i) list(
    id = todo$aid[i],
    sx = if (!is.na(todo$sex[i])) todo$sex[i] else NULL,
    bd = if (!is.na(todo$birthdate[i])) todo$birthdate[i] else NULL))
  # Chunked so progress is visible and a stall is obvious, rather than one
  # opaque multi-hour call.
  chunks <- split(seq_len(n), ceiling(seq_len(n) / 500))
  for (k in seq_along(chunks)) {
    idx <- chunks[[k]]
    got <- parallel::parLapply(cl, jobs[idx], function(j)
      fetch_one(j$id, j$sx, j$bd, CACHE))
    n_failed <- n_failed + sum(vapply(got, function(g) isTRUE(g$failed), logical(1)))
    # A chunk that returns nothing usable is a failure, not a quiet success.
    if (!any(vapply(got, function(g) !isTRUE(g$failed) && g$n > 0, logical(1)))) {
      cli::cli_alert_danger("Chunk {k} returned no data for any athlete - stopping.")
      break
    }
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    done <- max(idx)
    cat(sprintf("  %d/%d  (%.0f/min, ~%.0f min left)\n", done, n, done / el,
                (n - done) / (done / el)))
    flush.console()
  }
}

cli::cli_alert_info("{remaining_after} athlete{?s} still to fetch - run again.")
if (n_failed > 0L) {
  cli::cli_alert_warning("{n_failed} fetch{?es} failed and were left uncached for retry.")
}

# Assembly is optional and OFF by default. At ~92 results per athlete a complete
# sweep is ~8 million rows across 87k files; rbindlist-ing all of them every run
# costs minutes and a multi-GB peak, and this ecosystem has already OOM-killed a
# pipeline through exactly that kind of accumulation. The cache is the source of
# truth -- assemble once at the end, not after every batch.
if (!identical(Sys.getenv("CITIUS_ASSEMBLE", "0"), "1")) {
  cli::cli_alert_info("Cache holds {length(list.files(CACHE))} athlete file{?s}. Set CITIUS_ASSEMBLE=1 to build athletics_history.rds.")
} else {
  cached <- list.files(CACHE, full.names = TRUE)
  cli::cli_alert_info("Assembling {length(cached)} file{?s}...")
  # Chunked so peak memory is one chunk plus the accumulator, not two full
  # copies of everything.
  parts <- lapply(split(cached, ceiling(seq_along(cached) / 5000)), function(ch)
    rbindlist(lapply(ch, readRDS), use.names = TRUE, fill = TRUE))
  hist <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  rm(parts); invisible(gc())
  if (nrow(hist)) {
    saveRDS(hist, file.path(OUT, "athletics_history.rds"))
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::write_parquet(hist, file.path(OUT, "athletics_history.parquet"))
    } else {
      message("arrow not installed -- skipped athletics_history.parquet")
    }
    cat(sprintf("\n%s performance%s from %s athlete%s\n",
                format(nrow(hist), big.mark = ","), if (nrow(hist) == 1) "" else "s",
                format(uniqueN(hist$athlete_id), big.mark = ","),
                if (uniqueN(hist$athlete_id) == 1) "" else "s"))
    cat(sprintf("mean results per athlete: %.1f (competition harvest gave ~3)\n",
                nrow(hist) / uniqueN(hist$athlete_id)))
  }
}
