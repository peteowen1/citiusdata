# SUPERSEDED -- kept only for its stage-0 competition DISCOVERY.
#
# Stage 2 (athlete histories) is replaced by harvest_athletics_careers.R, which
# is 6.8x faster and materially more complete. This script is serial, refetches
# each athlete's PROFILE before their results (doubling the requests), and lacks
# the guards added after a parallel sweep silently wrote zero files.
#
# Concretely, on the same work: 51 athletes/min here against 347 there, and the
# profile it consults is MISSING sex for 37% of athletes, leaving 15.5% of
# results with no event_id against 11.5%.
#
# Do not extend this file. Use:
#   harvest_athletics_meets.R  -- stage 0/1 only, competition discovery
#   harvest_athletics_careers.R -- athlete careers
#
# Large-scale athletics harvest, in two resumable stages.
#
# Stage 1 — competitions. Gives whole fields, which is what makes shared race
# effects identifiable and no-mark rates measurable.
# Stage 2 — athlete histories, seeded from every athlete seen in stage 1. This
# is what lifts backtest coverage: a field can only be scored for athletes we
# hold history on, and coverage was the binding constraint at 78%.
#
# Both stages cache per item and skip what exists, so the sweep is resumable and
# idempotent. That matters: this is tens of thousands of rate-limited requests
# and earlier monolithic runs were killed mid-sweep, losing everything.
#
# Run repeatedly until it reports nothing left to do.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
source(here::here("citiusdata", "scripts", "_env.R"))

OUT <- here::here("citiusdata", "data")
COMP_CACHE <- file.path(OUT, "ath_comp_cache")
ATH_CACHE  <- file.path(OUT, "ath_athlete_cache")
dir.create(COMP_CACHE, recursive = TRUE, showWarnings = FALSE)
dir.create(ATH_CACHE, recursive = TRUE, showWarnings = FALSE)

# Day paging makes each competition ~12 requests, so the two stages are paced
# separately. Keep each run short enough to finish before any runtime cap.
MAX_COMPS <- .env_int("CITIUS_MAX_COMPS", "50")
MAX_ATHLETES <- .env_int("CITIUS_MAX_ATHLETES", "400")

# --- stage 0: discover competitions -----------------------------------------
QUERIES <- c(
  "World Athletics Championships", "Olympic Games", "Commonwealth Games",
  "European Athletics Championships", "African Championships",
  "Asian Athletics Championships", "Pan American Games", "Diamond League",
  "World Athletics Indoor Championships", "Continental Tour",
  "USA Championships", "Jamaican Championships", "British Championships",
  "Kenyan Championships", "Australian Championships", "NCAA Division I",
  "National Championships", "Grand Prix", "Meeting", "Memorial",
  "Athletissima", "Weltklasse", "Bislett", "Prefontaine", "Golden Gala",
  "Racers Grand Prix", "Doha", "Rabat", "Oslo", "Monaco", "Lausanne",
  "Silesia", "Chorzow", "Zurich", "Brussels", "Shanghai", "Xiamen",
  "European Championships", "World Relays", "Universiade"
)

comp_file <- file.path(OUT, "ath_competitions.rds")
if (file.exists(comp_file)) {
  comps <- readRDS(comp_file)
} else {
  comps <- rbindlist(lapply(QUERIES, function(q) {
    r <- tryCatch(athletics_find_competition(q), error = function(e) NULL)
    if (is.null(r) || !nrow(r)) return(NULL)
    r[has_results == TRUE & start >= as.Date("2010-01-01") & start <= Sys.Date()]
  }), use.names = TRUE, fill = TRUE)
  comps <- unique(comps, by = "competition_id")
  saveRDS(comps, comp_file)
}
cli::cli_alert_info("{nrow(comps)} competition{?s} known.")

# --- stage 1: competition results -------------------------------------------
todo_c <- comps[!file.exists(file.path(COMP_CACHE, paste0(competition_id, ".rds")))]
if (nrow(todo_c)) {
  n <- min(nrow(todo_c), MAX_COMPS)
  cli::cli_alert_info("Stage 1: {n} of {nrow(todo_c)} competition{?s} remaining.")
  # Page only the days the meet actually ran. 603 of 1,120 known meets are one
  # day long, so a blanket 1:12 wasted 84% of requests — 13,440 day-pages issued
  # against 2,161 needed. Sampling 30 meets found none with events beyond its
  # listed duration, so the bound is safe; +1 covers a listing off by a day.
  #
  # Deliberately NOT an early stop on the first empty day: day pages are not
  # contiguous (competition 7134069 has events on days 1, 9, 11 and 12), so
  # stopping at a gap would silently lose most of a championship.
  todo_c[, dur := as.integer(end - start) + 1L]
  todo_c[is.na(dur) | dur < 1L, dur := 12L]   # unknown duration: sweep fully
  todo_c[, dur := pmin(dur + 1L, 12L)]        # +1 buffer, capped
  n_failed_c <- 0L
  for (i in seq_len(n)) {
    cid <- todo_c$competition_id[i]
    # A fetch ERROR must not be cached: writing an empty table for it records
    # "this competition has no results" as a fact and it is never retried.
    # Only a successful fetch -- including a genuinely empty one -- is cached.
    r <- tryCatch(athletics_competition_results(cid, days = seq_len(todo_c$dur[i])),
                  error = function(e) NULL)
    if (is.null(r)) {
      n_failed_c <- n_failed_c + 1L
    } else {
      if (nrow(r)) {
        r[, `:=`(comp_name = todo_c$name[i], comp_start = todo_c$start[i],
                 comp_tier = todo_c$tier[i])]
      }
      saveRDS(r, file.path(COMP_CACHE, paste0(cid, ".rds")))
    }
    if (i %% 50 == 0) cli::cli_alert("  stage 1: {i}/{n}")
  }
  if (n_failed_c) cli::cli_alert_warning(
    "Stage 1: {n_failed_c} fetch{?es} failed and {?was/were} left uncached for retry.")
} else {
  cli::cli_alert_success("Stage 1 complete.")
}

champs <- rbindlist(lapply(list.files(COMP_CACHE, full.names = TRUE), readRDS),
                    use.names = TRUE, fill = TRUE)
if (nrow(champs)) {
  # Guarded and atomic, matching merge_referenced.R/merge_t3_*.R -- found by
  # review 2026-09-04 that this script writes the exact same file those
  # scripts protect, the same bare-saveRDS-straight-to-final-path way, with
  # neither the concurrent-run check nor crash-safety.
  citius_merge_guard("harvest_athletics_meets.R")
  citius_atomic_write(champs, file.path(OUT, "championship_results.rds"))
  cli::cli_alert_success(
    "{nrow(champs)} result{?s} from {uniqueN(champs$competition_id)} competition{?s}."
  )
}

# --- stage 2: athlete histories ---------------------------------------------
# Seeded from everyone who appeared in a harvested field. Coverage of the
# backtest is exactly the fraction of a field we hold history for.
if (nrow(champs)) {
  ids <- unique(suppressWarnings(as.integer(champs$athlete_id)))
  ids <- ids[!is.na(ids)]
  todo_a <- ids[!file.exists(file.path(ATH_CACHE, paste0(ids, ".rds")))]
  cli::cli_alert_info("Stage 2: {length(todo_a)} of {length(ids)} athlete{?s} remaining.")

  if (length(todo_a)) {
    n <- min(length(todo_a), MAX_ATHLETES)
    n_failed <- 0L
    for (i in seq_len(n)) {
      id <- todo_a[i]
      ok <- TRUE
      r <- tryCatch(athletics_athlete_results(id), error = function(e) { ok <<- FALSE; NULL })
      # An error leaves no cache file (retried next run); only a genuine empty
      # result is cached, so a real miss is not confused with an unretried one.
      if (!ok) { n_failed <- n_failed + 1L } else {
        saveRDS(if (is.null(r)) data.table() else r,
                file.path(ATH_CACHE, paste0(id, ".rds")))
      }
      if (i %% 50 == 0) cli::cli_alert("  stage 2: {i}/{n}")
    }
    if (n_failed > 0L) cli::cli_alert_warning("{n_failed} fetch{?es} failed and were left uncached for retry.")
  }

  cached <- list.files(ATH_CACHE, full.names = TRUE)
  if (length(cached)) {
    hist <- rbindlist(lapply(cached, readRDS), use.names = TRUE, fill = TRUE)
    if (nrow(hist)) {
      saveRDS(hist, file.path(OUT, "athletics_history.rds"))
      cli::cli_alert_success(
        "{nrow(hist)} performance{?s} from {uniqueN(hist$athlete_id)} athlete{?s}."
      )
    }
  }
  remaining <- length(todo_a) - min(length(todo_a), MAX_ATHLETES)
  if (remaining > 0) cli::cli_alert_warning("{remaining} athlete{?s} still to fetch - run again.")
}
