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

OUT <- here::here("citiusdata", "data")
COMP_CACHE <- file.path(OUT, "ath_comp_cache")
ATH_CACHE  <- file.path(OUT, "ath_athlete_cache")
dir.create(COMP_CACHE, recursive = TRUE, showWarnings = FALSE)
dir.create(ATH_CACHE, recursive = TRUE, showWarnings = FALSE)

# Day paging makes each competition ~12 requests, so the two stages are paced
# separately. Keep each run short enough to finish before any runtime cap.
MAX_COMPS <- as.integer(Sys.getenv("CITIUS_MAX_COMPS", "50"))
MAX_ATHLETES <- as.integer(Sys.getenv("CITIUS_MAX_ATHLETES", "400"))

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
    r <- tryCatch(find_competition(q), error = function(e) NULL)
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
  for (i in seq_len(n)) {
    cid <- todo_c$competition_id[i]
    r <- tryCatch(competition_results(cid, days = seq_len(todo_c$dur[i])),
                  error = function(e) NULL)
    if (!is.null(r) && nrow(r)) {
      r[, `:=`(comp_name = todo_c$name[i], comp_start = todo_c$start[i],
               comp_tier = todo_c$tier[i])]
    } else r <- data.table()
    saveRDS(r, file.path(COMP_CACHE, paste0(cid, ".rds")))
    if (i %% 50 == 0) cli::cli_alert("  stage 1: {i}/{n}")
  }
} else {
  cli::cli_alert_success("Stage 1 complete.")
}

champs <- rbindlist(lapply(list.files(COMP_CACHE, full.names = TRUE), readRDS),
                    use.names = TRUE, fill = TRUE)
if (nrow(champs)) {
  saveRDS(champs, file.path(OUT, "championship_results.rds"))
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
    for (i in seq_len(n)) {
      id <- todo_a[i]
      r <- tryCatch(athlete_results(id), error = function(e) NULL)
      saveRDS(if (is.null(r)) data.table() else r,
              file.path(ATH_CACHE, paste0(id, ".rds")))
      if (i %% 50 == 0) cli::cli_alert("  stage 2: {i}/{n}")
    }
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
