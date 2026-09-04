# Close the Aug 2026 gap: Glasgow's missing tail (29 Jul - 1 Aug), the whole
# of Birmingham 2026 (European Athletics Championships, entirely unharvested),
# and a light discovery probe for anything else in 2026-07-15..2026-08-18.
#
# WHY A FRESH RE-FETCH RATHER THAN THE CRS SCRAPE. The feed only carried four
# of Glasgow's seven competition days as of early August (see
# docs/reference/harvesting.md); watch_glasgow2026.R predicted the federation
# feed would catch up once World Athletics ingested Commonwealth results after
# the fact. It had, by 2026-08-18: a fresh athletics_competition_results() call
# returned 1,489 rows across 42 events (up from the stale 527), including both
# Josh Kerr Mile races (29 Jul heat, 1 Aug final) that the CRS-scrape-and-merge
# route (merge_athletics_crs.R) was built to backfill by hand. The federation
# feed source is preferred over the CRS scrape wherever it is complete: it
# carries none of the scrape's documented defects (dedup, sex-from-URL,
# reaction-time-as-mark -- see DECISIONS.md 2026-08-05).
#
# Birmingham 2026 (competition 7192415, Aug 10-16) was entirely absent from
# championship_results.rds -- score_meet.R harvests it for the daily card but
# never merges into the corpus, same gap as Glasgow's score_glasgow2026.R.
#
# Gentle throttle: an athlete-profile harvest running the same day hit
# sustained 429s from worldathletics.nimarion.de even at 0.75s spacing, with a
# flat 60s penalty per trip -- a burst-window limit, not a simple rate cap.
# This monkeypatches citius_get_json()'s default throttle for the session
# (citius_competition_results()/athletics_find_competition() do not expose a
# throttle parameter of their own) rather than editing the package.
#
# Usage:  Rscript scripts/harvest_gap_20260818.R
#   CITIUS_THROTTLE     seconds between requests (default 2.0; gentler than
#                       the package default 0.25 used elsewhere)
#   CITIUS_BUDGET_MIN   wall-time budget for the HARVEST phase only, not
#                       including the merge or the corpus/store rebuild
#                       (default 25)
#
# Result 2026-08-18: 22 competitions, 8,477 new rows, merged into
# championship_results.rds (3,330,917 -> 3,336,396 rows after dropping 11
# competitions already partially present and replacing them with the fresh,
# complete fetch). Backup kept at championship_results_pre20260818merge.rds.
# NEXT: build_athletics_corpus.R, build_stores.R (not run automatically here,
# unlike merge_referenced.R's chain -- kept manual so a calibration rebuild is
# never triggered as a side effect of a harvest).
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
source(here::here("citiusdata", "scripts", "_env.R"))

ns <- asNamespace("citius")
orig_get_json <- ns$citius_get_json
THROTTLE <- .env_num("CITIUS_THROTTLE", "2.0")
patched <- function(url, max_tries = 4L, throttle = THROTTLE) orig_get_json(url, max_tries = max_tries, throttle = throttle)
assignInNamespace("citius_get_json", patched, ns = "citius")

OUT <- here::here("citiusdata", "data")
t_start <- Sys.time()
BUDGET_MIN <- .env_num("CITIUS_BUDGET_MIN", "25")
time_left <- function() BUDGET_MIN - as.numeric(difftime(Sys.time(), t_start, units = "mins"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

say("throttle=%.2fs/request budget=%.0fmin", THROTTLE, BUDGET_MIN)

pieces <- list()

# --- Glasgow 2026 (Commonwealth Games), fresh full re-harvest ---------------
GLASGOW <- 7187518L
say("fetching Glasgow 2026 (%d) fresh, days 1:12 ...", GLASGOW)
glasgow <- tryCatch(athletics_competition_results(GLASGOW, days = 1:12),
                    error = function(e) { say("Glasgow FAILED: %s", conditionMessage(e)); NULL })
if (!is.null(glasgow) && nrow(glasgow)) {
  glasgow[, `:=`(comp_name = "XXIII Commonwealth Games", comp_start = as.Date("2026-07-27"), comp_tier = "A")]
  saveRDS(glasgow, file.path(OUT, "championship_results_glasgow_fresh.rds"))
  say("Glasgow: %d rows, %d events, %d races (was 527 rows through 2026-07-28 only)",
      nrow(glasgow), uniqueN(glasgow$event_id), uniqueN(glasgow$race_key))
  pieces$glasgow <- glasgow
} else say("Glasgow: no rows returned.")

# --- Birmingham 2026 (European Athletics Championships), fresh full harvest --
BHAM <- 7192415L
say("fetching Birmingham 2026 (%d) fresh, days 1:12 ...", BHAM)
bham <- tryCatch(athletics_competition_results(BHAM, days = 1:12),
                 error = function(e) { say("Birmingham FAILED: %s", conditionMessage(e)); NULL })
if (!is.null(bham) && nrow(bham)) {
  bham[, `:=`(comp_name = "European Athletics Championships", comp_start = as.Date("2026-08-10"), comp_tier = "A")]
  saveRDS(bham, file.path(OUT, "championship_results_birmingham_fresh.rds"))
  say("Birmingham: %d rows, %d events, %d races (was 0 rows -- entire meet missing)",
      nrow(bham), uniqueN(bham$event_id), uniqueN(bham$race_key))
  pieces$birmingham <- bham
} else say("Birmingham: no rows returned.")

# --- light discovery probe for anything else in the window -----------------
if (time_left() > 8) {
  QUERIES <- c("Diamond League", "British Championships", "USA Championships",
               "National Championships", "Memorial", "Meeting", "Grand Prix")
  say("light discovery probe: %d queries, time_left=%.1fmin", length(QUERIES), time_left())
  probed <- rbindlist(lapply(QUERIES, function(q) {
    if (time_left() <= 3) return(NULL)
    tryCatch(athletics_find_competition(q), error = function(e) NULL)
  }), use.names = TRUE, fill = TRUE)
  if (nrow(probed)) {
    probed <- unique(probed, by = "competition_id")
    cand <- probed[has_results == TRUE & start >= as.Date("2026-07-15") & start <= as.Date("2026-08-18") &
                     !competition_id %in% c(GLASGOW, BHAM)]
    say("candidates in window (2026-07-15..08-18), excluding Glasgow/Birmingham: %d", nrow(cand))
    if (nrow(cand)) {
      print(cand[order(start), .(competition_id, name = substr(name, 1, 50), start, end, tier)])
      saveRDS(cand, file.path(OUT, "gap_candidates_20260818.rds"))
      MAXN <- 25L
      cand <- head(cand[order(start)], MAXN)
      for (i in seq_len(nrow(cand))) {
        if (time_left() <= 3) { say("time budget low, stopping candidate sweep at %d/%d", i - 1L, nrow(cand)); break }
        cid <- cand$competition_id[i]
        dur <- suppressWarnings(as.integer(cand$end[i] - cand$start[i])) + 2L
        dur <- max(1L, min(if (is.na(dur)) 3L else dur, 10L))
        r <- tryCatch(athletics_competition_results(cid, days = seq_len(dur)), error = function(e) NULL)
        if (!is.null(r) && nrow(r)) {
          r[, `:=`(comp_name = cand$name[i], comp_start = cand$start[i], comp_tier = cand$tier[i])]
          pieces[[paste0("gap_", cid)]] <- r
          say("  %s (%d): %d rows", cand$name[i], cid, nrow(r))
        } else say("  %s (%d): no results", cand$name[i], cid)
      }
    }
  } else say("probe returned nothing.")
} else say("skipping discovery probe: time budget too low (%.1f min left)", time_left())

say("harvest phase done in %.1f min", as.numeric(difftime(Sys.time(), t_start, units = "mins")))

# --- merge -------------------------------------------------------------------
pieces <- Filter(function(x) !is.null(x) && nrow(x) > 0, pieces)
if (!length(pieces)) { say("nothing new fetched; nothing to merge."); quit(save = "no") }
new <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
say("total new rows fetched: %d across %d competition(s)", nrow(new), uniqueN(new$competition_id))

ch_f <- file.path(OUT, "championship_results.rds")
ch <- setDT(readRDS(ch_f))
say("existing championship_results.rds: %d rows / %d competitions, max date %s",
    nrow(ch), uniqueN(ch$competition_id), max(ch$date, na.rm = TRUE))

# Back up before overwriting, same rule as merge_referenced.R: never clobber
# an existing rollback, because a re-run's dedup would otherwise make it easy
# to overwrite the only route back with the already-merged state.
backup <- file.path(OUT, "championship_results_pre20260818merge.rds")
if (!file.exists(backup)) { saveRDS(ch, backup); say("backup written: %s", backup) } else say("backup already exists at %s; keeping it", backup)

# Drop whole competitions rather than de-duplicating rows: a half-merged field
# would corrupt the shared race effect, which is the entire reason these are
# worth fetching. See merge_referenced.R for the same reasoning.
dup <- intersect(unique(new$competition_id), unique(ch$competition_id))
if (length(dup)) {
  say("dropping %d competition(s) already (partially) present, replacing with fresh fetch: %s",
      length(dup), paste(dup, collapse = ", "))
  ch <- ch[!competition_id %in% dup]
}
stopifnot(!any(ch$competition_id == 0, na.rm = TRUE))
before_n <- nrow(ch); before_c <- uniqueN(ch$competition_id)
ch2 <- rbind(ch, new, fill = TRUE)
stopifnot(!any(ch2$competition_id == 0, na.rm = TRUE))
# Guarded and atomic, matching merge_referenced.R/merge_t3_*.R -- found by
# review 2026-09-04. The manual backup two blocks up (line 140) protects
# against a bad MERGE; this protects the write itself against a crash
# mid-saveRDS, which the backup does not.
citius_merge_guard("harvest_gap_20260818.R")
citius_atomic_write(ch2, ch_f)
say("merged -> %d rows / %d competitions (pre-merge base after drop: %d rows / %d comps)",
    nrow(ch2), uniqueN(ch2$competition_id), before_n, before_c)
say("new max date in championship_results.rds: %s", max(ch2$date, na.rm = TRUE))

# --- Kerr check --------------------------------------------------------------
kerr <- ch2[(athlete_id == "14533464" | athlete_id == 14533464) & competition_id == GLASGOW]
say("Josh Kerr (14533464) Glasgow rows in merged championship_results.rds: %d", nrow(kerr))
if (nrow(kerr)) print(kerr[, .(date, event_id, round, mark_string, place)])

say("DONE. Next: build_athletics_corpus.R, build_stores.R.")
