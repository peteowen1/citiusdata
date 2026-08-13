# Pre-harvest audit: are we capturing everything the feed offers, correctly?
#
# Run before any large re-harvest. Two questions, both cheap to answer now and
# expensive to answer later:
#
#   1. COVERAGE  - which fields exist in the raw JSON that we never read?
#                  A re-harvest is the only cheap moment to start capturing a
#                  field; adding one later means fetching everything again.
#   2. FIDELITY  - of the fields we do read, are any unexpectedly missing,
#                  duplicated, or malformed?
#
# This exists because two fields we DID need were sitting in the feed unread:
# `raceNumber` (heats collapsed into one race for the whole corpus) and the
# display `mark` string (trailing zeros dropped, turning 6.00m into 6cm).

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

N_COMPS <- as.integer(Sys.getenv("CITIUS_AUDIT_COMPS", "12"))
OUT <- here::here("citiusdata", "data")
comps <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))

# Sample across tiers and eras rather than taking the most recent: field
# availability varies by meet type, and a championship-only sample would miss
# what a one-day Diamond League omits.
set.seed(11)
comps[, dur := as.integer(end - start) + 1L]
samp <- comps[!is.na(start)][order(start)][round(seq(1, .N, length.out = N_COMPS))]
cli::cli_alert_info("Auditing {nrow(samp)} competition{?s} across {min(samp$start)} to {max(samp$start)}.")

# --- 1. enumerate every key present in the raw feed --------------------------
keys <- list()
note <- function(level, k) keys[[level]] <<- c(keys[[level]], k)
sample_vals <- list()

base_u <- athletics_base_url()
n_fetch <- 0L; n_fetch_failed <- 0L
for (i in seq_len(nrow(samp))) {
  cid <- samp$competition_id[i]
  ndays <- min(max(samp$dur[i], 1L) + 1L, 12L)
  for (d in seq_len(ndays)) {
    n_fetch <- n_fetch + 1L
    r <- tryCatch(citius_get_json(sprintf("%s/competitions/%d/results?day=%d", base_u, cid, d)),
                  error = function(e) { n_fetch_failed <<- n_fetch_failed + 1L; NULL })
    for (ev in r$events %||% list()) {
      note("event", names(ev))
      for (rc in ev$races %||% list()) {
        note("race", names(rc))
        for (x in rc$results %||% list()) {
          note("result", names(x))
          note("location", names(x$location %||% list()))
          for (a in x$athletes %||% list()) note("athlete", names(a))
          for (nm in names(x)) {
            if (!is.list(x[[nm]]) && is.null(sample_vals[[nm]])) {
              sample_vals[[nm]] <- as.character(x[[nm]])[1]
            }
          }
        }
      }
    }
  }
  cat(sprintf("  %d/%d\n", i, nrow(samp))); flush.console()
}
cli::cli_alert_info("day-page fetches: {n_fetch} attempted, {n_fetch_failed} failed.")

# What athletics_competition_results() actually produces, for the same competitions.
parsed <- rbindlist(lapply(samp$competition_id, function(cid) {
  tryCatch(athletics_competition_results(cid), error = function(e) NULL)
}), use.names = TRUE, fill = TRUE)

cli::cli_h2("1. Field coverage — what the feed offers vs what we keep")
# Fields we read but rename; everything else unlisted is genuinely dropped.
captured <- c(
  eventId = "race_key", discipline = "discipline", sex = "sex_code",
  isTechnical = "is_technical", category = "tier",
  raceId = "race_key", raceNumber = "race_key", race = "round", date = "date",
  place = "place", mark = "mark_string", wind = "wind",
  performanceValue = "value_raw", country = "(unused)", athletes = "athlete_id",
  location = "venue_*", indoor = "indoor", stadium = "venue_stadium",
  city = "venue_city", id = "athlete_id", firstname = "athlete_name",
  lastname = "athlete_name", birthdate = "birthdate"
)
for (lvl in names(keys)) {
  tab <- as.data.table(table(keys[[lvl]]))[order(-N)]
  setnames(tab, c("field", "seen"))
  tab[, kept := fifelse(field %in% names(captured), captured[field], "-- DROPPED --")]
  tab[, example := vapply(field, function(f) sample_vals[[f]] %||% "", character(1))]
  cat("\n  [", lvl, "]\n", sep = "")
  print(tab[, .(field, seen, kept, example = substr(example, 1, 22))])
}

dropped <- unique(unlist(lapply(keys, function(k) setdiff(unique(k), names(captured)))))
cli::cli_h3("Fields present in the feed but NOT captured")
# "Nothing dropped" is only a real pass if at least one fetch actually
# succeeded -- otherwise `keys` is empty because nothing was ever read, not
# because nothing was missing, and that must not read as a clean audit.
if (n_fetch > 0L && n_fetch_failed >= n_fetch) {
  cli::cli_alert_danger("UNVERIFIABLE: feed unreachable -- all {n_fetch} day-page fetch{?es} failed.")
} else if (n_fetch > 0L && n_fetch_failed > n_fetch / 2) {
  cli::cli_alert_danger("UNVERIFIABLE: feed unreachable -- {n_fetch_failed}/{n_fetch} day-page fetches failed.")
} else if (length(dropped)) {
  for (f in dropped) cat(sprintf("  %-20s e.g. %s\n", f, substr(sample_vals[[f]] %||% "(nested)", 1, 40)))
  cat("\n  Decide NOW whether to capture these -- adding one later costs a full re-harvest.\n")
} else cli::cli_alert_success("Nothing dropped.")

# --- 2. fidelity of what we captured -----------------------------------------
cli::cli_h2("2. Fidelity of the captured columns")
if (!nrow(parsed)) {
  cli::cli_alert_danger("No rows parsed.")
} else {
  cat(sprintf("  %s rows | %s races | %s athletes\n\n", format(nrow(parsed), big.mark = ","),
              format(uniqueN(parsed$race_key), big.mark = ","),
              format(uniqueN(parsed$athlete_id), big.mark = ",")))
  na <- parsed[, lapply(.SD, function(v) round(100 * mean(is.na(v)), 2))]
  na <- data.table(column = names(na), pct_na = unlist(na))[order(-pct_na)]
  # perf/place/wind/birthdate are legitimately missing; identifiers are not.
  na[, verdict := fifelse(column %in% c("athlete_id", "race_key", "date", "competition_id",
                                        "discipline", "round") & pct_na > 0,
                          "<- IDENTIFIER, investigate", "")]
  print(na)

  cli::cli_h3("Duplicates and race integrity")
  d1 <- parsed[, .N, by = .(race_key, athlete_id)][N > 1]
  cat(sprintf("  athlete twice in one race : %d\n", nrow(d1)))
  sp <- parsed[, .(dates = uniqueN(date), events = uniqueN(event_id)), by = race_key]
  cat(sprintf("  race_key spanning 2 dates : %d\n", sum(sp$dates > 1)))
  cat(sprintf("  race_key spanning 2 events: %d\n", sum(sp$events > 1)))
  reg <- citius_events()
  lane <- reg$event_id[reg$family %in% c("sprint", "hurdles")]
  ln <- parsed[event_id %in% lane, .N, by = race_key]
  cat(sprintf("  lane races over 10        : %d (max %d)\n", sum(ln$N > 10),
              if (nrow(ln)) max(ln$N) else 0L))
  dup <- parsed[!is.na(place) & place > 0L, .(dups = sum(duplicated(place))), by = race_key]
  cat(sprintf("  races with dup placings   : %d\n", sum(dup$dups > 0)))
  cat(sprintf("  unmatched events          : %d (%.1f%%)\n", sum(is.na(parsed$event_id)),
              100 * mean(is.na(parsed$event_id))))
  if (any(is.na(parsed$event_id))) {
    print(head(parsed[is.na(event_id), .N, by = discipline][order(-N)], 10))
  }
}
