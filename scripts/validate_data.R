# Data validation sweep over the harvested corpora.
#
# Run after any harvest. Nothing here fixes anything -- it reports, loudly, and
# every check exists because a silent bug of that shape has already happened:
#
#   - field sizes no track can hold        (race_key collapsed heats)
#   - marks outside physical bounds        (performanceValue dropped trailing zeros)
#   - unmatched events                     (registry built from the Olympic subset)
#   - duplicate placings within a race     (foul tie-break annihilated)
#
# The last one is the lesson: an outlier filter HID the vault bug and produced
# clean-looking output with real data deleted. So these checks compare against
# physical plausibility, not against the data's own distribution.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
problems <- 0L
flag <- function(fmt, ...) {
  problems <<- problems + 1L
  cat(sprintf(paste0("  [!] ", fmt, "\n"), ...))
}
ok <- function(fmt, ...) cat(sprintf(paste0("  ok  ", fmt, "\n"), ...))

check_corpus <- function(d, label, lane_check = TRUE) {
  cli::cli_h2(label)
  setDT(d)
  cat(sprintf("  %s rows | %s athletes | %s races | %s to %s\n",
              format(nrow(d), big.mark = ","),
              format(uniqueN(d$athlete_id), big.mark = ","),
              format(uniqueN(d$race_key), big.mark = ","),
              min(d$date, na.rm = TRUE), max(d$date, na.rm = TRUE)))

  # --- completeness of the columns models actually consume -------------------
  cat("\n  missingness in modelled columns:\n")
  for (col in intersect(c("athlete_id", "event_id", "date", "perf", "mark",
                          "place", "round", "race_key", "age"), names(d))) {
    na <- sum(is.na(d[[col]])); pct <- 100 * na / nrow(d)
    line <- sprintf("    %-12s %7s missing (%5.2f%%)", col, format(na, big.mark = ","), pct)
    # perf/place/age are legitimately missing (no-marks, DNS, unknown DOB).
    # An identifier or a key is not.
    if (col %in% c("athlete_id", "date", "race_key") && na > 0) {
      cat(line, " <- IDENTIFIER, should be 0\n"); problems <<- problems + 1L
    } else cat(line, "\n")
  }

  # --- event matching --------------------------------------------------------
  unmatched <- d[is.na(event_id)]
  if (nrow(unmatched)) {
    cat(sprintf("\n  %s rows (%.1f%%) have no event_id:\n",
                format(nrow(unmatched), big.mark = ","), 100 * nrow(unmatched) / nrow(d)))
    print(head(unmatched[, .N, by = discipline][order(-N)], 8))
    cat("  (relays and non-registry events are expected; a common individual\n")
    cat("   event appearing here means match_event() needs an alias)\n")
  } else ok("every row matched an event")

  # --- physical plausibility of marks ----------------------------------------
  cat("\n  marks outside physical bounds:\n")
  m <- merge(d[!is.na(mark) & !is.na(event_id)],
             citius_events()[, .(event_id, family, orientation)], by = "event_id")
  # Deliberately generous: catching unit errors (a 6cm vault), not outliers.
  # Upper bounds must accommodate the SLOWEST legitimate competitor, not the
  # elite range: a first pass used sprint hi = 70s and flagged 462 perfectly
  # ordinary 400m runs of 70-78s. A bound tuned to elites turns the check into
  # an outlier detector, which is the failure mode this script exists to avoid.
  bounds <- data.table(
    family = c("sprint", "middle", "distance", "road", "hurdles", "walk",
               "jump", "throw", "combined"),
    lo = c(5, 90, 400, 1500, 10, 3000, 1, 5, 1000),
    hi = c(150, 1200, 3600, 40000, 300, 30000, 20, 110, 11000))
  m <- merge(m, bounds, by = "family", all.x = TRUE)
  bad <- m[!is.na(lo) & (mark < lo | mark > hi)]
  if (nrow(bad)) {
    flag("%s mark%s outside plausible bounds", format(nrow(bad), big.mark = ","),
         if (nrow(bad) == 1) "" else "s")
    print(head(bad[, .(event_id, mark_string, mark, lo, hi)][order(mark)], 8))
  } else ok("all %s marks within physical bounds", format(nrow(m), big.mark = ","))

  # --- race integrity --------------------------------------------------------
  cat("\n  race integrity:\n")
  spans <- d[!is.na(race_key), .(dates = uniqueN(date), rounds = uniqueN(round),
                                 events = uniqueN(event_id)), by = race_key]
  if (any(spans$dates > 1)) flag("%d race_key%s span more than one date",
                                 sum(spans$dates > 1), if (sum(spans$dates > 1) == 1) "" else "s")
  else ok("no race_key spans two dates")
  if (any(spans$events > 1)) flag("%d race_key%s span more than one event",
                                  sum(spans$events > 1), if (sum(spans$events > 1) == 1) "" else "s")
  else ok("no race_key spans two events")

  dup <- d[!is.na(place) & place > 0L, .(dups = sum(duplicated(place))), by = race_key]
  if (sum(dup$dups) > 0) {
    flag("%s duplicated placing%s across %d race%s (dead heats are real, but check)",
         format(sum(dup$dups), big.mark = ","), if (sum(dup$dups) == 1) "" else "s",
         sum(dup$dups > 0), if (sum(dup$dups > 0) == 1) "" else "s")
  } else ok("no duplicated placings")

  if (lane_check) {
    reg <- citius_events()
    lane <- reg$event_id[reg$family %in% c("sprint", "hurdles")]
    ln <- d[event_id %in% lane & !is.na(race_key), .N, by = race_key]
    over <- ln[N > 10]
    if (nrow(over)) flag("%d lane-event race%s hold >10 athletes (max %d) - pooled heats?",
                         nrow(over), if (nrow(over) == 1) "" else "s", max(over$N))
    else ok("no lane-event race exceeds 10 athletes (max %d)",
            if (nrow(ln)) max(ln$N) else 0L)
  }

  # --- duplicates ------------------------------------------------------------
  cat("\n  duplication:\n")
  ident <- d[, .N, by = .(race_key, athlete_id)][N > 1]
  if (nrow(ident)) flag("%s athlete%s appear twice in the same race",
                        format(nrow(ident), big.mark = ","),
                        if (nrow(ident) == 1) "" else "s")
  else ok("no athlete appears twice in one race")

  # --- ages ------------------------------------------------------------------
  if ("age" %in% names(d)) {
    a <- d[!is.na(age)]
    weird <- a[age < 10 | age > 70]
    cat("\n  ages:\n")
    cat(sprintf("    range %.1f to %.1f | median %.1f | %.1f%% known\n",
                min(a$age), max(a$age), median(a$age), 100 * nrow(a) / nrow(d)))
    if (nrow(weird)) flag("%s age%s outside 10-70", format(nrow(weird), big.mark = ","),
                          if (nrow(weird) == 1) "" else "s")
    else ok("all ages within 10-70")
  }
  invisible(NULL)
}

champs <- readRDS(file.path(OUT, "championship_results.rds"))
check_corpus(champs, "Athletics — championship_results.rds")

sw <- file.path(OUT, "swimming_history.rds")
if (file.exists(sw)) check_corpus(readRDS(sw), "Swimming — swimming_history.rds",
                                  lane_check = FALSE)

cli::cli_h2("Summary")
if (problems == 0L) {
  cli::cli_alert_success("No problems found.")
} else {
  cli::cli_alert_danger("{problems} check{?s} flagged - see [!] above.")
}
