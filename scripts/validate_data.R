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
  # Bounds must span the SHORTEST and LONGEST event in each family, and the
  # slowest legitimate competitor — not the elite range. Two ways this has gone
  # wrong already:
  #   - tuned to elites: sprint hi = 70s flagged 462 ordinary 70-78s 400m runs,
  #     turning the check into the outlier detector it exists to replace;
  #   - stale after the registry grew from 80 to 122 events: a 600m (~72s) trips
  #     a 90s middle floor and a 5km walk (~1200s) trips a 3000s walk floor,
  #     flagging 8,666 perfectly good marks.
  # When the registry gains events, revisit this table.
  bounds <- data.table(
    family   = c("sprint", "middle", "distance", "road", "hurdles", "walk",
                 "jump", "throw", "combined"),
    # shortest: 60m ~6s | 600m ~72s | 2000m ~290s | 5km ~750s | 60mH ~7s | 3km walk ~600s
    lo       = c(5,        60,       280,         700,     6,         550,
                 0.5,      1,        1000),
    # longest: 400m slow | Mile slow | 10,000m slow | Marathon slow | 400mH slow | 50km walk
    hi       = c(150,      1200,     3600,        40000,   300,       30000,
                 20,       110,      11000))
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
  # A "count the violations, pass if zero" check passes VACUOUSLY on an empty
  # column: nrow(weird) is 0 because there is nothing to be weird. This printed
  # "ok all ages within 10-70" over swimming_history.rds on 2026-09-03, where
  # `age` is 0% populated and the real column is `age_at_result` (97%, range
  # 10-53) -- the same column-name mismatch that left comp_name and sex empty.
  # So check COVERAGE before checking values, and treat an empty column as a
  # failure rather than a clean pass.
  if ("age" %in% names(d)) {
    a <- d[!is.na(age)]
    cat("\n  ages:\n")
    if (!nrow(a)) {
      alt <- grep("^age|age$|birth|dob", setdiff(names(d), "age"), value = TRUE, ignore.case = TRUE)
      flag("age is 0%% populated -- nothing to validate%s",
           if (length(alt)) sprintf(" (is the real column %s?)", paste(alt, collapse = "/")) else "")
    } else {
      weird <- a[age < 10 | age > 70]
      cat(sprintf("    range %.1f to %.1f | median %.1f | %.1f%% known\n",
                  min(a$age), max(a$age), median(a$age), 100 * nrow(a) / nrow(d)))
      if (nrow(weird)) flag("%s age%s outside 10-70", format(nrow(weird), big.mark = ","),
                            if (nrow(weird) == 1) "" else "s")
      else ok("all ages within 10-70 (%.1f%% of rows carry one)", 100 * nrow(a) / nrow(d))
    }
  }
  invisible(NULL)
}

# Parameterised so a freshly harvested file can be validated BEFORE it is
# promoted over the live one. Hardcoding the path meant an attempt to validate
# a new harvest silently re-validated the old file instead.
ATH <- Sys.getenv("CITIUS_VALIDATE_ATH", "championship_results.rds")
champs <- readRDS(file.path(OUT, ATH))
check_corpus(champs, paste("Athletics —", ATH))

sw <- file.path(OUT, "swimming_history.rds")
if (file.exists(sw)) check_corpus(readRDS(sw), "Swimming — swimming_history.rds",
                                  lane_check = FALSE)

# ---------------------------------------------------------------------------
# INTEGRITY OF THE DATA LAYER ITSELF
#
# The checks above test whether a MARK is plausible. These test whether the
# TABLE is intact -- did the right rows join, are the columns populated, are the
# keys unique, do the stores agree. Added 2026-09-03 after four defects that
# every existing check passed:
#
#   comp_name  present, correctly typed, 0% populated on all 6.4M corpus rows
#              for months -- a `competition` vs `comp_name` mismatch that the
#              union turned into a fabricated NA column
#   sex        the same bug again (`sex_code`), 52% populated, unnoticed
#   meet_tier  407 meets in a tier their own class rule forbids, because the
#              consistency pass covered .K1/.K3/road_race and not KNOWN_T2
#   duckdb     silently stale for a day because the installed citius predated
#              the function the write-through called
#
# The common thread: every one had the right row count, the right column names
# and the right types. Counting rows is not checking values.
cli::cli_h2("Data layer integrity")

.fill <- function(v) mean(!is.na(v) & (!is.character(v) | nzchar(trimws(v))))

# 1. FILL RATES. A column that is 100% empty is a lost join, not a sparse field.
#    Floors are deliberately loose -- this catches collapse, not drift.
FLOOR <- c(athlete_id = 0.99, date = 0.99, discipline = 0.95, place = 0.90,
           comp_name = 0.20, sex = 0.90, event_id = 0.80, mark = 0.90)
cp <- file.path(OUT, "athletics_corpus.parquet")
if (file.exists(cp)) {
  # Read the whole 7.5M-row table and this check takes over ten minutes, which
  # means it stops being run. Take the schema and row count from parquet
  # metadata (instant), and read only the columns actually being judged.
  ds  <- arrow::open_dataset(cp)
  all_cols <- names(ds$schema)
  n_rows   <- ds$num_rows
  cat(sprintf("  athletics_corpus.parquet: %s rows, %d cols\n",
              format(n_rows, big.mark = ","), length(all_cols)))
  co <- setDT(arrow::read_parquet(cp, col_select = dplyr::all_of(
    intersect(unique(c(names(FLOOR), "competition_id")), all_cols))))
  for (n in names(co)) {
    f <- .fill(co[[n]])
    if (f == 0) flag("column %s is 100%% EMPTY -- a source column was renamed, mistyped or dropped", n)
    else if (n %in% names(FLOOR) && f < FLOOR[n])
      flag("column %s only %.1f%% populated (floor %.0f%%)", n, 100 * f, 100 * FLOOR[n])
  }
  if (!any(vapply(co, function(v) .fill(v) == 0, logical(1)))) ok("no checked column is entirely empty")

  # 2. REFERENTIAL INTEGRITY. Every competition_id must resolve to a catalogue
  #    row, or the inner join in form_ratings.R deletes those rows silently --
  #    indistinguishable from the deliberate T3 exclusion.
  ctp <- file.path(OUT, "competition_catalogue.parquet")
  if (file.exists(ctp)) {
    ct <- setDT(arrow::read_parquet(ctp))
    ct[, competition_id := as.character(competition_id)]
    co[, competition_id := as.character(competition_id)]
    orphan_ids <- setdiff(unique(co[!is.na(competition_id)]$competition_id), ct$competition_id)
    if (length(orphan_ids))
      flag("%s competition_id(s) in the corpus have no catalogue row", format(length(orphan_ids), big.mark = ","))
    else ok("every corpus competition_id resolves to a catalogue row")
    n_null <- co[is.na(competition_id), .N]
    cat(sprintf("  (%s rows carry no competition_id at all -- career-route results the\n   API returns with competitionId 0; they can never join and are excluded\n   by the same inner join as T3)\n",
                format(n_null, big.mark = ",")))

    # 3. KEY UNIQUENESS on the catalogue.
    if (any(duplicated(ct$competition_id)))
      flag("catalogue has %d duplicate competition_id(s)", sum(duplicated(ct$competition_id)))
    else ok("catalogue competition_id is unique (%s rows)", format(nrow(ct), big.mark = ","))

    # 4. TIER RULES HONOURED. Mirrors build_competition_catalogue.R's fcase.
    #    This is the check whose KNOWN_T2 gap hid 407 violations -- and this
    #    copy of K2 was ITSELF missing the four continental-games classes
    #    (asian_games/african_games/panam_games/european_games) that fix
    #    added, found by review 2026-09-04. The comment above names the exact
    #    bug this check exists to catch while its own list drifted out of sync
    #    with the source of truth (build_competition_catalogue.R,
    #    augment_catalogue_coverage.R, apply_strength_ew.R all agree; this file
    #    was the one holdout). Same failure family as everything else this
    #    week: a duplicated list drifts, and nothing enforces the two copies
    #    agree.
    K1 <- c("olympics","world_champs","commonwealth","world_indoor","diamond_league",
            "world_other","indoor_tour","european_champs")
    K2 <- c("continental","national_champs","ncaa","team_champs","continental_tour",
            "regional_games","asian_games","african_games","panam_games","european_games")
    K3 <- c("age_group","club_meet","ncaa_lower","team_champs_lower")
    for (spec in list(list(K1, "T1_elite"), list(K2, "T2_strong"), list(K3, "T3_development"))) {
      pop <- ct[class %chin% spec[[1]]]
      # Population-nonzero guard: without it, a band with ZERO meets (e.g. a
      # class name typo, or a live class silently renamed upstream) reports
      # NOTHING -- indistinguishable from "checked, no violations found." A
      # run can look clean specifically because the classification broke.
      if (!nrow(pop)) { flag("no meets found in any %s class -- classification may be broken", spec[[2]]); next }
      bad <- pop[meet_tier != spec[[2]]]
      if (nrow(bad)) flag("%d meet(s) in a %s class are not %s (e.g. %s)", nrow(bad),
                          spec[[2]], spec[[2]], bad[order(-athletes)][1]$comp_name)
      else ok("all %d %s-class meets are %s", nrow(pop), spec[[2]], spec[[2]])
    }
    unnamed <- ct[is.na(comp_name) | !nzchar(trimws(comp_name))]
    if (nrow(unnamed))
      flag("%d catalogue meet(s) have no name -- cat_of() cannot classify them, so they fall to strength banding", nrow(unnamed))
    else ok("every catalogue meet has a name")
  }
}

# 5. THE STORES AGREE. RDS, parquet and DuckDB are written by the same script;
#    a divergence means one write did not take. The DuckDB leg failed silently
#    for a day and build_stores.R fell back to RDS without failing.
if (file.exists(cp)) {
  np <- arrow::open_dataset(cp)$num_rows
  # DuckDB answers a COUNT(*) from its own metadata, so this is cheap and is the
  # check that would have caught the silently-stale DuckDB.
  nd <- tryCatch(citius::with_citius_db_connection(function(conn)
          DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM athletics_corpus")$n),
        error = function(e) NA_integer_)
  if (is.na(nd)) flag("could not read athletics_corpus from citius.duckdb -- the DuckDB leg may be failing silently")
  else if (nd != np) flag("citius.duckdb has %s rows vs parquet %s -- one write did not take", format(nd, big.mark=","), format(np, big.mark=","))
  else ok("citius.duckdb agrees with parquet (%s rows)", format(nd, big.mark = ","))

  # The RDS leg needs a full 287MB deserialise to count rows, which is the one
  # genuinely slow check here. Opt in when validating a migration; skip by
  # default so this suite stays fast enough to actually get run.
  if (nzchar(Sys.getenv("CITIUS_VALIDATE_RDS"))) {
    rp <- file.path(OUT, "athletics_corpus.rds")
    if (file.exists(rp)) {
      nr <- nrow(readRDS(rp))
      if (nr != np) flag("athletics_corpus rds %s rows vs parquet %s", format(nr, big.mark=","), format(np, big.mark=","))
      else ok("rds agrees with parquet (%s rows)", format(nr, big.mark = ","))
    }
  } else cat("  (rds row-count check skipped -- set CITIUS_VALIDATE_RDS=1 to include it)\n")
}

cli::cli_h2("Summary")
if (problems == 0L) {
  cli::cli_alert_success("No problems found.")
} else {
  cli::cli_alert_danger("{problems} check{?s} flagged - see [!] above.")
}
