# Fetch the major championships that are in the feed and were never harvested.
#
# Found 2026-07-31 by comparing ath_competitions.rds against what
# championship_results.rds actually holds. Coverage by feed tier:
#
#   F (club)  555 in feed, 538 harvested   96.9%
#   D          87            85            97.7%
#   GW         83            79            95.2%
#   OW          6             3            50.0%   <-- Olympics and Worlds
#
# We harvested 97% of the club meets and half of the majors. Every missing one
# reports has_results = TRUE, so this is an unfetched gap, not a data limit.
#
# It matters because the target population is tiny: the test period holds 86
# Olympic/World final races, which is far too few to resolve the effect sizes
# the arms have been producing. These competitions roughly double it.
#
# Writes to a SEPARATE file. Merging into championship_results.rds changes a
# shared input that calibrations, stores and every backtest arm read, so that
# step is deliberate and manual -- see the end of this script.
#
# Usage:  Rscript scripts/harvest_missing_majors.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_majors")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

# The base discovery list is a KEYWORD SWEEP, not the feed's catalogue, so it
# can only contain meets someone thought to search for. Probe worldathletics.org's
# own calendar endpoint (athletics_calendar(), citius/R/source_athletics_calendar.R)
# for the major names too, and union the two -- NOT the wrapper's
# athletics_find_competition() any more. Confirmed 2026-08-31 on the actual
# probe terms below: the calendar-endpoint probe is a STRICT SUPERSET of the
# old wrapper probe (0 lost, +198 gained on this exact term set) -- every
# competition the old method found, the new one found too, plus more. The
# extra hits are mostly minor meets (school/inter-city championships,
# "F"/"E"/"B" ranking_category) that the MAJOR/NOT regex below already
# filters out, same as it always has -- this is a pure discovery upgrade,
# not a change to what counts as a "major".
cc <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))
if (!nrow(cc)) cli::cli_abort("ath_competitions.rds loaded 0 rows.")
cli::cli_alert_info("ath_competitions.rds: {nrow(cc)} row{?s}, {min(cc$start, na.rm = TRUE)}..{max(cc$start, na.rm = TRUE)}")
probe_terms <- c("World Championships in Athletics", "IAAF World Championships",
                 "Olympic Games", "Commonwealth Games", "World Indoor Championships")
# athletics_calendar() caps at 100 rows/page and does not paginate itself --
# for a broad, decades-spanning term like "Olympic Games" the true hit count
# can exceed that once every Trials/Qualifier/Youth variant that substring-
# matches is counted, silently dropping the overflow with no warning. Use
# athletics_calendar_all() (it loops on offset until `complete`) and check
# both fetch_ok (a WAF/bot-detection block returns HTTP 200 with no real
# payload -- same zero-row shape as a genuine zero-hit term, see that
# function's own docs) and `complete` per term, warning rather than silently
# treating either failure mode as "this term found nothing." Found in review
# 2026-08-31, the day after the identical fetch_ok gap was fixed in the
# functions themselves.
probed <- rbindlist(lapply(probe_terms, function(t) {
  r <- tryCatch(athletics_calendar_all(query = t), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  if (!isTRUE(attr(r, "fetch_ok") %||% TRUE)) {
    cli::cli_warn("Probe term {.val {t}}: fetch unconfirmed (WAF/interstitial?) -- results may be incomplete, not a genuine zero-hit.")
  }
  if (!isTRUE(attr(r, "complete"))) {
    cli::cli_warn("Probe term {.val {t}}: pagination did not complete -- results may be truncated.")
  }
  r
}), fill = TRUE)
if (nrow(probed)) {
  keepc <- intersect(names(cc), names(probed))
  # athletics_calendar()'s start_date maps onto cc's start column when the
  # two tables are unioned below -- rename before the rbind, not after, so
  # `keepc` (computed from names BEFORE this rename) still lines both frames
  # up correctly rather than silently dropping the date column on one side.
  if ("start_date" %in% names(probed) && "start" %in% names(cc) && !"start" %in% keepc) {
    setnames(probed, "start_date", "start")
    keepc <- intersect(names(cc), names(probed))
  }
  # An unparseable id would otherwise collapse every such row into one
  # NA-keyed group at the dedup below -- the same bug shape fixed in
  # athletics_calendar_all() itself the day before (2026-08-30). Drop, don't
  # merge, before the dedup.
  probed <- probed[!is.na(competition_id)]
  cc <- unique(rbind(cc, probed[, ..keepc], fill = TRUE), by = "competition_id")
  cli::cli_alert_info("Feed probe added {nrow(probed)} row{?s}; list now {nrow(cc)} competitions.")
}
if (!"has_results" %in% names(cc)) cc[, has_results := TRUE]
cc[is.na(has_results), has_results := TRUE]
ch <- tryCatch(
  with_citius_db_connection(function(conn) load_championship_results(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(ch) || !nrow(ch)) ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
have <- unique(ch$competition_id)

# The senior global championships, and nothing that merely mentions one.
# NAMES CHANGE. The body was the IAAF until 2019, so the 2017 London and 2019
# Doha World Championships are "IAAF World Championships in Athletics" and were
# missed by the first version of this pattern, which only knew the modern name.
# A coverage check that assumes one naming convention finds only the meets named
# the way you expected.
MAJOR <- paste0("Olympic Games|XXX+ Olympic|Games of the [IVX]+ Olympiad|",
                "World Athletics Championships|IAAF World Championships|",
                "World Championships in Athletics|",
                "Commonwealth Games|",
                "World Athletics Indoor Championships|IAAF World Indoor|",
                "World Indoor Championships")
NOT   <- "Trials|Qualifier|Qualifying|Anniversary|Open Meeting|Selection|Throwing|Youth|U20|Junior"
want <- cc[grepl(MAJOR, name, ignore.case = TRUE, perl = TRUE) &
             !grepl(NOT, name, ignore.case = TRUE, perl = TRUE) &
             has_results == TRUE]
miss <- want[!competition_id %in% have]
cli::cli_h2("Majors in the feed: {nrow(want)} | already harvested: {sum(want$competition_id %in% have)} | missing: {nrow(miss)}")
print(miss[order(start), .(competition_id, name = substr(name, 1, 48),
                           start = substr(start, 1, 10), tier)])
if (!nrow(miss)) { cli::cli_alert_success("Nothing missing."); quit(save = "no") }

got <- list()
for (i in seq_len(nrow(miss))) {
  cid <- miss$competition_id[i]
  f <- file.path(CACHE, paste0(cid, ".rds"))
  if (file.exists(f)) {
    got[[length(got) + 1L]] <- readRDS(f)
    cli::cli_alert_info("{cid} cached.")
    next
  }
  cli::cli_alert("Fetching {i}/{nrow(miss)}: {miss$name[i]}")
  r <- tryCatch(setDT(athletics_competition_results(cid)),
                error = function(e) { cli::cli_alert_warning("failed: {conditionMessage(e)}"); NULL })
  if (is.null(r) || !nrow(r)) { cli::cli_alert_warning("{cid}: no results returned."); next }
  saveRDS(r, f)
  cli::cli_alert_success("{cid}: {nrow(r)} results, {uniqueN(r$race_key)} races.")
  got[[length(got) + 1L]] <- r
}
if (!length(got)) { cli::cli_alert_warning("Nothing fetched."); quit(save = "no") }

new <- rbindlist(got, fill = TRUE)
saveRDS(new, file.path(OUT, "championship_results_majors.rds"))
cli::cli_h2("Fetched {format(nrow(new), big.mark=',')} results across {uniqueN(new$competition_id)} competition{?s}")
print(new[, .(results = .N, races = uniqueN(race_key), athletes = uniqueN(athlete_id),
              finals = uniqueN(race_key[grepl("final", round, ignore.case = TRUE) &
                                          !grepl("semi", round, ignore.case = TRUE)])),
          by = .(competition_id)][order(-results)])

# TO MERGE (deliberate, changes a shared input every arm reads): do NOT
# hand-roll this. `unique(ch, by = c('competition_id','race_key','athlete_id'))`
# looks like a dedup but is NOT unique for multi-attempt field events (three
# throws by one athlete in one race collapse to one row) -- this exact snippet,
# copy-pasted as printed instructions, silently dropped 21,440 rows from
# championship_results.rds on 2026-08-29 before being caught. The correct
# pattern -- drop whole COMPETITIONS already present, never row-level dedup,
# assert the row count after -- lives in merge_referenced.R.
#
# merge_referenced.R defaults to championship_results_referenced.rds, which
# this script does not write -- pointing it at this file by name alone (an
# earlier version of this instruction) fails closed at merge_referenced.R's
# own file.exists() check and merges nothing. Use CITIUS_MERGE_INPUT to
# redirect it instead (added 2026-09-02 for exactly this case).
cat("\nTO MERGE: CITIUS_MERGE_INPUT=championship_results_majors.rds Rscript scripts/merge_referenced.R\n")
cat("  (do not hand-roll a dedup here -- see comment above).\n")
cat("  then: build_athletics_corpus.R, build_stores.R, recalibrate_corpus.R\n")
