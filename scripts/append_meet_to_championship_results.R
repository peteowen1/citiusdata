# Append ONE completed meet's results to championship_results.rds, and
# rebuild the derived athletics_corpus.rds from it.
#
# WHY THIS EXISTS. append_meet_to_store.R got a meet into the LIVE FORECAST
# path cheaply (fetch + one parquet write per event), but deliberately did not
# touch championship_results.rds or athletics_corpus.rds -- those are
# training/scoring inputs, and quietly adding a meet a calibration was not
# fitted on is how a model ends up evaluated on its own training data.
#
# This is the other half: get the same meet into the files that
# fit_event_params.R, backtest_athletics.R (full-corpus mode) and any future
# calibration refit actually read. Checked 2026-09-09 rather than assumed:
# the honest cost is NOT "a full re-discovery, a rate-limited harvest and two
# heavy rebuilds" -- that framing was written for the case of catching up
# EVERY competition since the corpus's last harvest date, which is a much
# bigger job. For a single meet whose results are already fetched (as
# append_meet_to_store.R already did), the actual cost is: load
# championship_results.rds (~25s, 4.5M rows), fill three competition-level
# columns the per-result fetch does not carry (see below), rbind, save
# (~similar), then re-run build_athletics_corpus.R to regenerate the derived
# union. Minutes, not hours.
#
# THE THREE MISSING COLUMNS. athletics_competition_results() returns every
# column championship_results.rds needs except comp_name, comp_start and
# comp_tier -- competition-level metadata, constant across every row of one
# meet, not carried by a per-result fetch:
#   comp_name   the meet's display name. Convention checked against existing
#               DF-tier rows (Weltklasse Zurich, Prefontaine Classic): a real
#               event name, sourced from athletics_calendar.csv, and NA is an
#               accepted value (2 of 8 existing DF rows carry it).
#   comp_start  the meet's first day. From the calendar's date_start.
#   comp_tier   left NA. Measured 2026-09-09: 96.0% of existing rows
#               (4,362,554 of 4,544,586) already carry NA here -- it is a
#               mostly-unpopulated secondary field, distinct from the
#               per-result `tier` column this script (and the store append)
#               already populate correctly. NA matches the norm, not a gap.
#
# WHAT THIS DOES NOT DO. It does not touch the partitioned store --
# append_meet_to_store.R already got this meet into the live forecast path,
# and rebuilding the store from the refreshed corpus is a separate, heavier
# decision (build_stores.R repartitions the whole corpus across every event),
# not needed just to get a meet into training data.
#
# Usage:
#   powershell -Command 'Rscript citiusdata/scripts/append_meet_to_championship_results.R <competition_id>'
#   CITIUS_APPEND_DRYRUN=1 to validate without writing.
#   CITIUS_APPEND_SKIP_CORPUS_REBUILD=1 to append to championship_results.rds
#     only, skipping the build_athletics_corpus.R re-run (e.g. to batch
#     several meets' appends before paying that cost once).

VERSE <- here::here()
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

args <- commandArgs(trailingOnly = TRUE)
CID <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_APPEND_COMP", "")
if (!nzchar(CID)) cli::cli_abort("Give a competition_id, e.g. {.code Rscript append_meet_to_championship_results.R 7214029}")
DRY <- nzchar(Sys.getenv("CITIUS_APPEND_DRYRUN", ""))
SKIP_CORPUS <- nzchar(Sys.getenv("CITIUS_APPEND_SKIP_CORPUS_REBUILD", ""))

# --- what championship_results.rds already holds ----------------------------
CH_F <- file.path(D, "championship_results.rds")
say("loading championship_results.rds ...")
t0 <- Sys.time()
ch <- readRDS(CH_F)
say("loaded %s rows in %.0fs", format(nrow(ch), big.mark = ","),
    as.numeric(difftime(Sys.time(), t0, units = "secs")))
already <- ch[competition_id == as.integer(CID)]
if (nrow(already)) cli::cli_abort(c(
  "x" = "competition {CID} already has {nrow(already)} row{?s} in championship_results.rds.",
  "i" = "Appending again would double-count every result."))
say("competition %s is not yet in championship_results.rds -- safe to append", CID)

# --- fetch (reuses an already-saved raw fetch if one exists) -----------------
RAW_F <- file.path(D, sprintf("meet_%s_raw_results.rds", CID))
brussels_alias <- file.path(D, "brussels2026_raw_results.rds")
if (file.exists(RAW_F)) {
  say("reusing cached fetch %s", basename(RAW_F))
  r <- as.data.table(readRDS(RAW_F))
} else if (identical(CID, "7214029") && file.exists(brussels_alias)) {
  say("reusing this session's earlier fetch %s", basename(brussels_alias))
  r <- as.data.table(readRDS(brussels_alias))
} else {
  suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
  say("fetching %s ...", CID)
  r <- as.data.table(athletics_competition_results(as.integer(CID)))
  saveRDS(r, RAW_F)
}
if (!nrow(r)) cli::cli_abort("The results endpoint returned nothing for {CID}; it may not be published yet.")
say("%s rows, %d events, %d athletes", format(nrow(r), big.mark = ","),
    uniqueN(r$event_id), uniqueN(r$athlete_id))

# --- fill the three competition-level columns the per-result fetch lacks ----
cal <- fread(file.path(D, "athletics_calendar.csv"))
crow <- cal[wa_competition_id == as.integer(CID)]
if (nrow(crow)) {
  r[, comp_name := crow$name[1]]
  r[, comp_start := as.Date(crow$date_start[1])]
  say("comp_name/comp_start from athletics_calendar.csv: %s, %s", crow$name[1], format(crow$date_start[1]))
} else {
  r[, comp_name := NA_character_]
  r[, comp_start := as.Date(NA)]
  say("competition %s not in athletics_calendar.csv -- comp_name/comp_start left NA (matches an accepted existing value)", CID)
}
r[, comp_tier := NA_character_]  # matches the 96.0%-NA norm; see header.

# --- validate columns match exactly, not just "enough of them" --------------
need <- names(ch)
missing <- setdiff(need, names(r))
if (length(missing)) cli::cli_abort(
  "Fetch is missing column{?s} {.field {missing}} that championship_results.rds needs -- fix the mapping, do not fabricate them silently.")
out <- r[, ..need]
extra_cov <- vapply(out, function(x) mean(!is.na(x)), numeric(1))
say("column fill rates for the new rows (0%% on a column that is normally populated is a mapping bug):")
print(round(sort(extra_cov), 3))
# PRINTING THE COVERAGE IS NOT THE SAME AS ASSERTING IT. This printed the
# smoking gun and kept going -- exactly the "assert coverage, not presence"
# rule this repo has already been bitten by, and a straight regression from
# append_meet_to_store.R's own gate, which this script otherwise mirrors.
# comp_name/comp_tier are exempt because they are legitimately NA for some
# tiers/rows even when the mapping is correct -- see the header.
zero <- names(extra_cov)[extra_cov == 0 & !(names(extra_cov) %in% c("comp_name", "comp_tier"))]
if (length(zero)) cli::cli_abort(
  "column{?s} 100%% empty after mapping: {.field {zero}} -- fix the mapping rather than writing an empty column.")

if (DRY) { say("DRY RUN -- nothing written"); quit(status = 0) }

# --- append and save ----------------------------------------------------------
n_before <- nrow(ch)
ch2 <- rbind(ch, out, use.names = TRUE)
say("appended: %s -> %s rows", format(n_before, big.mark = ","), format(nrow(ch2), big.mark = ","))
saveRDS(ch2, CH_F)
say("saved championship_results.rds")

# --- verify by re-reading -----------------------------------------------------
back <- readRDS(CH_F)
back_new <- back[competition_id == as.integer(CID)]
if (nrow(back_new) != nrow(out)) cli::cli_abort(
  "wrote {nrow(out)} rows but read back {nrow(back_new)} -- the append did not land cleanly.")
say("verified: %s rows for competition %s re-read cleanly. championship_results.rds is %s rows.",
    nrow(back_new), CID, format(nrow(back), big.mark = ","))

# --- regenerate the derived corpus -------------------------------------------
if (SKIP_CORPUS) {
  say("CITIUS_APPEND_SKIP_CORPUS_REBUILD set -- athletics_corpus.rds NOT regenerated. Run build_athletics_corpus.R before anything reads it.")
} else {
  say("regenerating athletics_corpus.rds via build_athletics_corpus.R ...")
  t1 <- Sys.time()
  rc <- system2("Rscript", shQuote(file.path(VERSE, "citiusdata", "scripts", "build_athletics_corpus.R")),
                stdout = "", stderr = "")
  say("build_athletics_corpus.R finished in %.1f min, exit %s",
      as.numeric(difftime(Sys.time(), t1, units = "mins")), rc)
  if (!identical(rc, 0L)) cli::cli_abort("build_athletics_corpus.R failed (exit {rc}) -- championship_results.rds was updated but athletics_corpus.rds was NOT regenerated.")
}
say("Done. Note: the partitioned store was NOT touched -- it already has this meet via append_meet_to_store.R.")
