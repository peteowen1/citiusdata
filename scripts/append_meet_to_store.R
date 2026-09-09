# Append ONE completed meet's results to the partitioned corpus store.
#
# WHY THIS EXISTS. Getting a single new meet into the forecasting path used to
# be framed as "re-discover competitions, rebuild the corpus, rebuild the store"
# -- hours of work with two heavy rebuilds. Almost none of that is necessary:
#
#   * DISCOVERY is only how competition IDs are FOUND by name. The calendar
#     already carries `wa_competition_id`, so a known meet needs no discovery.
#   * athletics_competition_results() returns rows already in corpus shape
#     (perf, orientation, mark, age, race_key all computed). Brussels took 10s.
#   * The PREDICTION path reads the partitioned store, not athletics_corpus.rds
#     (deployed_history() -> read_results_store()). A partitioned dataset is
#     designed to be extended by writing another file into a partition.
#
# So the honest cost of adding a meet is one fetch plus one parquet write per
# affected event. Minutes, not hours.
#
# WHAT THIS DOES NOT DO. It does not touch athletics_corpus.rds or
# championship_results.rds, so anything reading those (backtests, the marks lab,
# calibration refits) will NOT see this meet. That is deliberate: those are
# training/scoring inputs, and quietly adding a meet a calibration was not
# fitted on is how a model ends up evaluated on its own training data. This is
# for getting recent form into a FORECAST.
#
# meet_tier is assigned from the feed's per-race `tier` using the documented WAC
# mapping (OW/DF/GW/GL -> T1_elite, A/B/C/D -> T2_strong, E/F -> T3_development),
# because the catalogue only covers COMPLETED-and-harvested meets and will not
# carry a meet this fresh. Every DF meet already in the catalogue is T1_elite,
# so the mapping agrees with it.
#
# Usage:
#   powershell -Command 'Rscript citiusdata/scripts/append_meet_to_store.R <competition_id>'
#   CITIUS_APPEND_DRYRUN=1 to fetch and validate without writing.

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
D <- file.path(VERSE, "citiusdata", "data")
STORE <- file.path(D, "athletics_corpus_store")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

args <- commandArgs(trailingOnly = TRUE)
CID <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_APPEND_COMP", "")
if (!nzchar(CID)) cli::cli_abort("Give a competition_id, e.g. {.code Rscript append_meet_to_store.R 7214029}")
DRY <- nzchar(Sys.getenv("CITIUS_APPEND_DRYRUN", ""))

# --- what the store already holds -------------------------------------------
ds <- open_dataset(STORE)
store_cols <- names(ds)
say("store has %d partitions, %d columns", length(list.dirs(STORE, recursive = FALSE)), length(store_cols))

already <- as.data.table(ds |> dplyr::filter(competition_id == CID) |>
                           dplyr::select(competition_id) |> dplyr::collect())
if (nrow(already)) cli::cli_abort(c(
  "x" = "competition {CID} already has {nrow(already)} row{?s} in the store.",
  "i" = "Appending again would double-count every result. Remove them first if this is a deliberate re-append."))
say("competition %s is not yet in the store -- safe to append", CID)

# --- fetch -------------------------------------------------------------------
say("fetching %s ...", CID)
r <- as.data.table(athletics_competition_results(as.integer(CID)))
if (!nrow(r)) cli::cli_abort("The results endpoint returned nothing for {CID}; it may not be published yet.")
say("%s rows, %d events, %d athletes, %s to %s", format(nrow(r), big.mark = ","),
    uniqueN(r$event_id), uniqueN(r$athlete_id),
    format(min(as.Date(r$date), na.rm = TRUE)), format(max(as.Date(r$date), na.rm = TRUE)))

# --- meet_tier from the feed's tier, per the WAC mapping ---------------------
wac <- function(t) {
  t <- toupper(trimws(as.character(t)))
  data.table::fcase(t %in% c("OW", "DF", "GW", "GL"), "T1_elite",
                    t %in% c("A", "B", "C", "D"),     "T2_strong",
                    t %in% c("E", "F"),               "T3_development",
                    default = NA_character_)
}
r[, meet_tier := wac(tier)]
say("meet_tier assigned: %s", paste(sprintf("%s=%d", names(table(r$meet_tier, useNA = "ifany")),
                                             as.integer(table(r$meet_tier, useNA = "ifany"))), collapse = ", "))

# --- shape to the store's exact schema ---------------------------------------
# The feed names it sex_code and the store names it sex; values already agree
# (M/W). Renamed rather than NA-filled -- letting it fall through to the
# missing-column branch below would silently ship a 100% empty sex column, and
# .tier_class_of()/the event registry both read it.
if (!"sex" %in% names(r) && "sex_code" %in% names(r)) r[, sex := sex_code]
if (!"comp_name" %in% names(r)) r[, comp_name := NA_character_]

# Rows the registry could not resolve to an event carry NA event_id and cannot
# be modelled -- for Brussels these were 3 rows of a U18 4x100 relay. Dropped
# with a count rather than silently, because a large number here would mean the
# registry is missing something real rather than just ignoring relays.
n_noev <- sum(is.na(r$event_id))
if (n_noev) {
  say("dropping %d row%s with no registry event_id: %s", n_noev, if (n_noev==1) "" else "s",
      paste(unique(r[is.na(event_id)]$discipline), collapse = ", "))
  if (n_noev > 0.1 * nrow(r)) cli::cli_abort(
    "{n_noev} of {nrow(r)} rows have no event_id (>10%) -- that is a registry gap, not relays.")
  r <- r[!is.na(event_id)]
}
missing <- setdiff(store_cols, names(r))
if (length(missing)) {
  # Say it out loud rather than NA-filling silently: a column fabricated here is
  # exactly the union bug this repo has already been bitten by.
  say("columns the store has and the fetch does not, filled NA: %s", paste(missing, collapse = ", "))
  for (m in missing) set(r, j = m, value = NA)
}
out <- r[, ..store_cols]

# CAST TO THE STORE'S OWN SCHEMA. `athletics_competition_results()` returns
# `competition_id` as integer; the store's own type (set the first time
# anything wrote a competition_id column) is string. write_parquet() does not
# error on that -- it just writes an Int32 file next to Utf8 files in the same
# partition. A plain read (deployed_history(), backtest_athletics.R) silently
# re-types the WHOLE column across every file rather than failing, so the
# corruption is invisible until something filters on competition_id
# specifically -- this script's OWN verification step below does exactly that,
# which is what caught it: it throws "Expression not supported in Arrow" on a
# type-mixed partition instead of confirming the append.
#
# Cast every column generically, not just this one, so a future column this
# script hands NA-filled or type-mismatched values for gets the same
# protection rather than a second hand-fix.
ds_types <- setNames(vapply(store_cols, function(cn) ds$schema$GetFieldByName(cn)$type$ToString(),
                             character(1)), store_cols)
for (cn in store_cols) {
  want <- ds_types[[cn]]
  have <- out[[cn]]
  if (grepl("^string|^utf8", want, ignore.case = TRUE) && !is.character(have)) {
    set(out, j = cn, value = as.character(have))
  } else if (grepl("^double|^float", want, ignore.case = TRUE) && !is.double(have)) {
    set(out, j = cn, value = as.double(have))
  } else if (grepl("^int", want, ignore.case = TRUE) && !is.integer(have)) {
    set(out, j = cn, value = as.integer(have))
  } else if (grepl("^bool", want, ignore.case = TRUE) && !is.logical(have)) {
    set(out, j = cn, value = as.logical(have))
  }
}

# --- validate BEFORE writing --------------------------------------------------
stopifnot("perf must be finite"      = all(is.finite(out$perf) | is.na(out$perf)),
          "event_id must be present" = !anyNA(out$event_id),
          "date must be present"     = !anyNA(out$date))
cov <- vapply(out, function(x) mean(!is.na(x)), numeric(1))
say("column fill rates (any 0%% is a mapping bug, not a data gap):")
print(round(sort(cov), 3))
still_mismatched <- store_cols[vapply(store_cols, function(cn) {
  want <- ds_types[[cn]]
  have <- out[[cn]]
  (grepl("^string|^utf8", want, ignore.case = TRUE) && !is.character(have)) ||
    (grepl("^double|^float", want, ignore.case = TRUE) && !is.double(have)) ||
    (grepl("^int", want, ignore.case = TRUE) && !is.integer(have)) ||
    (grepl("^bool", want, ignore.case = TRUE) && !is.logical(have))
}, logical(1))]
if (length(still_mismatched)) cli::cli_abort(
  "type cast failed for column{?s} {.field {still_mismatched}} -- writing this
   would silently corrupt the store schema. Fix the cast rule above.")
zero <- names(cov)[cov == 0 & !(names(cov) %in% c("comp_name"))]
if (length(zero)) cli::cli_abort(
  "column{?s} 100% empty after mapping: {.field {zero}} -- fix the mapping rather than writing an empty column.")

if (DRY) { say("DRY RUN -- nothing written"); quit(status = 0) }

# --- write one file per event partition --------------------------------------
tag <- paste0("append_", CID)
n <- 0L
for (ev in unique(out$event_id)) {
  part <- file.path(STORE, paste0("event_id=", ev))
  dir.create(part, showWarnings = FALSE, recursive = TRUE)
  chunk <- out[event_id == ev][, !"event_id"]   # partition key is the directory
  write_parquet(chunk, file.path(part, paste0(tag, ".parquet")))
  n <- n + nrow(chunk)
}
say("wrote %s rows across %d event partitions", format(n, big.mark = ","), uniqueN(out$event_id))

# --- verify by re-reading through the same path the model uses ---------------
ds2 <- open_dataset(STORE)
back <- as.data.table(ds2 |> dplyr::filter(competition_id == CID) |> dplyr::collect())
say("re-read from the store: %s rows, %d events", format(nrow(back), big.mark = ","), uniqueN(back$event_id))
if (nrow(back) != nrow(out)) cli::cli_abort(
  "wrote {nrow(out)} rows but read back {nrow(back)} -- the append did not land cleanly.")
say("row count matches. Append complete.")
