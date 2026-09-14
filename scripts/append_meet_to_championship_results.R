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

# --- map the direct harvest onto championship_results' shape ----------------
#
# harvest_wa_results.R returns the raw World Athletics shape. Everything below
# is either a rename, or a derivation stated explicitly. NOTHING is invented:
# a column the direct route genuinely does not carry is left NA and named in
# the exempt list at the bottom of this script, with the reason, rather than
# filled with a plausible-looking value. That distinction is the whole lesson
# of the tier episode -- a fabricated tier silently reweighted 469k corpus rows
# once already.
.map_harvest_to_championship <- function(h, cid, D) {
  reg <- as.data.table(citius_events()[, c("event_id", "orientation", "technical")])
  cal <- fread(file.path(D, "athletics_calendar.csv"))
  crow <- cal[wa_competition_id == as.integer(cid)]

  out <- data.table::copy(h)
  out[, competition_id := as.integer(cid)]
  # WA dates are "11 SEP 2026", not ISO. as.Date() without a format throws
  # "character string is not in a standard unambiguous format" -- and parsing
  # month abbreviations depends on the C locale, so it is forced rather than
  # inherited from whatever the runner happens to use.
  .wa_date <- function(x) {
    old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
    suppressWarnings(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))
    x <- as.character(x)
    d <- suppressWarnings(as.Date(x, format = "%d %b %Y"))
    # as.Date() with no format THROWS on an unrecognised string rather than
    # returning NA, so the ISO fallback has to be guarded or one odd value
    # kills the whole append.
    iso <- suppressWarnings(tryCatch(as.Date(x), error = function(e) rep(as.Date(NA), length(x))))
    data.table::fifelse(is.na(d), iso, d)
  }
  # Prefer the race's own date; fall back to the meet-day date from
  # options.days, which is the only source when WA returns a NULL race.date
  # (all of Budapest), and finally to comp_start + (day - 1).
  out[, date := .wa_date(date)]
  if ("day_date" %in% names(out)) out[is.na(date), date := .wa_date(day_date)]
  if (nrow(crow)) out[is.na(date) & !is.na(day),
                      date := as.Date(crow$date_start[1]) + (as.integer(day) - 1L)]
  out[, birthdate := .wa_date(birth_date)]
  out[, athlete_id := as.character(athlete_id)]
  out[, sport := "Athletics"]
  out[, sex_code := gender]
  out[, comp_day := suppressWarnings(as.integer(day))]
  # eventTitle is NULL on some meets (all of Budapest); fall back to the event
  # name itself rather than leaving it empty. Stored event_name is only 24.2%
  # populated, so sparse here is the norm, not a mapping bug.
  out[, event_name := data.table::fifelse(
    !is.na(event_title) & nzchar(as.character(event_title)),
    as.character(event_title), as.character(event))]
  out[, wind := suppressWarnings(as.numeric(wind))]

  # Wind legality is a property of the reading, never an assumption -- the
  # source adapter's own comment records that hardcoding TRUE once treated
  # 7,423 wind-aided marks as legal.
  out[, legal := is.na(wind) | wind <= 2.0]

  # VENUE, parsed from the one string the API gives:
  # "Nemzeti Atlétikai Központ, Budapest (HUN)" -> stadium, city, country.
  # Falls back to NA per part rather than guessing when the shape differs.
  v <- as.character(out$comp_venue)
  out[, venue_country := sub(".*\\(([A-Z]{3})\\)\\s*$", "\\1", v)]
  out[venue_country == v, venue_country := NA_character_]
  vb <- trimws(sub("\\s*\\([A-Z]{3}\\)\\s*$", "", v))
  out[, venue_city := ifelse(grepl(",", vb), trimws(sub(".*,\\s*", "", vb)), vb)]
  out[, venue_stadium := ifelse(grepl(",", vb), trimws(sub(",.*$", "", vb)), NA_character_)]

  # INDOOR from the competition name, which is how WA signals it, rather than
  # defaulting to FALSE. An outdoor September championship is FALSE because the
  # name says nothing about indoors, not because FALSE is convenient.
  out[, indoor := grepl("indoor", as.character(comp_name), ignore.case = TRUE)]

  out <- merge(out, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  setnames(out, "technical", "is_technical")
  # orientation MUST stay NA for an unmatched event: defaulting it to -1 gives a
  # wrong-SIGNED perf for field events, undoing the guarantee match_event() exists
  # to provide.
  out[, perf := to_perf(mark, orientation)]
  out[, age := as.numeric(date - birthdate) / 365.25]

  out[, comp_name := if (nrow(crow)) crow$name[1] else NA_character_]
  out[, comp_start := if (nrow(crow)) as.Date(crow$date_start[1]) else as.Date(NA)]
  out[, comp_tier := NA_character_]

  # Genuinely absent from this API path. Named here and exempted from the
  # zero-coverage gate below, rather than back-filled with something plausible.
  out[, discipline_code := NA_character_]       # WA short code; not on this path
  out[, value_raw := NA_real_]                  # the integer perf value we distrust anyway
  out[, birthdate_year_only := NA]              # the API does not say
  out[]
}

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
} else if (length(staged <- Sys.glob(file.path(D, "*_raw_results.rds"))) &&
           length(staged <- Filter(function(f) {
             d <- tryCatch(as.data.table(readRDS(f)), error = function(e) NULL)
             !is.null(d) && "competition_id" %in% names(d) &&
               any(as.character(d$competition_id) == CID)
           }, staged))) {
  # THE DIRECT-HARVEST PATH, added 2026-09-14.
  #
  # The fetch below goes through athletics_competition_results(), which uses
  # the community mirror worldathletics.nimarion.de -- returning 500 on every
  # competition since 2026-09-12. That is what made appending Budapest look
  # impossible. harvest_wa_results.R talks to World Athletics directly and
  # works, so a meet staged by it is used here rather than re-fetched through
  # the dead route.
  #
  # The staged file is the RAW WA shape (44 columns), not championship_results'
  # (33), so it is mapped below rather than used as-is.
  say("using staged direct harvest %s", basename(staged[[1]]))
  suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
  r <- .map_harvest_to_championship(as.data.table(readRDS(staged[[1]])), CID, D)
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
# EXEMPT LIST, each entry with its reason. comp_name/comp_tier were always
# here (legitimately NA for some tiers -- see the header). The three added
# 2026-09-14 are columns the DIRECT harvest route genuinely does not carry:
#   discipline_code      WA's short code; not exposed on the results path
#   value_raw            the integer performanceValue, which .resolve_mark()
#                        distrusts anyway (round marks lose trailing zeros)
#   birthdate_year_only  the API does not say whether a birthdate is year-only
# They are NA rather than back-filled on purpose. An empty column that is
# named and explained can be filled later; one quietly given a plausible value
# cannot be found again.
.exempt_zero <- c("comp_name", "comp_tier",
                  "discipline_code", "value_raw", "birthdate_year_only")
zero <- names(extra_cov)[extra_cov == 0 & !(names(extra_cov) %in% .exempt_zero)]
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
