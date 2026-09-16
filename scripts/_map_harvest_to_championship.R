# Map ONE harvest_wa_results.R output onto championship_results.rds's shape.
#
# Extracted from append_meet_to_championship_results.R (2026-09-17) so
# merge_missing_meets_batch.R can reuse the exact same, already-tested
# transform rather than a second hand-written copy that could silently
# drift from it. Source this file, then call .map_harvest_to_championship().
#
# NOTHING is invented here: a column the direct route genuinely does not
# carry is left NA and named in the caller's exempt list, with the reason,
# rather than filled with a plausible-looking value -- the whole lesson of
# the tier episode (a fabricated tier silently reweighted 469k corpus rows
# once already).
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
  # ...and finally the HARVEST's own comp_start, which for a backfilled meet is
  # the only source there is.
  if ("comp_start" %in% names(out)) {
    out[, .hstart := .wa_date(comp_start)]
    out[is.na(date) & !is.na(.hstart) & !is.na(day),
        date := .hstart + (as.integer(day) - 1L)]
    out[is.na(date) & !is.na(.hstart), date := .hstart]
    out[, .hstart := NULL]
  }
  out[, birthdate := .wa_date(birth_date)]
  out[, athlete_id := as.character(athlete_id)]
  out[, sport := "Athletics"]
  out[, sex_code := gender]
  out[, comp_day := suppressWarnings(as.integer(day))]
  # eventTitle is NULL on some meets; fall back to the event name itself
  # rather than leaving it empty. Stored event_name is only 24.2% populated,
  # so sparse here is the norm, not a mapping bug.
  out[, event_name := data.table::fifelse(
    !is.na(event_title) & nzchar(as.character(event_title)),
    as.character(event_title), as.character(event))]
  out[, wind := suppressWarnings(as.numeric(wind))]

  # Wind legality is a property of the reading, never an assumption -- the
  # source adapter's own comment records that hardcoding TRUE once treated
  # 7,423 wind-aided marks as legal.
  out[, legal := is.na(wind) | wind <= 2.0]

  # VENUE, parsed from the one string the API gives:
  # "Nemzeti Atletikai Kozpont, Budapest (HUN)" -> stadium, city, country.
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

  # PREFER THE HARVEST'S OWN competition block over the calendar (which holds
  # only 7 hand-maintained meets -- every backfilled competition is absent
  # from it). The calendar still wins where it has a row: it is hand-
  # maintained and the only source that can disagree with the feed on purpose.
  # A Date's STORAGE type has to be forced, not assumed -- see
  # docs/reference/silent-bugs.md, "coverage gates cannot see a type change".
  .date_dbl <- function(x) structure(as.numeric(as.Date(x)), class = "Date")
  cal_name  <- if (nrow(crow)) as.character(crow$name[1]) else NA_character_
  cal_start <- if (nrow(crow)) .date_dbl(crow$date_start[1]) else as.Date(NA)
  if (!"comp_name" %in% names(out)) out[, comp_name := NA_character_]
  harvest_name  <- as.character(out$comp_name)
  harvest_start <- if ("comp_start" %in% names(out)) .date_dbl(.wa_date(out$comp_start)) else as.Date(NA)
  out[, comp_name := if (!is.na(cal_name)) cal_name else harvest_name]
  out[, comp_start := if (!is.na(cal_start)) cal_start else harvest_start]
  # Belt and braces: whatever route produced it, this column leaves the mapper
  # as Date/double or not at all. Cheaper than discovering the mismatch 4.7M
  # rows later.
  if (!is.double(out$comp_start))
    cli::cli_abort("comp_start left the mapper as {typeof(out$comp_start)}, not double.")
  out[, comp_tier := NA_character_]

  # Genuinely absent from this API path. Named here and exempted from the
  # zero-coverage gate below, rather than back-filled with something plausible.
  out[, discipline_code := NA_character_]       # WA short code; not on this path
  out[, value_raw := NA_real_]                  # the integer perf value we distrust anyway
  out[, birthdate_year_only := NA]              # the API does not say
  out[]
}
