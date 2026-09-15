# Turn the direct-WA career cache into a table the corpus can use.
#
# harvest_athlete_careers_wa.R writes one .rds per athlete in WA's own shape.
# Without this step those files are inert: ~512 results per athlete sitting on
# disk in a schema nothing reads.
#
# WHAT IT MAPS, and the two things that are NOT in the feed:
#   * `date` arrives as "18 JAN 2025", not ISO. as.Date() without a format
#     THROWS on it, and month abbreviations depend on LC_TIME, so the locale is
#     forced to C rather than inherited.
#   * `sex` IS ABSENT from every career row, and match_event() needs it -- the
#     same discipline name maps to a different event_id by sex. It is taken from
#     championship_results per athlete, and an athlete we hold no sex for is
#     left with event_id NA rather than guessed. match_event() returns NA rather
#     than guessing and must stay that way: fuzzy matching corrupts histories
#     undetectably.
#   * `place` arrives as "9." with a trailing dot, and non-finishers arrive as
#     text ("DNF", "DQ", "NM"), so a bare as.integer() silently NAs the lot.
#
# Everything the feed gives is kept, including competitionId/eventId/category/
# race, which the old mirror route did not carry at all -- that absence is why
# 98.8% of career-route corpus rows have no race_key.
#
# Writes wa_careers.rds/parquet ONLY. It does not touch championship_results,
# the corpus or any store; merging is a later, separate decision.
#
# Usage:
#   Rscript citiusdata/scripts/assemble_wa_careers.R

VERSE <- here::here()
suppressMessages({library(data.table); library(arrow); library(cli)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D <- file.path(VERSE, "citiusdata", "data")
CACHE <- file.path(D, "wa_career_cache")
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

fs <- list.files(CACHE, pattern = "^[0-9]+\\.rds$", full.names = TRUE)
if (!length(fs)) cli_abort("No career cache files in {.file {CACHE}}.")
say("reading %s cached athletes ...", format(length(fs), big.mark = ","))

parts <- vector("list", length(fs))
for (i in seq_along(fs)) {
  x <- tryCatch(readRDS(fs[i]), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) next
  parts[[i]] <- as.data.table(x)
  if (i %% 2000 == 0) say("  %s/%s", format(i, big.mark=","), format(length(fs), big.mark=","))
}
parts <- parts[!vapply(parts, is.null, logical(1))]
if (!length(parts)) cli_abort("Every cache file was empty.")
d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
rm(parts); invisible(gc(verbose = FALSE))
say("raw career rows: %s across %s athletes",
    format(nrow(d), big.mark = ","), format(uniqueN(d$athlete_id), big.mark = ","))

# --- dates --------------------------------------------------------------------
.wa_date <- function(x) {
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  suppressWarnings(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))
  x <- as.character(x)
  dd <- suppressWarnings(as.Date(x, format = "%d %b %Y"))
  iso <- suppressWarnings(tryCatch(as.Date(x), error = function(e) rep(as.Date(NA), length(x))))
  out <- fifelse(is.na(dd), iso, dd)
  structure(as.numeric(out), class = "Date")   # force double storage, never IDate
}
d[, date := .wa_date(date)]
say("date parsed on %.1f%% of rows", 100 * mean(!is.na(d$date)))

# --- sex, from what we already hold ------------------------------------------
sx <- as.data.table(with_citius_db_connection(function(cn) DBI::dbGetQuery(cn,
  "SELECT athlete_id, MAX(sex_code) sex FROM championship_results
   WHERE sex_code IS NOT NULL GROUP BY athlete_id"), read_only = TRUE))
sx[, aid := suppressWarnings(as.integer(athlete_id))]
d[, aid := suppressWarnings(as.integer(athlete_id))]
d <- merge(d, sx[, .(aid, sex)], by = "aid", all.x = TRUE)
say("sex known for %.1f%% of rows", 100 * mean(!is.na(d$sex)))

# --- events -------------------------------------------------------------------
d[, event_id := match_event(discipline, sex)]
say("event_id matched on %.1f%% of rows (%s distinct disciplines unmatched)",
    100 * mean(!is.na(d$event_id)),
    format(uniqueN(d[is.na(event_id)]$discipline), big.mark = ","))
if (nrow(d[is.na(event_id)])) {
  top <- d[is.na(event_id), .N, by = discipline][order(-N)]
  say("top unmatched disciplines (kept, not guessed):")
  print(head(top, 8))
}

# --- place and mark -----------------------------------------------------------
# "9." -> 9; "DNF"/"DQ"/"NM" -> NA, and recorded rather than silently dropped.
d[, place_raw := as.character(place)]
d[, place := suppressWarnings(as.integer(sub("\\.$", "", trimws(place_raw))))]
say("place numeric on %.1f%% of rows; non-finishers: %s",
    100 * mean(!is.na(d$place)),
    format(nrow(d[is.na(place) & !is.na(place_raw) & nzchar(place_raw)]), big.mark = ","))
d[, mark_string := as.character(mark)]
d[, mark := parse_mark(mark_string)]
say("mark parsed on %.1f%% of rows", 100 * mean(!is.na(d$mark)))

reg <- as.data.table(citius_events()[, c("event_id", "orientation")])
d <- merge(d, reg, by = "event_id", all.x = TRUE)
d[, perf := to_perf(mark, orientation)]
say("perf computed on %.1f%% of rows", 100 * mean(!is.na(d$perf)))

# --- tidy types ---------------------------------------------------------------
d[, competition_id := suppressWarnings(as.integer(competitionId))]
d[, wa_event_id    := suppressWarnings(as.integer(eventId))]
d[, wind           := suppressWarnings(as.numeric(wind))]
d[, legal          := !isTRUE(notLegal)]

# INDOOR IS JOINED FROM WHAT WE KNOW, NOT DERIVED. WA returns null for `indoor`
# on every career row (0 non-NA across 40 sampled athletes), and
# estimate_ability() reads it when the calibration carries an indoor offset, so
# it cannot simply be dropped.
#
# The obvious derivation -- WA suffixes indoor meets "(i)", as in "Ostrava
# Indoor I, Atleticka hala, Ostrava (i)" -- was tried and REJECTED BY ITS OWN
# CHECK: it agrees with the stored flag on only 89.1% of 42,431 checkable rows,
# and not because of mixed indoor/outdoor meets (there are zero of those). One
# row in nine wrong, on a field the model reads, is not worth having. Inventing
# a plausible value is the exact move that silently reweighted 469k corpus rows
# in the tier episode.
#
# So take the REAL value where we hold it, keyed on competition_id, and leave NA
# where we do not. Coverage is reported rather than assumed.
known <- as.data.table(with_citius_db_connection(function(cn) DBI::dbGetQuery(cn,
  "SELECT competition_id, MIN(CAST(indoor AS INTEGER)) lo, MAX(CAST(indoor AS INTEGER)) hi
   FROM championship_results WHERE indoor IS NOT NULL GROUP BY competition_id"),
  read_only = TRUE))
mixed <- known[lo != hi]
known <- known[lo == hi][, .(competition_id, .ind = as.logical(lo))]
d <- merge(d, known, by = "competition_id", all.x = TRUE)
d[, indoor := .ind][, .ind := NULL]
say("indoor joined from stored competitions: %.1f%% of rows (%s mixed meets excluded)",
    100 * mean(!is.na(d$indoor)), format(nrow(mixed), big.mark = ","))
d[, source         := "wa_career"]
d[, c("aid", "competitionId", "eventId", "notLegal") := NULL]

say("")
say("=== assembled ===")
say("rows %s | athletes %s | competitions %s | events %s",
    format(nrow(d), big.mark = ","), format(uniqueN(d$athlete_id), big.mark = ","),
    format(uniqueN(d$competition_id), big.mark = ","),
    format(uniqueN(d$event_id), big.mark = ","))
say("dates %s .. %s", as.character(min(d$date, na.rm = TRUE)), as.character(max(d$date, na.rm = TRUE)))

# Coverage, not presence: a column that lands 100% empty is a mapping fault and
# should be visible here rather than discovered downstream.
cov <- sort(vapply(d, function(x) mean(!is.na(x)), numeric(1)))
say("column coverage (lowest first):")
print(data.table(column = names(cov), populated = round(cov, 3)))
# Named, with the reason, rather than quietly tolerated. These three are null on
# EVERY career row in the feed itself (verified across 40 sampled athletes), so
# an empty column here records what WA sent, not a mapping fault. `indoor` used
# to be on this list and is now derived from the competition name instead.
exempt <- c("disciplineCode", "disciplineNameUrlSlug", "typeNameUrlSlug")
empty <- setdiff(names(cov)[cov == 0], exempt)
if (length(empty)) cli_abort(c(
  "Column{?s} {.field {empty}} landed 100% empty and {?is/are} not a known-absent field.",
  i = "Either the mapping is wrong, or the feed has changed and this exempt list needs updating -- decide which, do not widen the list by reflex."))
kept_empty <- intersect(names(cov)[cov == 0], exempt)
if (length(kept_empty))
  say("empty by SOURCE, kept for the record: %s", paste(kept_empty, collapse = ", "))

saveRDS(d, file.path(D, "wa_careers.rds"))
write_parquet(d, file.path(D, "wa_careers.parquet"))
say("wrote wa_careers.{rds,parquet}")
say("NOTHING was merged into championship_results or the corpus.")
