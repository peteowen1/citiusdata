# Harvest a competition's results straight from World Athletics' GraphQL API.
#
#   CITIUS_COMP=7212925 CITIUS_MEET=budapest2026 Rscript citiusdata/scripts/harvest_wa_results.R
#
# WHY THIS EXISTS. Our only athletics results route was the community mirror
# `worldathletics.nimarion.de`, which is a thin wrapper over this same API. On
# 2026-09-12 it returned 500 for EVERY competition -- Brussels, Budapest and
# Birmingham alike -- which left us unable to score a meet that was running that
# day. A single upstream with no fallback is not a route, it is a hope.
#
# This talks to World Athletics directly. The endpoint pair it needs rotates, so
# it is read fresh each run by `discover_wa_endpoint.R` rather than stored; see
# that script's header for why storing the key is the wrong instinct.
#
# It writes `<meet>_raw_results.rds` in the shape `score_meet.R` already expects,
# so the scorer needs no change: point it at the file with CITIUS_RESULTS_CACHE.
#
# ROUND NAMES ARE THE MEET'S OWN. Budapest was catalogued as "finals only - no
# heats" and day 1 in fact ran semifinals, so nothing here infers a round from
# the schedule -- `round` is whatever WA calls it.

suppressMessages({
  devtools::load_all(here::here("citius"))   # match_event()
  library(httr2)
  library(data.table)
  library(cli)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

COMP <- suppressWarnings(as.integer(Sys.getenv("CITIUS_COMP", "")))
MEET <- Sys.getenv("CITIUS_MEET", "")
DAYS <- as.integer(strsplit(Sys.getenv("CITIUS_DAYS", "1,2,3,4,5,6,7,8"), ",")[[1]])
if (is.na(COMP)) cli_abort("Set {.envvar CITIUS_COMP} to a World Athletics competition id.")
if (!nzchar(MEET)) cli_abort("Set {.envvar CITIUS_MEET} to a calendar meet_id (names the output file).")

# --- endpoint -----------------------------------------------------------------
ep <- local({
  o <- capture.output(v <- source(
    file.path(here::here("citiusdata", "scripts"), "discover_wa_endpoint.R"))$value)
  v
})
if (is.na(ep$status) || ep$status != 200L) {
  cli_abort(c("Endpoint discovery returned HTTP {ep$status}.",
              i = "Run {.file discover_wa_endpoint.R} on its own to see why."))
}
cli_alert_success("Endpoint live: {.val {ep$edge}}")

GQL <- 'query($id:Int,$day:Int){getCalendarCompetitionResults(competitionId:$id,day:$day){
 competition{name venue startDate endDate}
 eventTitles{eventTitle events{event eventId gender isRelay
  races{race raceId raceNumber date day
   results{competitor{id name urlSlug birthDate} mark nationality place points qualified records wind}}}}}}'

pull_day <- function(day) {
  r <- request(ep$url) |>
    req_headers(accept = "*/*", `content-type` = "application/json",
                `x-api-key` = ep$key, `x-amz-user-agent` = "aws-amplify/3.0.7") |>
    req_body_json(list(query = GQL, variables = list(id = COMP, day = day))) |>
    req_timeout(60) |>
    req_error(is_error = function(x) FALSE) |>
    req_perform()
  if (resp_status(r) != 200L) {
    cli_alert_warning("day {day}: HTTP {resp_status(r)}")
    return(NULL)
  }
  d <- resp_body_json(r)$data$getCalendarCompetitionResults
  if (is.null(d)) return(NULL)
  rbindlist(lapply(d$eventTitles, function(t)
    rbindlist(lapply(t$events, function(e)
      rbindlist(lapply(e$races, function(rc)
        rbindlist(lapply(rc$results, function(x) data.table(
          day           = day,
          event_title   = t$eventTitle %||% NA_character_,
          event         = e$event %||% NA_character_,
          wa_event_id   = as.character(e$eventId %||% NA),
          gender        = e$gender %||% NA_character_,
          is_relay      = e$isRelay %||% NA,
          round         = rc$race %||% NA_character_,
          race_id       = as.character(rc$raceId %||% NA),
          race_number   = as.character(rc$raceNumber %||% NA),
          date          = rc$date %||% NA_character_,
          athlete_hash  = x$competitor$id %||% NA_character_,
          athlete_name  = x$competitor$name %||% NA_character_,
          url_slug      = x$competitor$urlSlug %||% NA_character_,
          birth_date    = x$competitor$birthDate %||% NA_character_,
          mark_string   = x$mark %||% NA_character_,
          nationality   = x$nationality %||% NA_character_,
          place_raw     = x$place %||% NA_character_,
          points        = as.character(x$points %||% NA),
          qualified     = x$qualified %||% NA,
          records       = x$records %||% NA_character_,
          wind          = as.character(x$wind %||% NA)
        )), fill = TRUE)), fill = TRUE)), fill = TRUE)), fill = TRUE)
}

cli_h2("Harvest")
res <- rbindlist(lapply(DAYS, pull_day), fill = TRUE)
if (!nrow(res)) cli_abort("Competition {COMP} returned no results on days {.val {DAYS}}.")

# --- keys the scorer needs ----------------------------------------------------
# The `competitor.id` the API returns is an opaque hash, NOT the numeric World
# Athletics id our prediction cards are keyed on. The numeric id is the trailing
# number of the url slug ("united-states/chris-bailey-14850487"). Joining on the
# hash would match 0 of N finalists, which `score_meet.R` is explicitly built to
# catch -- but only after it has wasted a run.
res[, athlete_id := sub(".*-(\\d+)$", "\\1", url_slug)]
res[!grepl("^\\d+$", athlete_id), athlete_id := NA_character_]
res[, place := suppressWarnings(as.integer(sub("\\.$", "", place_raw)))]

# `event_id` MUST be the registry id ("AT-400Metres-M"), not WA's numeric event
# id, because that is what the prediction cards are keyed on. Derived with the
# package's own `match_event()` rather than a lookup written here -- a second
# mapping would drift from the one the model was trained through, and the
# failure is silent: the scorer just reports "no predicted final is complete".
res[, discipline := sub("^(Men's|Women's|Mixed) ", "", event)]
res[, event_id := match_event(discipline, gender)]
res[, race_key := paste(COMP, wa_event_id, round, race_number, sep = "|")]
res[, competition_id := COMP]
res[, wind := suppressWarnings(as.numeric(wind))]

# `mark` numeric, parsed from the string with the package's own parser.
# The API's results payload has no `performanceValue`, so unlike the mirror
# route there is NO numeric fallback when a string fails to parse -- which is
# why the parse rate is asserted below rather than printed and passed over.
res[, mark := parse_mark(mark_string)]
placed <- res[!is.na(place)]
parse_rate <- if (nrow(placed)) mean(!is.na(placed$mark)) else 1
cli_alert_info("mark parsed for {round(100 * parse_rate)}% of {nrow(placed)} placed row{?s}.")
if (parse_rate < 0.95) {
  cli_alert_danger("Under 95% of placed marks parsed - do not publish this without looking:")
  print(head(placed[is.na(mark), .(event, round, athlete_name, mark_string)], 10))
}

unmapped <- res[is.na(event_id), .N, by = .(event, gender)]
if (nrow(unmapped)) {
  cli_alert_warning("{nrow(unmapped)} event{?s} did not map to a registry id:")
  print(unmapped)
}

n_id <- sum(!is.na(res$athlete_id))
cli_alert_info("{nrow(res)} row{?s}, {uniqueN(res$event)} event{?s}, {uniqueN(res$race_id)} race{?s}, day{?s} {.val {sort(unique(res$day))}}.")
cli_alert_info("Numeric athlete id recovered for {n_id} of {nrow(res)} ({round(100*n_id/nrow(res))}%).")
if (n_id < 0.9 * nrow(res)) {
  cli_alert_danger("Under 90% of rows have a numeric id - the slug format has changed.")
}
print(res[, .N, by = round][order(-N)])

out <- here::here("citiusdata", "data", paste0(MEET, "_raw_results.rds"))
saveRDS(res, out)
cli_alert_success("Wrote {.file {basename(out)}} ({nrow(res)} rows).")
cli_alert_info("Score it: {.code CITIUS_MEET={MEET} CITIUS_RESULTS_CACHE={out} Rscript citiusdata/scripts/score_meet.R}")
