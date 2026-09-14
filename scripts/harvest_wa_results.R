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

# EVERY FIELD THE SCHEMA OFFERS on this path, enumerated by introspection
# 2026-09-14 rather than guessed. The previous query asked for five fields on
# the result and none of the competition metadata, which is why appending a
# meet looked blocked on the dead mirror: `tier` was assumed to be something
# only the mirror enriched, when in fact it is `rankingCategory` and we simply
# never asked. Verified against Brussels, already in championship_results.rds:
# the API returns DF/GW/A/F and the stored rows carry DF 237, F 57, A 31,
# GW 30 -- the same vocabulary.
#
# Per Pete's standing rule (~/.claude/CLAUDE.md, Data & Analysis): capture
# every field the API returns and decide what is useful later, because
# dropping one at parse time is a decision you cannot revisit without a
# re-harvest, and re-harvesting is expensive enough that "we can get it later"
# is false in practice.
#
# rankingCategory appears on BOTH competition and eventTitle. The eventTitle
# one is the one that matters: tier varies WITHIN a meet (2025 Weltklasse
# Zurich carries A, DF, F and GW across its own results), which is the whole
# reason build_competition_catalogue.R exists.
GQL <- 'query($id:Int,$day:Int){getCalendarCompetitionResults(competitionId:$id,day:$day){
 competition{name venue startDate endDate dateRange rankingCategory}
 eventTitles{eventTitle rankingCategory events{event eventId gender isRelay perResultWind withWind
  races{race raceId raceNumber date day wind
   results{id remark mark nationality place points qualified records wind
    competitor{id name urlSlug birthDate iaafId hasProfile
     teamMembers{id name urlSlug iaafId}}}}}}}}'

# TWO BRANCHES DELIBERATELY NOT TAKEN, recorded so the next person does not
# re-introspect to find out why (checked 2026-09-14 against Budapest and
# Brussels, both returned zero entries):
#
#   race.startList{order bib pb sb competitor{...}}
#       Lane draw, bib, and the athlete's PB/SB as at the meet. Empty for a
#       COMPLETED meet -- it is pre-race data -- so it belongs to an
#       entry-list harvest, not a results one. Worth having for a meet that
#       has not run.
#   event.summary{placeInRace placeInRound raceNumber mark ...}
#       Carries a placeInRace/placeInRound distinction we have nowhere else,
#       which would matter for heats. WA returns none for either meet tested.
#
# Both are per-event or per-race ARRAYS rather than per-result fields, so they
# would need their own output tables rather than more columns here. They are
# not dropped on a judgement that they are useless -- they are empty, and the
# shape they would need is a separate job.

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
        # EVERY field, at every level of the nesting, carried through. Nothing
        # is judged here -- judging happens downstream where it can be undone.
        # Competition-level values repeat on every row, which is redundant on
        # disk and free in parquet, and means a single meet file is
        # self-describing rather than needing its metadata looked up elsewhere.
        rbindlist(lapply(rc$results, function(x) data.table(
          # --- competition level (constant per meet) ---
          comp_name     = d$competition$name %||% NA_character_,
          comp_venue    = d$competition$venue %||% NA_character_,
          comp_start    = d$competition$startDate %||% NA_character_,
          comp_end      = d$competition$endDate %||% NA_character_,
          comp_daterange = d$competition$dateRange %||% NA_character_,
          comp_ranking_category = d$competition$rankingCategory %||% NA_character_,
          # --- event-title level ---
          day           = day,
          event_title   = t$eventTitle %||% NA_character_,
          # THE TIER. Per eventTitle, not per meet, because it genuinely varies
          # within one meet. Named `tier` to match championship_results.rds's
          # own column so the append needs no translation.
          tier          = t$rankingCategory %||% NA_character_,
          # --- event level ---
          event         = e$event %||% NA_character_,
          wa_event_id   = as.character(e$eventId %||% NA),
          gender        = e$gender %||% NA_character_,
          is_relay      = e$isRelay %||% NA,
          # Disambiguates a null wind: "not measured for this event" vs "no
          # reading for this result". wind is the most-empty column in the
          # corpus at 72.3% NA and these two say which kind of empty it is.
          per_result_wind = e$perResultWind %||% NA,
          with_wind     = e$withWind %||% NA,
          # --- race level ---
          round         = rc$race %||% NA_character_,
          race_id       = as.character(rc$raceId %||% NA),
          race_number   = as.character(rc$raceNumber %||% NA),
          date          = rc$date %||% NA_character_,
          race_wind     = as.character(rc$wind %||% NA),
          # --- result level ---
          result_id     = x$id %||% NA_character_,
          # CORRECTION to an earlier guess of mine: `remark` is NOT the DNF/DQ
          # reason. Measured on Budapest, which has 11 non-finishers: remark is
          # NA on all 439 rows, while `mark_string` carries "DNF", "DQ", "NM"
          # and "DNS" directly. So the no-mark signal calibrate() wants is in
          # the mark, not here. Kept anyway -- it costs nothing and an empty
          # field that is captured can be checked later, whereas one that is
          # dropped cannot.
          remark        = x$remark %||% NA_character_,
          mark_string   = x$mark %||% NA_character_,
          nationality   = x$nationality %||% NA_character_,
          place_raw     = x$place %||% NA_character_,
          points        = as.character(x$points %||% NA),
          qualified     = x$qualified %||% NA,
          records       = x$records %||% NA_character_,
          wind          = as.character(x$wind %||% NA),
          # --- competitor level ---
          athlete_hash  = x$competitor$id %||% NA_character_,
          athlete_name  = x$competitor$name %||% NA_character_,
          url_slug      = x$competitor$urlSlug %||% NA_character_,
          birth_date    = x$competitor$birthDate %||% NA_character_,
          # The legacy numeric id, useful for crosswalking to older sources.
          iaaf_id       = as.character(x$competitor$iaafId %||% NA),
          has_profile   = x$competitor$hasProfile %||% NA,
          # RELAY COMPOSITION. 32 entries on Budapest day 1, 0 on Brussels --
          # it populates only for relay events, which citius currently drops
          # entirely (citius#1). Collapsed to delimited strings rather than a
          # list-column so the table stays rectangular and survives a parquet
          # write; splitting on "|" recovers the members. Captured now because
          # a re-harvest to get it later is exactly the cost the capture-
          # everything rule exists to avoid.
          team_member_ids = paste(vapply(x$competitor$teamMembers %||% list(),
            function(tm) as.character(tm$id %||% NA), character(1)), collapse = "|"),
          team_member_names = paste(vapply(x$competitor$teamMembers %||% list(),
            function(tm) as.character(tm$name %||% NA), character(1)), collapse = "|")
        )), fill = TRUE)), fill = TRUE)), fill = TRUE)), fill = TRUE)
}

cli_h2("Harvest")

# ASK WHICH DAYS EXIST rather than brute-forcing a range. The API's
# `options.days` lists them with dates -- Budapest returns exactly 3 (11, 12,
# 13 Sep). Without this, CITIUS_DAYS defaults to 1..8 and every day past the
# end of the meet is a wasted request; the mirror route's equivalent produced
# "12 day-page fetches failed for competition 7212925", which reads like an
# outage and is really just asking for days that were never going to exist.
#
# Falls back to CITIUS_DAYS if the probe fails, so a schema change degrades to
# the old behaviour rather than harvesting nothing.
DAYS <- local({
  probe <- tryCatch({
    r <- request(ep$url) |>
      req_headers(accept = "*/*", `content-type` = "application/json",
                  `x-api-key` = ep$key, `x-amz-user-agent` = "aws-amplify/3.0.7") |>
      req_body_json(list(query = sprintf(
        'query{getCalendarCompetitionResults(competitionId:%d,day:1){options{days{day date}}}}', COMP))) |>
      req_timeout(60) |> req_error(is_error = function(x) FALSE) |> req_perform()
    resp_body_json(r)$data$getCalendarCompetitionResults$options$days
  }, error = function(e) NULL)
  d <- suppressWarnings(as.integer(unlist(lapply(probe, function(x) x$day))))
  d <- d[is.finite(d)]
  if (length(d)) {
    cli_alert_info("Meet runs {length(d)} day{?s}: {.val {vapply(probe, function(x) x$date %||% '?', character(1))}}")
    sort(unique(d))
  } else {
    cli_alert_warning("Could not read {.field options.days}; falling back to {.envvar CITIUS_DAYS}.")
    DAYS
  }
})

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
