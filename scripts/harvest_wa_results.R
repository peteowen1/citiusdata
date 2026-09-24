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
# REUSED IF THE CALLER ALREADY HAS ONE. Discovery costs 2.93s (measured
# 2026-09-17) against a ~4.2s total per meet, so a batch that spawns one
# process per meet spends ~70% of its life re-deriving the same endpoint --
# about 4.0 hours of the 5h48m the 4,949-meet run took. harvest_meets_batch.R
# discovers once and sources this in a loop; a standalone run is unchanged
# because `ep` does not exist in a fresh session.
#
# Guarded on the pair actually being usable, not merely present, so a stale or
# half-built object falls back to discovery rather than failing every meet.
if (exists("ep", inherits = FALSE) && is.list(ep) &&
    !is.null(ep$url) && !is.null(ep$key) && isTRUE(ep$status == 200L)) {
  cli_alert_info("Reusing endpoint {.val {ep$edge}} from the caller.")
} else ep <- local({
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
  summary{placeInRace placeInRound raceNumber mark nationality points records wind
   competitor{id name urlSlug birthDate iaafId}}
  races{race raceId raceNumber date day wind
   startList{order bib pb sb competitor{id name urlSlug birthDate country}}
   results{id remark mark nationality place points qualified records wind
    competitor{id name urlSlug birthDate iaafId hasProfile
     teamMembers{id name urlSlug iaafId}}}}}}}}'

# EVERY BRANCH IS NOW TAKEN, including two that returned zero rows on both
# meets tested (Budapest, Brussels) on 2026-09-14.
#
# An earlier version of this script skipped them on the reasoning that they
# were empty and would need their own output tables. That is the "decide what
# to do with it later" the capture rule exists to prevent: empty for two
# COMPLETED meets is not empty always.
#
#   race.startList   lane order, bib, and the athlete's PB and SB as at the
#                    meet. Pre-race data, so a finished meet has none -- but a
#                    meet that has NOT run is exactly when we would want it,
#                    and PB/SB at entry time is a signal we have nowhere else.
#   event.summary    placeInRace vs placeInRound, a distinction nothing else
#                    in our data carries and which matters for heats.
#
# Both are arrays at a different nesting level from results, so forcing them
# into the per-result table would duplicate rows. They are written as their
# OWN staged files instead -- see the writers at the bottom of this script.

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
  body <- resp_body_json(r)
  # A GraphQL 200 can still carry an `errors` array with data = null. That is a
  # SERVER FAULT, not "this meet has no results" -- and until 2026-09-17 the two
  # were indistinguishable downstream, because only $data was read. The wrong
  # one sends people chasing a data gap that no amount of harvesting can close.
  #
  # Measured: 38 catalogue meets flagged has_api_results = TRUE return
  # errorType "Lambda:Unhandled", message "Cannot read properties of undefined
  # (reading 'events')" -- for day:1, for every other day, and with no day
  # argument at all. WA's own resolver throws for those competitions; nothing we
  # send changes it. They had been reported as "returned no results on days
  # 1..8", which reads as an empty meet and is not what happened.
  if (length(body$errors)) {
    GQL_ERR$type <<- body$errors[[1]]$errorType %||% NA_character_
    GQL_ERR$msg  <<- body$errors[[1]]$message   %||% "unknown GraphQL error"
    cli_alert_warning("day {day}: server error {.val {GQL_ERR$type}} -- {GQL_ERR$msg}")
    return(NULL)
  }
  d <- body$data$getCalendarCompetitionResults
  # The two sibling branches, stashed for the writers below. They are arrays at
  # a different nesting level from results, so they cannot share this table
  # without duplicating rows -- they get their own files instead of being
  # dropped. `<<-` because this runs inside pull_day(), one scope down.
  if (!is.null(d)) {
    SIDE$startlist[[length(SIDE$startlist) + 1L]] <<- rbindlist(lapply(d$eventTitles, function(t)
      rbindlist(lapply(t$events, function(e)
        rbindlist(lapply(e$races, function(rc)
          rbindlist(lapply(rc$startList %||% list(), function(s) data.table(
            day = day, event_title = t$eventTitle %||% NA_character_,
            race_code = t$rankingCategory %||% NA_character_,
            event = e$event %||% NA_character_,
            wa_event_id = as.character(e$eventId %||% NA),
            round = rc$race %||% NA_character_,
            race_id = as.character(rc$raceId %||% NA),
            race_number = as.character(rc$raceNumber %||% NA),
            order = as.character(s$order %||% NA),
            bib = as.character(s$bib %||% NA),
            pb = as.character(s$pb %||% NA),
            sb = as.character(s$sb %||% NA),
            athlete_hash = s$competitor$id %||% NA_character_,
            athlete_name = s$competitor$name %||% NA_character_,
            url_slug = s$competitor$urlSlug %||% NA_character_,
            birth_date = s$competitor$birthDate %||% NA_character_,
            country = s$competitor$country %||% NA_character_
          )), fill = TRUE)), fill = TRUE)), fill = TRUE)), fill = TRUE)

    SIDE$summary[[length(SIDE$summary) + 1L]] <<- rbindlist(lapply(d$eventTitles, function(t)
      rbindlist(lapply(t$events, function(e)
        rbindlist(lapply(e$summary %||% list(), function(s) data.table(
          day = day, event_title = t$eventTitle %||% NA_character_,
          race_code = t$rankingCategory %||% NA_character_,
          event = e$event %||% NA_character_,
          wa_event_id = as.character(e$eventId %||% NA),
          place_in_race = as.character(s$placeInRace %||% NA),
          place_in_round = as.character(s$placeInRound %||% NA),
          race_number = as.character(s$raceNumber %||% NA),
          mark_string = s$mark %||% NA_character_,
          nationality = s$nationality %||% NA_character_,
          points = as.character(s$points %||% NA),
          records = s$records %||% NA_character_,
          wind = as.character(s$wind %||% NA),
          athlete_hash = s$competitor$id %||% NA_character_,
          athlete_name = s$competitor$name %||% NA_character_,
          url_slug = s$competitor$urlSlug %||% NA_character_,
          birth_date = s$competitor$birthDate %||% NA_character_,
          iaaf_id = as.character(s$competitor$iaafId %||% NA)
        )), fill = TRUE)), fill = TRUE)), fill = TRUE)
  }
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
          meet_code = d$competition$rankingCategory %||% NA_character_,
          # --- event-title level ---
          day           = day,
          event_title   = t$eventTitle %||% NA_character_,
          # THE TIER. Per eventTitle, not per meet, because it genuinely varies
          # within one meet. Named `race_code` to match championship_results.rds's
          # own column so the append needs no translation.
          race_code          = t$rankingCategory %||% NA_character_,
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
          # The meet-day's calendar date from options.days. rc$date is NULL on
          # some meets (all of Budapest), and this is the only other place the
          # real date exists.
          day_date      = mget(as.character(day), envir = DAY_DATES,
                               ifnotfound = list(NA_character_))[[1]],
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

# Accumulators for the two sibling branches, filled inside pull_day().
# Set by pull_day() when the API returns a GraphQL `errors` array. Kept so the
# final message can say SERVER FAULT rather than "no results" -- see pull_day().
GQL_ERR <- new.env(parent = emptyenv())
GQL_ERR$type <- NA_character_; GQL_ERR$msg <- NA_character_

SIDE <- new.env(parent = emptyenv())
SIDE$startlist <- list(); SIDE$summary <- list()

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
DAY_DATES <- new.env(parent = emptyenv())   # day -> "11 SEP 2026"
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
    # Keep the day -> date mapping. WA returns a NULL `race.date` on some meets
    # (Budapest: every race), so this is the only place the actual calendar
    # date of a day is available. Without it `date` lands 100% NA downstream.
    for (x in probe) if (!is.null(x$day)) assign(as.character(x$day), x$date %||% NA_character_, envir = DAY_DATES)
    cli_alert_info("Meet runs {length(d)} day{?s}: {.val {vapply(probe, function(x) x$date %||% '?', character(1))}}")
    sort(unique(d))
  } else {
    cli_alert_warning("Could not read {.field options.days}; falling back to {.envvar CITIUS_DAYS}.")
    DAYS
  }
})

res <- rbindlist(lapply(DAYS, pull_day), fill = TRUE)
if (!nrow(res)) {
  # Name which of the two it is. They are not the same problem: an empty meet
  # is a data fact, a resolver crash is WA's bug and is not fixable by us.
  if (!is.na(GQL_ERR$msg)) cli_abort(c(
    "Competition {COMP}: the WA API errored rather than returning no results.",
    "x" = "{GQL_ERR$type}: {GQL_ERR$msg}",
    "i" = "Server-side fault, identical for every day and with no day argument. Not harvestable by retrying; record it as known-unfixable rather than re-queuing it."))
  cli_abort("Competition {COMP} returned no results on days {.val {DAYS}}.")
}

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

# THE TWO SIBLING BRANCHES, written whether or not they have rows.
#
# An empty file is the point, not a waste: it records that we ASKED and the
# meet had none, which is different from never having looked. A completed meet
# has no start list because the start list is pre-race -- writing the empty
# file is what lets a later reader tell that apart from a harvest that simply
# ignored the branch, which is what every version of this script before
# 2026-09-14 did.
for (nm in c("startlist", "summary")) {
  tbl <- rbindlist(SIDE[[nm]], fill = TRUE)
  f <- sub("_raw_results\\.rds$", sprintf("_raw_%s.rds", nm), out)
  saveRDS(tbl, f)
  if (nrow(tbl)) {
    cli_alert_success("Wrote {.file {basename(f)}} ({nrow(tbl)} rows).")
  } else {
    cli_alert_info("Wrote {.file {basename(f)}} (0 rows -- WA returned none for this meet).")
  }
}
cli_alert_info("Score it: {.code CITIUS_MEET={MEET} CITIUS_RESULTS_CACHE={out} Rscript citiusdata/scripts/score_meet.R}")
