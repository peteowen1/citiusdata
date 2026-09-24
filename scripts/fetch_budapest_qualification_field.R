# Fetch the OFFICIAL qualification field for the World Athletics Ultimate
# Championship (Budapest 2026) from World Athletics' own "Road to the Ultimate"
# endpoint, and write it in the shape the Diamond-League card pipeline reads.
#
# WHY THIS MEET GETS A BETTER SOURCE THAN BRUSSELS. Brussels' card had to be
# built from a third-party compiled qualifier list, because World Athletics
# publishes nothing machine-readable for a Diamond League final: no entry list
# (has_startlist = false), and this very endpoint returns a server error for DL
# competition ids -- DL qualification is points across 14 meets, a system it
# does not model. Budapest is a WA championship with a formal qualification
# system, so the same endpoint answers properly. Measured 2026-08-31: 26 of 26
# individual events return a field, and every athlete arrives with an id
# already attached.
#
# THE RESOLVER IS SKIPPED ON PURPOSE, and that is the real prize here. Every
# competitor comes back with a urlSlug whose trailing segment IS the World
# Athletics athlete id -- the same id namespace as the corpus. So there is no
# name matching to do at all: no transliteration, no reversed-name tier, no
# fuzzy guess. Brussels needed all three (its normalise step DELETED non-ASCII
# letters, silently losing 16 real finalists including the reigning discus
# world champion until it was fixed), and every one of those tiers is a place
# a wrong athlete could enter a published card. None of that risk exists here.
# Running resolve_diamond_league_athletes.R over this field would be strictly
# worse: it would throw away known-correct ids to re-derive them from names.
#
# THE FIELD IS OFFICIAL BUT PROVISIONAL, and the card must say both. The
# world-rankings qualification window closed 2026-09-01, and the Brussels DL
# Final (Sep 4-5) awards auto-qualifying slots that will displace some current
# bottom-ranked world-rankings qualifiers. So this is genuinely WA's own
# standing, and it is genuinely not the final field. Stamped as such rather
# than presented as settled -- see field_type/field_source on the card.
#
# Relays (mixed 4x100m, 4x400m) are dropped: this package does not model relays
# at all (citius#1).
#
# Usage:  Rscript scripts/fetch_budapest_qualification_field.R [meet_id]

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(httr2))
D <- file.path(VERSE, "citiusdata", "data")

PAUSE <- 1.0

# DISCOVER THE PAIR, do not require one to be configured.
#
# This previously demanded CITIUS_WA_GRAPHQL_KEY and defaulted the URL to edge
# 4883 -- which stopped resolving in DNS on 2026-09-12, so the default was dead
# and a correctly-set key would still have failed. Chasing that by hand is a
# Chrome session per rotation, and the rotations are frequent: 4881 retired
# 09-09, 4883 gone by 09-12, 4888 then, 4892 by 09-14.
#
# Neither value is a secret -- both are compiled into the public Next.js bundle
# every visitor downloads -- so discover_wa_endpoint.R reads them fresh in
# about three seconds. That is strictly better than a stored secret, which is
# wrong the moment WA rotates and cannot be made right by refreshing it on a
# schedule: a key refreshed at 03:00 can be dead by noon.
#
# Same pattern harvest_wa_results.R already uses. An explicitly configured
# option or env var still wins, for a deliberate override.
.ep <- NULL
GQL_URL <- getOption("citius.wa_graphql_url", Sys.getenv("CITIUS_WA_GRAPHQL_URL", ""))
WA_KEY  <- getOption("citius.wa_graphql_key", Sys.getenv("CITIUS_WA_GRAPHQL_KEY", ""))
if (!nzchar(GQL_URL) || !nzchar(WA_KEY)) {
  .ep <- local({
    o <- capture.output(v <- source(
      file.path(VERSE, "citiusdata", "scripts", "discover_wa_endpoint.R"))$value)
    v
  })
  if (is.na(.ep$status) || .ep$status != 200L) {
    cli::cli_abort(c(
      "Endpoint discovery returned HTTP {.ep$status}.",
      i = "Run {.file discover_wa_endpoint.R} on its own to see why."))
  }
  # THE PAIR IS ATOMIC -- take BOTH discovered values, never one of each.
  # The bundle carries several `da2-` keys and only the one sitting in the same
  # chunk as this hostname works, so a fresh edge with a stale key fails with
  # CloudFront's 503 "Lambda function is invalid" -- indistinguishable from a
  # retired edge, and it cost an hour on 2026-09-12 before nearly being written
  # up as "WA now blocks non-browser clients".
  #
  # Filling in only the missing half is what produced exactly that pairing in
  # CI: meet-forecast.yml sets CITIUS_WA_GRAPHQL_KEY (a secret stored
  # 2026-09-09, the day edge 4881 was retired) and does NOT set
  # CITIUS_WA_GRAPHQL_URL, so the URL was discovered fresh while the dead key
  # was kept. A half-configured override is not an override; it is a broken pair.
  if (nzchar(GQL_URL) != nzchar(WA_KEY)) {
    cli::cli_alert_warning(c(
      "Only one half of the WA endpoint pair was configured; ignoring it and using
       the discovered pair. Set BOTH {.envvar CITIUS_WA_GRAPHQL_URL} and
       {.envvar CITIUS_WA_GRAPHQL_KEY} to override deliberately."))
  }
  GQL_URL <- .ep$url
  WA_KEY  <- .ep$key
  cli::cli_alert_success("Endpoint discovered: {.val {.ep$edge}}")
}

args <- commandArgs(trailingOnly = TRUE)
MEET <- if (length(args)) args[[1]] else "budapest2026"

cal <- fread(file.path(D, "athletics_calendar.csv"))
row <- cal[meet_id == MEET]
if (!nrow(row)) cli::cli_abort("{.val {MEET}} is not in athletics_calendar.csv.")
COMPETITION_ID <- suppressWarnings(as.integer(row$wa_competition_id[1]))
if (is.na(COMPETITION_ID)) {
  cli::cli_abort("{.val {MEET}} has no wa_competition_id on the calendar -- look it up via athletics_calendar() and fill it in.")
}
cli::cli_alert_info("{MEET}: competition {COMPETITION_ID} ({row$name[1]}).")

gql <- function(q, v = NULL) {
  b <- list(query = q); if (!is.null(v)) b$variables <- v
  resp <- request(GQL_URL) |>
    req_headers(`content-type` = "application/json", `x-api-key` = WA_KEY,
                `x-amz-user-agent` = "aws-amplify/3.0.2") |>
    req_body_json(b, auto_unbox = TRUE) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
  if (resp_status(resp) != 200L) {
    cli::cli_abort("GraphQL returned HTTP {resp_status(resp)} -- refusing to build a field from an unconfirmed response.")
  }
  j <- resp_body_json(resp)
  if (!is.null(j$errors)) {
    for (e in j$errors) cli::cli_alert_danger("GraphQL error: {e$message}")
    cli::cli_abort("GraphQL reported errors -- see above.")
  }
  j$data
}

# --- the event list ---------------------------------------------------------
Q_EVENTS <- 'query Q($competitionId: Int!) { getChampionshipQualifications(competitionId: $competitionId) {
  events { eventId genderCode disciplineName } } }'
d <- gql(Q_EVENTS, list(competitionId = COMPETITION_ID))
ev <- rbindlist(lapply(d$getChampionshipQualifications$events, function(e) data.table(
  wa_event_id = e$eventId %||% NA_character_,
  sex = e$genderCode %||% NA_character_,
  disciplineName = e$disciplineName %||% NA_character_)), fill = TRUE)
cli::cli_alert_info("{nrow(ev)} event{?s} on the programme.")

n_relay <- ev[!sex %in% c("M", "W"), .N]
ev <- ev[sex %in% c("M", "W")]
if (n_relay) cli::cli_alert_info("Dropped {n_relay} relay/mixed event{?s} (relays are unmodelled, citius#1).")

# "Women's 100 Metres" -> discipline "100 Metres" + sex "W", which is the shape
# match_event() expects. The prefix is the ONLY place the sex is encoded in the
# discipline string, and genderCode already carries it, so stripping it is not
# discarding information.
ev[, event := trimws(sub("^(Men's|Women's|Mixed)\\s+", "", disciplineName))]
ev[, event_id := match_event(event, sex)]
if (ev[is.na(event_id), .N]) {
  print(ev[is.na(event_id), .(disciplineName, sex)])
  cli::cli_abort("{ev[is.na(event_id), .N]} event{?s} did not match the citius registry -- extend it rather than dropping them silently.")
}
cli::cli_alert_success("All {nrow(ev)} individual events matched the registry.")

# --- per-event qualification standings --------------------------------------
Q_QUAL <- 'query Q($competitionId: Int!, $eventId: Int) {
  getChampionshipQualifications(competitionId: $competitionId, eventId: $eventId) {
    eventId disciplineName entryNumber
    qualifications { qualified qualifiedBy qualificationPosition name urlSlug countryCode birthDate } } }'

rows <- list()
for (i in seq_len(nrow(ev))) {
  e <- ev[i]
  dd <- gql(Q_QUAL, list(competitionId = COMPETITION_ID,
                         eventId = as.integer(e$wa_event_id)))$getChampionshipQualifications
  q <- dd$qualifications %||% list()
  tb <- rbindlist(lapply(q, function(x) data.table(
    qualified = isTRUE(x$qualified),
    qualified_by = x$qualifiedBy %||% NA_character_,
    qual_position = x$qualificationPosition %||% NA_integer_,
    athlete = x$name %||% NA_character_,
    url_slug = x$urlSlug %||% NA_character_,
    country = x$countryCode %||% NA_character_,
    birth_date = x$birthDate %||% NA_character_)), fill = TRUE)
  n_in <- if (nrow(tb)) tb[qualified == TRUE, .N] else 0L
  cli::cli_alert_info("{e$disciplineName}: {n_in} qualified of {nrow(tb)} in contention (entryNumber {dd$entryNumber %||% NA}).")
  if (nrow(tb)) {
    tb[, `:=`(event = e$event, sex = e$sex, event_id = e$event_id,
              wa_event_id = e$wa_event_id, entry_number = dd$entryNumber %||% NA_integer_)]
    rows[[length(rows) + 1L]] <- tb
  }
  if (i < nrow(ev)) Sys.sleep(PAUSE)
}

all_rows <- rbindlist(rows, fill = TRUE)
q <- all_rows[qualified == TRUE]
cli::cli_alert_success("{nrow(q)} qualified slot{?s} across {uniqueN(q$event_id)} event{?s}.")

# --- ids, straight off the slug ---------------------------------------------
# The trailing numeric segment of urlSlug ("sweden/armand-duplantis-14679502")
# is the World Athletics athlete id -- the same namespace as the corpus. This
# is an extraction, not a match: nothing is being guessed, so a failure here is
# a real defect in the source, not an ambiguity to resolve.
q[, athlete_id := sub("^.*-(\\d+)$", "\\1", url_slug)]
bad <- q[is.na(url_slug) | !grepl("^\\d+$", athlete_id)]
if (nrow(bad)) {
  print(bad[, .(athlete, event, url_slug)])
  cli::cli_abort("{nrow(bad)} qualified athlete{?s} had no parseable id in urlSlug.")
}
cli::cli_alert_success("All {nrow(q)} ids parsed from urlSlug -- no name matching needed.")

dupe <- q[, .N, by = .(event_id, athlete_id)][N > 1]
if (nrow(dupe)) { print(dupe); cli::cli_abort("Duplicate athlete-event rows in the source field.") }

# --- write the pipeline's two artefacts -------------------------------------
entries <- q[, .(event, sex, athlete, country, qualified_by, qual_position, birth_date)]
setorder(entries, event, sex, qual_position, na.last = TRUE)
fwrite(entries, file.path(D, paste0(MEET, "_entries.csv")))

# Same columns resolve_diamond_league_athletes.R emits, so the downstream
# predict/sanity scripts read this identically for either meet. match_tier
# records HOW the id was obtained -- "official_id" is the honest value here and
# is deliberately distinguishable from Brussels' exact/reversed/fuzzy tiers.
# n_cand = 1: the source names exactly one athlete, so there is no collision to
# report; that is a fact about this source, not an unchecked assumption.
ids <- q[, .(row = .I, athlete, country, event, event_id,
             match_tier = "official_id", aid = athlete_id,
             n_ev = NA_integer_, n_all = NA_integer_, n_cand = 1L, athlete_id)]
fwrite(ids, file.path(D, paste0(MEET, "_athlete_ids.csv")))

cli::cli_alert_success("Wrote {MEET}_entries.csv ({nrow(entries)}) and {MEET}_athlete_ids.csv ({nrow(ids)}).")
print(q[, .(qualified = .N), by = .(event, sex)][order(sex, event)])
cli::cli_h3("Qualification routes")
print(q[, .N, by = qualified_by][order(-N)])
