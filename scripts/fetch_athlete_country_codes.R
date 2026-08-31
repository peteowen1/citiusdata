# Build/extend a durable athlete_id -> ISO country code lookup from World
# Athletics' own competitor records.
#
# WHY THIS EXISTS. A meet card needs each athlete's NATIONALITY, and the corpus
# does not carry it: championship_results.rds has venue_country (where a meet
# was HELD), which is a different fact entirely -- a Kenyan running in Zurich is
# not Swiss. Third-party entry lists give nationality as free text, and
# inconsistently: the Brussels source mixes bare codes ("USA") with full names
# ("Saint Lucia", "Trinidad and Tobago" -- 19 characters where the site renders
# a badge sized for three). So the code has to come from somewhere authoritative.
#
# The alternative considered and rejected was a hand-maintained name->code
# mapping. This repo's own convention (global CLAUDE.md, "Data & Analysis") is
# that when one entry of hand-maintained reference data is found wrong you must
# audit the ENTIRE set -- one error predicts siblings. A ~60-country table
# nobody re-checks is exactly that liability, and it rots silently as athletes
# transfer allegiance. This asks World Athletics for its own countryCode per
# athlete id, so it is a lookup of record rather than a guess.
#
# WHY THE GRAPHQL BULK ROUTE, NOT athletics_athlete_official_profile().
# The obvious implementation -- loop athletics_athlete_official_profile() over
# every id -- was written first and MEASURED, and it does not survive contact
# with the source: worldathletics.org soft-throttles after roughly 27
# consecutive profile-page requests, returning HTTP 202 with an EMPTY BODY
# (2026-08-31, killed a 249-athlete run at 25). That is a deliberately
# unhelpful response shape: not a 429, not an error status, just an accepted
# request with nothing in it.
#
# getMultipleCompetitors(ids: [Int!]!) on the GraphQL host answers the same
# question in BATCHES -- 249 athletes in ~5 requests instead of 249 -- against
# a different host that the page-scrape throttle does not apply to. Fewer
# requests is also simply the more courteous way to ask, which is the point:
# this is someone else's public infrastructure.
# athletics_athlete_official_profile() remains the right tool for ONE athlete
# (it needs no API key); this is the right tool for a field.
#
# THE API KEY IS CONFIGURATION, NOT A CONSTANT. docs/reference/harvesting.md
# records that World Athletics rotates its upstream key -- absorbing that
# rotation is part of why the nimarion wrapper exists at all. Hardcoding it
# here would mean a silent breakage the day it turns over, so it reads from an
# option/env with a failure message that says exactly what to do.
#
# BLOCKED IS NOT THE SAME AS ABSENT, and the difference is the whole point. A
# throttled batch and a genuinely unknown athlete both yield "no country code";
# recording the first as NA would permanently cache "this athlete has no
# nationality" for what was really a temporary block. So only ids the source
# actually answered for are written, ids it did not answer for are reported and
# left absent (so the next run retries them), and a run of failed batches stops
# rather than hammering through.
#
# Usage:  Rscript scripts/fetch_athlete_country_codes.R <meet_id> [more...]
#   e.g.  Rscript scripts/fetch_athlete_country_codes.R brussels2026
#         Rscript scripts/fetch_athlete_country_codes.R brussels2026 budapest2026

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(httr2))
D <- file.path(VERSE, "citiusdata", "data")

CACHE <- file.path(D, "athlete_country_codes.rds")
GQL_URL <- "https://graphql-prod-4881.edge.aws.worldathletics.org/graphql"
BATCH <- 50L
PAUSE <- 1.5           # between batches; ~5 requests total for a full field
MAX_CONSECUTIVE_FAIL <- 2L

WA_KEY <- getOption("citius.wa_graphql_key", Sys.getenv("CITIUS_WA_GRAPHQL_KEY", ""))
if (!nzchar(WA_KEY)) {
  cli::cli_abort(c(
    "No World Athletics GraphQL key configured.",
    i = "Set {.envvar CITIUS_WA_GRAPHQL_KEY} or {.code options(citius.wa_graphql_key=)}.",
    i = "The key is the x-api-key the worldathletics.org front end sends; it ROTATES (see docs/reference/harvesting.md), so it is not baked in here."
  ))
}

args <- commandArgs(trailingOnly = TRUE)
MEETS <- if (length(args)) args else Sys.getenv("CITIUS_CC_MEETS", "brussels2026")

# --- who do we need? --------------------------------------------------------
want <- unique(unlist(lapply(MEETS, function(m) {
  f <- file.path(D, paste0(m, "_athlete_ids.csv"))
  if (!file.exists(f)) {
    cli::cli_alert_warning("No id file for {.val {m}} at {.file {basename(f)}}; skipping.")
    return(character(0))
  }
  ids <- fread(f)
  as.character(ids[!is.na(athlete_id)]$athlete_id)
})))
want <- want[!is.na(want) & nzchar(want)]
if (!length(want)) cli::cli_abort("No athlete ids found across {.val {MEETS}}.")

cache <- if (file.exists(CACHE)) setDT(readRDS(CACHE)) else
  data.table(athlete_id = character(), country_code = character(),
             country_name = character(), fetched_at = as.POSIXct(character()))
cache[, athlete_id := as.character(athlete_id)]

todo <- setdiff(want, cache$athlete_id)
cli::cli_alert_info("{length(want)} athlete{?s} wanted across {length(MEETS)} meet{?s}; {nrow(cache)} cached; {length(todo)} to fetch.")
if (!length(todo)) {
  cli::cli_alert_success("Nothing to fetch -- cache already covers every athlete.")
  quit(save = "no")
}

Q <- 'query Q($ids: [Int!]!) { getMultipleCompetitors(ids: $ids) {
  basicData { aaId givenName familyName countryCode countryFullName } } }'

fetch_batch <- function(ids) {
  resp <- tryCatch(
    request(GQL_URL) |>
      req_headers(`content-type` = "application/json", `x-api-key` = WA_KEY,
                  `x-amz-user-agent` = "aws-amplify/3.0.2") |>
      req_body_json(list(query = Q, variables = list(ids = as.integer(ids))),
                    auto_unbox = TRUE) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform(),
    error = function(e) { cli::cli_alert_warning("batch request errored: {conditionMessage(e)}"); NULL })
  if (is.null(resp)) return(NULL)
  if (resp_status(resp) != 200L) {
    cli::cli_alert_warning("batch returned HTTP {resp_status(resp)}.")
    return(NULL)
  }
  j <- tryCatch(resp_body_json(resp), error = function(e) NULL)
  if (is.null(j)) { cli::cli_alert_warning("batch body was not parseable JSON."); return(NULL) }
  if (!is.null(j$errors)) {
    for (e in j$errors) cli::cli_alert_warning("GraphQL error: {e$message}")
    return(NULL)
  }
  d <- j$data$getMultipleCompetitors
  if (is.null(d)) return(NULL)
  rbindlist(lapply(d, function(x) {
    b <- x$basicData
    if (is.null(b) || is.null(b$aaId)) return(NULL)
    data.table(athlete_id = as.character(b$aaId),
               country_code = b$countryCode %||% NA_character_,
               country_name = b$countryFullName %||% NA_character_,
               fetched_at = Sys.time())
  }), fill = TRUE)
}

chunks <- split(todo, ceiling(seq_along(todo) / BATCH))
got <- list(); consecutive_fail <- 0L; n_fail <- 0L
for (i in seq_along(chunks)) {
  b <- fetch_batch(chunks[[i]])
  if (is.null(b)) {
    n_fail <- n_fail + 1L
    consecutive_fail <- consecutive_fail + 1L
    cli::cli_alert_warning("batch {i}/{length(chunks)} UNCONFIRMED ({length(chunks[[i]])} ids left unrecorded, will retry next run).")
    if (consecutive_fail >= MAX_CONSECUTIVE_FAIL) {
      cli::cli_alert_danger("{consecutive_fail} consecutive failed batches -- treating as throttling and STOPPING.")
      break
    }
  } else {
    consecutive_fail <- 0L
    got[[length(got) + 1L]] <- b
    cli::cli_alert_info("batch {i}/{length(chunks)}: asked {length(chunks[[i]])}, got {nrow(b)}.")
  }
  if (i < length(chunks)) Sys.sleep(PAUSE)
}

new <- if (length(got)) rbindlist(got, fill = TRUE) else data.table()
if (nrow(new)) {
  cache <- unique(rbind(cache, new, fill = TRUE), by = "athlete_id")
  saveRDS(cache, CACHE)
}

cli::cli_h2("Country codes")
cli::cli_alert_success("Cache holds {nrow(cache)} athlete{?s} -> {basename(CACHE)}")
cli::cli_alert_info("This run: {nrow(new)} newly recorded across {length(got)} good batch{?es}, {n_fail} failed batch{?es}.")

still <- setdiff(want, cache$athlete_id)
if (length(still)) {
  # NOT recorded as "no country" -- an id the source did not answer for is
  # unknown, not absent, and conflating those is the exact failure this script's
  # header is about.
  cli::cli_alert_warning("{length(still)} wanted athlete{?s} still have NO cache entry (unanswered, not 'no country'):")
  print(utils::head(still, 30))
} else {
  cli::cli_alert_success("Every wanted athlete has a cache entry.")
}
n_na <- cache[athlete_id %in% want & is.na(country_code), .N]
if (n_na) cli::cli_alert_warning("{n_na} wanted athlete{?s} answered but carry NO country code.")
