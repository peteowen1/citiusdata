# Discover World Athletics' CURRENT GraphQL edge and client key.
#
#   Rscript citiusdata/scripts/discover_wa_endpoint.R
#
# WHY THIS EXISTS. Both of our athletics routes depend on a pair of values that
# World Athletics rotates without notice:
#
#   * the CloudFront edge number -- `graphql-prod-<N>.edge.aws.worldathletics.org`
#   * the AppSync client key     -- `da2-...`
#
# 4881 was retired on 2026-09-09. 4883 replaced it, then stopped resolving in DNS
# entirely by 2026-09-12, at which point 4888 was live. Chasing that by hand costs
# a Chrome session each time, and the community mirror
# (`worldathletics.nimarion.de`) goes down with us because it absorbs the same
# pair.
#
# Neither value is a secret. Both are compiled into the public Next.js bundle
# that every visitor's browser downloads, because the browser has to make the
# same call we do. So the durable fix is not to store the key -- a stored key is
# wrong the moment WA rotates -- but to READ IT FRESH from the bundle each run.
#
# HOW. Fetch any results page, list its `/_next/static/chunks/*.js`, and scan for
# the two patterns. One chunk carries both (2026-09-12: the chunk beginning
# `2e654d84`, but the hash changes on every WA deploy, so we scan rather than
# hardcode).
#
# THE BUNDLE CONTAINS MORE THAN ONE `da2-` KEY, and only one of them is the
# results key. Take the one that sits in the SAME chunk as the edge hostname,
# which is what the loop below does. A first pass at this on 2026-09-12 pulled
# the keys out with `grep -oE | sort -u | head -1`, which returns the
# alphabetically-first key rather than the live one; every request with it came
# back as CloudFront's "Lambda function is invalid" 503, which reads exactly like
# a retired edge. That cost an hour and very nearly got written up as "WA now
# blocks non-browser clients". It does not: with the right key R gets HTTP 200.
# The reachability probe at the bottom exists so that mistake cannot be made
# silently again -- if the pair does not answer, this script says so.
#
# The nimarion mirror's own 500s (every competition, 2026-09-12) are consistent
# with it simply holding a stale pair.

suppressMessages({
  library(httr2)
  library(cli)
})

UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
            "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36")

# Any results page will do; this one is only a vehicle for the script tags.
PAGE <- getOption(
  "citius.wa_bundle_page",
  "https://worldathletics.org/competition/calendar-results/results/7212925")

cli_h1("World Athletics endpoint discovery")

html <- tryCatch(
  request(PAGE) |>
    req_user_agent(UA) |>
    req_timeout(45) |>
    req_perform() |>
    resp_body_string(),
  error = function(e) cli_abort(c("Could not fetch {.url {PAGE}}.", x = conditionMessage(e)))
)
cli_alert_info("Fetched {.url {PAGE}} ({format(nchar(html), big.mark = ',')} chars).")

chunks <- unique(regmatches(html, gregexpr("/_next/static/[^\"']+\\.js", html))[[1]])
if (!length(chunks)) {
  cli_abort(c("No {.path /_next/static/*.js} chunks in the page.",
              i = "WA has changed its build layout; this scan needs rewriting."))
}
cli_alert_info("{length(chunks)} script chunk{?s} to scan.")

KEY_RE  <- "da2-[a-z0-9]{26}"
EDGE_RE <- "graphql-prod-[0-9]+"

key <- NA_character_; edge <- NA_character_; found_in <- NA_character_

for (ch in chunks) {
  body <- tryCatch(
    request(paste0("https://worldathletics.org", ch)) |>
      req_user_agent(UA) |> req_timeout(30) |> req_perform() |> resp_body_string(),
    error = function(e) NULL
  )
  if (is.null(body)) next
  k <- regmatches(body, regexpr(KEY_RE, body))
  e <- regmatches(body, regexpr(EDGE_RE, body))
  if (length(k) && length(e)) { key <- k; edge <- e; found_in <- ch; break }
}

if (is.na(key) || is.na(edge)) {
  cli_abort(c("Scanned {length(chunks)} chunk{?s} and found no key/edge pair.",
              i = "Either WA moved the config, or the regexes are stale:",
              i = "key {.code {KEY_RE}}, edge {.code {EDGE_RE}}."))
}

url <- sprintf("https://%s.edge.aws.worldathletics.org/graphql", edge)
cli_alert_success("Found in {.path {basename(found_in)}}")
cli_alert_info("edge {.val {edge}}")
cli_alert_info("url  {.url {url}}")
# The key is public, but printing it in full invites it being pasted into a
# secret store, which is exactly the habit this script exists to break.
cli_alert_info("key  {.val {paste0(substr(key, 1, 8), strrep('.', 14), substr(key, 27, 30))}} (read fresh; do not store)")

# --- does it actually answer? -------------------------------------------------
cli_h2("Reachability")
probe <- tryCatch(
  request(url) |>
    req_headers(accept = "*/*", `content-type` = "application/json",
                `x-api-key` = key, `x-amz-user-agent` = "aws-amplify/3.0.7") |>
    req_body_raw('{"query":"query{__typename}"}', type = "application/json") |>
    req_user_agent(UA) |> req_timeout(30) |> req_error(is_error = function(r) FALSE) |>
    req_perform(),
  error = function(e) NULL
)

status <- if (is.null(probe)) NA_integer_ else resp_status(probe)
if (!is.na(status) && status == 200) {
  cli_alert_success("HTTP 200 - the pair works from R. Set it and harvest:")
  cli_code(sprintf('options(citius.wa_graphql_url = "%s", citius.wa_graphql_key = <key>)', url))
} else {
  cli_alert_danger("HTTP {if (is.na(status)) 'no response' else status} - the pair did not answer.")
  cli_alert_info("A 503 here is most often the WRONG KEY, not a dead edge: the
                  bundle carries several {.code da2-} values and only the one
                  paired with this hostname works. Check the chunk scan picked
                  the right chunk before concluding WA has changed anything.")
}

invisible(list(edge = edge, url = url, key = key, status = status))
