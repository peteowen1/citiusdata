# Is there an ARCHIVE of World Athletics rankings, and is it usable?
#
# WHY THIS MATTERS. The profile feed gives `currentWorldRankings` with no date,
# so athlete_wa_rankings.parquet is a snapshot and cannot be used as a
# walk-forward predictor - a rolling 12-18 month ranking already reflects the
# races we would be predicting. The question is whether that is fixable by
# backfill or only by capturing from now on. Those imply very different plans:
# a backfill gives seven seasons immediately, capturing gives a usable series in
# about six months.
#
# A first pass found 400 archived snapshots under worldathletics.org/world-
# rankings*, spanning 2019-11 to 2026-07 at roughly 60-70 a year. That is the
# encouraging headline. This checks whether it survives contact: WHICH urls are
# archived, whether individual EVENTS are covered or only the landing page, and
# whether a fetched snapshot actually contains ranking rows.
#
# The CDX API rate-limits and answers with an HTML error page rather than JSON,
# which parses as a lexical error and reads like a bug in this script. Retries
# and reports refusal plainly instead.
suppressMessages(library(data.table))
CDX <- "http://web.archive.org/cdx/search/cdx"

get_cdx <- function(url, extra = "") {
  u <- paste0(CDX, "?url=", utils::URLencode(url, TRUE), extra)
  for (i in 1:3) {
    r <- tryCatch(httr::GET(u, httr::timeout(60)), error = function(e) NULL)
    if (!is.null(r) && httr::status_code(r) == 200) {
      txt <- httr::content(r, "text", encoding = "UTF-8")
      if (!startsWith(trimws(txt), "<")) return(txt)
    }
    Sys.sleep(10)
  }
  NULL
}

cat("=== which ranking URLs are archived at all? ===\n")
txt <- get_cdx("worldathletics.org/world-rankings*",
               "&fl=original&collapse=urlkey&limit=400")
if (is.null(txt)) {
  cat("CDX refused or rate-limited. Re-run later; this is not a negative result.\n")
} else {
  u <- unique(trimws(strsplit(txt, "\n")[[1]])); u <- u[nzchar(u)]
  cat(sprintf("distinct URLs: %d\n", length(u)))
  print(utils::head(sort(u), 20))
  # Does any of them name an EVENT, or is it only the landing page? That is the
  # difference between a usable backfill and a curiosity.
  ev <- grep("100|200|400|800|1500|5000|10000|marathon|jump|throw|hurdl",
             u, ignore.case = TRUE, value = TRUE)
  cat(sprintf("\nURLs naming a specific event: %d of %d\n", length(ev), length(u)))
  if (length(ev)) print(utils::head(sort(ev), 12))
}

cat("\n=== how often is the most-archived ranking URL captured? ===\n")
Sys.sleep(5)
txt2 <- get_cdx("worldathletics.org/world-rankings",
                "&fl=timestamp&collapse=timestamp:8&limit=500")
if (is.null(txt2)) {
  cat("CDX refused on the second query.\n")
} else {
  ts <- unique(trimws(strsplit(txt2, "\n")[[1]])); ts <- ts[nzchar(ts)]
  d <- substr(ts, 1, 8)
  cat(sprintf("dated captures: %d, %s .. %s\n", length(d), min(d), max(d)))
  print(table(substr(d, 1, 4)))
  cat("\nGaps between consecutive captures (days), summary:\n")
  dd <- sort(as.Date(d, "%Y%m%d"))
  if (length(dd) > 2) print(summary(as.numeric(diff(dd))))
}

cat("\nWHAT WOULD MAKE THIS USABLE: event-level URLs captured at a known date,\n")
cat("with parseable ranking rows inside. A landing page captured weekly is not\n")
cat("enough - it carries no per-athlete places. Check a real snapshot before\n")
cat("planning any backfill on this.\n")
