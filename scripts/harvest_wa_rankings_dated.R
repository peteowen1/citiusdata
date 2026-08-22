# Fetch World Athletics' own ranking AS IT STOOD on a given date.
#
# WHY THIS IS POSSIBLE AT ALL. The profile feed gives `currentWorldRankings`
# with no date, so athlete_wa_rankings.parquet is an undated snapshot and using
# it as a predictor is leakage - a rolling 12-18 month ranking already contains
# the races being predicted. That looked unfixable. It is not: the ranking pages
# take a `rankDate` parameter, verified honoured on the live site (two dates
# return different bodies, 519,021 against 514,908 chars), and each row carries
# the athlete's numeric WA id in its profile link. 84% of the ids on a sample
# page are athletes we already hold, so this joins by ID and needs no name
# matching - which is where this project's silent errors live.
#
# WHAT IT IS FOR. The outside check the model has never had: does the rating
# order a field better than World Athletics' own published ranking, using the
# ranking as it stood BEFORE the race? Every other benchmark here is something
# we computed ourselves.
#
# SCOPE DELIBERATELY SMALL. 84 events x 2 sexes x weekly x 7 years is tens of
# thousands of requests for a question that only matters where the fields are
# strong. Fetch one date shortly before each major championship instead - a few
# dozen requests, aimed at exactly the races the model is judged on.
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D     <- here::here("citiusdata", "data")
OUT   <- file.path(D, "wa_rankings_dated.parquet")
PAUSE <- .env_num("WA_RANK_PAUSE", "2.0")     # polite; this is a public website
PAGES <- .env_int("WA_RANK_PAGES", "2")       # 100 athletes per page
UA    <- "citiusverse research (github.com/peteowen1)"

# WA's own event slugs. Deliberately explicit rather than derived: our event_ids
# do not map onto their url slugs by any rule, and guessing would produce 404s
# that look like missing data.
EVENTS <- data.table(
  slug = c("100m","200m","400m","800m","1500m","5000m","10000m","marathon",
           "110mh","100mh","400mh","3000mSC","high-jump","pole-vault",
           "long-jump","triple-jump","shot-put","discus-throw","hammer-throw",
           "javelin-throw"),
  sexes = c("men,women","men,women","men,women","men,women","men,women","men,women",
            "men,women","men,women","men","women","men,women","men,women",
            "men,women","men,women","men,women","men,women","men,women",
            "men,women","men,women","men,women"))

fetch_one <- function(slug, sex, rank_date, page) {
  u <- sprintf("https://worldathletics.org/world-rankings/%s/%s?regionType=world&page=%d&rankDate=%s&limitByCountry=0",
               slug, sex, page, rank_date)
  r <- tryCatch(httr::GET(u, httr::timeout(45), httr::user_agent(UA)), error = function(e) NULL)
  if (is.null(r) || httr::status_code(r) != 200) return(NULL)
  txt <- httr::content(r, "text", encoding = "UTF-8")
  # the id lives in the profile link; place is the row order on the page, which
  # is what `page` and the ordering preserve
  links <- regmatches(txt, gregexpr("/athletes/[a-z-]+/[a-z0-9-]+-[0-9]{6,}", txt))[[1]]
  # AN EMPTY TABLE IS A FAILURE, not an empty ranking. A valid page carries ~100
  # athlete links; an unpublished date carries zero and still returns HTTP 200.
  if (length(links) < 5) return(NULL)
  links <- unique(links)
  data.table(athlete_id = sub(".*-([0-9]{6,})$", "\\1", links),
             wa_place = seq_along(links) + (page - 1L) * 100L,
             event_slug = slug, sex = sex, rank_date = as.Date(rank_date))
}

# ---- VALID RANKING DATES ARE A PUBLISHED LIST, NOT A WEEKDAY RULE ----------
#
# An invalid rankDate returns HTTP 200 with an EMPTY table - 364,423 chars and
# zero athlete links, against 519,021 and 100 for a valid one. Nothing errors.
# Measured on 100m men: 2024-06-30, -07-09, -07-16 and -07-23 all return rows;
# -07-02 and -07-30 return none. Three of those are Tuesdays and two are not, so
# there is no weekday rule to derive - and note 2024-07-30 falls DURING Paris,
# so rankings appear to pause for championships, which is exactly when we most
# want them. Asking for a championship-eve date will therefore often miss.
#
# So: probe backwards from the requested date until a page actually returns
# rows, and report which date was really used. Silently accepting the empty page
# would produce a file full of dates with no athletes and a coverage figure that
# looks like a scraping problem rather than a calendar one.
nearest_valid <- function(want, back = 21L) {
  d0 <- as.Date(want)
  for (k in 0:back) {
    d <- d0 - k
    x <- fetch_one("100m", "men", as.character(d), 1L)
    Sys.sleep(PAUSE)
    if (!is.null(x) && nrow(x) > 20) {
      if (k > 0) cat(sprintf("  %s has no published ranking; using %s (%d days earlier)%s",
                             want, d, k, "\n"))
      return(as.character(d))
    }
  }
  cat(sprintf("  no published ranking within %d days before %s%s", back, want, "\n"))
  NA_character_
}

DATES <- strsplit(Sys.getenv("WA_RANK_DATES", ""), ",")[[1]]
if (!length(DATES) || !nzchar(DATES[1])) {
  cat("Set WA_RANK_DATES to one or more YYYY-MM-DD values.\n")
  cat("Championship-eve dates are the useful ones - the ranking as it stood\n")
  cat("going into the meet the model is judged on. Example:\n")
  cat("  WA_RANK_DATES=2024-07-30,2025-09-09 Rscript citiusdata/scripts/harvest_wa_rankings_dated.R\n")
  quit(status = 0)
}
cat(sprintf("dates: %s | events: %d | pages each: %d\n",
            paste(DATES, collapse = ", "), nrow(EVENTS), PAGES))

acc <- list(); n_fail <- 0L
# resolve each requested date to one that actually has a ranking, once, before
# spending dozens of requests per date on a calendar that has no data
cat("resolving requested dates to published ranking dates:\n")
DATES <- unique(stats::na.omit(vapply(DATES, nearest_valid, character(1))))
stopifnot("none of the requested dates resolved to a published ranking" = length(DATES) > 0)
cat(sprintf("using: %s%s", paste(DATES, collapse = ", "), "\n"))

for (dt in DATES) for (i in seq_len(nrow(EVENTS))) {
  for (sx in strsplit(EVENTS$sexes[i], ",")[[1]]) for (pg in seq_len(PAGES)) {
    x <- fetch_one(EVENTS$slug[i], sx, dt, pg)
    if (is.null(x)) { n_fail <- n_fail + 1L } else acc[[length(acc) + 1L]] <- x
    Sys.sleep(PAUSE)
  }
  cat(sprintf("  %s %-14s done\n", dt, EVENTS$slug[i]))
}
stopifnot("every request failed - check the site before re-running" = length(acc) > 0)
res <- rbindlist(acc, fill = TRUE)
# THE DATE IS THE POINT. A row without one is useless and must never reach the file.
stopifnot("rows arrived without a rank_date" = res[is.na(rank_date), .N] == 0)
res <- unique(res, by = c("athlete_id", "event_slug", "sex", "rank_date"))

if (file.exists(OUT)) {
  old <- setDT(arrow::read_parquet(OUT))
  old <- old[!paste(rank_date, event_slug, sex) %chin%
               res[, unique(paste(rank_date, event_slug, sex))]]
  res <- rbindlist(list(old, res), use.names = TRUE, fill = TRUE)
}
arrow::write_parquet(res, OUT)
cat(sprintf("\nwrote %s: %s rows | %s dates | %s events | %s athletes | %d failed requests\n",
            basename(OUT), format(nrow(res), big.mark = ","),
            res[, uniqueN(rank_date)], res[, uniqueN(event_slug)],
            format(res[, uniqueN(athlete_id)], big.mark = ","), n_fail))
cat("\nNOTE: wa_place here is the row position on the page, which is the ranking\n")
cat("order. It is not read from a place column, because the page does not expose\n")
cat("one in the HTML - so a change to the page's ordering would corrupt this\n")
cat("silently. Spot-check a known athlete before trusting a new vintage.\n")
