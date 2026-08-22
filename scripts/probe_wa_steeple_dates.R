# IS THE STEEPLECHASE SLUG WRONG, OR JUST UNPUBLISHED ON THE DATE I TRIED?
#
# The site's own selector JSON gives disciplineNameUrlSlug
# "3000-metres-steeplechase". That spelling was in the earlier guess list and
# returned the empty shell against 2024-07-23 - so either the ranking slug
# differs from the results slug, or the steeplechase ranking was simply not
# published on that date. A single date cannot tell those apart, which is the
# same mistake as reading an unpublished date as a scraping failure.
#
# So cross a small set of candidate slugs against several dates known to publish
# rankings for the 100m. A slug that returns rows on ANY date is the right slug.
suppressMessages(library(data.table))
UA    <- "citiusverse research (github.com/peteowen1)"
PAUSE <- 2

CAND  <- c("3000-metres-steeplechase", "3000m-steeplechase", "3000mSC")
DATES <- c("2024-07-23", "2025-09-02", "2024-06-30", "2023-08-15")

n_links <- function(slug, sex, when) {
  u <- sprintf("https://worldathletics.org/world-rankings/%s/%s?regionType=world&page=1&rankDate=%s&limitByCountry=0",
               slug, sex, when)
  r <- tryCatch(httr::GET(u, httr::timeout(45), httr::user_agent(UA)),
                error = function(e) NULL)
  Sys.sleep(PAUSE)
  if (is.null(r) || httr::status_code(r) != 200) return(NA_integer_)
  txt <- httr::content(r, "text", encoding = "UTF-8")
  length(unique(regmatches(txt,
    gregexpr("/athletes/[a-z-]+/[a-z0-9-]+-[0-9]{6,}", txt))[[1]]))
}

# CONTROL FIRST. If 100m does not return rows on a date, that date is
# unpublished and a zero for the steeplechase there says nothing. Without this
# the whole table is uninterpretable.
cat("control - 100m men, to establish which dates are published at all:\n")
ok <- vapply(DATES, function(d) n_links("100m", "men", d), integer(1))
for (i in seq_along(DATES))
  cat(sprintf("  %s  %3d links%s\n", DATES[i], ok[i],
              if (isTRUE(ok[i] >= 5)) "" else "   <-- date unpublished, ignore its column"))
live <- DATES[!is.na(ok) & ok >= 5]
stopifnot("no control date returned rows - the site or the parser changed" =
            length(live) > 0)

cat("\nsteeplechase candidates, published dates only:\n")
res <- rbindlist(lapply(CAND, function(s) {
  v <- vapply(live, function(d) n_links(s, "men", d), integer(1))
  data.table(slug = s, date = live, links = v)
}))
print(dcast(res, slug ~ date, value.var = "links"))

win <- res[!is.na(links) & links >= 5, unique(slug)]
if (length(win)) {
  cat(sprintf("\nWORKING SLUG: %s\n", paste(win, collapse = ", ")))
} else {
  cat("\nNo candidate returned rows on any published date. The steeplechase\n")
  cat("ranking is then either not published as a world ranking at all, or lives\n")
  cat("under a slug not guessable from the results selector - do not add it to\n")
  cat("the harvester on a guess, and record the benchmark as 19 of 20 events.\n")
}
