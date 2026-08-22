# Does World Athletics serve rankings AS THEY STOOD on a past date?
#
# THE DISCOVERY THIS TESTS. Archived ranking URLs carry an explicit `rankDate`
# parameter - e.g. .../world-rankings/10000m/women?regionType=world&page=1&
# rankDate=2022-01-04. That is a parameter on the LIVE site, not an artefact of
# archiving. If it still works, historical rankings are directly fetchable for
# any past date and the whole "capture weekly or lose it forever" framing is
# wrong: a walk-forward WA benchmark becomes available this week rather than
# next season.
#
# WHY IT MATTERS. athlete_wa_rankings.parquet is an undated snapshot, so scoring
# it as a predictor is leakage - a rolling 12-18 month ranking already contains
# the races being predicted. A dated ranking fixes that outright, and WA's own
# ranking is the benchmark anyone outside this project would ask about first.
#
# POLITE: three requests, spaced. This only establishes whether the door opens.
suppressMessages(library(data.table))

try_url <- function(u, label) {
  r <- tryCatch(httr::GET(u, httr::timeout(45),
                          httr::user_agent("citiusverse research (contact via github.com/peteowen1)")),
                error = function(e) NULL)
  if (is.null(r)) { cat(sprintf("%-34s no response\n", label)); return(NULL) }
  code <- httr::status_code(r)
  txt <- tryCatch(httr::content(r, "text", encoding = "UTF-8"), error = function(e) "")
  # a ranking table should mention places and athlete names; a soft-404 or a JS
  # shell will be short and mention neither
  has_table <- grepl("rankingTable|world-rankings|<table", txt, ignore.case = TRUE)
  cat(sprintf("%-34s HTTP %s | %s chars | looks like a table: %s\n",
              label, code, format(nchar(txt), big.mark = ","), has_table))
  invisible(txt)
}

base <- "https://worldathletics.org/world-rankings/100m/men"
cat("=== does rankDate change what comes back? ===\n")
a <- try_url(paste0(base, "?regionType=world&page=1&rankDate=2024-06-30&limitByCountry=0"),
             "rankDate=2024-06-30")
Sys.sleep(3)
b <- try_url(paste0(base, "?regionType=world&page=1&rankDate=2022-01-04&limitByCountry=0"),
             "rankDate=2022-01-04")
Sys.sleep(3)
c0 <- try_url(paste0(base, "?regionType=world&page=1&limitByCountry=0"), "no rankDate (current)")

# THE TEST THAT DECIDES IT. Two different dates must return DIFFERENT content.
# Identical bodies would mean the parameter is ignored and the site is serving
# today's ranking whatever is asked for - which would look like success and be
# worthless, the same shape as every other identical-output trap this project
# has hit.
cat("\n=== are the two dates actually different? ===\n")
if (!is.null(a) && !is.null(b)) {
  cat(sprintf("2024 body %s chars, 2022 body %s chars, identical: %s\n",
              format(nchar(a), big.mark = ","), format(nchar(b), big.mark = ","),
              identical(a, b)))
  if (identical(a, b))
    cat("IDENTICAL - rankDate is being ignored. This route does not work.\n")
  else
    cat("DIFFERENT - the parameter is honoured. Backfill is possible.\n")
} else cat("could not fetch both dates\n")

cat("\nIf this works, the next question is whether the rows are parseable and\n")
cat("whether athlete IDs are present - a ranking without ids cannot be joined\n")
cat("to our athletes and would need name matching, which this project already\n")
cat("knows is where silent errors live.\n")
