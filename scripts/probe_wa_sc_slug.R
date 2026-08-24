# WHICH URL SLUG DOES WORLD ATHLETICS USE FOR THE STEEPLECHASE?
#
# The dated-ranking harvest asked for "3000mSC" and got nothing - those were the
# 8 failed requests, and steeplechase is the one event of 20 missing from the
# benchmark. A wrong slug returns HTTP 200 with an empty table, exactly like an
# unpublished date, so it cannot be told from a calendar miss without probing.
#
# Try the plausible spellings against a date already known to be published, so
# an empty result means the slug is wrong rather than the date.
suppressMessages(library(data.table))
UA   <- "citiusverse research (github.com/peteowen1)"
WHEN <- "2024-07-23"   # known good: the Paris harvest resolved to this date
CAND <- c("3000mSC", "3000m-steeplechase", "3000msc", "3000m-sc",
          "3000-metres-steeplechase", "steeplechase", "3000mst")

for (s in CAND) {
  u <- sprintf("https://worldathletics.org/world-rankings/%s/men?regionType=world&page=1&rankDate=%s&limitByCountry=0",
               s, WHEN)
  r <- tryCatch(httr::GET(u, httr::timeout(45), httr::user_agent(UA)),
                error = function(e) NULL)
  if (is.null(r)) { cat(sprintf("%-26s request failed\n", s)); Sys.sleep(2); next }
  txt <- httr::content(r, "text", encoding = "UTF-8")
  n <- length(unique(regmatches(txt,
        gregexpr("/athletes/[a-z-]+/[a-z0-9-]+-[0-9]{6,}", txt))[[1]]))
  cat(sprintf("%-26s HTTP %s | %6s chars | %3d athlete links%s\n",
              s, httr::status_code(r), format(nchar(txt), big.mark = ","), n,
              if (n >= 5) "   <-- WORKS" else ""))
  Sys.sleep(2)
}
cat("\nA slug with 200 and zero links is wrong, not empty - the site returns a\n")
cat("full page shell either way.\n")
