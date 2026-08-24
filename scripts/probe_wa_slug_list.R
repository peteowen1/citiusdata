# ASK THE SITE WHICH RANKING SLUGS EXIST, instead of guessing them.
#
# Seven plausible spellings of the steeplechase slug all returned the empty page
# shell (364,4xx chars, zero athlete links) against a date known to be
# published. Guessing an eighth is not a method: a wrong slug and an unpublished
# date look identical, so a guess that fails teaches nothing.
#
# The rankings landing page links to every event it publishes. Extract the slugs
# from it and diff against the ones the harvester asks for - that turns "which
# spelling?" into a list, and also catches any OTHER event we are silently
# missing for the same reason.
suppressMessages(library(data.table))
UA <- "citiusverse research (github.com/peteowen1)"

r <- tryCatch(httr::GET("https://worldathletics.org/world-rankings",
                        httr::timeout(45), httr::user_agent(UA)),
              error = function(e) NULL)
stopifnot("could not fetch the rankings index" =
            !is.null(r) && httr::status_code(r) == 200)
txt <- httr::content(r, "text", encoding = "UTF-8")
cat(sprintf("index page: %s chars\n", format(nchar(txt), big.mark = ",")))

hits <- regmatches(txt, gregexpr('/world-rankings/[a-zA-Z0-9-]+/(men|women)', txt))[[1]]
slugs <- sort(unique(sub('^/world-rankings/([a-zA-Z0-9-]+)/(men|women)$', '\\1', hits)))
cat(sprintf("\n%d distinct event slugs published on the index:\n", length(slugs)))
print(slugs)

ASKED <- c("100m","200m","400m","800m","1500m","5000m","10000m","marathon",
           "110mh","100mh","400mh","3000mSC","high-jump","pole-vault",
           "long-jump","triple-jump","shot-put","discus-throw","hammer-throw",
           "javelin-throw")
cat("\n=== slugs the harvester asks for that the site does NOT publish ===\n")
print(setdiff(ASKED, slugs))
cat("\n=== slugs the site publishes that the harvester does NOT ask for ===\n")
print(setdiff(slugs, ASKED))
cat("\nThe second list is the one to read carefully: anything in it is an event\n")
cat("we could benchmark and currently do not, not just the steeplechase.\n")
