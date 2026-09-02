# FIND THE STEEPLECHASE SLUG INSIDE A PAGE THAT ALREADY WORKS.
#
# Two dead ends so far: seven guessed spellings all returned the empty shell,
# and the rankings index exposes only one event as a plain link because its
# selector is rendered client-side. So the previous probe's "the site does not
# publish these" list was an artefact of my own extraction - we have already
# harvested 19 of those events, so they obviously exist. Recording that because
# a wrong method that produces a confident-looking list is worse than no list.
#
# A working ranking page must carry the other events somewhere, in the selector
# data if not in the markup. Search its raw text for anything steeplechase-like
# and print the surrounding characters, rather than testing one more guess.
suppressMessages(library(data.table))
UA <- "citiusverse research (github.com/peteowen1)"
U  <- paste0("https://worldathletics.org/world-rankings/100m/men",
             "?regionType=world&page=1&rankDate=2024-07-23&limitByCountry=0")

r <- tryCatch(httr::GET(U, httr::timeout(45), httr::user_agent(UA)),
              error = function(e) NULL)
stopifnot("could not fetch a working ranking page" =
            !is.null(r) && httr::status_code(r) == 200)
txt <- httr::content(r, "text", encoding = "UTF-8")
cat(sprintf("working page: %s chars\n", format(nchar(txt), big.mark = ",")))

show <- function(label, pat) {
  m <- gregexpr(pat, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (m[1] == -1) { cat(sprintf("\n%s: no match\n", label)); return(invisible()) }
  cat(sprintf("\n%s: %d match(es), first few in context:\n", label, length(m)))
  for (i in head(seq_along(m), 6)) {
    a <- max(1, m[i] - 70); b <- min(nchar(txt), m[i] + attr(m, "match.length")[i] + 70)
    cat("  ...", gsub("[[:space:]]+", " ", substr(txt, a, b)), "...\n", sep = "")
  }
}

show("literal 'steeple'", "steeple")
show("any 3000-ish token", "3000[a-zA-Z-]{0,14}")
# the selector may hold every event as url-ish strings
show("world-rankings paths", "world-rankings/[a-zA-Z0-9-]{2,30}")
