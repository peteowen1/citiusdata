# Can a dated ranking page be parsed, and does it carry ATHLETE IDS?
#
# rankDate is honoured (probe_wa_rankdate.R), so historical rankings are
# fetchable. Feasibility now turns on one thing: whether each row carries an
# athlete id we can join on.
#
# WHY THE ID IS THE WHOLE QUESTION. Without it we would join on names, and this
# project has already been bitten there - exact-name merging fused different
# British athletes, and "RAK" matched Marrakesh in a venue match. A name-joined
# ranking would attach the wrong place to a few hundred athletes and NOTHING
# would look wrong: the file would be full, the coverage percentage healthy, and
# the benchmark quietly measuring a mixture of people. So this reports the id
# situation plainly rather than reporting a row count and moving on.
suppressMessages(library(data.table))
U <- paste0("https://worldathletics.org/world-rankings/100m/men",
            "?regionType=world&page=1&rankDate=2024-06-30&limitByCountry=0")

r <- tryCatch(httr::GET(U, httr::timeout(45),
                        httr::user_agent("citiusverse research (github.com/peteowen1)")),
              error = function(e) NULL)
stopifnot("could not fetch the page" = !is.null(r) && httr::status_code(r) == 200)
txt <- httr::content(r, "text", encoding = "UTF-8")
cat(sprintf("fetched %s chars\n", format(nchar(txt), big.mark = ",")))

# --- 1. is the ranking in the HTML, or loaded later by javascript? -----------
# A Next.js site often ships the data as JSON in __NEXT_DATA__. If it is there,
# parsing is trivial and reliable; if the table is only assembled client-side,
# scraping the HTML is brittle and a headless browser would be needed.
has_next <- grepl("__NEXT_DATA__", txt, fixed = TRUE)
cat(sprintf("carries __NEXT_DATA__ JSON: %s\n", has_next))

if (has_next) {
  m <- regmatches(txt, regexpr('__NEXT_DATA__"[^>]*>\\{.*?\\}</script>', txt))
  if (length(m)) {
    j <- sub('^__NEXT_DATA__"[^>]*>', '', m)
    j <- sub('</script>$', '', j)
    dat <- tryCatch(jsonlite::fromJSON(j, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(dat)) cat("  found the block but could not parse it as JSON\n")
    else {
      # walk the tree for anything that looks like a ranking row
      hits <- new.env(); hits$n <- 0; hits$sample <- NULL
      walk <- function(x, depth = 0) {
        if (depth > 12 || hits$n > 0) return(invisible())
        if (is.list(x)) {
          nm <- names(x)
          if (!is.null(nm) && any(grepl("^competitor|athlete", nm, ignore.case = TRUE)) &&
              any(grepl("place|rank", nm, ignore.case = TRUE))) {
            hits$n <- hits$n + 1; hits$sample <- x; return(invisible())
          }
          for (e in x) walk(e, depth + 1)
        }
      }
      walk(dat)
      if (hits$n) { cat("  found a ranking-shaped node, keys:\n"); print(names(hits$sample)) }
      else cat("  no obvious ranking node found by key-name search\n")
    }
  }
}

# --- 2. ARE THERE ATHLETE IDS ANYWHERE ON THE PAGE? -------------------------
# WA athlete urls look like /athletes/<country>/<name>-<id>. The id is what makes
# this joinable, so count them directly rather than trusting the structure.
ids <- unique(regmatches(txt, gregexpr("/athletes/[a-z-]+/[a-z0-9-]+-([0-9]{6,})", txt))[[1]])
cat(sprintf("\nathlete profile links on the page: %d\n", length(ids)))
if (length(ids)) {
  num <- unique(sub(".*-([0-9]{6,})$", "\\1", ids))
  cat(sprintf("distinct numeric athlete ids: %d\n", length(num)))
  print(utils::head(ids, 4))
  # do they match the ids we already hold?
  D <- here::here("citiusdata", "data")
  f <- file.path(D, "athlete_wa_rankings.parquet")
  if (file.exists(f)) {
    ours <- unique(as.character(setDT(arrow::read_parquet(f))$athlete_id))
    hit <- sum(num %chin% ours)
    cat(sprintf("\nof %d ids on the page, %d are athletes we already hold (%.0f%%)\n",
                length(num), hit, 100 * hit / max(length(num), 1)))
    cat("A high overlap means the join is by ID and no name matching is needed,\n")
    cat("which is the difference between a day's work and a minefield.\n")
  }
} else {
  cat("NO athlete ids found in the HTML. Either the table is rendered client-side\n")
  cat("or the links are built by javascript - in which case this needs a headless\n")
  cat("browser, or the underlying API call the page itself makes.\n")
}
