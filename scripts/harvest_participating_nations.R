# Scrape the participating-nation count for every games edition from its
# Wikipedia infobox, and reconcile against the hand-maintained fallback map.
#
# Written because `nations_count_map` had `commonwealth_1986 = 26` where both
# the infobox and the article body say 27. That count is the denominator of the
# "expected gold share" baseline (100 / competing_nations), so an error in it
# moves every field-size-adjusted ranking that edition appears in. One wrong
# entry in hand-maintained reference data is a reason to check all of them, not
# to patch the one.

library(rvest)
library(httr)
library(data.table)

OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "wiki_cache_editions")
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
source(here::here("citiusdata", "scripts", "games_reference.R"))

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) CitiusVerseScraper/5.0"

fetch_cached <- function(slug) {
  cf <- file.path(CACHE, paste0(gsub("[^A-Za-z0-9_.-]", "_", slug), ".html"))
  if (file.exists(cf)) {
    txt <- readLines(cf, warn = FALSE, encoding = "UTF-8")
    if (length(txt) == 1 && identical(txt[1], "__404__")) return(NULL)
    return(read_html(paste(txt, collapse = "\n")))
  }
  r <- tryCatch(GET(paste0("https://en.wikipedia.org/wiki/", slug),
                    user_agent(ua), timeout(25)), error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) {
    # Only a real 404 is cached as absent; see harvest_sport_medal_tables.R for
    # why caching every non-200 as "__404__" is a permanent data loss.
    code <- if (is.null(r)) NA_integer_ else status_code(r)
    if (!is.na(code) && code == 404L) writeLines("__404__", cf)
    else message(sprintf("  transient fetch failure (%s) for %s -- not cached",
                         if (is.na(code)) "no response" else code, slug))
    return(NULL)
  }
  txt <- content(r, "text", encoding = "UTF-8")
  writeLines(txt, cf, useBytes = TRUE)
  read_html(txt)
}

#' Read "Nations" (or "Participating nations") out of the infobox.
#'
#' Values look like "80", "72 teams", "206 (2 under the Olympic flag)". Take the
#' first integer and record the raw string so a surprising parse is visible
#' rather than merely wrong.
infobox_nations <- function(page) {
  rows <- html_nodes(page, "table.infobox tr")
  for (tr in rows) {
    lab <- html_text(html_node(tr, "th"), trim = TRUE)
    if (is.na(lab)) next
    if (!grepl("^\\s*(participating\\s+)?nations?\\b|^\\s*teams?\\b|^\\s*countries\\b",
               lab, ignore.case = TRUE)) next
    val <- html_text(html_node(tr, "td"), trim = TRUE)
    if (is.na(val)) next
    raw <- gsub("\u00a0", " ", val)
    raw <- gsub("\\[.*?\\]", "", raw)
    num <- regmatches(raw, regexpr("[0-9][0-9,]*", raw))
    if (!length(num)) next
    return(list(n = as.integer(gsub(",", "", num)), raw = trimws(raw), label = lab))
  }
  list(n = NA_integer_, raw = NA_character_, label = NA_character_)
}

eds <- all_editions()
out <- vector("list", length(eds))
for (i in seq_along(eds)) {
  y <- eds[[i]][[1]]; slug <- eds[[i]][[2]]; g <- eds[[i]][[3]]
  page <- fetch_cached(slug)
  info <- if (is.null(page)) list(n = NA_integer_, raw = NA_character_, label = NA_character_)
          else infobox_nations(page)
  key <- paste0(g, "_", y)
  out[[i]] <- data.table(
    games = g, year = as.integer(y), slug = slug,
    scraped = info$n, raw = info$raw, infobox_label = info$label,
    hand = if (is.null(nations_count_map[[key]])) NA_integer_ else as.integer(nations_count_map[[key]])
  )
  if (i %% 20 == 0) cat(sprintf("  %d/%d\n", i, length(eds)))
}

nat <- rbindlist(out)
nat[, diff := scraped - hand]
setorder(nat, games, year)

cat(sprintf("\nEditions: %d.  Scraped a count for %d.  Missing: %d.\n",
            nrow(nat), sum(!is.na(nat$scraped)), sum(is.na(nat$scraped))))

cat("\n=== DISAGREEMENTS between the scrape and the hand-maintained map ===\n")
bad <- nat[!is.na(scraped) & !is.na(hand) & scraped != hand]
if (nrow(bad)) {
  print(bad[order(-abs(diff)), .(games, year, scraped, hand, diff, raw)])
  cat(sprintf("\n%d of %d editions disagree (%.0f%%).\n",
              nrow(bad), sum(!is.na(nat$scraped) & !is.na(nat$hand)),
              100 * nrow(bad) / sum(!is.na(nat$scraped) & !is.na(nat$hand))))
} else {
  cat("none -- the hand map agrees with every infobox.\n")
}

cat("\n=== editions with no scraped count (hand value is all there is) ===\n")
print(nat[is.na(scraped), .(games, year, hand)])

# The scraped value is authoritative; the hand value is the fallback.
nat[, competing_nations := fifelse(!is.na(scraped), scraped, hand)]
nat[, source := fifelse(!is.na(scraped), "wikipedia_infobox", "hand_map")]

fwrite(nat, file.path(OUT, "games_participating_nations.csv"))
cat("\nSaved games_participating_nations.csv\n")
