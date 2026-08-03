# Per-nation competitor counts for each sport-edition, from the "Participating
# nations" section of the cached Wikipedia sport articles.
#
# Why this exists: every dominance measure up to now used
# `expected share = 1 / competing_nations`, which assumes a nation that sends 86
# athletes and one that sends 3 are equally likely to win. They are not. The
# right null is entry-based -- a nation's expected golds are its share of the
# entrants, not its share of the flags.
#
# It also decides an open question about the host effect. Hosts get automatic
# qualification and full quotas, so they enter MORE athletes at home. If the
# home gold bump disappears once entry share is controlled for, the effect is
# access, not judging and not home advantage.
#
# Reads only from data/wiki_cache/, populated by harvest_sport_medal_tables.R.
# No network access.

library(rvest)
library(data.table)

OUT   <- "C:/dev/citiusverse/citiusdata/data"
CACHE <- file.path(OUT, "wiki_cache")
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))

read_cached <- function(slug) {
  cf <- file.path(CACHE, paste0(gsub("[^A-Za-z0-9_.-]", "_", slug), ".html"))
  if (!file.exists(cf)) return(NULL)
  txt <- readLines(cf, warn = FALSE, encoding = "UTF-8")
  if (length(txt) == 1 && identical(txt[1], "__404__")) return(NULL)
  tryCatch(read_html(paste(txt, collapse = "\n")), error = function(e) NULL)
}

#' "Australia (86)" -> nation "Australia", competitors 86.
#'
#' Entries carry footnotes (`Australia (42)[14]`) and host markers, and some
#' editions list nations with no count at all. A missing count returns NA rather
#' than 0 -- a nation that competed with an unknown squad is not a nation that
#' sent nobody.
parse_entry <- function(txt) {
  s <- gsub("\u00a0", " ", txt)
  s <- gsub("\\[[^]]*\\]", "", s)              # footnotes
  s <- trimws(s)
  m <- regmatches(s, regexec("^(.*?)\\s*\\((\\d+)\\)\\s*$", s))[[1]]
  if (length(m) == 3) {
    list(nation = trimws(m[2]), competitors = as.integer(m[3]))
  } else {
    list(nation = trimws(gsub("\\s*\\*$", "", s)), competitors = NA_integer_)
  }
}

#' Total competitors and nations from the infobox, e.g. "923+8 guides from 71 nations".
infobox_totals <- function(page) {
  out <- list(competitors = NA_integer_, nations = NA_integer_)
  for (tr in html_nodes(page, "table.infobox tr")) {
    lab <- html_text(html_node(tr, "th"), trim = TRUE)
    if (is.na(lab)) next
    val <- gsub("\u00a0", " ", html_text(html_node(tr, "td"), trim = TRUE))
    val <- gsub("\\[[^]]*\\]|,", "", val)
    if (grepl("competitor", lab, ignore.case = TRUE)) {
      n <- regmatches(val, regexpr("[0-9]+", val))
      if (length(n)) out$competitors <- as.integer(n)
      f <- regmatches(val, regexec("from\\s+([0-9]+)\\s*nation", val))[[1]]
      if (length(f) == 2) out$nations <- as.integer(f[2])
    }
    if (is.na(out$nations) && grepl("^\\s*(nations|teams)\\b", lab, ignore.case = TRUE)) {
      n <- regmatches(val, regexpr("[0-9]+", val))
      if (length(n)) out$nations <- as.integer(n)
    }
  }
  out
}

#' The list following a "Participating nations" heading.
participation_list <- function(page) {
  nodes <- html_nodes(page, "h2, h3, ul, ol, div, table")
  names_ <- html_name(nodes)
  txt <- html_text(nodes)
  hd <- which(names_ %in% c("h2", "h3") &
              grepl("participating\\s+(nations|cgas|noc|countries|teams)",
                    txt, ignore.case = TRUE))
  if (!length(hd)) return(NULL)
  for (start in hd) {
    for (j in seq(start + 1, min(start + 6, length(nodes)))) {
      if (names_[j] %in% c("h2", "h3")) break
      items <- html_text(html_nodes(nodes[[j]], "li"), trim = TRUE)
      if (length(items) >= 3) {
        parsed <- lapply(items, parse_entry)
        d <- data.table(
          nation = vapply(parsed, `[[`, character(1), "nation"),
          competitors = vapply(parsed, `[[`, integer(1), "competitors"))
        d <- d[nzchar(nation) & !grepl("^(total|host)", nation, ignore.case = TRUE)]
        if (nrow(d) >= 3) return(unique(d, by = "nation"))
      }
    }
  }
  NULL
}

meta <- fread(file.path(OUT, "sport_medal_tables_meta.csv"))
rows <- list(); tots <- list()

for (i in seq_len(nrow(meta))) {
  pg <- read_cached(meta$slug[i])
  if (is.null(pg)) next
  ib <- infobox_totals(pg)
  pl <- participation_list(pg)
  tots[[length(tots) + 1]] <- data.table(
    games = meta$games[i], year = meta$year[i], sport = meta$sport[i],
    infobox_competitors = ib$competitors, infobox_nations = ib$nations,
    listed_nations = if (is.null(pl)) NA_integer_ else nrow(pl),
    listed_with_counts = if (is.null(pl)) NA_integer_ else sum(!is.na(pl$competitors)))
  if (!is.null(pl)) {
    pl[, `:=`(games = meta$games[i], year = meta$year[i], sport = meta$sport[i])]
    rows[[length(rows) + 1]] <- pl
  }
  if (i %% 150 == 0) cat(sprintf("  %d/%d\n", i, nrow(meta)))
}

part <- rbindlist(rows, fill = TRUE)
tot  <- rbindlist(tots, fill = TRUE)
part[, canon := canonical_nation(nation)]
part <- part[, .(competitors = sum(competitors)), by = .(games, year, sport, canon)]

cat(sprintf("\n%d sport-editions probed.\n", nrow(tot)))
cat(sprintf("  with a participating-nations list ....... %d\n", sum(!is.na(tot$listed_nations))))
cat(sprintf("  with per-nation COUNTS in that list ..... %d\n",
            sum(tot$listed_with_counts > 0, na.rm = TRUE)))
cat(sprintf("  with an infobox competitor total ........ %d\n", sum(!is.na(tot$infobox_competitors))))

cat("\n=== coverage of per-nation counts by series and decade ===\n")
tot[, has_counts := !is.na(listed_with_counts) & listed_with_counts > 0]
print(tot[, .(sport_editions = .N, with_counts = sum(has_counts),
              pct = round(100 * mean(has_counts))),
          by = .(games, decade = 10 * (year %/% 10))][order(games, decade)])

# Cross-check: the per-nation counts should sum to the infobox total.
chk <- merge(part[, .(summed = sum(competitors, na.rm = TRUE),
                      n_nations = .N), by = .(games, year, sport)],
             tot[, .(games, year, sport, infobox_competitors, infobox_nations)],
             by = c("games", "year", "sport"))
chk[, diff := summed - infobox_competitors]
cat("\n=== per-nation counts vs the infobox total ===\n")
cat(sprintf("exact match: %d of %d comparable sport-editions\n",
            sum(chk$diff == 0, na.rm = TRUE), sum(!is.na(chk$diff))))
cat("worst disagreements:\n")
print(head(chk[!is.na(diff)][order(-abs(diff)),
      .(games, year, sport, summed, infobox_competitors, diff, n_nations)], 12))

fwrite(part, file.path(OUT, "sport_participation.csv"))
fwrite(tot,  file.path(OUT, "sport_participation_totals.csv"))
saveRDS(part, file.path(OUT, "sport_participation.rds"))
cat("\nSaved sport_participation.\n")
