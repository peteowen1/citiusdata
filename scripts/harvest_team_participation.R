# Which nations actually fielded a team, per team sport-edition.
#
# The entry variable for a team sport is not squad size. Every team in a
# netball tournament brings twelve players, so a nation's share of the
# entrants is ~1/n_teams whatever it does. What varies -- and what a host gets
# handed -- is **whether you are in the tournament at all**. Hosts qualify
# automatically for team events they might never have earned a place in.
#
# So the question behind the +8.11 pp team-sport host advantage is: do hosts
# win more at home, or do they simply TURN UP more at home? That needs a
# competing-team list per sport-edition, which the group-standings tables carry
# on almost every tournament page.
#
# Reads only from data/wiki_cache/. No network access.

library(rvest)
library(data.table)

OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "wiki_cache")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
source(here::here("citiusdata", "scripts", "games_reference.R"))

read_cached <- function(slug) {
  cf <- file.path(CACHE, paste0(gsub("[^A-Za-z0-9_.-]", "_", slug), ".html"))
  if (!file.exists(cf)) return(NULL)
  txt <- readLines(cf, warn = FALSE, encoding = "UTF-8")
  if (length(txt) == 1 && identical(txt[1], "__404__")) return(NULL)
  tryCatch(read_html(paste(txt, collapse = "\n")), error = function(e) NULL)
}

med <- as.data.table(readRDS(file.path(OUT, "multisport_medal_tables.rds")))
med[, canon := canonical_nation(nation)]
KNOWN <- sort(unique(med$canon)); KNOWN <- KNOWN[nzchar(KNOWN)]
KNOWN_BY_LEN <- KNOWN[order(-nchar(KNOWN))]

leading_nation <- function(txt) {
  s <- trimws(gsub("\u00a0", " ", as.character(txt)))
  s <- gsub("\\[[^]]*\\]", "", s)
  s <- gsub("\\s*\\((H|h)\\)\\s*$", "", s)     # host marker
  s <- gsub("\\s*\\*+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  # Try canonicalising the whole cell first. Standings use abbreviations that
  # are not prefixes of the canonical name -- the 1992 Unified Team appears as
  # "CIS" -- so prefix matching alone silently loses them.
  full <- canonical_nation(s)
  if (!is.na(full) && full %in% KNOWN) return(full)
  hit <- KNOWN_BY_LEN[startsWith(s, KNOWN_BY_LEN)]
  if (length(hit)) hit[1] else NA_character_
}

#' Nations named in the tournament's group-standings tables.
#'
#' A group table is identifiable by carrying a team column alongside the
#' played/won/drawn/lost columns. Every team in the tournament appears in one,
#' so the union across tables is the entry list. Knockout-only tournaments have
#' none, and return nothing rather than a partial list.
teams_from_standings <- function(page) {
  out <- character(0)
  for (tb in html_nodes(page, "table.wikitable")) {
    d <- tryCatch(setDT(html_table(tb, fill = TRUE)), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    setnames(d, make.unique(tolower(gsub("[^a-zA-Z0-9]", "_", names(d)))))
    cn <- names(d)
    team_col <- cn[grepl("^team|^nation|^country|^noc|^cga", cn)][1]
    if (is.na(team_col)) next
    # A standings table needs a team column plus match-record columns. Demanding
    # Pld AND W AND L looked safe and was in fact a filter on whole sports:
    # basketball tables are [team, w, l, pf, pa, pd, pts] with no Pld at all,
    # so every basketball group table was rejected. Require any TWO of the
    # record columns instead -- a medal table (rank/nation/gold/silver/bronze)
    # still matches none of them.
    record <- c("^pld$|^played$|^mp$", "^w$|^won$|^mw$", "^l$|^lost$|^ml$",
                "^d$|^drawn$|^t$|^tie$", "^pts$|^points$",
                "^gf$|^pf$|^sw$", "^ga$|^pa$|^sl$", "^gd$|^pd$|^sr$")
    if (sum(vapply(record, function(p) any(grepl(p, cn)), logical(1))) < 2) next
    nats <- vapply(d[[team_col]], leading_nation, character(1), USE.NAMES = FALSE)
    out <- c(out, nats[!is.na(nats)])
  }
  unique(out)
}

#' Nations listed under a "Participating nations/teams" heading.
teams_from_participation <- function(page) {
  nodes <- html_nodes(page, "h2, h3, ul, ol, div, table")
  nm <- html_name(nodes); tx <- html_text(nodes)
  hd <- which(nm %in% c("h2", "h3") &
              grepl("participating\\s+(nations|teams|cgas|nocs|countries)",
                    tx, ignore.case = TRUE))
  for (start in hd) {
    for (j in seq(start + 1, min(start + 5, length(nodes)))) {
      if (nm[j] %in% c("h2", "h3")) break
      items <- html_text(html_nodes(nodes[[j]], "li"), trim = TRUE)
      if (length(items) >= 3) {
        nats <- vapply(items, leading_nation, character(1), USE.NAMES = FALSE)
        nats <- unique(nats[!is.na(nats)])
        if (length(nats) >= 3) return(nats)
      }
    }
  }
  character(0)
}

#' Nations appearing in fixture, bracket or result tables.
#'
#' Knockout-only tournaments have no standings at all -- the only record that a
#' team was there is that it played a match. A fixture row pairs two nations
#' with a score, and a bracket cell holds a nation and its score, so a table
#' with several distinct known nations in it is a record of who competed.
#'
#' Deliberately the LAST resort: it is the loosest rule here, so it only runs
#' when neither a standings table nor a participation list exists, and its
#' output is still checked against the medallists.
teams_from_fixtures <- function(page) {
  out <- character(0)
  for (tb in html_nodes(page, "table")) {
    d <- tryCatch(html_table(tb, fill = TRUE), error = function(e) NULL)
    if (is.null(d) || !nrow(d) || ncol(d) < 2) next
    cn <- tolower(gsub("[^a-zA-Z0-9]", "_", names(d)))
    # Skip medal tables -- those are handled properly elsewhere.
    if (any(grepl("^gold", cn)) && any(grepl("^silver", cn))) next
    cells <- as.character(unlist(d, use.names = FALSE))
    nats <- unique(vapply(cells, leading_nation, character(1), USE.NAMES = FALSE))
    nats <- nats[!is.na(nats)]
    if (length(nats) >= 3) out <- c(out, nats)
  }
  unique(out)
}

TEAM_SPORTS <- sport_subjectivity()[team_sport == TRUE, sport]

SUFFIX <- list(
  olympics_summer = function(y) sprintf("the_%d_Summer_Olympics", y),
  commonwealth    = function(y) paste0("the_", Filter(function(it) it[[1]] == y, cw_slugs)[[1]][[2]]),
  asian_games     = function(y) sprintf("the_%d_Asian_Games", y),
  panam_games     = function(y) sprintf("the_%d_Pan_American_Games", y),
  african_games   = function(y) paste0("the_", Filter(function(it) it[[1]] == y, afr_slugs)[[1]][[2]])
)

rows <- list(); meta <- list()
for (e in all_editions()) {
  y <- e[[1]]; g <- e[[3]]
  if (is.null(SUFFIX[[g]])) next
  suffix <- SUFFIX[[g]](y)
  for (sp in TEAM_SPORTS) {
    slug <- paste0(gsub(" ", "_", sp), "_at_", suffix)
    page <- read_cached(slug)
    if (is.null(page)) next
    ct <- html_attr(html_node(page, 'link[rel="canonical"]'), "href")
    ct <- if (!is.na(ct)) sub("^.*/wiki/", "", ct) else NA_character_
    if (is.na(ct) || !grepl(paste0("_at_", suffix, "$"), ct)) next
    sport_name <- gsub("_", " ", sub(paste0("_at_", suffix, "$"), "", ct))

    st <- teams_from_standings(page)
    pt <- teams_from_participation(page)
    teams <- unique(c(st, pt))
    src <- if (length(st) && length(pt)) "both" else if (length(st)) "standings" else
           if (length(pt)) "participation" else "none"
    if (!length(teams)) {                       # last resort, see above
      teams <- teams_from_fixtures(page)
      if (length(teams)) src <- "fixtures"
    }
    meta[[length(meta) + 1]] <- data.table(
      games = g, year = y, sport = sport_name,
      n_teams = length(teams), source = src)
    if (!length(teams)) next
    rows[[length(rows) + 1]] <- data.table(
      games = g, year = y, sport = sport_name, canon = teams)
  }
}

part <- unique(rbindlist(rows))
mt   <- rbindlist(meta)

cat(sprintf("Team sport-editions probed: %d\n", nrow(mt)))
print(mt[, .N, by = source][order(-N)])
cat(sprintf("With a team list: %d (%.0f%%)\n",
            sum(mt$n_teams > 0), 100 * mean(mt$n_teams > 0)))
cat("\nteams per sport-edition:\n")
print(summary(mt[n_teams > 0, n_teams]))
cat("\nby series:\n")
print(mt[n_teams > 0, .(sport_editions = .N, median_teams = as.double(median(n_teams))),
         by = games][order(-sport_editions)])

# Sanity: a medal-winning nation must be in its own tournament's team list.
pods <- as.data.table(readRDS(file.path(OUT, "team_sport_podiums.rds")))
chk <- merge(pods[gold > 0, .(games, year, sport, canon)],
             part[, .(games, year, sport, canon, listed = TRUE)],
             by = c("games", "year", "sport", "canon"), all.x = TRUE)
chk <- merge(chk, mt[n_teams > 0, .(games, year, sport, has_list = TRUE)],
             by = c("games", "year", "sport"))
cat(sprintf("\nGold medallists present in their own tournament's team list: %d/%d\n",
            sum(!is.na(chk$listed)), nrow(chk)))
if (any(is.na(chk$listed))) print(head(chk[is.na(listed)], 12))

saveRDS(part, file.path(OUT, "team_participation.rds"))
fwrite(mt, file.path(OUT, "team_participation_meta.csv"))
cat("\nSaved team_participation.\n")
