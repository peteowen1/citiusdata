# Medal results for team sports, which have no NOC medal table to parse.
#
# Netball, and a long tail of single-event team competitions, give a page with
# group standings and a final -- never a nation x gold/silver/bronze table. The
# sport-level harvester therefore records nothing for them, which is why the
# `opponent` class was the least reliable of the three and why only two team
# sports reached the host-effect regression.
#
# Three fallbacks, in order of how much they can be trusted:
#
#   1. The infobox "Medalists" row. It lists gold, silver and bronze as links,
#      in that order. Cleanest signal on the page.
#   2. A "Final standings"/"Final ranking" table with a place and a nation.
#      Places 1-3 are the podium.
#   3. A podium table whose columns are Gold/Silver/Bronze and whose cells are
#      squad listings ("AustraliaCoach: Stacey Marinkovich Liz Watson ...").
#      The nation is the leading substring, recovered by longest-prefix match
#      against the nations that actually appear in the medal tables.
#
# Every result is cross-checked: the three medallists must be distinct, and
# must be nations that appear in that edition's overall medal table. A podium
# that fails either check is dropped rather than guessed at.

library(rvest)
library(data.table)

OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "wiki_cache")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
source(here::here("citiusdata", "scripts", "games_reference.R"))
# Pure parsing rules live in lib/ so the citius test suite tests the REAL
# functions -- test-podium-helpers.R used to test a hand-copied twin.
source(here::here("citiusdata", "scripts", "lib", "podium_parsing.R"))

read_cached <- function(slug) {
  cf <- file.path(CACHE, paste0(gsub("[^A-Za-z0-9_.-]", "_", slug), ".html"))
  if (!file.exists(cf)) return(NULL)
  txt <- readLines(cf, warn = FALSE, encoding = "UTF-8")
  if (length(txt) == 1 && identical(txt[1], "__404__")) return(NULL)
  tryCatch(read_html(paste(txt, collapse = "\n")), error = function(e) NULL)
}

med <- as.data.table(readRDS(file.path(OUT, "multisport_medal_tables.rds")))
med[, canon := canonical_nation(nation)]
KNOWN <- sort(unique(med$canon))
KNOWN <- KNOWN[nzchar(KNOWN)]
# Longest first, so "South Africa" wins over any shorter prefix.
KNOWN_BY_LEN <- pp_nation_prefixes(KNOWN)

# The nation a cell starts with, or NA -- see lib/podium_parsing.R. This
# script does NOT pass a canonicaliser (participation does); podium cells are
# squad listings, not abbreviations.
leading_nation <- function(txt) pp_leading_nation(txt, KNOWN_BY_LEN)

#' Nations from the infobox "Medalists" block.
#'
#' The medallists are NOT in the "Medalists" row itself -- that row holds only
#' the label. They are in the rows that FOLLOW it, one per medal, each pairing
#' a medal-icon link ("gold medal") with a team link whose title reads
#' "New Zealand national netball team". Reading only the labelled row returns
#' nothing, which is why this path never fired on the first attempt.
podium_from_infobox <- function(page) {
  trs <- html_nodes(page, "table.infobox tr")
  if (!length(trs)) return(NULL)
  labs <- vapply(trs, function(tr) {
    v <- html_text(html_node(tr, "th"), trim = TRUE); if (is.na(v)) "" else v
  }, character(1))
  start <- which(grepl("^medal(l)?ists?$", trimws(labs), ignore.case = TRUE))[1]
  if (is.na(start)) return(NULL)

  found <- c(gold = NA_character_, silver = NA_character_, bronze = NA_character_)
  for (j in seq(start, min(start + 6, length(trs)))) {
    if (j > start && nzchar(labs[j])) break        # next labelled field
    a <- html_nodes(trs[[j]], "a")
    if (!length(a)) next
    titles <- html_attr(a, "title")
    titles[is.na(titles)] <- html_text(a, trim = TRUE)[is.na(titles)]
    which_medal <- grep("^(gold|silver|bronze) medal", titles, ignore.case = TRUE)
    if (!length(which_medal)) next
    medal <- tolower(sub("^(\\w+) medal.*$", "\\1", titles[which_medal[1]]))
    nats <- vapply(titles[-which_medal], leading_nation, character(1),
                   USE.NAMES = FALSE)
    nats <- nats[!is.na(nats)]
    if (length(nats) && medal %in% names(found) && is.na(found[[medal]])) {
      found[[medal]] <- nats[1]
    }
  }
  if (anyNA(found) || anyDuplicated(found)) return(NULL)
  unname(found[c("gold", "silver", "bronze")])
}

#' Places 1-3 of a "Final standings" table.
podium_from_standings <- function(page) {
  for (tb in html_nodes(page, "table.wikitable")) {
    d <- tryCatch(setDT(html_table(tb, fill = TRUE)), error = function(e) NULL)
    if (is.null(d) || nrow(d) < 3) next
    setnames(d, make.unique(tolower(gsub("[^a-zA-Z0-9]", "_", names(d)))))
    cn <- names(d)
    pos_col <- cn[grepl("^(place|pos|rank)", cn)][1]
    nat_col <- cn[grepl("^(nation|team|country|noc|cga)", cn)][1]
    if (is.na(pos_col) || is.na(nat_col)) next
    # A group-stage table also has Pos and Team -- reject anything carrying
    # match columns, and require at least 3 distinct placings.
    if (any(grepl("^(pld|w|d|l|gf|ga|gd|pts|qualification)$", cn))) next
    nat <- vapply(d[[nat_col]], leading_nation, character(1), USE.NAMES = FALSE)
    pos <- suppressWarnings(as.integer(gsub("[^0-9]", "", d[[pos_col]])))
    # The top three places are rendered as medal ICONS, so they parse to NA
    # while places 4 onward are numeric. Falling back to row order only when
    # EVERY position is NA therefore missed exactly the rows we want; fill each
    # missing position from its row index instead.
    # Filling a missing placing from its row index assumes the table is ordered
    # by finish. Check that against the placings that DID parse before relying
    # on it: if the numeric ones are not ascending in row order, the table is
    # sorted some other way and row index would swap gold with bronze -- a
    # swap the medallist validation downstream cannot see, because both are
    # genuine medallists in that edition.
    known <- which(!is.na(pos))
    if (length(known) >= 2 && is.unsorted(pos[known])) next
    pos <- pp_fill_missing_places(pos)
    got <- vapply(1:3, function(k) {
      v <- nat[which(pos == k)]
      if (length(v) && !is.na(v[1])) v[1] else NA_character_
    }, character(1))
    if (!anyNA(got)) return(got)
  }
  NULL
}

#' One podium per row of a Gold/Silver/Bronze table (one row per event).
podium_from_medal_cells <- function(page) {
  out <- list()
  for (tb in html_nodes(page, "table.wikitable")) {
    d <- tryCatch(setDT(html_table(tb, fill = TRUE)), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    setnames(d, make.unique(tolower(gsub("[^a-zA-Z0-9]", "_", names(d)))))
    cn <- names(d)
    g <- cn[grepl("^gold", cn)][1]; s <- cn[grepl("^silver", cn)][1]
    b <- cn[grepl("^bronze", cn)][1]
    if (any(is.na(c(g, s, b)))) next
    # A numeric gold column means this is the NOC table, handled elsewhere.
    if (pp_is_noc_count_column(d[[g]])) next
    ev <- cn[grepl("^event", cn)][1]
    for (i in seq_len(nrow(d))) {
      trio <- c(leading_nation(d[[g]][i]), leading_nation(d[[s]][i]),
                leading_nation(d[[b]][i]))
      if (anyNA(trio) || anyDuplicated(trio)) next
      out[[length(out) + 1]] <- list(
        event = if (!is.na(ev)) trimws(gsub("details$", "", d[[ev]][i])) else "",
        podium = trio)
    }
    if (length(out)) return(out)
  }
  NULL
}

TEAM_SPORTS <- c("Netball", "Rugby sevens", "Rugby union", "Hockey",
                 "Field hockey", "Football", "Basketball", "3x3 basketball",
                 "Volleyball", "Beach volleyball", "Handball", "Water polo",
                 "Cricket", "Baseball", "Softball", "Polo", "Tug of war",
                 "Lacrosse", "Curling", "Ice hockey")

sport_dt <- as.data.table(readRDS(file.path(OUT, "sport_medal_tables.rds")))
have <- unique(sport_dt[, .(games, year, sport)])
have[, key := paste(games, year, sport)]

# ---------------------------------------------------------------------------
# Work out the gap first: pages that exist but produced no NOC medal table
# ---------------------------------------------------------------------------
eds <- all_editions()
SUFFIX <- list(
  olympics_summer = function(y) sprintf("the_%d_Summer_Olympics", y),
  commonwealth    = function(y) paste0("the_", Filter(function(it) it[[1]] == y, cw_slugs)[[1]][[2]]),
  asian_games     = function(y) sprintf("the_%d_Asian_Games", y),
  panam_games     = function(y) sprintf("the_%d_Pan_American_Games", y),
  african_games   = function(y) paste0("the_", Filter(function(it) it[[1]] == y, afr_slugs)[[1]][[2]])
)

rows <- list(); gap <- list()
for (e in eds) {
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
    k <- paste(g, y, sport_name)
    if (k %in% have$key) next                    # already has an NOC table
    gap[[length(gap) + 1]] <- data.table(games = g, year = y, sport = sport_name)

    src <- NA_character_; podiums <- NULL
    p <- podium_from_medal_cells(page)
    if (!is.null(p)) { podiums <- p; src <- "medal_cells" }
    if (is.null(podiums)) {
      p <- podium_from_infobox(page)
      if (!is.null(p)) { podiums <- list(list(event = "", podium = p)); src <- "infobox" }
    }
    if (is.null(podiums)) {
      p <- podium_from_standings(page)
      if (!is.null(p)) { podiums <- list(list(event = "", podium = p)); src <- "standings" }
    }
    if (is.null(podiums)) next

    for (pd in podiums) {
      rows[[length(rows) + 1]] <- data.table(
        games = g, year = y, sport = sport_name, event = pd$event,
        gold_nation = pd$podium[1], silver_nation = pd$podium[2],
        bronze_nation = pd$podium[3], source = src)
    }
  }
}

gap_dt <- unique(rbindlist(gap))
pod <- rbindlist(rows)
pod <- unique(pod, by = c("games", "year", "sport", "event"))

cat(sprintf("Team sport-editions with a page but no NOC medal table: %d\n", nrow(gap_dt)))
cat(sprintf("Podiums recovered: %d across %d sport-editions\n",
            nrow(pod), uniqueN(pod[, .(games, year, sport)])))
cat("\nby source:\n"); print(pod[, .N, by = source][order(-N)])
cat("\nby series:\n"); print(pod[, .(recovered = uniqueN(paste(year, sport))), by = games])
cat("\nstill missing:\n")
missing <- gap_dt[!paste(games, year, sport) %in% pod[, paste(games, year, sport)]]
print(missing[, .N, by = .(games, sport)][order(-N)][1:min(15, .N)])

# ---------------------------------------------------------------------------
# Validate against the edition's own medal table before believing any of it
# ---------------------------------------------------------------------------
elig <- unique(med[, .(games, year, canon)])
chk <- melt(pod, id.vars = c("games", "year", "sport", "event"),
            measure.vars = c("gold_nation", "silver_nation", "bronze_nation"),
            variable.name = "medal", value.name = "canon")
chk <- merge(chk, elig[, .(games, year, canon, in_table = TRUE)],
             by = c("games", "year", "canon"), all.x = TRUE)
bad <- chk[is.na(in_table)]
cat(sprintf("\nMedallists not present in their edition's overall medal table: %d of %d\n",
            nrow(bad), nrow(chk)))
if (nrow(bad)) print(head(bad[, .(games, year, sport, medal, canon)], 20))

# Drop any podium with an unverifiable nation -- a nation that won a team gold
# must appear in that edition's medal table.
bad_key <- unique(bad[, paste(games, year, sport, event)])
pod_ok <- pod[!paste(games, year, sport, event) %in% bad_key]
cat(sprintf("Podiums kept after validation: %d (dropped %d)\n",
            nrow(pod_ok), nrow(pod) - nrow(pod_ok)))

# ---------------------------------------------------------------------------
# Emit in the same shape as sport_medal_tables so it can be bound straight on
# ---------------------------------------------------------------------------
long <- melt(pod_ok, id.vars = c("games", "year", "sport", "event"),
             measure.vars = c("gold_nation", "silver_nation", "bronze_nation"),
             variable.name = "medal", value.name = "nation")
long[, medal := sub("_nation$", "", medal)]
tbl <- dcast(long, games + year + sport + nation ~ medal, fun.aggregate = length,
             value.var = "medal")
for (m in c("gold", "silver", "bronze")) if (!m %in% names(tbl)) tbl[, (m) := 0L]
tbl[, total := gold + silver + bronze]
tbl[, canon := canonical_nation(nation)]
setcolorder(tbl, c("games", "year", "sport", "nation", "gold", "silver", "bronze", "total"))

cat(sprintf("\nEmitted %d nation-rows across %d sport-editions.\n",
            nrow(tbl), uniqueN(tbl[, .(games, year, sport)])))
cat("\nGolds added per series:\n")
print(tbl[, .(golds = sum(gold), sport_editions = uniqueN(paste(year, sport))), by = games])

saveRDS(tbl, file.path(OUT, "team_sport_podiums.rds"))
fwrite(pod_ok, file.path(OUT, "team_sport_podiums_detail.csv"))
fwrite(gap_dt, file.path(OUT, "team_sport_gap.csv"))
cat("\nSaved team_sport_podiums.\n")
