# Harvest SPORT-LEVEL medal tables (nation x sport x games edition) from
# Wikipedia's "{Sport} at the {Year} {Games}" articles.
#
# This is the dataset the objective/subjective question actually needs. The
# overall medal table cannot answer it: it has no sport dimension at all. The
# file it replaces, data/all_multisport_hosts_analysis.csv, had no producing
# script anywhere in the repo and contained invented totals (see
# docs/reviews/medal-pipeline-audit-2026-08-03.md).
#
# Two tables on these pages carry Gold/Silver/Bronze headers and only one is a
# medal table: the NOC table holds integers, the medallists table holds athlete
# names. `looks_like_noc_table()` is what tells them apart -- without it the
# medallists table parses to all-zero counts and looks like a sport nobody won.
#
# Responses are cached under data/wiki_cache/ so re-runs cost nothing.

library(rvest)
library(httr)
library(data.table)

OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "wiki_cache")
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

source(here::here("citiusdata", "scripts", "games_reference.R"))

ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) CitiusVerseScraper/5.0"

REFRESH <- identical(Sys.getenv("CITIUS_REFRESH"), "1")

fetch_cached <- function(slug) {
  cf <- file.path(CACHE, paste0(gsub("[^A-Za-z0-9_.-]", "_", slug), ".html"))
  if (file.exists(cf) && !REFRESH) {
    txt <- readLines(cf, warn = FALSE, encoding = "UTF-8")
    if (length(txt) == 1 && identical(txt[1], "__404__")) return(NULL)
    return(read_html(paste(txt, collapse = "\n")))
  }
  u <- paste0("https://en.wikipedia.org/wiki/", slug)
  r <- tryCatch(GET(u, user_agent(ua), timeout(25)), error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) {
    # Cache the negative ONLY for a real 404. Writing "__404__" for any non-200
    # made a rate limit (429), a server error or a timeout permanently
    # indistinguishable from "this sport was never contested" -- and because the
    # cache never expires, one transient blip would remove a sport from an
    # edition for good. Anything else is left uncached so the next run retries.
    code <- if (is.null(r)) NA_integer_ else status_code(r)
    if (!is.na(code) && code == 404L) {
      writeLines("__404__", cf)
    } else {
      message(sprintf("  transient fetch failure (%s) for %s -- not cached",
                      if (is.na(code)) "no response" else code, slug))
    }
    return(NULL)
  }
  txt <- content(r, "text", encoding = "UTF-8")
  writeLines(txt, cf, useBytes = TRUE)
  read_html(txt)
}

#' The article a slug actually resolved to.
#'
#' Wikipedia answers 200 for a redirect, so a slug for a sport an edition did
#' not contest silently lands somewhere else. Two ways that corrupts the data,
#' both seen on the 2026 Commonwealth cache:
#'
#'  - "Cricket_at_the_2026_Commonwealth_Games" redirects to the Games article,
#'    whose first NOC table is the OVERALL medal table. Cricket was recorded as
#'    a 216-gold sport in which Australia won 70 -- the whole Games, filed under
#'    one sport.
#'  - "Cycling_...", "Gymnastics_..." and "Lawn_bowls_..." redirect to the
#'    track cycling, artistic gymnastics and bowls articles, so each of those
#'    sports was counted twice.
#'
#' Keying on the canonical title fixes both: a redirect out of the
#' "{Sport} at the {Edition}" pattern is rejected, and two slugs landing on one
#' article are deduplicated.
canonical_title <- function(page) {
  ln <- html_attr(html_node(page, 'link[rel="canonical"]'), "href")
  if (!is.na(ln) && nzchar(ln)) {
    return(utils::URLdecode(sub("^.*/wiki/", "", ln)))
  }
  h <- html_text(html_node(page, "h1"), trim = TRUE)
  if (!is.na(h)) gsub(" ", "_", h) else NA_character_
}

clean_num_cell <- function(x) {
  st <- gsub("\\[.*?\\]|\\(.*?\\)", "", as.character(x))
  trimws(st)
}

clean_nation <- function(vec) {
  vapply(vec, function(x) {
    if (is.na(x)) return("")
    st <- gsub("\u00a0", " ", as.character(x))
    st <- gsub("\\[.*?\\]|\\(.*?\\)|\\*|\u2020|\u2021", "", st)
    st <- gsub("[0-9]+[a-z]?$", "", trimws(st))
    trimws(st)
  }, character(1), USE.NAMES = FALSE)
}

as_int <- function(vec) {
  vapply(vec, function(x) {
    st <- gsub("[^0-9]", "", clean_num_cell(x))
    if (!nzchar(st)) 0L else as.integer(st)
  }, integer(1), USE.NAMES = FALSE)
}

#' Is this the nation medal table, rather than the list of medallists?
#'
#' The medallists table on the same page has Gold/Silver/Bronze columns too,
#' but filled with athlete names. Requiring the gold column to be numeric is
#' what separates them; without it those name cells coerce to 0 and the sport
#' silently records no medals at all.
looks_like_noc_table <- function(dt, gold_col, cn) {
  if (any(grepl("^event", cn))) return(FALSE)
  if (!any(grepl("noc|nation|team|country|cga", cn))) return(FALSE)
  vals <- clean_num_cell(dt[[gold_col]])
  vals <- vals[nzchar(vals)]
  if (!length(vals)) return(FALSE)
  mean(grepl("^[0-9]+$", vals)) >= 0.8
}

parse_sport_page <- function(page) {
  tables <- html_nodes(page, "table.wikitable")
  if (!length(tables)) return(NULL)

  noc <- NULL; n_events <- NA_integer_

  for (tb in tables) {
    dt <- tryCatch(setDT(html_table(tb, fill = TRUE)), error = function(e) NULL)
    if (is.null(dt) || !nrow(dt)) next
    setnames(dt, make.unique(tolower(gsub("[^a-zA-Z0-9]", "_", names(dt)))))
    cn <- names(dt)
    if (!(any(grepl("gold", cn)) && any(grepl("silver", cn)) && any(grepl("bronze", cn)))) next

    gold_col <- cn[grepl("gold", cn)][1]

    # The medallists table gives the event count for free.
    if (any(grepl("^event", cn)) && is.na(n_events)) {
      ev <- clean_nation(dt[[cn[grepl("^event", cn)][1]]])
      n_events <- sum(nzchar(ev) & !grepl("^total", ev, ignore.case = TRUE))
    }

    if (!is.null(noc) || !looks_like_noc_table(dt, gold_col, cn)) next

    nation_col <- cn[grepl("noc|nation|team|country|cga", cn)][1]
    silver_col <- cn[grepl("silver", cn)][1]
    bronze_col <- cn[grepl("bronze", cn)][1]
    total_col  <- cn[grepl("^total", cn)][1]

    out <- data.table(
      nation = clean_nation(dt[[nation_col]]),
      gold   = as_int(dt[[gold_col]]),
      silver = as_int(dt[[silver_col]]),
      bronze = as_int(dt[[bronze_col]]),
      total  = if (!is.na(total_col)) as_int(dt[[total_col]]) else 0L
    )
    tot_idx <- which(grepl("^\\s*totals?\\b", out$nation, ignore.case = TRUE))
    declared_gold <- if (length(tot_idx)) out$gold[tot_idx[1]] else NA_integer_
    out <- out[!grepl("^\\s*totals?\\b", nation, ignore.case = TRUE) & nation != ""]
    out[total == 0, total := gold + silver + bronze]
    if (!nrow(out)) next
    attr(out, "declared_gold") <- declared_gold
    noc <- out
  }

  if (is.null(noc)) return(NULL)
  list(noc = noc, n_events = n_events,
       declared_gold = attr(noc, "declared_gold"))
}

# Candidate sport slugs. Wikipedia 404s the ones an edition did not contest,
# which is how the programme per edition is discovered -- no hardcoded
# per-edition sport list to drift out of date.
SPORTS <- c(
  "Athletics", "Para-athletics", "Swimming", "Para-swimming", "Diving",
  "Synchronised swimming", "Artistic swimming", "Water polo",
  "Artistic gymnastics", "Rhythmic gymnastics", "Trampoline gymnastics",
  "Gymnastics", "Boxing", "Judo", "Wrestling", "Taekwondo", "Fencing",
  "Karate", "Weightlifting", "Para powerlifting", "Powerlifting",
  "Track cycling", "Road cycling", "Mountain biking", "BMX", "Cycling",
  "Rowing", "Canoeing", "Sailing", "Shooting", "Archery", "Equestrian",
  "Modern pentathlon", "Triathlon", "Badminton", "Table tennis", "Tennis",
  "Squash", "Hockey", "Field hockey", "Netball", "Basketball",
  "3x3 basketball", "Volleyball", "Beach volleyball", "Handball", "Football",
  "Rugby sevens", "Cricket", "Bowls", "Lawn bowls", "Baseball", "Softball",
  "Golf", "Surfing", "Skateboarding", "Sport climbing", "Breaking"
)

slugify <- function(s) gsub(" ", "_", s)

# Demonstration sports have a Wikipedia article with a full medal table, but
# their medals are not in the official count. Judo at Edinburgh 1986 is the
# worked example: the sport pages summed to 177 golds against the edition's
# official 163, and the gap was exactly judo's 14. Nothing errors -- the
# edition simply gains a sport it never contested, which for a head-to-head
# sport shifts the whole "opponent" class in that edition.
#
# These are found by the reconciliation report at the end of this script, not
# by guesswork: any edition whose sport sum overshoots its official total is a
# candidate, and the offending sport is then confirmed against the edition's
# article before being listed here.
DEMONSTRATION <- list(
  "commonwealth_1986"     = c("Judo"),
  # Barcelona 1992 ran taekwondo as a demonstration sport with a full 16-gold
  # medal table on Wikipedia. Including it put the edition at 103.5% of its own
  # official total -- found by the edition reconciliation below, exactly as the
  # 1986 judo case was.
  "olympics_summer_1992"  = c("Taekwondo")
)

is_demonstration <- function(series, year, sport_name) {
  key <- paste0(series, "_", year)
  d <- DEMONSTRATION[[key]]
  !is.null(d) && sport_name %in% d
}

slug_for <- function(tbl, y) Filter(function(it) it[[1]] == y, tbl)[[1]][[2]]

EDITION_SUFFIX <- list(
  olympics_summer = function(y) sprintf("the_%d_Summer_Olympics", y),
  commonwealth    = function(y) paste0("the_", slug_for(cw_slugs, y)),
  asian_games     = function(y) sprintf("the_%d_Asian_Games", y),
  panam_games     = function(y) sprintf("the_%d_Pan_American_Games", y),
  african_games   = function(y) paste0("the_", slug_for(afr_slugs, y))
)

harvest_series_sports <- function(series, years) {
  rows <- list(); meta <- list()
  for (y in years) {
    suffix <- EDITION_SUFFIX[[series]](y)
    seen <- character(0)     # canonical titles already taken this edition
    found <- 0; redirected <- 0; errored <- 0; untitled <- 0
    for (sp in SPORTS) {
      slug <- paste0(slugify(sp), "_at_", suffix)
      page <- fetch_cached(slug)
      if (is.null(page)) next

      canon_title <- canonical_title(page)
      # Counted, not silently skipped: a page that fetched but whose title
      # could not be read is a parser problem, and lumping it in with slugs
      # that 404'd hides it from every diagnostic this loop prints.
      if (is.na(canon_title)) { untitled <- untitled + 1; next }
      # Must still be a "{Something} at the {this edition}" article. A redirect
      # to the Games article itself, or to another edition, is rejected.
      if (!grepl(paste0("_at_", suffix, "$"), canon_title)) { redirected <- redirected + 1; next }
      if (canon_title %in% seen) { redirected <- redirected + 1; next }

      # A real parse EXCEPTION and a documented "this page has no NOC table"
      # are different events. Collapsing both to NULL meant a malformed table
      # or an unexpected column shape vanished into the same silence as a sport
      # that simply was not contested.
      p <- tryCatch(parse_sport_page(page),
                    error = function(e) {
                      errored <<- errored + 1
                      message(sprintf("  parse error on %s: %s", slug,
                                      conditionMessage(e)))
                      NULL
                    })
      if (is.null(p)) next

      # Name the sport from the article we actually landed on, not the guess.
      sport_name <- gsub("_", " ", sub(paste0("_at_", suffix, "$"), "", canon_title))
      seen <- c(seen, canon_title)
      if (is_demonstration(series, y, sport_name)) { redirected <- redirected + 1; next }

      r <- copy(p$noc)
      r[, `:=`(games = series, year = y, sport = sport_name)]
      rows[[length(rows) + 1]] <- r
      meta[[length(meta) + 1]] <- data.table(
        games = series, year = y, sport = sport_name, probed_as = sp,
        n_events_listed = p$n_events,
        declared_gold = p$declared_gold,
        summed_gold = sum(r$gold),
        n_nations = nrow(r), slug = slug, canonical = canon_title
      )
      found <- found + 1
    }
    cat(sprintf("  %s %d: %d sports (%d redirects/aliases%s%s)\n",
                series, y, found, redirected,
                if (errored)  sprintf(", %d PARSE ERRORS", errored) else "",
                if (untitled) sprintf(", %d untitled", untitled) else ""))
  }
  list(rows = rbindlist(rows, fill = TRUE), meta = rbindlist(meta, fill = TRUE))
}

cat("=== Commonwealth Games ===\n")
cw <- harvest_series_sports("commonwealth", vapply(cw_slugs, function(x) x[[1]], numeric(1)))

cat("=== Summer Olympics ===\n")
so <- harvest_series_sports("olympics_summer", series_years$olympics_summer)

cat("=== Asian Games ===\n")
ag <- harvest_series_sports("asian_games", series_years$asian_games)

cat("=== Pan American Games ===\n")
pg <- harvest_series_sports("panam_games", series_years$panam_games)

cat("=== African Games ===\n")
af <- harvest_series_sports("african_games",
                            vapply(afr_slugs, function(x) x[[1]], numeric(1)))

sport_dt <- rbindlist(list(cw$rows, so$rows, ag$rows, pg$rows, af$rows), fill = TRUE)
meta_dt  <- rbindlist(list(cw$meta, so$meta, ag$meta, pg$meta, af$meta), fill = TRUE)

setcolorder(sport_dt, c("games", "year", "sport", "nation", "gold", "silver", "bronze", "total"))

cat(sprintf("\n%d rows; %d edition-sports; %d editions\n",
            nrow(sport_dt), nrow(meta_dt), uniqueN(meta_dt[, .(games, year)])))

cat("\n=== reconciliation: summed gold vs the page's own totals row ===\n")
meta_dt[, recon := fifelse(is.na(declared_gold), "no_totals_row",
                    fifelse(declared_gold == summed_gold, "ok", "MISMATCH"))]
print(meta_dt[, .N, by = recon])
mm <- meta_dt[recon == "MISMATCH"]
if (nrow(mm)) print(head(mm[, .(games, year, sport, declared_gold, summed_gold)], 30))

# --- edition-level reconciliation ------------------------------------------
# The per-page totals row proves each sport table parsed correctly. This proves
# the SET of sports is right: an edition's sports must sum to the gold total in
# the official medal table. An overshoot means a sport is in that the edition
# never contested (a demonstration sport, or a redirect that slipped the
# canonical check); a shortfall means a sport is missing.
overall <- unique(as.data.table(readRDS(file.path(OUT, "multisport_medal_tables.rds")))[
  , .(games, year, official_golds = total_golds_in_games)])
ed <- sport_dt[, .(sport_golds = sum(gold), sports = uniqueN(sport)), by = .(games, year)]
ed <- merge(ed, overall, by = c("games", "year"), all.x = TRUE)
ed[, gap := sport_golds - official_golds]
ed[, pct := round(100 * sport_golds / official_golds, 1)]
setorder(ed, games, year)

cat("\n=== EDITION RECONCILIATION: sport sum vs official gold total ===\n")
cat(sprintf("editions within 2%%: %d/%d\n", sum(abs(ed$pct - 100) <= 2, na.rm = TRUE), nrow(ed)))
cat("\nOvershooting editions (a sport is in that should not be):\n")
print(ed[gap > 0][order(-gap)][1:min(15, sum(ed$gap > 0, na.rm = TRUE))])
cat("\nWorst shortfalls (sports missing from the harvest):\n")
print(head(ed[gap < 0][order(gap)], 15))
fwrite(ed, file.path(OUT, "sport_medal_tables_edition_recon.csv"))

# Both reconciliations travel on the data. Without this the checks below are
# console decoration: a wrong-table pick or a demonstration sport is correctly
# detected, printed among hundreds of lines, and then written to the file the
# analysis actually reads with nothing marking the row.
sport_dt <- merge(sport_dt, meta_dt[, .(games, year, sport, page_recon = recon)],
                  by = c("games", "year", "sport"), all.x = TRUE)
sport_dt <- merge(sport_dt, ed[, .(games, year, edition_pct = pct)],
                  by = c("games", "year"), all.x = TRUE)
sport_dt[, edition_overshoots := !is.na(edition_pct) & edition_pct > 102]

cat(sprintf("\nRows carrying a non-ok page reconciliation: %d\n",
            nrow(sport_dt[page_recon != "ok"])))
cat(sprintf("Rows in an edition overshooting its official total: %d\n",
            nrow(sport_dt[edition_overshoots == TRUE])))

saveRDS(sport_dt, file.path(OUT, "sport_medal_tables.rds"))
fwrite(sport_dt, file.path(OUT, "sport_medal_tables.csv"))
fwrite(meta_dt,  file.path(OUT, "sport_medal_tables_meta.csv"))
cat("\nSaved sport_medal_tables.\n")
