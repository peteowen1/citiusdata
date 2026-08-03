# Harvest historic multi-sport medal tables from Wikipedia.
#
# Every table is cross-checked against its own "Totals" row before it is
# accepted. That check is the point of this script: the previous version summed
# the nation rows and called the sum the truth, so when the 2026 Commonwealth
# table was scraped mid-competition on 2 August it recorded 201 golds against a
# real 216 and nothing said otherwise. A parse that disagrees with the page's
# own total is now a hard failure, not a silent number.

library(rvest)
library(httr)
library(data.table)
library(arrow)

OUT  <- "C:/dev/citiusverse/citiusdata/data"
INST <- "C:/dev/citiusverse/citius/inst/extdata"
dir.create(OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(INST, showWarnings = FALSE, recursive = TRUE)

user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) CitiusVerseScraper/5.0"

source("C:/dev/citiusverse/citiusdata/scripts/games_reference.R")

# Participating-nation counts scraped from each edition's infobox. These are
# authoritative; `nations_count_map` in games_reference.R is the fallback for
# the one edition with no infobox figure. The hand map disagreed with the
# infobox on 18 of 148 editions -- including commonwealth_1986, where it said
# 26 against a documented 27 -- and that count is the denominator of the
# expected-share baseline, so run harvest_participating_nations.R first.
NAT_PATH <- file.path(OUT, "games_participating_nations.csv")
nations_ref <- if (file.exists(NAT_PATH)) {
  fread(NAT_PATH)[, .(games, year, competing_nations, source)]
} else {
  warning("games_participating_nations.csv missing; falling back to the hand map.")
  NULL
}

lookup_nations <- function(games_name, year_val) {
  if (!is.null(nations_ref)) {
    v <- nations_ref[games == games_name & year == year_val, competing_nations]
    if (length(v) && !is.na(v[1])) return(as.integer(v[1]))
  }
  k <- paste0(games_name, "_", year_val)
  if (!is.null(nations_count_map[[k]])) as.integer(nations_count_map[[k]]) else NA_integer_
}

# ---------------------------------------------------------------- parsing ----

clean_num <- function(vec) {
  vapply(vec, function(x) {
    if (is.na(x)) return(0L)
    st <- as.character(x)
    st <- gsub("\\[.*?\\]|\\(.*?\\)", "", st)   # footnote and parenthetical
    st <- strsplit(st, "/")[[1]][1]
    st <- gsub("[^0-9]", "", trimws(st))
    if (st == "") return(0L)
    as.integer(st)
  }, integer(1), USE.NAMES = FALSE)
}

clean_nation <- function(vec) {
  vapply(vec, function(x) {
    if (is.na(x)) return("")
    st <- as.character(x)
    st <- gsub("\u00a0", " ", st)
    st <- gsub("\\[.*?\\]|\\(.*?\\)|\\*|\u2020|\u2021", "", st)
    st <- gsub("[0-9]+[a-z]?$", "", trimws(st))
    trimws(st)
  }, character(1), USE.NAMES = FALSE)
}

is_totals_row <- function(nation_vec) {
  grepl("^\\s*totals?\\b", nation_vec, ignore.case = TRUE)
}

#' Parse one wikitable into a medal table, or return NULL.
parse_medal_table <- function(tb) {
  dt <- tryCatch(setDT(html_table(tb, fill = TRUE)), error = function(e) NULL)
  if (is.null(dt) || !nrow(dt)) return(NULL)

  # Schedule tables repeat a date header, so the incoming names can already be
  # duplicated -- rename positionally, which tolerates that, and de-duplicate.
  setnames(dt, make.unique(tolower(gsub("[^a-zA-Z0-9]", "_", names(dt)))))
  cn <- names(dt)
  if (!(any(grepl("gold", cn)) && any(grepl("silver", cn)) && any(grepl("bronze", cn)))) {
    return(NULL)
  }

  nation_col <- cn[grepl("noc|nation|team|country|cga", cn)][1]
  gold_col   <- cn[grepl("gold", cn)][1]
  silver_col <- cn[grepl("silver", cn)][1]
  bronze_col <- cn[grepl("bronze", cn)][1]
  total_col  <- cn[grepl("^total", cn)][1]
  rank_col   <- cn[grepl("rank", cn)][1]
  if (is.na(nation_col) || is.na(gold_col)) return(NULL)

  nat <- clean_nation(dt[[nation_col]])
  out <- data.table(
    rank   = if (!is.na(rank_col)) as.character(dt[[rank_col]]) else NA_character_,
    nation = nat,
    gold   = clean_num(dt[[gold_col]]),
    silver = clean_num(dt[[silver_col]]),
    bronze = clean_num(dt[[bronze_col]]),
    total  = if (!is.na(total_col)) clean_num(dt[[total_col]]) else 0L
  )

  tot_idx <- which(is_totals_row(out$nation))
  declared <- if (length(tot_idx)) {
    list(gold   = out$gold[tot_idx[1]],
         silver = out$silver[tot_idx[1]],
         bronze = out$bronze[tot_idx[1]])
  } else NULL

  out <- out[!is_totals_row(nation) & nation != ""]
  out[total == 0, total := gold + silver + bronze]
  list(rows = out, declared = declared)
}

#' Fetch a games edition's medal table, validating against the page's own totals.
#'
#' @return A data.table, or NULL if no table was found. Attribute `"check"`
#'   carries the reconciliation outcome for the audit log.
fetch_wiki_medal_table <- function(slug, games_name, year_val) {
  urls_to_try <- c(
    paste0("https://en.wikipedia.org/wiki/", slug, "_medal_table"),
    paste0("https://en.wikipedia.org/wiki/", slug)
  )

  page <- NULL; used_url <- NA_character_
  for (u in urls_to_try) {
    res <- tryCatch(GET(u, user_agent(user_agent_str), timeout(20)), error = function(e) NULL)
    if (!is.null(res) && status_code(res) == 200) {
      page <- read_html(content(res, "text", encoding = "UTF-8"))
      used_url <- u
      break
    }
  }
  if (is.null(page)) return(NULL)

  tables <- html_nodes(page, "table.wikitable")
  if (!length(tables)) return(NULL)

  # Prefer a table that carries its own totals row -- that is the medal table
  # proper, not a per-discipline summary that happens to share column names.
  parsed <- NULL
  for (tb in tables) {
    p <- parse_medal_table(tb)
    if (is.null(p)) next
    if (is.null(parsed)) parsed <- p
    if (!is.null(p$declared)) { parsed <- p; break }
  }
  if (is.null(parsed)) return(NULL)

  dt_sub <- parsed$rows
  summed <- list(gold   = sum(dt_sub$gold),
                 silver = sum(dt_sub$silver),
                 bronze = sum(dt_sub$bronze))

  check <- if (is.null(parsed$declared)) {
    "no_totals_row"
  } else if (identical(as.integer(unlist(summed)), as.integer(unlist(parsed$declared)))) {
    "ok"
  } else {
    sprintf("MISMATCH summed G/S/B %d/%d/%d vs declared %d/%d/%d",
            summed$gold, summed$silver, summed$bronze,
            parsed$declared$gold, parsed$declared$silver, parsed$declared$bronze)
  }

  dt_sub[, `:=`(games = games_name, year = as.integer(year_val))]

  tot_golds  <- if (!is.null(parsed$declared)) parsed$declared$gold else summed$gold
  tot_medals <- if (!is.null(parsed$declared)) {
    parsed$declared$gold + parsed$declared$silver + parsed$declared$bronze
  } else sum(dt_sub$total)

  dt_sub[, total_golds_in_games  := tot_golds]
  dt_sub[, total_medals_in_games := tot_medals]
  dt_sub[, medalling_nations     := uniqueN(nation)]
  dt_sub[, gold_share  := if (tot_golds  > 0) as.numeric(gold)  / tot_golds  else 0.0]
  dt_sub[, medal_share := if (tot_medals > 0) as.numeric(total) / tot_medals else 0.0]

  hk <- paste0(games_name, "_", year_val)
  dt_sub[, host := if (!is.null(hosts_map[[hk]])) hosts_map[[hk]] else NA_character_]
  dt_sub[, competing_nations := lookup_nations(games_name, year_val)]
  dt_sub[, source_url := used_url]
  dt_sub[, harvested_at := as.character(Sys.time())]

  setcolorder(dt_sub, c("games", "year", "rank", "nation", "gold", "silver", "bronze", "total"))
  attr(dt_sub, "check") <- check
  dt_sub
}

# ---------------------------------------------------------------- harvest ----

all_tables <- list()
audit <- list()

harvest_series <- function(label, slugs) {
  cat(sprintf("=== %s ===\n", label))
  for (item in slugs) {
    yr <- item[[1]]; slug <- item[[2]]; gname <- item[[3]]
    res <- fetch_wiki_medal_table(slug, gname, yr)
    if (is.null(res)) {
      cat(sprintf("  %s %s: NO TABLE FOUND\n", gname, yr))
      audit[[length(audit) + 1]] <<- data.table(games = gname, year = yr, check = "NO_TABLE")
      next
    }
    chk <- attr(res, "check")
    if (chk != "ok") cat(sprintf("  %s %s: %s\n", gname, yr, chk))
    audit[[length(audit) + 1]] <<- data.table(games = gname, year = yr, check = chk)
    all_tables[[length(all_tables) + 1]] <<- res
  }
}

harvest_series("Summer Olympics", lapply(series_years$olympics_summer, function(y)
  list(y, sprintf("%d_Summer_Olympics", y), "olympics_summer")))

harvest_series("Winter Olympics", lapply(series_years$olympics_winter, function(y)
  list(y, sprintf("%d_Winter_Olympics", y), "olympics_winter")))

harvest_series("Commonwealth Games", lapply(seq_along(cw_slugs), function(i)
  list(cw_slugs[[i]][[1]], cw_slugs[[i]][[2]], "commonwealth")))

harvest_series("Asian Games", lapply(series_years$asian_games, function(y)
  list(y, sprintf("%d_Asian_Games", y), "asian_games")))

harvest_series("Pan American Games", lapply(series_years$panam_games, function(y)
  list(y, sprintf("%d_Pan_American_Games", y), "panam_games")))

harvest_series("African Games", lapply(seq_along(afr_slugs), function(i)
  list(afr_slugs[[i]][[1]], afr_slugs[[i]][[2]], "african_games")))

harvest_series("European Games", lapply(series_years$european_games, function(y)
  list(y, sprintf("%d_European_Games", y), "european_games")))

harvest_series("Pacific Games", lapply(seq_along(pac_slugs), function(i)
  list(pac_slugs[[i]][[1]], pac_slugs[[i]][[2]], "pacific_games")))

master_dt <- rbindlist(all_tables, fill = TRUE)
audit_dt  <- rbindlist(audit, fill = TRUE)

cat(sprintf("\nHarvested %d rows across %d editions.\n",
            nrow(master_dt), uniqueN(paste(master_dt$games, master_dt$year))))
cat("\n=== RECONCILIATION AGAINST EACH PAGE'S OWN TOTALS ROW ===\n")
print(audit_dt[, .N, by = .(status = fifelse(check == "ok", "ok", check))][order(-N)])

bad <- audit_dt[check != "ok"]
if (nrow(bad)) {
  cat("\nEditions needing review:\n")
  print(bad)
}

miss_nations <- master_dt[is.na(competing_nations),
                          .N, by = .(games, year)]
if (nrow(miss_nations)) {
  cat("\nEditions with no competing_nations reference entry:\n")
  print(miss_nations)
}

write_parquet(master_dt, file.path(OUT, "multisport_medal_tables.parquet"))
saveRDS(master_dt, file.path(OUT, "multisport_medal_tables.rds"))
saveRDS(master_dt, file.path(INST, "multisport_medal_tables.rds"))
fwrite(audit_dt, file.path(OUT, "multisport_medal_tables_audit.csv"))
cat("\nSaved multisport_medal_tables + audit log.\n")
