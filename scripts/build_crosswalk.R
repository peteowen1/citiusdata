# Build the cross-source athlete crosswalk for both sports.
#
# Every source spells the same athlete differently and only some carry a stable
# id, so without this each script re-derives its own match and they drift apart.
# This writes ONE table per sport that any script can join against, and that can
# be corrected by hand when a match is wrong.
#
# The unmatched rows are not a failure report -- they are the harvest to-do
# list, which is the main thing this is for.
#
# Usage:  Rscript scripts/build_crosswalk.R
# Resolve paths from the verse root rather than the working directory, so the
# script runs identically from citiusdata/, from the verse root, or from a
# scheduler with no meaningful cwd.
VERSE <- here::here()
suppressMessages({
  devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE)
  library(data.table)
})
D <- file.path(VERSE, "citiusdata", "data")
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))

`%||%` <- function(a, b) if (is.null(a)) b else a
say <- function(...) cat(sprintf(...), "\n", sep = "")

# The headline number is coverage of the GAMES field, not of the whole crosswalk.
# Most historical athletes are simply not at Glasgow, so a raw "unmatched" count
# is dominated by them and reads as a failure when nothing is wrong.
report_gap <- function(xw, label) {
  linked <- xw[, .(n = uniqueN(source)), by = person_id][n > 1L, person_id]
  games <- xw[source == "crs_glasgow2026"]
  if (!nrow(games)) return(invisible())
  todo <- games[!person_id %in% linked]
  say("  Glasgow %s with prior history: %d of %d (%.0f%%) -- %d to harvest",
      label, nrow(games) - nrow(todo), nrow(games),
      100 * (1 - nrow(todo) / nrow(games)), nrow(todo))
}

# ---- athletics -------------------------------------------------------------
# Name orders genuinely differ and getting one wrong compares a surname against
# a given name: World Athletics writes "Taoufik Makhloufi" (given first, no case
# signal), the Games entry list writes "Trenton BROOKS" (caps surname).
say("=== athletics ===")
ath_src <- list()

f <- file.path(D, "championship_results.rds")
if (file.exists(f)) {
  h <- setDT(readRDS(f))
  ath_src$worldathletics <- unique(h[!is.na(athlete_name),
    .(source = "worldathletics", athlete_id = as.character(athlete_id),
      athlete_name, country = NA_character_,
      birthdate = as.Date(birthdate))])[, .SD[1L], by = athlete_id]
  say("  worldathletics: %s athletes (birthdate %.0f%%)",
      format(nrow(ath_src$worldathletics), big.mark = ","),
      100 * mean(!is.na(ath_src$worldathletics$birthdate)))
}

f <- file.path(D, "glasgow2026_entries.json")
if (file.exists(f)) {
  j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  e <- rbindlist(lapply(j$rows, function(r) data.table(
    athlete_name = r[[3]], country = r[[2]],
    birthdate = as.Date(r[[4]], format = "%d %b %Y"))), fill = TRUE)
  ath_src$crs_glasgow2026 <- unique(e)[, `:=`(
    source = "crs_glasgow2026", athlete_id = NA_character_)]
  say("  crs_glasgow2026: %s entrants (birthdate %.0f%%)",
      format(nrow(ath_src$crs_glasgow2026), big.mark = ","),
      100 * mean(!is.na(ath_src$crs_glasgow2026$birthdate)))
}

if (length(ath_src)) {
  xw_ath <- athlete_crosswalk(
    rbindlist(ath_src, fill = TRUE)[, sport := "Athletics"],
    name_order = c(worldathletics = "given_first",
                   crs_glasgow2026 = "given_first"))
  say("  -> %s rows | %s persons | methods: %s",
      format(nrow(xw_ath), big.mark = ","),
      format(uniqueN(xw_ath$person_id), big.mark = ","),
      paste(sprintf("%s %d", names(table(xw_ath$match_method)),
                    table(xw_ath$match_method)), collapse = ", "))
  arrow::write_parquet(xw_ath, file.path(D, "athlete_crosswalk_athletics.parquet"))
  report_gap(xw_ath, "athletics")
}

# ---- swimming --------------------------------------------------------------
say("\n=== swimming ===")
sw_src <- list()

f <- file.path(D, "swim_athlete_history.rds")
if (file.exists(f)) {
  s <- setDT(readRDS(f))
  sw_src$worldaquatics <- unique(s[!is.na(athlete_name),
    .(source = "worldaquatics", athlete_id = as.character(athlete_id),
      athlete_name, country,
      birthdate = if ("birthdate" %in% names(s)) as.Date(birthdate) else as.Date(NA))])
  sw_src$worldaquatics <- sw_src$worldaquatics[, .SD[1L], by = athlete_id]
  say("  worldaquatics: %s athletes (birthdate %.0f%%)",
      format(nrow(sw_src$worldaquatics), big.mark = ","),
      100 * mean(!is.na(sw_src$worldaquatics$birthdate)))
}

# glasgow_swimming() stop()s if NONE of its four capture files exist -- fine
# as an internal contract, but every other optional source in this script
# degrades gracefully on a missing/fresh-clone data/ (data/* is gitignored,
# so that's a real state, not hypothetical). Match that convention here too,
# so a missing Games capture doesn't abort the worldaquatics/swimengland/
# swimcloud sources that come after it.
g <- tryCatch(setDT(glasgow_swimming(D))[!is.na(event_id)],
              error = function(e) { say("  crs_glasgow2026: SKIPPED (%s)", conditionMessage(e)); NULL })
if (!is.null(g)) {
  stopifnot("glasgow_swimming() returned implausibly few rows - capture files may be stale/truncated" =
              nrow(g) > 300)
  sw_src$crs_glasgow2026 <- unique(g[, .(
    source = "crs_glasgow2026", athlete_id = NA_character_,
    athlete_name, country, birthdate = as.Date(NA))])
  say("  crs_glasgow2026: %s swimmers (no birthdate -- results pages omit it)",
      format(nrow(sw_src$crs_glasgow2026), big.mark = ","))
}

# Swim England and SwimCloud each carry their own stable id, so within a source
# linking is exact -- but the three namespaces are disjoint (a uuid, SE123456,
# SC123456). Unlinked, one swimmer becomes three people with three partial
# careers and empirical-Bayes shrinkage is applied to each, which under-rates
# exactly the athletes we have the most data for.
f <- file.path(D, "swimengland_rankings.rds")
if (file.exists(f)) {
  se <- setDT(readRDS(f))
  sw_src$swimengland <- unique(se[!is.na(athlete_name),
    .(source = "swimengland", athlete_id, athlete_name,
      country = NA_character_,
      # Year of birth is not a birthdate, but it is a strong disambiguator and
      # is kept for the human-review path rather than fed to the birthdate pass.
      birthdate = as.Date(NA))])[, .SD[1L], by = athlete_id]
  say("  swimengland: %s athletes", format(nrow(sw_src$swimengland), big.mark = ","))
}

d <- file.path(D, "swimcloud_cache")
if (dir.exists(d) && length(list.files(d))) {
  cache_files <- list.files(d, full.names = TRUE)
  raw <- lapply(cache_files, function(p) tryCatch(readRDS(p), error = function(e) NULL))
  bad <- cache_files[vapply(raw, is.null, logical(1))]
  if (length(bad)) {
    say("  swimcloud cache: %d corrupt file%s dropped silently otherwise: %s%s",
        length(bad), if (length(bad) == 1L) "" else "s",
        paste(basename(head(bad, 10)), collapse = ", "),
        if (length(bad) > 10) sprintf(" and %d more", length(bad) - 10) else "")
  }
  sc <- rbindlist(raw, fill = TRUE)
  if (nrow(sc)) {
    sw_src$swimcloud <- unique(sc[!is.na(athlete_name) & !is.na(athlete_id),
      .(source = "swimcloud", athlete_id, athlete_name,
        country = team, birthdate = as.Date(NA))])[, .SD[1L], by = athlete_id]
    say("  swimcloud: %s athletes", format(nrow(sw_src$swimcloud), big.mark = ","))
  }
}

if (length(sw_src)) {
  # Identities confirmed by the World Aquatics search resolver. These override
  # the ambiguity guards: "Mikkel Jun Jie LEE" has the loose key LEE|M, which is
  # ambiguous across many swimmers, so without an explicit link he stays
  # unmatched even though his career is already harvested.
  links <- NULL
  f <- file.path(D, "glasgow_swimming_gap_resolution.rds")
  if (file.exists(f)) {
    rz <- setDT(readRDS(f))[!is.na(wa_id)]
    if (nrow(rz)) {
      links <- rbind(
        rz[, .(source = "crs_glasgow2026", athlete_id = NA_character_,
               athlete_name, link_id = wa_id)],
        rz[, .(source = "worldaquatics", athlete_id = wa_id,
               athlete_name = NA_character_, link_id = wa_id)])
      say("  verified links from the gap resolver: %d identities", nrow(rz))
    }
  }
  # Hand-verified identities, for cases no rule and no search can reach --
  # a swimmer who changed their name between the two feeds, for instance.
  f <- file.path(D, "athlete_links_manual.csv")
  if (file.exists(f)) {
    mn <- setDT(read.csv(f, stringsAsFactors = FALSE))
    mn[, `:=`(athlete_name = ifelse(nzchar(athlete_name), athlete_name, NA_character_),
              athlete_id = ifelse(nzchar(as.character(athlete_id)),
                                  as.character(athlete_id), NA_character_),
              link_id = as.character(link_id))]
    links <- rbind(links, mn[, .(source, athlete_id, athlete_name, link_id)],
                   fill = TRUE)
    say("  hand-verified links: %d row%s", nrow(mn), if (nrow(mn) == 1L) "" else "s")
  }
  xw_sw <- athlete_crosswalk(
    rbindlist(sw_src, fill = TRUE)[, sport := "Swimming"],
    # Name order differs per source and getting one wrong compares a surname
    # against a given name: World Aquatics results write "SHORT Samuel", while
    # the Games feed, Swim England and SwimCloud all write given-name first.
    name_order = c(worldaquatics = "surname_first",
                   crs_glasgow2026 = "given_first",
                   swimengland = "given_first",
                   swimcloud = "given_first"),
    links = links,
    # Fuzzy name matching is only attempted for the Games field. Corpus-wide it
    # merged different people -- Sophie Bateman with BATEMAN Sarah, Kate Ward
    # with WARD Kristy -- because surname-plus-initial stops identifying anyone
    # at 40,000 x 23,000 athletes. Swim England and SwimCloud records link to
    # each other only through verified ids.
    fuzzy_scope = "crs_glasgow2026")
  say("  -> %s rows | %s persons | methods: %s",
      format(nrow(xw_sw), big.mark = ","),
      format(uniqueN(xw_sw$person_id), big.mark = ","),
      paste(sprintf("%s %d", names(table(xw_sw$match_method)),
                    table(xw_sw$match_method)), collapse = ", "))
  arrow::write_parquet(xw_sw, file.path(D, "athlete_crosswalk_swimming.parquet"))

  report_gap(xw_sw, "swimmers")
  # How much did linking the two new id spaces actually achieve? A person
  # spanning several sources is one career instead of several fragments.
  spread <- xw_sw[, .(n_src = uniqueN(source),
                      srcs = paste(sort(unique(source)), collapse = "+")), by = person_id]
  say("  persons by source coverage:")
  print(spread[, .(persons = .N), by = .(n_src, srcs)][order(-n_src, -persons)][1:8])
}

say("\nwrote athlete_crosswalk_{athletics,swimming}.parquet to %s", D)
