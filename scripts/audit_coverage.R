# Coverage audit for BOTH sports, run BEFORE committing to another harvest.
#
# We have re-harvested twice. Each time the trigger was discovering a gap after
# the fact -- collapsed race keys, then a registry missing 42 events. This asks
# the question up front instead: what exists that we do not have, and what would
# we regret not capturing?
#
# Four dimensions, because a harvest can be complete in one and empty in another:
#   1. COMPETITIONS  listed by the feed vs cached by us
#   2. ATHLETES      appearing in fields we predict but carrying no history
#   3. TIME          span and gaps
#   4. ROUNDS        heats/semis/finals, since finals alone destroy variance
#                    estimation and no-mark rates

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

hdr <- function(x) cli::cli_h2(x)
line <- function(fmt, ...) cat(sprintf(paste0("  ", fmt, "\n"), ...))

# ---------------------------------------------------------------- athletics --
hdr("ATHLETICS")
ch <- tryCatch(
  with_citius_db_connection(function(conn) load_championship_results(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(ch) || !nrow(ch)) ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
comps <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))
line("harvested : %s results | %s meets | %s athletes",
     format(nrow(ch), big.mark = ","), uniqueN(ch$competition_id),
     format(uniqueN(ch$athlete_id), big.mark = ","))
line("known list: %s competitions", format(nrow(comps), big.mark = ","))
line("cached    : %s of them",
     format(sum(comps$competition_id %in% unique(ch$competition_id)), big.mark = ","))
line("span      : %s to %s", min(ch$date, na.rm = TRUE), max(ch$date, na.rm = TRUE))

cat("\n  rounds held (finals alone would break variance and no-mark estimation):\n")
print(ch[, .(results = .N, races = uniqueN(race_key)), by = round][order(-results)][1:6])

cat("\n  results per YEAR — a thin recent year is the one that hurts predictions:\n")
ch[, yr := data.table::year(date)]
print(ch[, .(results = .N, meets = uniqueN(competition_id)), by = yr][order(yr)])

# The population that actually matters: athletes in championship FIELDS whose
# history is too thin to rate.
fin <- ch[!is.na(event_id) & !is.na(place) & grepl("final", round, ignore.case = TRUE)]
per <- ch[!is.na(event_id), .N, by = .(athlete_id, event_id)]
thin <- merge(unique(fin[, .(athlete_id, event_id)]), per,
              by = c("athlete_id", "event_id"), all.x = TRUE)
thin[is.na(N), N := 0L]
cat("\n  finalists by how many results we hold for them in that event:\n")
print(thin[, .(athlete_events = .N), by = .(results = cut(N, c(-1, 0, 1, 2, 5, 10, Inf),
    labels = c("0", "1", "2", "3-5", "6-10", "10+")))][order(results)])
line("finalists with <=2 results in the event: %.1f%%",
     100 * mean(thin$N <= 2))

# ----------------------------------------------------------------- swimming --
hdr("SWIMMING")
sw <- setDT(readRDS(file.path(OUT, "swimming_history.rds")))
line("harvested : %s swims | %s meets | %s athletes",
     format(nrow(sw), big.mark = ","), uniqueN(sw$competition_id),
     format(uniqueN(sw$athlete_id), big.mark = ","))
pages <- rbindlist(lapply(0:25, function(p) {
  tryCatch(aquatics_competitions(page = p, page_size = 100, sort = "dateFrom,desc"),
           error = function(e) NULL)
}), use.names = TRUE, fill = TRUE)
pages <- unique(pages, by = "competition_id")
setDT(pages)
line("listed by the feed: %s competitions", format(nrow(pages), big.mark = ","))

OTHER <- "Water Polo|Diving|Artistic|Open Water|High Diving"
pool <- pages[!grepl(OTHER, official_name, ignore.case = TRUE) &
                date_from <= Sys.Date() & date_from >= as.Date("2015-01-01")]
line("plausibly POOL SWIMMING since 2015: %s", format(nrow(pool), big.mark = ","))
line("of those, already cached: %s",
     format(sum(pool$competition_id %in% unique(sw$competition_id)), big.mark = ","))
line("NOT cached: %s  <- the gap", format(sum(!pool$competition_id %in% unique(sw$competition_id)), big.mark = ","))

cat("\n  what the current filter keeps vs what it discards:\n")
CURRENT <- "Olympic Games|World Aquatics Championships|Swimming World Cup|World Swimming Championships"
EXCLUDE <- "Water Polo|Diving|Artistic|Open Water|Masters|Junior|Youth|25m"
pool[, kept := grepl(CURRENT, official_name, ignore.case = TRUE) &
       !grepl(EXCLUDE, official_name, ignore.case = TRUE)]
print(pool[, .(competitions = .N), by = kept])
cat("\n  a sample of what the filter DISCARDS:\n")
print(head(pool[kept == FALSE][order(-date_from),
                               .(name = substr(official_name, 1, 52), date_from)], 15))
line("")
line("The 37%% of Glasgow swimmers with no history are national-level athletes.")
line("The current filter keeps only Olympics/Worlds/World Cup, so by construction")
line("it cannot contain them.")

cat("\n  swimming rounds held:\n")
print(sw[, .(swims = .N), by = round][order(-swims)][1:6])
cat("\n  swimming results per year:\n")
sw[, yr := data.table::year(date)]
print(sw[, .(swims = .N, meets = uniqueN(competition_id)), by = yr][order(yr)])
