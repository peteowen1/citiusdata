# Fetch the major championships that are in the feed and were never harvested.
#
# Found 2026-07-31 by comparing ath_competitions.rds against what
# championship_results.rds actually holds. Coverage by feed tier:
#
#   F (club)  555 in feed, 538 harvested   96.9%
#   D          87            85            97.7%
#   GW         83            79            95.2%
#   OW          6             3            50.0%   <-- Olympics and Worlds
#
# We harvested 97% of the club meets and half of the majors. Every missing one
# reports has_results = TRUE, so this is an unfetched gap, not a data limit.
#
# It matters because the target population is tiny: the test period holds 86
# Olympic/World final races, which is far too few to resolve the effect sizes
# the arms have been producing. These competitions roughly double it.
#
# Writes to a SEPARATE file. Merging into championship_results.rds changes a
# shared input that calibrations, stores and every backtest arm read, so that
# step is deliberate and manual -- see the end of this script.
#
# Usage:  Rscript scripts/harvest_missing_majors.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_majors")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

# The discovery list is a KEYWORD SWEEP, not the feed's catalogue, so it can
# only contain meets someone thought to search for. Probe the feed directly for
# the major names as well, and union the two.
cc <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))
if (!nrow(cc)) cli::cli_abort("ath_competitions.rds loaded 0 rows.")
cli::cli_alert_info("ath_competitions.rds: {nrow(cc)} row{?s}, {min(cc$start, na.rm = TRUE)}..{max(cc$start, na.rm = TRUE)}")
probe_terms <- c("World Championships in Athletics", "IAAF World Championships",
                 "Olympic Games", "Commonwealth Games", "World Indoor Championships")
probed <- rbindlist(lapply(probe_terms, function(t)
  tryCatch(setDT(athletics_find_competition(t)), error = function(e) NULL)), fill = TRUE)
if (nrow(probed)) {
  idc <- intersect(c("competition_id", "id"), names(probed))[1]
  if (!is.null(idc) && idc != "competition_id") setnames(probed, idc, "competition_id")
  keepc <- intersect(names(cc), names(probed))
  cc <- unique(rbind(cc, probed[, ..keepc], fill = TRUE), by = "competition_id")
  cli::cli_alert_info("Feed probe added {nrow(probed)} row{?s}; list now {nrow(cc)} competitions.")
}
if (!"has_results" %in% names(cc)) cc[, has_results := TRUE]
cc[is.na(has_results), has_results := TRUE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
have <- unique(ch$competition_id)

# The senior global championships, and nothing that merely mentions one.
# NAMES CHANGE. The body was the IAAF until 2019, so the 2017 London and 2019
# Doha World Championships are "IAAF World Championships in Athletics" and were
# missed by the first version of this pattern, which only knew the modern name.
# A coverage check that assumes one naming convention finds only the meets named
# the way you expected.
MAJOR <- paste0("Olympic Games|XXX+ Olympic|Games of the [IVX]+ Olympiad|",
                "World Athletics Championships|IAAF World Championships|",
                "World Championships in Athletics|",
                "Commonwealth Games|",
                "World Athletics Indoor Championships|IAAF World Indoor|",
                "World Indoor Championships")
NOT   <- "Trials|Qualifier|Qualifying|Anniversary|Open Meeting|Selection|Throwing|Youth|U20|Junior"
want <- cc[grepl(MAJOR, name, ignore.case = TRUE, perl = TRUE) &
             !grepl(NOT, name, ignore.case = TRUE, perl = TRUE) &
             has_results == TRUE]
miss <- want[!competition_id %in% have]
cli::cli_h2("Majors in the feed: {nrow(want)} | already harvested: {sum(want$competition_id %in% have)} | missing: {nrow(miss)}")
print(miss[order(start), .(competition_id, name = substr(name, 1, 48),
                           start = substr(start, 1, 10), tier)])
if (!nrow(miss)) { cli::cli_alert_success("Nothing missing."); quit(save = "no") }

got <- list()
for (i in seq_len(nrow(miss))) {
  cid <- miss$competition_id[i]
  f <- file.path(CACHE, paste0(cid, ".rds"))
  if (file.exists(f)) {
    got[[length(got) + 1L]] <- readRDS(f)
    cli::cli_alert_info("{cid} cached.")
    next
  }
  cli::cli_alert("Fetching {i}/{nrow(miss)}: {miss$name[i]}")
  r <- tryCatch(setDT(athletics_competition_results(cid)),
                error = function(e) { cli::cli_alert_warning("failed: {conditionMessage(e)}"); NULL })
  if (is.null(r) || !nrow(r)) { cli::cli_alert_warning("{cid}: no results returned."); next }
  saveRDS(r, f)
  cli::cli_alert_success("{cid}: {nrow(r)} results, {uniqueN(r$race_key)} races.")
  got[[length(got) + 1L]] <- r
}
if (!length(got)) { cli::cli_alert_warning("Nothing fetched."); quit(save = "no") }

new <- rbindlist(got, fill = TRUE)
saveRDS(new, file.path(OUT, "championship_results_majors.rds"))
cli::cli_h2("Fetched {format(nrow(new), big.mark=',')} results across {uniqueN(new$competition_id)} competition{?s}")
print(new[, .(results = .N, races = uniqueN(race_key), athletes = uniqueN(athlete_id),
              finals = uniqueN(race_key[grepl("final", round, ignore.case = TRUE) &
                                          !grepl("semi", round, ignore.case = TRUE)])),
          by = .(competition_id)][order(-results)])

cat("\nTO MERGE (deliberate, changes a shared input every arm reads):\n")
cat("  ch <- rbind(readRDS('championship_results.rds'),\n")
cat("              readRDS('championship_results_majors.rds'), fill = TRUE)\n")
cat("  saveRDS(unique(ch, by = c('competition_id','race_key','athlete_id')),\n")
cat("          'championship_results.rds')\n")
cat("  then: build_athletics_corpus.R, build_stores.R, recalibrate_corpus.R\n")
