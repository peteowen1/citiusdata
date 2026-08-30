# Harvest every competition the feed says has results and we do not hold.
#
# The general form of harvest_missing_majors.R. That script checks the majors;
# this one closes the whole gap, because coverage was 97% of club meets and 50%
# of Olympics/Worlds and nobody knew until someone diffed the two lists.
#
# Usage:  Rscript scripts/harvest_gap.R          (all)
#         CITIUS_GAP_FROM=2016 Rscript ...       (backtest era only)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_gap")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

cc <- setDT(readRDS(file.path(OUT, "ath_competitions.rds")))
if (!nrow(cc)) cli::cli_abort("ath_competitions.rds loaded 0 rows.")
cli::cli_alert_info("ath_competitions.rds: {nrow(cc)} row{?s}, {min(cc$start, na.rm = TRUE)}..{max(cc$start, na.rm = TRUE)}")
ch <- tryCatch(
  with_citius_db_connection(function(conn) load_championship_results(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(ch) || !nrow(ch)) ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
if (!nrow(ch)) cli::cli_abort("championship_results.rds loaded 0 rows.")
cli::cli_alert_info("championship_results.rds: {format(nrow(ch), big.mark = ',')} row{?s}, {min(ch$date, na.rm = TRUE)}..{max(ch$date, na.rm = TRUE)}")
cc[, harvested := competition_id %in% unique(ch$competition_id)]
if (!"has_results" %in% names(cc)) cc[, has_results := TRUE]
cc[is.na(has_results), has_results := TRUE]
gap <- cc[has_results == TRUE & !harvested]
FROM <- suppressWarnings(.env_int("CITIUS_GAP_FROM", "0"))
if (!is.na(FROM) && FROM > 0) gap <- gap[suppressWarnings(as.integer(substr(start, 1, 4))) >= FROM]
cli::cli_h2("Gap: {nrow(gap)} competition{?s} with results and no data")

got <- list(); failed <- character()
for (i in seq_len(nrow(gap))) {
  cid <- gap$competition_id[i]
  f <- file.path(CACHE, paste0(cid, ".rds"))
  if (file.exists(f)) { got[[length(got)+1L]] <- readRDS(f); next }
  r <- tryCatch(setDT(athletics_competition_results(cid)), error = function(e) {
    failed <<- c(failed, paste0(cid, ": ", conditionMessage(e))); NULL })
  if (is.null(r) || !nrow(r)) next
  saveRDS(r, f); got[[length(got)+1L]] <- r
  if (i %% 20 == 0) cli::cli_alert_info("{i}/{nrow(gap)} ... {length(got)} fetched")
}
if (length(got)) {
  new <- rbindlist(got, fill = TRUE)
  saveRDS(new, file.path(OUT, "championship_results_gap.rds"))
  cli::cli_alert_success("{format(nrow(new), big.mark=',')} results from {uniqueN(new$competition_id)} competition{?s}")
} else cli::cli_alert_warning("Nothing fetched.")
# Failures are REPORTED, not swallowed -- that is how five majors went missing.
if (length(failed)) {
  cli::cli_alert_danger("{length(failed)} competition{?s} failed:")
  writeLines(utils::head(failed, 20))
}
