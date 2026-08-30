# Re-harvest competitions whose races we only hold PARTIALLY.
#
# Distinct from harvest_gap.R, which fetches competitions we hold NOTHING for.
# This one targets competitions we DO hold -- from the athlete (career) endpoint,
# which returns one athlete's races and therefore a fraction of each field.
#
# WHY IT MATTERS BEYOND COVERAGE. `decompose_races()` treats whatever was
# harvested AS the race, and its de-biasing assumes that subset is an UNBIASED
# sample of the field. Career harvesting pulls the histories of athletes we
# already track, who are systematically the better ones -- so a partial race
# looks FASTER than it was and `c_r` absorbs the difference. That is a coverage
# bias flowing straight into a fitted model parameter.
#
# Measured on the 2026-08-13 corpus, using `place` as a free completeness check:
#   479,666 races carry a place; 119,450 (24.9%) have fewer rows than max(place)
#   career-sourced races      45.5% partial
#   competition-sourced races  1.6% partial
# So the competition endpoint essentially solves it, and this is a re-harvest
# rather than a new source.
#
# ORDERED BY TIER THEN RECENCY, because the tail is flat: the top 500
# competitions hold only 15.8% of partial races, so there is no volume 80/20 --
# but a partial Diamond League final matters far more than a partial club race,
# and 2023+ holds 80% of them.
#
# RESUMABLE. Every competition is cached to its own file and skipped on a
# re-run, so a job killed at hour 8 keeps its first 8 hours. That is not
# hypothetical: the first calibration of 2026-08-13 lost 90 minutes to a
# Windows Update reboot with nothing on disk.
#
#   Rscript scripts/harvest_partial_races.R
#   CITIUS_PARTIAL_FROM=2023 CITIUS_PARTIAL_MAX=2000 Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_partial")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

FROM <- suppressWarnings(.env_int("CITIUS_PARTIAL_FROM", "2023"))
MAXN <- suppressWarnings(.env_int("CITIUS_PARTIAL_MAX", "0"))
PAUSE <- .env_num("CITIUS_PARTIAL_PAUSE", "0.15")

x <- tryCatch(
  with_citius_db_connection(function(conn) load_athletics_corpus(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to athletics_corpus.rds.")
    NULL
  }
)
if (is.null(x) || !nrow(x)) x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
if (!nrow(x)) stop("athletics_corpus.rds loaded 0 rows.", call. = FALSE)
say(sprintf("athletics_corpus.rds: %s rows, %s..%s", format(nrow(x), big.mark = ","),
            min(x$date, na.rm = TRUE), max(x$date, na.rm = TRUE)))
x <- x[!is.na(race_key) & !is.na(place) & place > 0]
r <- x[, .(harvested = .N, max_place = max(place, na.rm = TRUE),
           competition_id = competition_id[1], src = source[1],
           tier = tier[1], yr = year(date[1])), by = race_key]
r <- r[harvested < max_place]
say(sprintf("partial races: %s", format(nrow(r), big.mark = ",")))

# Competition is the fetch unit: one call returns every race in the meet.
tier_rank <- c(OW = 1, DF = 2, GW = 3, A = 4, B = 5, C = 6, D = 7, F = 8)
comp <- r[, .(partial_races = .N,
              missing_rows = sum(max_place - harvested),
              best_tier = min(tier_rank[as.character(tier)], na.rm = TRUE),
              yr = max(yr)), by = competition_id]
comp[!is.finite(best_tier), best_tier := 99]
if (!is.na(FROM) && FROM > 0) comp <- comp[yr >= FROM]
setorder(comp, best_tier, -yr, -partial_races)
if (!is.na(MAXN) && MAXN > 0) comp <- comp[seq_len(min(MAXN, .N))]

done <- sub("[.]rds$", "", list.files(CACHE, pattern = "[.]rds$"))
todo <- comp[!(as.character(competition_id) %in% done)]
say(sprintf("competitions: %s to fetch (%s already cached), %s missing rows implied",
            format(nrow(todo), big.mark = ","), format(length(done), big.mark = ","),
            format(sum(todo$missing_rows), big.mark = ",")))
if (!nrow(todo)) { say("nothing to do"); quit(save = "no") }

failed <- character(); n_ok <- 0L; t0 <- Sys.time()
for (i in seq_len(nrow(todo))) {
  cid <- as.character(todo$competition_id[i])
  f <- file.path(CACHE, paste0(cid, ".rds"))
  res <- tryCatch(setDT(athletics_competition_results(cid)),
                  error = function(e) { failed <<- c(failed, paste0(cid, ": ", conditionMessage(e))); NULL })
  if (!is.null(res) && nrow(res)) { saveRDS(res, f); n_ok <- n_ok + 1L }
  if (PAUSE > 0) Sys.sleep(PAUSE)   # be a good citizen; the feed is a free community wrapper
  if (i %% 50 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    say(sprintf("%d/%d  ok %d  failed %d  %.1f min  eta %.0f min",
                i, nrow(todo), n_ok, length(failed), el, el / i * (nrow(todo) - i)))
  }
}
say(sprintf("done: %d fetched, %d failed, %.1f min", n_ok, length(failed),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
# Failures are REPORTED, not swallowed -- that is how five majors went missing.
if (length(failed)) { cli::cli_alert_danger("{length(failed)} failed:"); writeLines(utils::head(failed, 20)) }
say("cache: ", CACHE, "  -- merge into the corpus is a SEPARATE, reviewable step")
