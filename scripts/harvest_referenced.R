# Harvest the competitions our own data already points at.
#
# The career route gives an athlete's marks but NOT the field they raced in, and
# whole fields are what make the shared race effect identifiable and the no-mark
# rate measurable -- see citius/CLAUDE.md. So 4.5M career rows are only partly
# usable, while the calibration runs off the 490k competition-route slice.
#
# We do not need to search for those meets. Every career row carries the
# competition_id it was set at:
#
#   distinct competition_ids in the corpus     32,076
#   harvested as competitions                   1,341
#   substantial and unharvested (100+ rows,
#     5+ events), 2016 onward                   5,485   ~3.9M rows
#
# Harvesting them converts those rows from "one athlete's mark" into "whole
# field", which is the difference between data the calibration can use and data
# it can only partly use.
#
# Ordered biggest-first so that an interrupted run still banks the most valuable
# meets. Resumable: each competition caches to its own file and is skipped on a
# later pass. Failures are COUNTED AND REPORTED, never swallowed -- that is how
# five major championships stayed missing for months.
#
# Usage:  Rscript scripts/harvest_referenced.R
#         CITIUS_REF_MAX=500 Rscript ...      (cap one run)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_ref")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

MAXN <- .env_int("CITIUS_REF_MAX", "6000")
FROM_YEAR <- .env_int("CITIUS_REF_FROM", "2016")

x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[!is.na(competition_id)]
if (!nrow(x)) cli::cli_abort("athletics_corpus.rds loaded 0 rows (post competition_id filter).")
cli::cli_alert_info("athletics_corpus.rds: {format(nrow(x), big.mark = ',')} row{?s}, {min(x$date, na.rm = TRUE)}..{max(x$date, na.rm = TRUE)}")
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
if (!nrow(ch)) cli::cli_abort("championship_results.rds loaded 0 rows.")
cli::cli_alert_info("championship_results.rds: {format(nrow(ch), big.mark = ',')} row{?s}, {min(ch$date, na.rm = TRUE)}..{max(ch$date, na.rm = TRUE)}")
have <- unique(ch$competition_id)
cand <- x[, .(rows = .N, events = uniqueN(event_id), yr = year(min(date, na.rm = TRUE))),
          by = competition_id]
rm(x); invisible(gc())
cand <- cand[!competition_id %in% have & rows >= 100 & events >= 5 & yr >= FROM_YEAR]
setorder(cand, -rows)
done <- sub("[.]rds$", "", list.files(CACHE, pattern = "[.]rds$"))
todo <- cand[!as.character(competition_id) %in% done]
cli::cli_h2("{nrow(cand)} referenced competitions | {length(done)} cached | {nrow(todo)} to fetch")
if (!nrow(todo)) { cli::cli_alert_success("Nothing left."); quit(save = "no") }
todo <- head(todo, MAXN)

ok <- 0L; empty <- 0L; failed <- 0L; t0 <- Sys.time()
for (i in seq_len(nrow(todo))) {
  cid <- todo$competition_id[i]
  r <- tryCatch(setDT(athletics_competition_results(cid)),
                error = function(e) { failed <<- failed + 1L; NULL })
  if (is.null(r)) next
  if (!nrow(r)) { empty <- empty + 1L; saveRDS(data.table(), file.path(CACHE, paste0(cid, ".rds"))); next }
  saveRDS(r, file.path(CACHE, paste0(cid, ".rds"))); ok <- ok + 1L
  if (i %% 25 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cli::cli_alert_info("{i}/{nrow(todo)} | ok {ok} empty {empty} failed {failed} | {round(el)} min | {round(i/el,1)}/min")
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cli::cli_alert_success("fetched {ok}, empty {empty}, failed {failed} in {round(el)} min")

fs <- list.files(CACHE, pattern = "[.]rds$", full.names = TRUE)
blobs <- Filter(function(z) is.data.table(z) && nrow(z), lapply(fs, readRDS))
if (length(blobs)) {
  new <- rbindlist(blobs, fill = TRUE)
  saveRDS(new, file.path(OUT, "championship_results_referenced.rds"))
  cli::cli_alert_success("{format(nrow(new), big.mark=',')} results across {uniqueN(new$competition_id)} competitions")
  cat("\nMERGE when no arm is running:\n")
  cat("  rbind into championship_results.rds, then build_athletics_corpus.R,\n")
  cat("  build_stores.R, build_competition_catalogue.R, rebaseline_chain.R\n")
}
