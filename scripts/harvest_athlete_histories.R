# Harvest complete athlete careers from World Athletics.
#
# We hold a MEDIAN OF 6% of each athlete's career. Sampled profiles carry 50-271
# results where our competition harvest holds 3-17. Competition discovery was
# built from ~50 hand-written name queries, so any meet not matching a keyword
# is invisible -- and most meets do not match.
#
# This is very probably the root of the open calibration problem: 73% of
# finalists have <=2 results in their event, so w_total is tiny, shrinkage is
# heavy, and every thinly-observed athlete regresses toward the event mean. A
# model whose probabilities are "spread too evenly" is what thin evidence looks
# like from the outside.
#
# The athlete endpoint returns a whole career in ONE request, so this is far
# cheaper per result than competition discovery: ~87k requests to go from 6% to
# effectively complete.
#
# Ordered by relevance rather than by id: athletes who reach FINALS are the ones
# whose ability estimates actually decide a forecast, so they are fetched first
# and an interrupted sweep still leaves the useful half done.
#
#   CITIUS_MAX_ATHLETES  per run (resumable; run until it reports 0 remaining)

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_athlete_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, aid := suppressWarnings(as.integer(athlete_id))]
ch <- ch[!is.na(aid)]

# Priority 1: anyone who has contested a final -- their estimate decides races.
# Priority 2: anyone we hold few results for, since they gain the most.
# Priority 3: everyone else.
fin <- unique(ch[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
                   !grepl("semi", round, ignore.case = TRUE)]$aid)
held <- ch[, .(n_held = .N), by = aid]
held[, priority := fifelse(aid %in% fin, 1L, fifelse(n_held <= 3L, 2L, 3L))]
setorder(held, priority, n_held)
cli::cli_alert_info(
  "{format(nrow(held), big.mark = ',')} athlete{?s}: {sum(held$priority == 1L)} finalist{?s}, {sum(held$priority == 2L)} thinly held, {sum(held$priority == 3L)} other."
)

done <- sub("\\.rds$", "", list.files(CACHE))
todo <- held[!as.character(aid) %in% done]
cli::cli_alert_info("{format(nrow(todo), big.mark = ',')} remaining ({round(100 * nrow(todo) / nrow(held))}%).")

n <- min(nrow(todo), as.integer(Sys.getenv("CITIUS_MAX_ATHLETES", "2000")))
t0 <- Sys.time()
for (i in seq_len(n)) {
  id <- todo$aid[i]
  r <- tryCatch(athlete_results(id), error = function(e) NULL)
  # An empty file records the miss so a rerun does not retry it forever.
  saveRDS(if (is.null(r)) data.table() else r, file.path(CACHE, paste0(id, ".rds")))
  if (i %% 100 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("  %d/%d  (%.1f/min, ~%.0f min left this run)\n", i, n, i / el,
                (n - i) / (i / el)))
    flush.console()
  }
}

cached <- list.files(CACHE, full.names = TRUE)
hist <- rbindlist(lapply(cached, readRDS), use.names = TRUE, fill = TRUE)
if (nrow(hist)) {
  saveRDS(hist, file.path(OUT, "athletics_history.rds"))
  cat(sprintf("\n%s performance%s from %s athlete%s\n",
              format(nrow(hist), big.mark = ","), if (nrow(hist) == 1) "" else "s",
              format(uniqueN(hist$athlete_id), big.mark = ","),
              if (uniqueN(hist$athlete_id) == 1) "" else "s"))
  cat(sprintf("mean results per athlete: %.1f (competition harvest gave ~3)\n",
              nrow(hist) / uniqueN(hist$athlete_id)))
}
cli::cli_alert_info("{max(0, nrow(todo) - n)} athlete{?s} still to fetch - run again.")
