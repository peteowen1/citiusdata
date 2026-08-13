# Rebuild the calibration chain on the merged corpus, then re-run the reference
# arm, so the first framework-compliant scorecard is measured on the new data
# rather than against stale artefacts.
#
# Everything on disk before this -- calibrations, backtest caches, the arms
# adopted today -- was built on a harvest missing five major championships. An
# arm scored against a corpus it was not run on is the stale-shared-input
# confound that produced the inert-sigma incident, so the caches are invalidated
# rather than reused.
#
# Chain, in order, because each step reads the one before:
#   1. calibrate the corpus            -> calibration_corpus.rds
#   2. attach the wind coefficient     -> calibration_corpus_w.rds
#   3. fit and attach sigma_context    -> calibration_corpus_csigma.rds
#
# Usage:  Rscript scripts/rebaseline_chain.R
#   CITIUS_CAL_SUFFIX   written into every output name, so an experimental chain
#                       cannot overwrite the deployed one. ALWAYS set it for an
#                       arm; the empty default reproduces the canonical names.
#   CITIUS_CAL_CENTRE   "always" (default) or "auto" -- the convergence fix.
#   CITIUS_CAL_MAX_ITER sweeps; "auto" needed 1,038 on the men's 100m, so 400 is
#                       not enough for it.
#
# NOTE 2026-08-13: the three canonical outputs on disk are BYTE-IDENTICAL to each
# other (md5 03a82a06...), so they were not produced by this script -- it saves an
# accumulating object, and step 1's output should carry neither wind nor
# sigma_context. Any experiment swapping calibration_corpus.rds for
# calibration_corpus_w.rds to test wind on/off is currently a no-op.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
SUF <- Sys.getenv("CITIUS_CAL_SUFFIX", "")
CENTRE <- Sys.getenv("CITIUS_CAL_CENTRE", "always")
MAXIT <- as.integer(Sys.getenv("CITIUS_CAL_MAX_ITER", "400"))
if (!CENTRE %in% c("always", "auto")) stop("CITIUS_CAL_CENTRE must be always|auto")
outfile <- function(stem) file.path(OUT, paste0(stem, SUF, ".rds"))
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")
say("centre=", CENTRE, " max_iter=", MAXIT, " suffix='", SUF, "'")

x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[!is.na(date)]
say("corpus: ", format(nrow(x), big.mark = ","), " rows, ",
    format(uniqueN(x$race_key), big.mark = ","), " races")
keep <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "sex", "round",
          "tier", "race_key", "competition_id", "discipline", "orientation",
          "is_technical", "nomark_observable", "source", "wind")
x <- x[, intersect(keep, names(x)), with = FALSE]
clean <- flag_implausible(x); rm(x); invisible(gc())

say("calibrating ...")
cal <- calibrate(clean, min_races = 30L, centre = CENTRE, max_iter = MAXIT)

# Stamp what this was fitted on. Without it there is no way to tell, later,
# whether a calibration overlaps the meets a backtest scores against it -- and
# the per-athlete `sensitivity` it carries IS consumed at prediction time by
# condition_sensitivity(), so that overlap is real leakage rather than a
# theoretical one. Measured 2026-08-03 on the then-current pair: all 49 scored
# meets were inside the calibration corpus, and a scored athlete's own races
# were a median 7.4% of the history behind their sensitivity.
cal$provenance <- list(
  input = "athletics_corpus.rds",
  fitted_at = Sys.time(),
  n_rows = nrow(clean),
  n_races = data.table::uniqueN(clean$race_key),
  n_meets = data.table::uniqueN(clean$competition_id),
  date_min = min(clean$date, na.rm = TRUE),
  date_max = max(clean$date, na.rm = TRUE),
  competition_ids = sort(unique(as.character(clean$competition_id)))
)
saveRDS(cal, outfile("calibration_corpus"))
say("calibrated in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")

a <- as.data.table(cal$athlete)
say("sensitivity sd ", signif(sd(a$sensitivity, na.rm = TRUE), 3),
    " | 5-95% ", paste(signif(quantile(a$sensitivity, c(.05, .95), na.rm = TRUE), 3),
                       collapse = " .. "))

# Wind. Re-fitted here rather than reused: the corpus changed, and a coefficient
# fitted on a different vintage is exactly what METHODOLOGY warns about.
say("fitting wind ...")
w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) NULL)
if (!is.null(w) && nrow(w)) {
  saveRDS(w, file.path(OUT, "wind_effect_corpus.rds"))
  cal$wind <- w
  say("wind fitted on ", nrow(w), " event", if (nrow(w) == 1) "" else "s")
} else {
  prev <- file.path(OUT, "wind_effect_corpus.rds")
  if (file.exists(prev)) { cal$wind <- as.data.table(readRDS(prev)); say("wind re-fit failed; reused previous") }
}
saveRDS(cal, outfile("calibration_corpus_w"))

say("fitting sigma_context ...")
sc <- fit_sigma_context(clean)
cal$sigma_context <- sc
print(sc[order(ratio), .(family, n_champ, ratio = round(ratio, 3))])
saveRDS(cal, outfile("calibration_corpus_csigma"))
say("wrote ", basename(outfile("calibration_corpus_csigma")), " | converged=", cal$converged, " sweeps=", cal$sweeps)

say("total ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
cat("\nNEXT: the backtest caches are now stale. Run with a FRESH cache dir:\n")
cat("  CITIUS_BT_CACHE=backtest_cache_ref CITIUS_BT_OUT=backtest_ref.rds \\\n")
cat("  CITIUS_BT_CALIBRATION=calibration_corpus_csigma.rds CITIUS_BT_MEETS=2000 \\\n")
cat("  Rscript scripts/backtest_athletics.R\n")
cat("then: CITIUS_SCORE_ARM=backtest_ref.rds Rscript scripts/score_arm.R\n")
