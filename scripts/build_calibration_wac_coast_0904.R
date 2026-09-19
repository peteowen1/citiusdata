# WAC-tier (meet_tier) + coasting calibration, rebuilt on the CURRENT corpus
# and catalogue (2026-09-04), to re-verify whether the 2026-08-29 full-history
# rejection (T1 marks MAE +3.15%, p=3e-15 -- .scratch/athletics-calendar/
# issues/03-diamond-league-tier-defect.md addendum) still holds after today's
# catalogue rebuild (comp_name reclassification, road_race T1 tiering, EW
# baseline work). Copied from build_calibration_coasting.R rather than editing
# it in place -- that file is the archived artefact of a superseded 2026-08-13
# confound and must not be silently changed out from under its own header.
#
# Differs from the deployed calibration_corpus_csigma_coast.rds in exactly one
# input: tier offsets fitted on the catalogue's meet_tier (WAC-based: OW/DF/GW/
# GL -> M1, A/B/C/D -> M2, E/F -> M3) instead of the
# feed's raw per-result tier. Same coasting trait fit, same wind fit, same
# sigma_context fit -- so a control (deployed) vs treatment (this) backtest
# arm-pair isolates the tier-basis question alone.
#
# Usage:  Rscript scripts/build_calibration_wac_coast_0904.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

t0 <- Sys.time()
x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
x[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
x <- merge(x, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

cov <- 100 * mean(!is.na(x$meet_tier))
say(sprintf("meet_tier attached to %.1f%% of corpus rows", cov))
print(x[, .N, by = meet_tier][order(-N)])
stopifnot(cov > 50)

# NO-LEAK HOOK (2026-09-19, leakage audit follow-through). CITIUS_EXCLUDE_SCORED
# names a backtest artefact (data/backtest_<tag>.rds); every competition whose
# races that backtest scored is dropped from the corpus BEFORE calibrate(), so
# no channel -- tier/round offsets, sigma_within, condition_sd, tail_df, race
# shock via the downstream chain -- is fitted on the outcomes it is later scored
# against. Competitions, not a date cutoff: a cutoff strips every athlete who
# debuted after it (build_calibration_coast_noleak.R's argument). The output
# name gets a `_noleak` suffix via CITIUS_WAC_OUT so the leaky base is kept.
EXCL <- Sys.getenv("CITIUS_EXCLUDE_SCORED", "")
if (nzchar(EXCL)) {
  bt <- readRDS(file.path(OUT, EXCL))
  ids <- unique(as.character(as.data.table(bt$outcomes)$race_id))
  ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
  scored_comp <- unique(as.character(ch[as.character(race_key) %in% ids, competition_id]))
  scored_comp <- scored_comp[!is.na(scored_comp)]
  stopifnot("could not resolve the scored competitions; refusing to claim a no-leak fit" = length(scored_comp) > 0)
  n0 <- nrow(x); x <- x[!competition_id %chin% scored_comp]
  say(sprintf("NO-LEAK: dropped %s rows from %d scored competitions (%s races) named by %s",
              format(n0 - nrow(x), big.mark = ","), length(scored_comp), format(length(ids), big.mark = ","), EXCL))
}
say("calibrating base on meet_tier ...")
clean <- flag_implausible(x)
cal <- calibrate(clean, min_races = 30L)
say(sprintf("base calibrate() done at +%.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
print(as.data.table(cal$tier))

say("fitting athlete coasting traits ...")
ct <- fit_coasting_trait(clean, min_heats = 2L, shrink_k = 5.0)
cal$coasting_trait <- ct
say(sprintf("fitted coasting trait for %d athletes at +%.1f min",
            nrow(ct), as.numeric(difftime(Sys.time(), t0, units = "mins"))))

w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) {
  say("wind fit failed: ", conditionMessage(e)); NULL })
if (!is.null(w) && nrow(w)) { cal$wind <- w; say("wind fitted on ", nrow(w), " events") }
cal$sigma_context <- fit_sigma_context(clean)

cal$provenance <- list(
  n_meets = uniqueN(clean$competition_id),
  date_min = min(clean$date, na.rm = TRUE), date_max = max(clean$date, na.rm = TRUE),
  built_at = Sys.time(), built_from = "athletics_corpus.rds + competition_catalogue.parquet (2026-09-04)",
  tier_basis = "meet_tier (WAC)")

WAC_OUT <- Sys.getenv("CITIUS_WAC_OUT", if (nzchar(EXCL)) "calibration_corpus_wac_coast_0904_noleak.rds" else "calibration_corpus_wac_coast_0904.rds")
cal$provenance$excluded_scored <- if (nzchar(EXCL)) list(from = EXCL, competitions = length(scored_comp)) else NULL
saveRDS(cal, file.path(OUT, WAC_OUT))
say(sprintf("wrote %s", WAC_OUT))
say(sprintf("wrote calibration_corpus_wac_coast_0904.rds, total %.1f min",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
