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
# GL -> T1_elite, A/B/C/D -> T2_strong, E/F -> T3_development) instead of the
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

saveRDS(cal, file.path(OUT, "calibration_corpus_wac_coast_0904.rds"))
say(sprintf("wrote calibration_corpus_wac_coast_0904.rds, total %.1f min",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
