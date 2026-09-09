# Compose the candidate deployment calibration from the three fitted tables
# built on 2026-09-06, each validated on its own arm:
#
#   race table + $race_shock      from calibration_race_eb_perevent_persist.rds
#                                  (EB-shrunk race effects; expected effect per
#                                  event x tier x round; beta by tier)
#   $condition_sd_context         from calibration_corpus_wac_coast_0904_ctxsd_scaled.rds
#   $spread_scales                from the same file
#
# Everything else (events, offsets, coasting trait, sigma_context, ...) is the
# deployed calibration_corpus_wac_coast_0904.rds that all three descend from.
# The race table and $race_shock feed estimate_ability(adjust_race = TRUE)
# only; the context table and scales feed simulate_event(context = ) only, so
# the tables do not interact and each arm's verdict carries over.
#
# Usage:  Rscript citiusdata/scripts/build_calibration_compose.R
# Env:    CITIUS_COMPOSE_OUT (default calibration_corpus_wac_coast_0904_full.rds)
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
DST <- Sys.getenv("CITIUS_COMPOSE_OUT", "calibration_corpus_wac_coast_0904_full.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")

# The scales file: the METHOD was validated out of sample (fit 2022+2023+2025,
# judge 2024: pooled 0.488 / 0.897 / PIT sd 0.273); the deployed table is then
# fitted on all four seasons, which is what _scaled_all carries.
SCALES <- Sys.getenv("CITIUS_COMPOSE_SCALES", "calibration_corpus_wac_coast_0904_ctxsd_scaled_all.rds")
base   <- readRDS(file.path(OUT, SCALES))
SHOCK  <- Sys.getenv("CITIUS_COMPOSE_SHOCK", "calibration_race_eb_perevent_persist5.rds")
shock  <- readRDS(file.path(OUT, SHOCK))
stopifnot(inherits(base, "citius_calibration"), inherits(shock, "citius_calibration"),
          !is.null(base$condition_sd_context), !is.null(base$spread_scales),
          !is.null(shock$race_shock), !is.null(shock$race))
# Same parent: the events table must be identical, or the two files were not
# built from the same calibration and composing them mixes vintages.
stopifnot(identical(as.data.frame(base$events)[, c("event_id", "sigma_within", "condition_sd")],
                    as.data.frame(shock$events)[, c("event_id", "sigma_within", "condition_sd")]))
out <- base
out$race       <- shock$race
out$race_shock <- shock$race_shock
out$provenance$composed <- list(
  race_and_shock_from = SHOCK,
  context_and_scales_from = SCALES,
  built = Sys.time())
saveRDS(out, file.path(OUT, DST))
say("wrote %s: race table %s rows, race_shock beta %.3f (%d tier rows), condition_sd_context %d cells, spread_scales %d families",
    DST, format(nrow(out$race), big.mark = ","), out$race_shock$beta, NROW(out$race_shock$by_tier),
    nrow(out$condition_sd_context), nrow(out$spread_scales))
