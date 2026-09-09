# A calibration whose condition_sd is sized so the PREDICTED SPREAD MATCHES
# WHAT ATHLETES ACTUALLY DO.
#
# THE BUG. simulate_event() draws an athlete's own sigma and adds condition_sd
# on top. sigma is computed on context-REMOVED residuals; condition_sd is
# `sqrt(var(c_r) - bias)` (calibrate.R:406), straight from the race effects,
# which are inflated ~1.7x by decompose_races() running 400 sweeps where the
# signal is extracted in 2. The two terms are calibrated on different bases and
# double-count the same variance.
#
# MEASURED (fit_condition_sd_from_total.R, 56 calibrated events):
#   pooled deployed total 0.02781 against an observed 0.02060 -- the predicted
#   distribution is +35% TOO WIDE, on 54 of 56 events, ranging +68% to +7%.
#
# That is what gave Noah Lyles a 5% chance of beating the world record, and it
# under-confidences every favourite, since win probability is driven by spread.
#
# THE FIX. The total is directly observable -- an athlete's actual spread of
# future marks IS sigma and conditions combined -- so condition_sd is solved
# rather than estimated independently:
#
#   condition_sd = sqrt(max(total_observed^2 - sigma^2, 0))
#
# fitted per event on pre-2024 estimates against post-2024 outcomes, so it is
# out of sample with respect to the window it is scored on.
#
# WHAT THIS DOES NOT DO. It does not fix sigma's RANKING weakness (it predicts
# an athlete's future scatter at ~0.06-0.13 however measured). This is a
# calibration fix for the total spread, not an estimator fix. Keeping the two
# separate matters: they have different causes and different tests.
#
# Usage:  Rscript citiusdata/scripts/build_calibration_condsd.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
SRC  <- Sys.getenv("CITIUS_CSD_SRC", "calibration_corpus_wac_coast_0904.rds")
TBL  <- Sys.getenv("CITIUS_CSD_TBL", "condition_sd_from_total.csv")
DEST <- Sys.getenv("CITIUS_CSD_DEST", "calibration_condsd.rds")

cal <- readRDS(file.path(OUT, SRC))
fit <- fread(file.path(OUT, TBL))
ev <- as.data.table(cal$events)
stopifnot("calibration$events has no condition_sd" = "condition_sd" %in% names(ev))

before <- ev[, .(event_id, old = condition_sd)]
ev <- merge(ev, fit[, .(event_id, condition_sd_fitted)], by = "event_id", all.x = TRUE)
n_fit <- sum(is.finite(ev$condition_sd_fitted))
# Events with no fitted value keep the deployed one rather than inheriting a
# pooled substitute: a wrong-but-known value is easier to reason about later
# than a silently borrowed one, and the fit covers the events that matter.
ev[is.finite(condition_sd_fitted), condition_sd := condition_sd_fitted]
ev[, condition_sd_fitted := NULL]

cmp <- merge(before, ev[, .(event_id, new = condition_sd)], by = "event_id")
cmp <- cmp[is.finite(old) & is.finite(new)]
cat(sprintf("condition_sd replaced on %d of %d events\n", n_fit, nrow(ev)))
cat(sprintf("  median %.5f -> %.5f  (%+.1f%%)\n", median(cmp$old), median(cmp$new),
            100*(median(cmp$new)/median(cmp$old) - 1)))
cat(sprintf("  events reduced: %d | increased: %d\n",
            sum(cmp$new < cmp$old), sum(cmp$new > cmp$old)))

cal$events <- ev[]
cal$condition_sd_source <- sprintf("solved from observed total spread (%s)", TBL)
saveRDS(cal, file.path(OUT, DEST))
cat(sprintf("wrote %s\n", DEST))
