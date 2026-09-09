# A calibration whose race decomposition ACTUALLY CONVERGES.
#
# THE BUG THIS ADDRESSES. decompose_races(centre = "always") -- the deployed
# setting -- never converges on athletics. Its own measurement, in
# calibrate.R:147:
#
#   variant           delta @400   delta @2000   sd(c_r) 400->2000
#   centred (today)   3.21e-04     2.72e-04      +117.3%, never converges
#   NOT centred       1.36e-05     9.98e-09      +0.0%, converged at 1,038
#
# and "cor between the centred and uncentred answers is 0.40, so this is a
# WRONG ANSWER, not a slow one." The deployed c_r has sd 0.02623 against a
# converged 0.01390 -- inflated 1.89x by an optimiser that grows the effects
# every sweep and is stopped arbitrarily at 400.
#
# That inflation is measurable from the outside: regressing what a field
# actually averaged in LATER races on the fitted c_r gives a slope of 0.647
# (docs/reviews/race-shock-is-real-but-oversized-2026-09-05.md), and 1/1.89 =
# 0.53 sits beside it. Two routes, same conclusion.
#
# WHY THIS IS WORTH RE-TESTING DESPITE AN AUGUST REJECTION. centre = "auto" was
# A/B'd on 2026-08-13 and lost on gold Brier by +0.56% (p = 0.00078), so it was
# not adopted. But that arm ran with adjust_race = FALSE, where c_r never
# reaches a prediction and only shapes residuals. The moment race effects are
# APPLIED, c_r is the quantity being subtracted, and the converged version has
# never been measured in that role. A rejection measures a configuration and
# expires when the configuration changes -- the same lesson as the 2026-09-04
# WAC reversal.
#
# max_iter is raised well past 400 because the converged solution needs ~1,038
# sweeps; leaving it at the default would reproduce the very truncation this
# script exists to avoid.
#
# Usage:  Rscript citiusdata/scripts/build_calibration_converged.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
DEST <- Sys.getenv("CITIUS_CONV_OUT", "calibration_converged.rds")
ITER <- as.integer(Sys.getenv("CITIUS_CONV_ITER", "2500"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

t0 <- Sys.time()
x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
x[, competition_id := as.character(competition_id)]
ctl[, competition_id := as.character(competition_id)]
x <- merge(x, ctl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
cov <- 100 * mean(!is.na(x$meet_tier))
say(sprintf("meet_tier on %.1f%% of corpus rows", cov))
stopifnot(cov > 50)

say(sprintf("calibrating with centre='auto', max_iter=%d ...", ITER))
clean <- flag_implausible(x)
cal <- calibrate(clean, min_races = 30L, centre = "auto", max_iter = ITER)
say(sprintf("converged: %s | delta %.3g | sweeps %s",
            format(cal$converged), cal$delta, format(cal$sweeps)))
r <- as.data.table(cal$race)
say(sprintf("race effects: %s, sd(c_r) = %.5f  (deployed divergent fit: 0.02623)",
            format(nrow(r), big.mark = ","), sd(r$c_r, na.rm = TRUE)))
# The whole point of this build. If it did not converge, say so loudly rather
# than shipping a file whose name claims it did.
if (!isTRUE(cal$converged)) cli::cli_alert_warning(
  "STILL NOT CONVERGED at {ITER} sweeps -- this file does not deliver what its name says.")

say("fitting athlete coasting traits ...")
cal$coasting_trait <- fit_coasting_trait(clean, min_heats = 2L, shrink_k = 5.0)
w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) NULL)
if (!is.null(w) && nrow(w)) cal$wind <- w
cal$sigma_context <- fit_sigma_context(clean)
cal$provenance <- list(n_meets = uniqueN(clean$competition_id),
                       date_min = min(clean$date, na.rm = TRUE),
                       date_max = max(clean$date, na.rm = TRUE),
                       built_at = Sys.time(),
                       tier_basis = "meet_tier (WAC)", centre = "auto", max_iter = ITER)
saveRDS(cal, file.path(OUT, DEST))
say(sprintf("wrote %s in %.1f min", DEST, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
