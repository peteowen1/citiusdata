# A calibration whose race decomposition stops where the effects are the RIGHT
# SIZE, rather than where an arbitrary iteration cap happens to fall.
#
# THE MEASUREMENT (2026-09-05, sweep_decompose_iters.R). Fitted race effects are
# scored out of sample by regressing what a field ACTUALLY averaged in LATER
# races on c_r. Slope 1.0 means correctly sized. Swept over max_iter:
#
#   sweeps      100m slope   sd(c_r)   r2      shot put slope
#   1           1.137        0.01132   0.668   1.20
#   2           1.007        0.01298   0.671   0.944     <- optimum
#   3           0.950        0.01375   0.663   0.83
#   8           0.859        0.01508   0.634   0.67
#   25          0.803        0.01588   0.61    0.556
#   400 (ship)  0.744        0.01708   0.561   0.380
#   2000        0.75         --        --      0.37
#
# The slope passes THROUGH 1.0 between sweeps 1 and 2 and then degrades
# monotonically for the next 1,998, on two events that behave very differently.
# r2 is HIGHEST at 2 sweeps and falls all the way to 400, so this is not a
# shrink-to-zero artefact: at 2 sweeps c_r still carries 76% of the deployed
# spread while explaining MORE of what fields did later.
#
# WHAT IS ACTUALLY HAPPENING. Alternating least squares is not converging toward
# truth here, it is converging toward overfitting. With a median of 5 athletes
# per race there is not enough information to separate "the race was fast" from
# "these athletes were fast", so every extra sweep lets the race effect absorb
# more athlete-specific signal. Two passes extract the shared component; the
# rest is the optimiser eating the thing being measured. Early stopping was
# doing regularisation work by accident -- 400 is a hyperparameter nobody chose
# (framing owed to BOUNCER, 2026-09-05).
#
# THE CAVEAT THAT STOPS THIS BEING OBVIOUSLY RIGHT. The sweep optimises c_r as a
# CORRECTION. But sigma_within, condition_sd and tail_df are computed from the
# decomposition's RESIDUALS, and those may well prefer a different iteration
# count -- the 2026-08-13 A/B found the 400-sweep fit forecast better on gold
# Brier, which is consistent with residual-derived quantities liking more
# sweeps even while c_r likes fewer. So this file is a CANDIDATE for a backtest,
# not a fix to ship on the strength of the sweep alone.
#
# Usage:  CITIUS_LOWITER=2 Rscript citiusdata/scripts/build_calibration_lowiter.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
ITER <- as.integer(Sys.getenv("CITIUS_LOWITER", "2"))
DEST <- Sys.getenv("CITIUS_LOWITER_OUT", sprintf("calibration_iter%d.rds", ITER))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

t0 <- Sys.time()
x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
x[, competition_id := as.character(competition_id)]
ctl[, competition_id := as.character(competition_id)]
x <- merge(x, ctl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
cov <- 100 * mean(!is.na(x$meet_tier))
say(sprintf("meet_tier on %.1f%% of corpus rows", cov)); stopifnot(cov > 50)

say(sprintf("calibrating with max_iter = %d (deployed is 400) ...", ITER))
clean <- flag_implausible(x)
cal <- calibrate(clean, min_races = 30L, max_iter = ITER)
r <- as.data.table(cal$race)
say(sprintf("race effects: %s, sd(c_r) = %.5f  (deployed 400-sweep fit: 0.02623)",
            format(nrow(r), big.mark = ","), sd(r$c_r, na.rm = TRUE)))
say(sprintf("delta %.3g after %s sweeps (expected: NOT converged, deliberately)",
            cal$delta, format(cal$sweeps)))

say("fitting athlete coasting traits ...")
cal$coasting_trait <- fit_coasting_trait(clean, min_heats = 2L, shrink_k = 5.0)
w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) NULL)
if (!is.null(w) && nrow(w)) cal$wind <- w
cal$sigma_context <- fit_sigma_context(clean)
cal$provenance <- list(n_meets = uniqueN(clean$competition_id),
                       date_min = min(clean$date, na.rm = TRUE),
                       date_max = max(clean$date, na.rm = TRUE),
                       built_at = Sys.time(), tier_basis = "meet_tier (WAC)",
                       max_iter = ITER,
                       why = "max_iter swept: c_r slope vs later marks passes 1.0 at 2 sweeps")
saveRDS(cal, file.path(OUT, DEST))
say(sprintf("wrote %s in %.1f min", DEST, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
