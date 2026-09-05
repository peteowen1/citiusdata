# Shrink each race effect by HOW WELL IDENTIFIED IT IS, not by a threshold.
#
# THE PROBLEM THIS SOLVES. Applying c_r indiscriminately damages exactly the
# predictions where there was no shock to correct. Measured 2026-09-05 by
# splitting scored predictions on how much race-shock contamination the
# athlete's history carried (race_adjust_by_exposure.R):
#
#   exposure band   marks delta   medal logloss delta
#   lowest 25%      +0.342        -0.0017
#   25-50%          +0.211        -0.0073
#   50-75%          +0.097        -0.0036
#   75-90%          +0.097         0.0000
#   top 10%         +0.032        -0.0073
#
# The marks damage falls 10x monotonically as real shock exposure rises, and
# logloss improves in four bands of five. The mechanism is right; applying it
# where there is nothing to correct is what costs.
#
# WHY NOT A FIELD-SIZE THRESHOLD. Because it is a hack, and Pete said so
# explicitly. "Use the effect only if n >= 8" encodes a guess as a rule and
# still applies the full effect at n = 8 and none at n = 7.
#
# WHAT THIS DOES INSTEAD. Standard empirical Bayes, the same two-level shrinkage
# fit_family_pool_offsets.R already uses:
#
#   se_r^2 = sigma_resid^2 / n_in_race     how noisily c_r is measured
#   tau^2  = var(c_r) - mean(se_r^2)       how much races genuinely differ
#   w_r    = tau^2 / (tau^2 + se_r^2)      the shrinkage weight
#   c_r'   = w_r * c_r
#
# A two-athlete race has a large se_r and is shrunk almost to zero -- not
# because of a rule, but because there is genuinely no information to support
# an effect. The Gout Gout race, 8 athletes and a +5% shock, keeps nearly all
# of its correction. That is the behaviour Pete asked for.
#
# It also addresses the over-sizing measured separately (fitted c_r runs
# 1.5-2.6x too big) without a hand-set scale factor, because the same weight
# that kills unidentifiable effects also pulls back over-fitted ones.
#
# Usage:  Rscript citiusdata/scripts/build_calibration_race_eb.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
SRC  <- Sys.getenv("CITIUS_EB_SRC", "calibration_corpus_wac_coast_0904.rds")
DEST <- Sys.getenv("CITIUS_EB_OUT", "calibration_race_eb.rds")

cal <- readRDS(file.path(OUT, SRC))
r <- as.data.table(cal$race)
stopifnot("need c_r and n_in_race" = all(c("c_r", "n_in_race") %in% names(r)))

# Residual spread, per event where available, else pooled. This is the
# within-race noise that makes a small race's effect unreliable.
ev <- as.data.table(cal$events)
sw <- if ("sigma_within" %in% names(ev))
        setNames(ev$sigma_within, ev$event_id) else NULL
r[, sigma_resid := if (is.null(sw)) NA_real_ else sw[event_id]]
pooled <- stats::median(r$sigma_resid, na.rm = TRUE)
if (!is.finite(pooled)) pooled <- stats::sd(r$c_r, na.rm = TRUE)
r[!is.finite(sigma_resid), sigma_resid := pooled]

r[, se2 := (sigma_resid^2) / pmax(n_in_race, 1L)]
# tau^2 is the BETWEEN-race variance left after removing measurement noise. If
# the observed spread of c_r is entirely explained by noise this goes to zero
# and every effect is shrunk away -- which is the correct answer in that case,
# not a failure.
# tau^2 CANNOT be taken from var(c_r) here, and this is the subtle part.
#
# The textbook estimate is var(c_r) - mean(se^2). But var(c_r) is itself
# inflated ~1.9x by the over-iteration (400 sweeps where 2 is correct), so the
# method would infer "races genuinely differ a lot, trust the effects" from the
# very corruption it is meant to correct. Run that way it gives weight 0.81 at
# n = 2 and 0.95 at n = 8 -- almost no differentiation, and no size correction.
#
# The out-of-sample slope IS the reliability. Regressing what a field actually
# averaged LATER on the fitted c_r gave 0.647 pooled, and under classical
# regression dilution that slope equals tau^2 / (tau^2 + se^2) -- the same
# quantity as the shrinkage weight. So solve for the tau^2 whose AVERAGE weight
# reproduces the measured reliability, and let n_in_race distribute it: small
# races land below the average, large ones above.
#
# This uses a measured, out-of-sample quantity to set the prior instead of an
# internal variance known to be contaminated.
TARGET_W <- as.numeric(Sys.getenv("CITIUS_EB_RELIABILITY", "0.647"))
REL_TABLE <- Sys.getenv("CITIUS_EB_REL_TABLE", "race_reliability_by_event.csv")

solve_tau2 <- function(se2v, target) {
  if (!length(se2v) || !is.finite(target)) return(NA_real_)
  f <- function(lt) mean(exp(lt) / (exp(lt) + se2v), na.rm = TRUE) - target
  lo <- log(1e-10); hi <- log(1e3)
  if (f(lo) > 0 || f(hi) < 0) return(NA_real_)
  exp(stats::uniroot(f, c(lo, hi))$root)
}

# PER EVENT where measured, because reliability differs by discipline in a way
# a single number cannot express: road 0.792 (a marathon field really does
# share a course and weather) against middle distance 0.411 (an 800m time
# reflects TACTICS, which are athlete-specific, not shared). Applying one
# global value moved shot put the right way while pushing long jump past 1.0.
if (nzchar(REL_TABLE) && file.exists(file.path(OUT, REL_TABLE))) {
  rel <- fread(file.path(OUT, REL_TABLE))
  r <- merge(r, rel[, .(event_id, reliability)], by = "event_id", all.x = TRUE)
  n_fallback <- sum(is.na(r$reliability))
  r[is.na(reliability), reliability := TARGET_W]
  cat(sprintf("per-event reliability from %s; %s of %s races fell back to %.3f\n",
              REL_TABLE, format(n_fallback, big.mark = ","),
              format(nrow(r), big.mark = ","), TARGET_W))
  taus <- r[, .(tau2 = solve_tau2(se2, reliability[1])), by = event_id]
  taus[!is.finite(tau2), tau2 := stats::median(taus$tau2, na.rm = TRUE)]
  r <- merge(r, taus, by = "event_id", all.x = TRUE)
  tau2 <- stats::median(r$tau2, na.rm = TRUE)
  cat(sprintf("tau^2 solved per event: median %.3g (range %.3g to %.3g)\n",
              tau2, min(r$tau2, na.rm = TRUE), max(r$tau2, na.rm = TRUE)))
  r[, w := tau2 / (tau2 + se2)]
  r[, tau2 := NULL][, reliability := NULL]
} else {
  tau2 <- solve_tau2(r$se2, TARGET_W)
  if (!is.finite(tau2)) tau2 <- max(0, stats::var(r$c_r, na.rm=TRUE) - mean(r$se2, na.rm=TRUE))
  cat(sprintf("NO reliability table; one global tau^2 %.3g at reliability %.3f\n", tau2, TARGET_W))
  r[, w := tau2 / (tau2 + se2)]
}
r[!is.finite(w), w := 0]
cat(sprintf("tau^2 = %.3g | mean se^2 = %.3g\n", tau2, mean(r$se2, na.rm = TRUE)))
cat(sprintf("shrinkage weight: median %.3f | at n=2 %.3f | at n=8 %.3f | at n=20 %.3f\n",
            median(r$w), median(r[n_in_race == 2]$w, na.rm = TRUE),
            median(r[n_in_race == 8]$w, na.rm = TRUE),
            median(r[n_in_race >= 20]$w, na.rm = TRUE)))

before <- sd(r$c_r, na.rm = TRUE)
r[, c_r := c_r * w]
cat(sprintf("sd(c_r) %.5f -> %.5f\n", before, sd(r$c_r, na.rm = TRUE)))
r[, c(".", "se2", "w", "sigma_resid") := NULL][]
cal$race <- r[]
cal$race_eb <- list(tau2 = tau2, method = "empirical Bayes on n_in_race",
                    built_at = Sys.time())
saveRDS(cal, file.path(OUT, DEST))
cat(sprintf("wrote %s\n", DEST))
