# WHICH variance term makes the predicted spread +35% too wide?
#
# THE OPEN QUESTION. fit_condition_sd_from_total.R measured a pooled deployed
# total of 0.02781 against an observed 0.02060 across 56 calibrated events --
# the predicted distribution is +35% too wide on 54 of them. That is what gives
# Noah Lyles a 5% chance of beating the world record and under-confidences every
# favourite, since win probability is driven by spread.
#
# build_calibration_condsd.R tried to fix it by SOLVING condition_sd from the
# observed total. That arm was built and it FAILED. The reason it could fail is
# that solving one term from the total assumes the other term is right -- and
# nothing had checked which of the two is actually wrong.
#
# THE SEPARATION. The model's spread is two terms that live at different levels:
#
#   predicted_total_i^2  =  sigma_i^2            (individual, within race)
#                        +  (s_i * condition_sd)^2   (shared, whole race)
#
# Those are separately observable, because a SHARED shock moves every athlete in
# a race together and an INDIVIDUAL one does not. Decompose each observed mark:
#
#   perf_ar  =  race_mean_r  +  (perf_ar - race_mean_r)
#               ^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^
#               shared          individual
#
# then compare:
#   sd(race_mean_r) around an athlete-mix-adjusted expectation  <->  condition_sd
#   sd(perf_ar - race_mean_r) within athlete                    <->  sigma_i
#
# WHY THIS IS THE RIGHT TEST AND THE AGGREGATE ONE IS NOT. The +35% is a single
# pooled number and both terms feed it, so it cannot say which to shrink.
# Shrinking the wrong one is not neutral: condition_sd only moves ABSOLUTE marks
# (a shared shock cancels out of every pairwise comparison, which is the repo's
# core documented rule and is verified in the test suite), whereas sigma moves
# PLACINGS and therefore every medal probability. Getting this backwards would
# fix the marks distribution while damaging the thing the verse exists to
# predict.
#
# CONFOUND THIS MUST AVOID. A race mean moves for two reasons: a genuine shared
# shock, and the fact that a fast field is in the race. Only the first is
# condition_sd. So the race mean is taken RELATIVE to the entrants' own prior
# abilities -- the same quantity decompose_races() fits -- not raw.
#
# Usage:
#   Rscript citiusdata/scripts/diagnostics/split_spread_shared_vs_individual.R
# Env:
#   CITIUS_SPLIT_CAL   calibration to score (default: the deployed one)
#   CITIUS_SPLIT_FROM  hold-out start date (default 2024-01-01)
#   CITIUS_SPLIT_MINR  min races per athlete-event in the hold-out (default 4)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
CAL  <- Sys.getenv("CITIUS_SPLIT_CAL",  "calibration_corpus_wac_coast_0904.rds")
FROM <- as.Date(Sys.getenv("CITIUS_SPLIT_FROM", "2024-01-01"))
MINR <- as.integer(Sys.getenv("CITIUS_SPLIT_MINR", "4"))
say  <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

cal <- readRDS(file.path(OUT, CAL))
x   <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))
x   <- x[!is.na(mark) & !is.na(race_key) & !is.na(date)]
x[, perf := to_perf(mark, event_id)]
x   <- x[is.finite(perf)]
say(sprintf("corpus %s rows", format(nrow(x), big.mark = ",")))

# Ability from BEFORE the hold-out only, so nothing here is fitted on what it
# is scored against.
train <- x[date < FROM]
test  <- x[date >= FROM]
stopifnot("no training rows" = nrow(train) > 0, "no hold-out rows" = nrow(test) > 0)
say(sprintf("train %s rows / test %s rows (split %s)",
            format(nrow(train), big.mark = ","), format(nrow(test), big.mark = ","),
            format(FROM)))

ab <- estimate_ability(train, as_of = FROM, calibration = cal)
ab <- as.data.table(ab)[, .(athlete_id = as.character(athlete_id), event_id,
                            ability, sigma)]
stopifnot("estimate_ability returned no sigma" = "sigma" %in% names(ab))

test[, athlete_id := as.character(athlete_id)]
d <- merge(test, ab, by = c("athlete_id", "event_id"))
say(sprintf("%s hold-out marks have a prior ability", format(nrow(d), big.mark = ",")))

# --- the decomposition -------------------------------------------------------
# Residual against the athlete's OWN prior ability. A shared shock moves every
# residual in a race together; an individual one does not.
d[, resid := perf - ability]
# Races need >= 3 entrants with a prior, or the race mean IS the athlete and the
# split is vacuous -- the within-race term would be identically zero.
d[, n_prior := .N, by = race_key]
d <- d[n_prior >= 3L]
d[, race_mean := mean(resid), by = race_key]
d[, indiv := resid - race_mean]
say(sprintf("%s marks in %s races with 3+ priored entrants",
            format(nrow(d), big.mark = ","), format(uniqueN(d$race_key), big.mark = ",")))

# --- observed vs predicted, per event ---------------------------------------
# Shared: spread of race means. One value per race, not per row, or big races
# would be counted many times.
shared_obs <- unique(d[, .(race_key, event_id, race_mean)], by = "race_key")[
  , .(n_races = .N, shared_sd = sd(race_mean)), by = event_id]

# Individual: within-athlete-event spread of the race-removed residual.
d[, n_ae := .N, by = .(athlete_id, event_id)]
indiv_obs <- d[n_ae >= MINR, .(n_ae_groups = uniqueN(paste(athlete_id, event_id)),
                               indiv_sd = sd(indiv)), by = event_id]

ev <- as.data.table(cal$events)[, .(event_id, condition_sd,
                                    sigma_target = if ("sigma_target" %in% names(cal$events))
                                      sigma_target else NA_real_)]
mod_sigma <- ab[, .(model_sigma = median(sigma, na.rm = TRUE)), by = event_id]

cmp <- Reduce(function(a, b) merge(a, b, by = "event_id", all = FALSE),
              list(shared_obs, indiv_obs, ev, mod_sigma))
cmp[, shared_ratio := shared_sd / condition_sd]
cmp[, indiv_ratio  := indiv_sd  / model_sigma]
setorder(cmp, -n_races)

cat("\n=== per event: observed / predicted, by variance term ===\n")
cat("(ratio > 1 = the model is too NARROW on that term; < 1 = too WIDE)\n\n")
print(cmp[, .(event_id, n_races,
              shared_obs = round(shared_sd, 5), cond_sd = round(condition_sd, 5),
              shared_ratio = round(shared_ratio, 3),
              indiv_obs = round(indiv_sd, 5), sigma = round(model_sigma, 5),
              indiv_ratio = round(indiv_ratio, 3))], nrows = 60)

cat("\n=== the verdict ===\n")
cat(sprintf("shared term (condition_sd): median observed/predicted = %.3f  on %d events\n",
            median(cmp$shared_ratio, na.rm = TRUE), sum(is.finite(cmp$shared_ratio))))
cat(sprintf("individual term (sigma)   : median observed/predicted = %.3f  on %d events\n",
            median(cmp$indiv_ratio, na.rm = TRUE), sum(is.finite(cmp$indiv_ratio))))
cat("\nA ratio near 1.0 means that term is right and the OTHER one carries the\n")
cat("+35%. Which matters: condition_sd moves only absolute marks (a shared shock\n")
cat("cancels from every pairwise comparison), while sigma moves placings and so\n")
cat("every medal probability. Shrink the wrong one and the marks distribution\n")
cat("improves while the medal forecast gets worse.\n")

fwrite(cmp, file.path(OUT, "spread_split_shared_vs_individual.csv"))
say("wrote spread_split_shared_vs_individual.csv")
