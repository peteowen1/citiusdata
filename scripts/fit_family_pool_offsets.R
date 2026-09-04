# Fit and PERSIST the family x sex -> event partial-pooling offset table that
# check_family_partial_pool_debias.R validated 2026-09-01: T1 elite raw marks
# MAE flips from losing to last-5 to beating it (-2.96% to -3.10% across 5
# split dates, 3 of 5 significant at p<0.03) when this offset is subtracted
# from the model's predicted ability before scoring.
#
# WHY A SEPARATE FIT SCRIPT RATHER THAN FITTING INLINE IN THE BACKTEST LOOP.
# The offset table is fit ONCE, from data strictly before FIT_HOLDOUT, and then
# used as a fixed lookup by every meet the backtest arm scores on or after that
# date -- the same "single global correction, not a per-meet refit" shape
# project_tier's lambda and the selection-shrinkage lambda already use. Fitting
# inside the per-meet loop would need a point-in-time refit at every cutoff
# date, which is a materially bigger change than what this arm is testing.
#
# FIT POPULATION IS DELIBERATELY ALL TIERS POOLED, NOT T1-ONLY. Tested and
# rejected 2026-09-01 (CITIUS_POOL_FIT_T1_ONLY=TRUE on the check script): a
# T1-only fit gives a similar T1 result (-2.96%) but fails its own transfer
# check (r 0.649 vs 0.888) and makes T2/T3 dramatically WORSE (+14.98%,
# p=0.003) -- the T1 population is itself selected (finalists partly earn their
# spot on a lucky recent result), so offsets fit only there do not generalise.
# Pooling tiers for the FIT is what makes this usable as a single arm-wide
# correction rather than a T1-specific hack.
#
# METHOD: identical two-level empirical-Bayes shrinkage (family x sex -> global,
# event -> its family x sex) as check_family_partial_pool_debias.R. See that
# script's header for the estimator and the anchor checks that validated it;
# this script only fits and writes, it does not re-litigate the design.
#
#   Rscript citiusdata/scripts/fit_family_pool_offsets.R
#
# Writes citiusdata/data/family_pool_offsets.rds: a list with
#   mu0        - grand mean offset (race-level), the fallback for an event AND
#                family x sex never seen in the fit population
#   fs_map     - named num, family x sex -> shrunk offset
#   ev_map     - named num, event_id -> shrunk offset (falls back to fs_map)
#   fit_holdout, fit_arm, fitted_at - provenance
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
# Lower bound of the fit window. Env-driven since 2026-09-01: with the apply-
# date gate live, the offsets only affect meets on/after FIT_HOLDOUT, so
# evaluating on a long history requires fitting on an EARLY window and applying
# forward. Hardcoding 2023 made every pre-2025 race in a full-history score run
# on the uncorrected model, which is how the same arm reads -2.72% on a 2025+
# holdout and +5.17% on the full T1 history.
HOLDOUT_LO <- as.Date(Sys.getenv("CITIUS_FAMILY_POOL_FIT_LO", "2023-01-01"))
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_FAMILY_POOL_FIT_HOLDOUT", "2025-01-01"))
ARM <- Sys.getenv("CITIUS_FAMILY_POOL_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("fit arm %s | fit holdout %s (data strictly before this date only)", ARM, format(FIT_HOLDOUT))

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d <- d[!is.na(act_perf) & !is.na(a_perf) & date >= HOLDOUT_LO & date < FIT_HOLDOUT]
d[, em := 100 * (a_perf - act_perf)]
d[, fs := paste(family, sex, sep = "|")]
say("fit population: %s rows, %s races", format(nrow(d), big.mark=","), format(uniqueN(d$race_id), big.mark=","))

eb_level <- function(dd, err_col, group_col, parent_col) {
  rl <- dd[, .(m = mean(get(err_col))), by = c("race_id", group_col, parent_col)]
  gs <- rl[, .(n = .N, mean_g = mean(m), var_g = var(m)), by = c(group_col, parent_col)]
  gs[, se2 := ifelse(n > 1, var_g / n, NA_real_)]
  tau2 <- max(0, var(gs$mean_g) - mean(gs$se2, na.rm = TRUE), na.rm = TRUE)
  gs[, se2f := fifelse(is.na(se2), tau2, se2)]
  gs[, w := if (tau2 + 0 == 0 && all(se2f == 0)) 0 else tau2 / (tau2 + se2f)]
  gs[, w := fifelse(is.finite(w), w, 0)]
  parent_mean <- gs[[parent_col]]
  gs[, shrunk := w * mean_g + (1 - w) * get(parent_col)]
  list(table = gs, tau2 = tau2)
}

rl0 <- d[, .(m = mean(em)), by = race_id]
mu0 <- mean(rl0$m)
d1 <- copy(d); d1[, parent0 := mu0]
l1 <- eb_level(d1, "em", "fs", "parent0")
fs_map <- setNames(l1$table$shrunk, l1$table$fs)
d2 <- copy(d); d2[, parent1 := fs_map[fs]]
l2 <- eb_level(d2, "em", "event_id", "parent1")
ev_map <- setNames(l2$table$shrunk, l2$table$event_id)

say("mu0 (global) = %.3f | %d family x sex cells | %d events", mu0, length(fs_map), length(ev_map))

out <- list(mu0 = mu0, fs_map = fs_map, ev_map = ev_map,
           fit_holdout = FIT_HOLDOUT, fit_arm = ARM, fitted_at = Sys.time())
f <- file.path(OUT, "family_pool_offsets.rds")
saveRDS(out, f)
say("wrote %s", f)

cat("\nlargest event-level offsets:\n")
print(head(l2$table[order(-abs(shrunk)), .(event_id, n, shrunk = round(shrunk, 2), w = round(w, 2))], 10))
