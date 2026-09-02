# Does a HIERARCHICAL, PARTIALLY POOLED per-event offset (family x sex -> event)
# beat last-5 on T1 elite marks MAE out-of-sample, where a flat per-(event,sex)
# offset with a hard n>=25 cutoff did not (check_per_event_debias.R, 2026-09-01:
# T1 raw MAE still +2.70% worse than last-5 after that debias)?
#
# WHAT MOTIVATES TRYING AGAIN WITH POOLING. check_marks_cuts_sweep.R (same day)
# found the model-vs-last5 gap is not scattered across events -- it is a clean
# FAMILY split, same direction in every sex within every family: model loses
# badly on sprint/hurdles/throw/jump (+18 to +43%) and BEATS last-5 on distance/
# middle (-23 to -29%). A flat per-event mean with a hard n-cutoff throws away
# exactly the information this creates: a thin event's true offset is not zero
# (the old fallback) -- it is close to its family's, which is well estimated.
# Partial pooling is the tool built for exactly this shape of problem.
#
# METHOD: empirical-Bayes (Morris/DerSimonian-Laird-style) shrinkage, two
# levels. Unit of replication is the RACE MEAN of the per-athlete error, not
# the athlete-row -- races within an event are not independent draws of the
# event's offset, athletes within a race even less so.
#   level 1 (family x sex): shrink toward the GLOBAL mean, weight tau1^2 /
#     (tau1^2 + se_g^2), tau1^2 = between-group variance of the 12 family x sex
#     race-level means minus their average sampling variance (method of
#     moments; floored at 0 -- a negative estimate means the groups are
#     statistically indistinguishable from one mean).
#   level 2 (event): shrink toward its OWN family x sex's level-1 shrunk mean,
#     same tau^2 estimator computed within each family x sex separately.
# Both last-5 and the model get the identical procedure, fitted on TRAIN only,
# applied to TEST -- the fairness rule from check_per_event_debias.R, unchanged
# here: de-biasing only the model would rig the comparison.
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  train/test disjoint in time and race (same split as check_per_event_
#       debias.R: 2025-01-01, zero shared races).
#   A2  in-sample, the pooled correction must reduce train MAE (true by
#       construction for any shrinkage strictly between 0 and the flat mean).
#   A3  shrinkage weights must be in [0,1] and event-level weights must be
#       SMALLER on average than family x sex weights (more shrinkage where
#       there is less data to estimate from) -- if not, the estimator is
#       wired wrong, not just underpowered.
#   A4  the pooled offsets must correlate positively train->test at the event
#       level (same transfer check as before) -- otherwise this is fitting
#       noise with extra steps.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_cluster.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")
SPLIT   <- as.Date(Sys.getenv("CITIUS_POOL_SPLIT", "2025-01-01"))
ARM     <- Sys.getenv("CITIUS_POOL_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s | split %s", ARM, format(HOLDOUT), format(SPLIT))

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, unique(m[, .(race_id, athlete_id, l5)], by = c("race_id","athlete_id")),
           by = c("race_id", "athlete_id"))

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d <- d[!is.na(act_perf) & !is.na(a_perf) & !is.na(l5) & date >= HOLDOUT]
d[, `:=`(em = 100 * (a_perf - act_perf), eb = 100 * (l5 - act_perf))]
d[, fs := paste(family, sex, sep = "|")]

tr <- d[date <  SPLIT]; te <- d[date >= SPLIT]
cat("\n==== ANCHOR A1: disjoint split ====\n")
say("train %s rows (%s..%s) | test %s rows (%s..%s) | shared races %d",
    format(nrow(tr), big.mark=","), format(min(tr$date)), format(max(tr$date)),
    format(nrow(te), big.mark=","), format(min(te$date)), format(max(te$date)),
    length(intersect(tr$race_id, te$race_id)))

# ---- empirical-Bayes shrinkage of one level, given a per-row parent mean ----
# race-level means first: the unit of replication is the RACE, not the row.
eb_level <- function(dd, err_col, group_col, parent_col) {
  rl <- dd[, .(m = mean(get(err_col))), by = c("race_id", group_col, parent_col)]
  gs <- rl[, .(n = .N, mean_g = mean(m), var_g = var(m)), by = c(group_col, parent_col)]
  gs[, se2 := ifelse(n > 1, var_g / n, NA_real_)]
  # method-of-moments tau^2: between-group variance of the race-level group
  # means, less their average sampling variance. Floored at 0 -- see header.
  tau2 <- max(0, var(gs$mean_g) - mean(gs$se2, na.rm = TRUE), na.rm = TRUE)
  gs[, se2f := fifelse(is.na(se2), tau2, se2)]      # a singleton group gets no shrinkage info of its own
  gs[, w := if (tau2 + 0 == 0 && all(se2f == 0)) 0 else tau2 / (tau2 + se2f)]
  gs[, w := fifelse(is.finite(w), w, 0)]
  parent_mean <- gs[[parent_col]]
  gs[, shrunk := w * mean_g + (1 - w) * get(parent_col)]
  list(table = gs, tau2 = tau2)
}

fit_hierarchy <- function(dd, err_col) {
  # level 0: global mean (race-level, so a deep field doesn't dominate it)
  rl0 <- dd[, .(m = mean(get(err_col))), by = race_id]
  mu0 <- mean(rl0$m)
  # level 1: family x sex, parent = mu0
  dd1 <- copy(dd); dd1[, parent0 := mu0]
  l1 <- eb_level(dd1, err_col, "fs", "parent0")
  fs_map <- setNames(l1$table$shrunk, l1$table$fs)
  # level 2: event, parent = its family x sex's level-1 shrunk mean
  dd2 <- copy(dd); dd2[, parent1 := fs_map[fs]]
  l2 <- eb_level(dd2, err_col, "event_id", "parent1")
  ev_map <- setNames(l2$table$shrunk, l2$table$event_id)
  list(mu0 = mu0, fs = l1$table, event = l2$table, fs_map = fs_map, ev_map = ev_map)
}

# Fitting population for the offsets is a separate choice from the evaluation
# population. Default pools all tiers for more races per group; T1_ONLY tests
# whether that pooling is itself hiding a selection effect -- T1 finalists earn
# their spot partly on a lucky recent result, which is the same regression-to-
# the-mean mechanism check_sigma_coverage.R/the selection-shrinkage arm target
# elsewhere in this session -- by fitting ONLY on the population being scored.
FIT_T1_ONLY <- as.logical(Sys.getenv("CITIUS_POOL_FIT_T1_ONLY", "FALSE"))
tr_fit <- if (FIT_T1_ONLY) tr[meet_tier == "T1_elite"] else tr
say("fitting population: %s (%s races)", if (FIT_T1_ONLY) "T1_elite only" else "all tiers pooled",
    format(uniqueN(tr_fit$race_id), big.mark = ","))
fit_m <- fit_hierarchy(tr_fit, "em")
fit_b <- fit_hierarchy(tr_fit, "eb")

cat("\n==== ANCHOR A3: shrinkage weights (mean, by level) ====\n")
say("model:  family x sex mean w = %.3f (tau1^2=%.3f) | event mean w = %.3f (tau2 pooled)",
    mean(fit_m$fs$w), var(fit_m$fs$mean_g) - mean(fit_m$fs$se2, na.rm=TRUE), mean(fit_m$event$w))
say("last5:  family x sex mean w = %.3f | event mean w = %.3f", mean(fit_b$fs$w), mean(fit_b$event$w))
ok_a3 <- mean(fit_m$event$w) < mean(fit_m$fs$w) && mean(fit_b$event$w) < mean(fit_b$fs$w)
say("A3 %s", if (ok_a3) "PASS - event level shrinks harder than family x sex" else "FAIL - shrinkage not monotone by data depth")

# ---- apply to TEST: unseen events fall back to their fs map, else mu0 ------
apply_offset <- function(dd, fit) {
  ev  <- fit$ev_map[dd$event_id]
  fsm <- fit$fs_map[dd$fs]
  off <- ifelse(!is.na(ev), ev, ifelse(!is.na(fsm), fsm, fit$mu0))
  unname(off)
}
te[, off_m := apply_offset(te, fit_m)]
te[, off_b := apply_offset(te, fit_b)]
tr[, off_m := apply_offset(tr, fit_m)]
tr[, off_b := apply_offset(tr, fit_b)]

cat("\n==== ANCHOR A2: in-sample sanity ====\n")
say("train raw MAE (model)  %.4f -> %.4f  (%s)", mean(abs(tr$em)), mean(abs(tr$em - tr$off_m)),
    if (mean(abs(tr$em - tr$off_m)) < mean(abs(tr$em))) "PASS" else "FAIL")

cat("\n==== ANCHOR A4: do event-level offsets transfer? ====\n")
ev_tr <- fit_m$event[, .(event_id, tr_off = shrunk)]
ev_te <- te[, .(te_off = mean(em)), by = event_id][, .N := .N, by = event_id]
te_n  <- te[, .N, by = event_id]
cmp <- merge(ev_tr, merge(ev_te, te_n, by = "event_id"), by = "event_id")
cmp <- cmp[N >= 10]
say("events compared %d | r(train shrunk offset, test raw mean) = %.3f",
    nrow(cmp), cor(cmp$tr_off, cmp$te_off))

rel <- function(x, y) 100 * (mean(abs(x)) - mean(abs(y))) / mean(abs(y))
ctr <- function(v) v - mean(v, na.rm = TRUE)
report <- function(dd, label) {
  if (!nrow(dd)) return(invisible())
  em0 <- dd$em; eb0 <- dd$eb
  emc <- dd$em - dd$off_m; ebc <- dd$eb - dd$off_b
  say("\n---- %s : %s rows, %s races ----", label, format(nrow(dd), big.mark=","),
      format(uniqueN(dd$race_id), big.mark=","))
  say("  %-32s model %8.4f  last5 %8.4f  rel %+7.2f%%", "raw MAE (status quo)",
      mean(abs(em0)), mean(abs(eb0)), rel(em0, eb0))
  say("  %-32s model %8.4f  last5 %8.4f  rel %+7.2f%%", "raw MAE (pooled de-bias)",
      mean(abs(emc)), mean(abs(ebc)), rel(emc, ebc))
  r <- cluster_rel_mae(emc, ebc, dd$race_id)
  say("  clustered (raw, pooled):  %s", fmt_cl(r))
  # CENTRED MAE -- did the level fix cost anything on spread/discrimination?
  # A uniform per-group shift does not change within-group ranking at all, so
  # this should barely move; a real drop here would mean the offsets are doing
  # more than levelling (e.g. reacting to within-group composition), which is
  # not what this correction is supposed to do.
  emc0 <- ctr(em0); ebc0 <- ctr(eb0); emcc <- ctr(emc); ebcc <- ctr(ebc)
  say("  %-32s model %8.4f  last5 %8.4f  rel %+7.2f%%", "centred MAE (status quo)",
      mean(abs(emc0)), mean(abs(ebc0)), rel(emc0, ebc0))
  say("  %-32s model %8.4f  last5 %8.4f  rel %+7.2f%%", "centred MAE (pooled de-bias)",
      mean(abs(emcc)), mean(abs(ebcc)), rel(emcc, ebcc))
}

cat("\n================ OUT-OF-SAMPLE TEST ================\n")
report(te, "TEST, all tiers")
report(te[meet_tier == "T1_elite"], "TEST, T1_elite")

cat("\n---- fitted family x sex offsets (train, model) ----\n")
print(fit_m$fs[order(-abs(shrunk)), .(fs, n, mean_g = round(mean_g,2), shrunk = round(shrunk,2), w = round(w,2))])
