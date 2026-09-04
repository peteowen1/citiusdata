# WHERE does the model lose to a five-race mean on marks?
#
# check_marks_vs_naive_baselines.R established THAT it loses on T1 (+3.33%
# centred MAE, p=0.032). This locates the loss: same leakage-safe construction,
# same centred-MAE metric, cut every way that could plausibly explain it.
#
# Centring convention matches rep_pop() in the sibling script exactly: each
# predictor's mean error is removed WITHIN the subgroup being reported, so a
# cut's number isolates discrimination from that cut's own calibration offset.
# That is also why a small subgroup flatters itself -- the removed mean is
# estimated from few points -- so MIN_N is enforced and n is printed always.
#
# ANCHOR CHECKS, written before any output was looked at. A failure here is a
# fault in THIS script's method, not a finding about the model:
#   B1  model and last-5 must score the IDENTICAL (race_id, athlete_id) set.
#   B2  last-5 must be NA for an athlete's first result in that event; if
#       nothing is NA the roll-to-(date-1) guard is not doing anything.
#   B3  pooled T1 centred MAE must reproduce the known +3.33% to within ~0.5pp.
#       A wildly different number means MY rebuild is wrong, not the prior one.
#   B4  every cut must partition its population -- subgroup n must sum to total.
#   B5  last-5 mean signed error ~0 (it is a central estimate); model ~+1.1%
#       optimistic. If these flip, the error orientation is backwards.
#   B6  the trajectory slope must use only results strictly before the race.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")
MIN_N   <- 150L   # predictions; below this a centred cut is not resolvable

say <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- identical construction to check_marks_vs_naive_baselines.R -------------
b <- readRDS(file.path(OUT, "backtest_ctrl_now.rds"))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id,
            round, wind, indoor)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, family, sex, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, class, meet_tier)], by = "competition_id", all.x = TRUE)

hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)

g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
# span of the last-5 window, and a trajectory slope, both from PRIOR rows only
hist[, d5 := shift(date, 5L), by = g]
hist[, p1 := shift(perf, 1L), by = g][, p5 := shift(perf, 5L), by = g]
# In a rolling join the returned `date` is the QUERY date, not the matched
# row's -- so the matched date must be carried as its own column or every
# "days since" comes out as 1. (Found by every row landing in the <2wk bin.)
hist[, rowdate := date]

q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5, lastdate = rowdate, d5, p1, p5, lastperf = perf)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
# span of the last-5 window: newest prior result minus 5th-newest
m[, l5_span := as.numeric(lastdate - d5)]
# trajectory: change in perf across the last-5 window, per 100 days.
# Uses only rows strictly before the race (B6) -- lastperf and p5 are both prior.
m[, traj := fifelse(is.finite(l5_span) & l5_span > 0,
                    100 * (lastperf - p5) / l5_span, NA_real_)]

d <- merge(d, m[, .(race_id, athlete_id, l5, k_prior = k, l5_span, traj,
                    last1 = lastperf, lastdate)],
           by = c("race_id", "athlete_id"))
d <- d[date >= HOLDOUT]
d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d[, days_since := as.numeric(date - lastdate)]

full <- d[!is.na(l5) & !is.na(a_perf) & !is.na(act_perf)]
full[, `:=`(e_model = 100 * (a_perf - act_perf), e_l5 = 100 * (l5 - act_perf))]

# ---- anchors ---------------------------------------------------------------
cat("\n==== ANCHOR CHECKS ====\n")
say("B1  model and last-5 scored on identical rows: %s (both %s)",
    nrow(full) == sum(!is.na(full$e_model) & !is.na(full$e_l5)), format(nrow(full), big.mark = ","))
say("B2  last-5 NA before restriction: %d of %d rows (%.1f%%) -- first result in event",
    sum(is.na(d$l5)), nrow(d), 100 * mean(is.na(d$l5)))
say("B5  mean signed error: model %+.3f%%  last5 %+.3f%%  (model should be ~+1.1, last5 ~0)",
    mean(full$e_model), mean(full$e_l5))
say("B6  trajectory uses prior rows only: max(lastdate) < min(race date)? per-row by construction; %d rows have a slope",
    sum(!is.na(full$traj)))

# ---- the reporting engine --------------------------------------------------
# rel > 0 = model WORSE than last-5. Paired t on centred absolute errors.
gap <- function(dd) {
  if (nrow(dd) < MIN_N) return(NULL)
  em <- dd$e_model - mean(dd$e_model)
  el <- dd$e_l5    - mean(dd$e_l5)
  mm <- mean(abs(em)); ml <- mean(abs(el))
  t  <- tryCatch(t.test(abs(em), abs(el), paired = TRUE), error = function(e) NULL)
  data.table(n = nrow(dd), races = uniqueN(dd$race_id),
             mae_model = mm, mae_l5 = ml,
             rel = 100 * (mm - ml) / ml,
             se_rel = if (is.null(t)) NA_real_ else 100 * (t$stderr) / ml,
             p = if (is.null(t)) NA_real_ else t$p.value,
             bias_model = mean(dd$e_model), bias_l5 = mean(dd$e_l5))
}

sweep <- function(dd, byvar, label) {
  r <- dd[, if (.N >= MIN_N) gap(.SD) else NULL, by = byvar]
  if (!nrow(r)) { say("\n[%s] no level reaches n>=%d", label, MIN_N); return(invisible(NULL)) }
  setorder(r, -rel)
  cat(sprintf("\n---- %s ----\n", label))
  cat(sprintf("%-26s %6s %6s %8s %8s %8s %9s %8s\n", "level", "n", "races", "rel%", "+/-se", "p", "bias_mdl", "bias_l5"))
  for (i in seq_len(nrow(r))) {
    cat(sprintf("%-26s %6d %6d %+8.2f %8.2f %8.3g %+9.2f %+8.2f\n",
                substr(as.character(r[[byvar]][i]), 1, 26), r$n[i], r$races[i],
                r$rel[i], r$se_rel[i], r$p[i], r$bias_model[i], r$bias_l5[i]))
  }
  # B4: partition check
  say("  B4 partition: subgroup n sums to %s of %s (%.1f%% covered; rest below MIN_N or NA)",
      format(sum(r$n), big.mark = ","), format(nrow(dd), big.mark = ","),
      100 * sum(r$n) / nrow(dd))
  invisible(r)
}

POP <- full[meet_tier == "T1_elite"]
cli::cli_h1("T1 elite: where does the model lose to last-5?")
o <- gap(POP)
say("B3  POOLED T1: rel %+0.2f%% (+/- %.2f se), p=%.3g, n=%s, races=%d  [expect ~+3.3%%]",
    o$rel, o$se_rel, o$p, format(o$n, big.mark = ","), o$races)
say("    noise floor here: 1 se = %.2fpp, so |rel| under ~%.1fpp is not resolvable",
    o$se_rel, 2 * o$se_rel)

POP[, `:=`(
  depth_bin = cut(k_prior, c(0, 3, 6, 12, 25, Inf), labels = c("1-3", "4-6", "7-12", "13-25", "26+")),
  span_bin  = cut(l5_span, c(-Inf, 90, 200, 400, 800, Inf), labels = c("<90d", "90-200d", "200-400d", "400-800d", "800d+")),
  since_bin = cut(days_since, c(-Inf, 14, 35, 90, 250, Inf), labels = c("<2wk", "2-5wk", "5-13wk", "13-36wk", "36wk+")),
  traj_bin  = cut(traj, c(-Inf, -0.15, -0.04, 0.04, 0.15, Inf),
                  labels = c("declining fast", "declining", "flat", "improving", "improving fast")),
  round_bin = fifelse(grepl("final", round, ignore.case = TRUE) & !grepl("semi", round, ignore.case = TRUE),
                      "final", fifelse(grepl("semi", round, ignore.case = TRUE), "semi", "heat/other")),
  yr        = year(date),
  windy     = fifelse(is.na(wind), "no wind recorded",
                      fifelse(abs(wind) >= 1.5, "|wind| >= 1.5", "|wind| < 1.5"))
)]
fs <- POP[, .(field = .N), by = race_id]
POP <- merge(POP, fs, by = "race_id")
POP[, field_bin := cut(field, c(0, 6, 8, 10, Inf), labels = c("<=6", "7-8", "9-10", "11+"))]

sweep(POP, "family",     "EVENT FAMILY")
sweep(POP, "discipline", "EVENT (discipline)")
sweep(POP, "sex",        "SEX")
sweep(POP, "depth_bin",  "EVIDENCE DEPTH (prior results in event)")
sweep(POP, "span_bin",   "LAST-5 WINDOW SPAN (how old the 5th-newest result is)")
sweep(POP, "since_bin",  "DAYS SINCE LAST RACE")
sweep(POP, "traj_bin",   "TRAJECTORY (perf change per 100d over last-5 window)")
sweep(POP, "round_bin",  "ROUND")
sweep(POP, "field_bin",  "FIELD SIZE")
sweep(POP, "yr",         "YEAR")
sweep(POP, "windy",      "WIND")
sweep(POP, "indoor",     "INDOOR")
sweep(full, "meet_tier", "MEET TIER (all populations)")
sweep(full, "class",     "COMPETITION CLASS (all populations)")

# window-span fact for the half-life hypothesis
say("\n---- LAST-5 WINDOW vs DEPLOYED half_life (%s days) ----", DEPLOYED$half_life)
qs <- quantile(POP$l5_span, c(.1, .25, .5, .75, .9), na.rm = TRUE)
say("l5 window span days, T1: p10 %.0f | p25 %.0f | median %.0f | p75 %.0f | p90 %.0f",
    qs[1], qs[2], qs[3], qs[4], qs[5])
say("share of T1 rows whose last-5 window is SHORTER than the half-life: %.1f%%",
    100 * mean(POP$l5_span < DEPLOYED$half_life, na.rm = TRUE))

# ---- is the pooled loss a CALIBRATION artefact? ----------------------------
# Most families come out NEGATIVE (model better) while the pooled number is
# POSITIVE. That is the Simpson signature: pooled centring removes ONE global
# mean, so any per-group offset the model carries stays in its residual and
# inflates its MAE. Test it directly -- re-centre each predictor WITHIN a
# grouping, then pool the absolute errors. If the sign flips, the pooled loss
# is per-group calibration, not worse discrimination.
regroup <- function(dd, byvar, label) {
  x <- copy(dd)
  x[, `:=`(cm = e_model - mean(e_model), cl = e_l5 - mean(e_l5)), by = byvar]
  mm <- mean(abs(x$cm)); ml <- mean(abs(x$cl))
  t <- t.test(abs(x$cm), abs(x$cl), paired = TRUE)
  say("  centred within %-22s  model %.4f  last5 %.4f  rel %+6.2f%%  p=%.3g",
      label, mm, ml, 100 * (mm - ml) / ml, t$p.value)
}
cat("\n---- DOES PER-GROUP CALIBRATION EXPLAIN THE POOLED LOSS? ----\n")
say("  (rel > 0 = model worse. Pooled global centring is the status quo.)")
POP[, one := 1L]
regroup(POP, "one",        "nothing (= pooled, status quo)")
regroup(POP, "family",     "event family")
regroup(POP, "discipline", "event")
regroup(POP, c("discipline", "sex"), "event x sex")

saveRDS(POP, file.path(OUT, "marks_mae_by_cut_T1.rds"))
say("\nwrote marks_mae_by_cut_T1.rds (%s rows)", format(nrow(POP), big.mark = ","))
