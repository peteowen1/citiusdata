# Where does the model still lose to the last-5 baseline on T1 marks?
#
# The headline number -- centred MAE -0.86%, p = 0.202 -- is one average over
# 812 races, and an average that close to zero is usually two populations
# cancelling rather than uniform parity. Tuning against the average without
# knowing which populations it is made of is how the last week was spent moving
# 0.2% knobs.
#
# So: split the paired per-prediction difference by things that could plausibly
# drive it, and report where the model wins and loses. Every split is chosen
# BEFORE looking (event family, data richness, round, field quality, era) so
# this cannot become a hunt for a flattering slice.
#
# Usage:  Rscript scripts/diagnose_marks.R
#         CITIUS_DIAG_ARM=backtest_ref2.rds Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
ARM <- Sys.getenv("CITIUS_DIAG_ARM", "backtest_ref2.rds")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_SCORE_HOLDOUT", "2023-01-01"))

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation, family)], by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d <- merge(d, cat_tbl[, .(competition_id, meet_tier, strength)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]

# The baseline, rebuilt exactly as score_arm.R builds it: the mean of an
# athlete's last five oriented performances before the race.
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
setDT(hist)
hist <- hist[!is.na(perf) & !is.na(date), .(athlete_id = as.character(athlete_id), event_id, date, perf)]
setkey(hist, athlete_id, event_id, date)

base_of <- function(aid, eid, dt) {
  h <- hist[.(aid, eid)][date < dt]
  if (!nrow(h)) return(c(NA_real_, 0))
  p <- tail(h$perf, 5L)
  c(mean(p), nrow(h))
}
bb <- mapply(base_of, d$athlete_id, d$event_id, d$date)
d[, b_perf := bb[1, ]]
d[, n_prior := bb[2, ]]
d <- d[is.finite(b_perf)]

d[, a_perf := orientation * log(a_mark)]
d[, t_perf := orientation * log(actual)]

# Centred: remove each race's mean from prediction and truth alike, so this
# measures within-race differentiation rather than the level. That is the metric
# still short of the target.
d[, `:=`(a_c = a_perf - mean(a_perf), b_c = b_perf - mean(b_perf),
         t_c = t_perf - mean(t_perf)), by = race_id]
d[, `:=`(ae_a = abs(a_c - t_c), ae_b = abs(b_c - t_c))]
d[, diff := ae_a - ae_b]          # negative = model better

report <- function(lab, by) {
  r <- d[, .(n = .N,
             model = round(100 * mean(ae_a), 3), base = round(100 * mean(ae_b), 3),
             rel = round(100 * (mean(ae_a) / mean(ae_b) - 1), 2),
             p = tryCatch(signif(stats::t.test(diff)$p.value, 3), error = function(e) NA_real_)),
         by = by]
  setorderv(r, "rel")
  cli::cli_h3(lab)
  print(r)
}

cli::cli_h1("{ARM}: T1 centred marks, model vs last-5  ({nrow(d)} predictions, {uniqueN(d$race_id)} races)")
cat(sprintf("\noverall: model %.3f  base %.3f  rel %+.2f%%  p=%.3g\n\n",
            100 * mean(d$ae_a), 100 * mean(d$ae_b),
            100 * (mean(d$ae_a) / mean(d$ae_b) - 1), stats::t.test(d$diff)$p.value))

report("by event family", "family")

# DATA RICHNESS is the split the lever ratio predicts should matter most: if the
# model only ties the baseline where an athlete's history is thin, the remaining
# harvest fixes it and no amount of tuning would have.
d[, prior_band := cut(n_prior, c(-1, 5, 10, 20, 40, Inf),
                      labels = c("1-5", "6-10", "11-20", "21-40", "40+"))]
report("by number of prior races (the harvest's lever)", "prior_band")

d[, qb := cut(strength, c(-1, 60, 75, 85, 101), labels = c("<60", "60-75", "75-85", "85+"))]
report("by meet strength", "qb")

d[, yr := year(date)]
report("by year", "yr")

# Field size: a bigger field gives the centring more to work with, and is also
# where a compressed ability spread should hurt most.
d[, fs := .N, by = race_id]
d[, fsb := cut(fs, c(0, 6, 8, 10, Inf), labels = c("<=6", "7-8", "9-10", "11+"))]
report("by field size", "fsb")

cli::cli_h3("worst events by relative loss (n >= 100)")
ev <- d[, .(n = .N, rel = round(100 * (mean(ae_a) / mean(ae_b) - 1), 2)), by = event_id][n >= 100]
setorder(ev, -rel)
print(head(ev, 12))
print(head(ev[order(rel)], 8))
