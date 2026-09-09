# Does race adjustment help WHERE THE SHOCK IS BIG, even if it does nothing
# on average?
#
# Pete's push, 2026-09-05: the Gout Gout race (5 of 6 athletes PB'd, field
# +0.587 s slower since) is exactly the case the adjustment exists for, so a
# flat pooled result does not mean the mechanism is useless -- it may mean big
# shocks are rare and the average is dominated by races where there is nothing
# to correct.
#
# THE TEST. Each scored prediction is tagged by how much race-shock
# CONTAMINATION the athlete's own history carried: the decay-weighted mean
# |c_r| over their prior races in that event. Then compare the deployed arm
# (race effects OFF) against the race-adjusted arm ON THE SAME predictions,
# split by that exposure.
#
# If the adjustment is real and simply diluted, the high-exposure group should
# show a gain that the low-exposure group does not. If it is flat in every
# group, the pooled null is a real null and not an averaging artefact.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
A_OFF <- Sys.getenv("CITIUS_EXP_OFF", "backtest_wac_trt_0904.rds")     # race OFF
A_ON  <- Sys.getenv("CITIUS_EXP_ON",  "backtest_racescaled.rds")       # race ON, scaled
cal <- deployed_calibration(OUT)

grab <- function(f, tag) {
  b <- readRDS(file.path(OUT, f))
  p <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                        mark = median_mark, p_medal)]
  o <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                     hit_medal)]
  m <- merge(p, o, by = c("race_id", "athlete_id"))
  setnames(m, c("mark", "p_medal"), paste0(c("mark_", "pmed_"), tag))
  m
}
d <- merge(grab(A_OFF, "off"), grab(A_ON, "on"), by = c("race_id", "athlete_id", "hit_medal"))
cat(sprintf("common predictions across both arms: %s\n", format(nrow(d), big.mark = ",")))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date,
                   competition_id)], by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]; ctl[, competition_id := as.character(competition_id)]
d <- merge(d, ctl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= as.Date("2020-01-01")]
cat(sprintf("T1 2020+ predictions: %s\n", format(nrow(d), big.mark = ",")))

# EXPOSURE: decay-weighted mean |c_r| over the athlete's prior races in this
# event. This is the quantity the adjustment acts on -- an athlete whose history
# carries no shocks has nothing to correct and should show no difference either
# way, which is exactly the control group this test needs.
rr <- as.data.table(cal$race)[, .(race_key, c_r, n_in_race)]
hist <- ch[!is.na(race_key) & is.finite(mark),
           .(athlete_id, event_id, date, race_key)]
hist <- merge(hist, rr, by = "race_key")
setkey(hist, athlete_id, event_id)
exp_of <- function(aid, eid, dt) {
  h <- hist[.(aid, eid)][date < dt]
  if (!nrow(h)) return(NA_real_)
  w <- 0.5 ^ (as.numeric(dt - h$date) / 365)
  sum(w * abs(h$c_r)) / sum(w)
}
set.seed(11L)
samp <- d[sample(.N, min(.N, 12000L))]
samp[, exposure := mapply(exp_of, athlete_id, event_id, date)]
samp <- samp[is.finite(exposure)]
cat(sprintf("scored with an exposure value: %s\n\n", format(nrow(samp), big.mark = ",")))

eps <- 1e-6
samp[, `:=`(ae_off = 100*abs(mark_off - actual)/actual,
            ae_on  = 100*abs(mark_on  - actual)/actual,
            ll_off = -(hit_medal*log(pmax(pmed_off,eps)) + (1-hit_medal)*log(pmax(1-pmed_off,eps))),
            ll_on  = -(hit_medal*log(pmax(pmed_on ,eps)) + (1-hit_medal)*log(pmax(1-pmed_on ,eps))))]
samp[, band := cut(exposure, quantile(exposure, c(0,.25,.5,.75,.9,1), na.rm=TRUE),
                   labels = c("lowest 25%","25-50%","50-75%","75-90%","top 10%"),
                   include.lowest = TRUE)]

tab <- samp[, .(n = .N,
                exposure = round(median(exposure), 5),
                marks_off = round(mean(ae_off, na.rm=TRUE), 3),
                marks_on  = round(mean(ae_on , na.rm=TRUE), 3),
                marks_delta = round(mean(ae_on, na.rm=TRUE) - mean(ae_off, na.rm=TRUE), 3),
                ll_off = round(mean(ll_off), 4),
                ll_on  = round(mean(ll_on ), 4),
                ll_delta = round(mean(ll_on) - mean(ll_off), 4)), by = band][order(band)]
cat("NEGATIVE delta = race adjustment HELPED\n\n")
print(tab)

hi <- samp[band == "top 10%"]
cat(sprintf("\nhighest-exposure decile, paired t-tests (n = %s):\n", format(nrow(hi), big.mark=",")))
tt <- function(a, b, lab) {
  ok <- is.finite(a) & is.finite(b)
  r <- stats::t.test(b[ok] - a[ok])
  cat(sprintf("  %-14s mean delta %+.4f  p = %.3g  (%s)\n", lab, mean(b[ok]-a[ok]), r$p.value,
              if (mean(b[ok]-a[ok]) < 0) "adjustment helped" else "adjustment hurt"))
}
tt(hi$ae_off, hi$ae_on, "marks MAE")
tt(hi$ll_off, hi$ll_on, "medal logloss")
