# Does race adjustment stop us over-predicting athletes who JUST overperformed?
#
# Pete's mechanism, stated plainly: when a field collectively beats their marks,
# they do not keep doing it, so the adjustment should stop the model expecting
# them to. The Gout Gout race is the case -- 5 of 6 PB'd, all 6 slower since.
#
# WHY THE EARLIER TEST WAS THE WRONG ONE (race_adjust_by_exposure.R). It split
# predictions by the decay-weighted average |c_r| over an athlete's WHOLE
# history. That is career-average shock exposure, not "this athlete just ran a
# shocked race", and averaging over ten years dilutes exactly the signal being
# tested. It also scored ABSOLUTE marks error, which penalises the adjustment
# for a level shift: stripping shocks from history makes ability
# conditions-neutral, and predicting a championship final -- normally a FAST
# race -- then under-predicts because nothing adds the target race's shock back.
#
# THIS TEST INSTEAD:
#   cut  = the shock in the athlete's MOST RECENT races before the scored one,
#          which is what actually inflates their current estimate
#   score = SIGNED error (optimism), because the question is whether we predict
#           them BETTER than they turn out, not how far off we are either way
#
# If the mechanism works, athletes coming off a big positive shock should be
# over-predicted by the unadjusted model, and less so by the adjusted one.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
A_OFF <- Sys.getenv("CITIUS_RS_OFF", "backtest_wac_trt_0904.rds")
A_ON  <- Sys.getenv("CITIUS_RS_ON",  "backtest_racescaled.rds")
NREC  <- as.integer(Sys.getenv("CITIUS_RS_NRECENT", "3"))
cal <- deployed_calibration(OUT)

grab <- function(f, tag) {
  b <- readRDS(file.path(OUT, f))
  p <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                        mark = median_mark, p_medal)]
  o <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), hit_medal)]
  m <- merge(p, o, by = c("race_id", "athlete_id"))
  setnames(m, c("mark", "p_medal"), paste0(c("mark_", "pmed_"), tag)); m
}
d <- merge(grab(A_OFF, "off"), grab(A_ON, "on"), by = c("race_id", "athlete_id", "hit_medal"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date,
                   competition_id)], by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]; ctl[, competition_id := as.character(competition_id)]
d <- merge(d, ctl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation)], by = "event_id")
d <- d[meet_tier == "T1_elite" & date >= as.Date("2020-01-01")]

# RECENT shock: mean c_r over the athlete's last NREC races in this event before
# the scored one. Positive c_r means the race was FAST, so a positive value here
# means their recent marks were flattered.
rr <- as.data.table(cal$race)[, .(race_key, c_r)]
hist <- merge(ch[!is.na(race_key) & is.finite(mark), .(athlete_id, event_id, date, race_key)],
              rr, by = "race_key")
setkey(hist, athlete_id, event_id)
recent_shock <- function(aid, eid, dt) {
  h <- hist[.(aid, eid)][date < dt]
  if (!nrow(h)) return(NA_real_)
  setorder(h, -date)
  mean(utils::head(h$c_r, NREC))
}
set.seed(11L)
samp <- d[sample(.N, min(.N, 14000L))]
samp[, shock := mapply(recent_shock, athlete_id, event_id, date)]
samp <- samp[is.finite(shock)]
cat(sprintf("scored predictions with a recent-shock value: %s\n\n", format(nrow(samp), big.mark=",")))

# SIGNED optimism: positive = we predicted a BETTER performance than happened.
samp[, opt_off := orientation * 100 * (mark_off - actual) / actual]
samp[, opt_on  := orientation * 100 * (mark_on  - actual) / actual]
eps <- 1e-6
samp[, ll_off := -(hit_medal*log(pmax(pmed_off,eps)) + (1-hit_medal)*log(pmax(1-pmed_off,eps)))]
samp[, ll_on  := -(hit_medal*log(pmax(pmed_on ,eps)) + (1-hit_medal)*log(pmax(1-pmed_on ,eps)))]

samp[, band := cut(shock, quantile(shock, c(0,.2,.4,.6,.8,1), na.rm=TRUE),
                   labels = c("most negative","", "middle", " ", "most POSITIVE (just overperformed)"),
                   include.lowest = TRUE)]
tab <- samp[, .(n = .N, median_shock = round(median(shock), 4),
                optimism_off = round(mean(opt_off, na.rm=TRUE), 3),
                optimism_on  = round(mean(opt_on , na.rm=TRUE), 3),
                reduction = round(mean(opt_off, na.rm=TRUE) - mean(opt_on, na.rm=TRUE), 3),
                ll_delta = round(mean(ll_on) - mean(ll_off), 4)), by = band][order(band)]
cat("OPTIMISM = we predicted better than they ran. The adjustment should CUT it\n")
cat("most for athletes coming off a fast (positive-shock) race.\n\n")
print(tab)

hi <- samp[shock >= quantile(shock, .8, na.rm = TRUE)]
cat(sprintf("\ntop shock quintile (n = %s), paired:\n", format(nrow(hi), big.mark=",")))
ok <- is.finite(hi$opt_off) & is.finite(hi$opt_on)
r <- stats::t.test(hi$opt_on[ok] - hi$opt_off[ok])
cat(sprintf("  optimism %+.3f -> %+.3f  (change %+.3f, p = %.3g)\n",
            mean(hi$opt_off[ok]), mean(hi$opt_on[ok]),
            mean(hi$opt_on[ok] - hi$opt_off[ok]), r$p.value))
r2 <- stats::t.test(hi$ll_on - hi$ll_off)
cat(sprintf("  medal logloss %+.4f (p = %.3g, %s)\n", mean(hi$ll_on - hi$ll_off), r2$p.value,
            if (mean(hi$ll_on - hi$ll_off) < 0) "adjustment helped" else "adjustment hurt"))
