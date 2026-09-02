# Where does the model lose to last-5 on T1 elite marks, and by how much, cut
# every way there is decent sample size to ask the question?
#
# GOAL THIS SERVES: T1 elite marks MAE currently loses to last-5 (measured
# 2026-09-01, backtest_tierctrl.rds vs last-5: 2.0000 vs 1.8254, model +9.56%
# worse, p=3.42e-16). check_per_event_debias.R already showed a flat per-
# (event,sex) mean offset does NOT close this out-of-sample (T1 raw MAE still
# +2.70% worse after debiasing). This script is the wider net: cut by family,
# sex, season (indoor/outdoor, quarter), wind, venue country and age band, not
# just event x sex, to find where the loss actually concentrates and whether
# any cut both (a) shows a real gap and (b) has enough races to trust.
#
# METHOD. Same leakage-safe last-5 construction as check_marks_vs_naive_
# baselines.R (history rolled to date-1). Race-clustered inference throughout
# via _cluster.R (a plain paired t.test over athlete-race rows understates the
# SE by the field size -- measured 8.6x on one cut in this same session).
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  T1 elite pooled model-vs-last5 relative MAE here must match the
#       standalone score_arm.R number to within rounding (2.0000/1.8254,
#       +9.56%) -- if it doesn't, this script's population or join differs
#       from the one the campaign has been deciding on, and nothing below is
#       comparable to it.
#   A2  every reported cut level clears MIN_RACES or is not reported at all --
#       a filtered display is not the same as a filtered computation, and
#       printing a thin cell invites reading noise as signal.
#   A3  wind and age cuts must report their coverage (%non-NA) alongside the
#       result -- a cut over 8% of the population answers a different, much
#       narrower question than the header implies.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_cluster.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")
MIN_RACES <- 20L

ARM <- Sys.getenv("CITIUS_CUTS_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s | MIN_RACES %d", ARM, format(HOLDOUT), MIN_RACES)

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date,
            competition_id, sex_code, wind, indoor, age, venue_country)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- last-5 baseline, leakage-safe (same construction as every sibling check) ----
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)
g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, unique(m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id")),
           by = c("race_id", "athlete_id"))

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d <- d[!is.na(act_perf) & !is.na(a_perf) & !is.na(l5) & date >= HOLDOUT]
d[, `:=`(em = 100 * (a_perf - act_perf), eb = 100 * (l5 - act_perf))]

t1 <- d[meet_tier == "T1_elite"]
say("\nT1 elite population: %s rows, %s races", format(nrow(t1), big.mark=","),
    format(uniqueN(t1$race_id), big.mark=","))

cat("\n==== ANCHOR A1: pooled T1 number must match score_arm.R ====\n")
r0 <- cluster_rel_mae(t1$em, t1$eb, t1$race_id)
say("pooled T1 raw MAE, model vs last5: %s", fmt_cl(r0))
say("A1 %s (score_arm.R reference: +9.56%%, n=6033/568 races)",
    if (abs(r0$est - 9.56) < 1.5) "PASS - within rounding/holdout noise" else
      "CHECK - see note below before trusting cuts as comparable")

# ---- season / indoor / wind / age / venue derived fields -------------------
t1[, season_q := paste0("Q", quarter(date))]
t1[, era := fifelse(indoor, "indoor", "outdoor")]
t1[, wind_cov := !is.na(wind)]
t1[wind_cov == TRUE, wind_bin := cut(wind, c(-Inf, -1, 0, 1, 2, Inf),
                                     labels = c("<-1", "-1..0", "0..1", "1..2", ">2"))]
t1[, age_cov := !is.na(age)]
t1[age_cov == TRUE, age_bin := cut(age, c(-Inf, 22, 25, 28, 31, Inf),
                                    labels = c("<=22", "23-25", "26-28", "29-31", "32+"))]

cat(sprintf("\nwind coverage in T1 elite: %.1f%% non-NA\n", 100 * mean(t1$wind_cov)))
cat(sprintf("age coverage in T1 elite: %.1f%% non-NA\n", 100 * mean(t1$age_cov)))

# ---- generic cut runner -----------------------------------------------------
# One row per level, sorted worst-for-the-model first, levels below MIN_RACES
# silently excluded (A2) rather than printed with a caveat nobody will read
# twice.
run_cut <- function(dd, by_col, label, min_races = MIN_RACES) {
  cat(sprintf("\n---- cut: %s ----\n", label))
  lv <- dd[!is.na(get(by_col)), unique(get(by_col))]
  rows <- list()
  for (v in lv) {
    sub <- dd[get(by_col) == v]
    nr <- uniqueN(sub$race_id)
    if (nr < min_races) next
    r <- cluster_rel_mae(sub$em, sub$eb, sub$race_id)
    rows[[length(rows) + 1L]] <- data.table(
      level = as.character(v), races = nr, rows = nrow(sub),
      model_mae = round(mean(abs(sub$em)), 4), last5_mae = round(mean(abs(sub$eb)), 4),
      rel_pct = round(r$est, 2), lo = round(r$lo, 2), hi = round(r$hi, 2),
      p = signif(r$p, 3), se_infl = round(r$infl, 1))
  }
  if (!length(rows)) { cat("  no level clears MIN_RACES\n"); return(invisible(NULL)) }
  res <- rbindlist(rows)[order(-rel_pct)]
  print(res, row.names = FALSE)
  invisible(res)
}

cat("\n\n================ CUT SWEEP: T1 elite, model vs last-5, raw MAE % ================\n")
cat("positive rel_pct = model WORSE than last-5 on that cut.\n")

res_event  <- run_cut(t1, "event_id", "event")
res_family <- run_cut(t1, "family",   "family")
res_sex    <- run_cut(t1, "sex",      "sex", min_races = 10L)
res_year   <- run_cut(t1[, yr := as.character(year(date))], "yr", "season year")
res_q      <- run_cut(t1, "season_q", "quarter of year")
res_era    <- run_cut(t1, "era",      "indoor vs outdoor", min_races = 10L)
res_venue  <- run_cut(t1, "venue_country", "venue country")

cat(sprintf("\n---- wind (coverage %.1f%%; treat as a narrower question) ----\n",
            100 * mean(t1$wind_cov)))
res_wind <- run_cut(t1[wind_cov == TRUE], "wind_bin", "wind bin", min_races = 15L)

cat(sprintf("\n---- age (coverage %.1f%%) ----\n", 100 * mean(t1$age_cov)))
res_age <- run_cut(t1[age_cov == TRUE], "age_bin", "age band", min_races = 15L)

cat("\n\n================ family x sex (finer, still gated) ================\n")
t1[, grp := paste(family, sex, sep = "|")]
res_famsex <- run_cut(t1, "grp", "family x sex", min_races = 15L)

cat("\n\nDone. Read se_infl before trusting any single row -- it is the ratio of\n")
cat("the clustered SE to what a naive paired t.test would have reported; deep\n")
cat("fields inflate it several-fold, and a rel_pct with a wide [lo,hi] spanning\n")
cat("zero is not evidence of anything.\n")
