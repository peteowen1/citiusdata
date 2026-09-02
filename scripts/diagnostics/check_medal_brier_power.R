# How big an effect can 276 T1 races actually DETECT on medal Brier?
#
# Every medal-Brier gap measured on 2026-09-01 is statistically insignificant:
# +0.72% p=0.671 on the deployed arm (score_arm.R), and +1.67% to +2.86% with
# p=0.145-0.365 across the post-hoc sweeps. Before spending more on closing a
# gap, size the noise floor -- otherwise the standing goal is chasing a
# difference the sample cannot resolve, and any "fix" that appears to close it
# is as likely to be sampling variation.
#
# Reports the paired CI on the arm-vs-baseline difference, and the minimum
# detectable effect at 80% power for this many races. Uses score_arm.R's own
# population and baseline construction so the numbers are commensurable with
# the ones the campaign has been deciding on.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_POW_HOLDOUT", "2025-01-01"))
ARM <- Sys.getenv("CITIUS_POW_ARM", "backtest_tier05fd2.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s", ARM, format(HOLDOUT))

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            p_gold, p_medal, median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal, merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE, .(event_id, sigma_within)]
d <- merge(d, evs, by = "event_id")
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]

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
d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
say("population: %s rows, %s races", format(nrow(d), big.mark=","),
    format(uniqueN(d$race_id), big.mark=","))

base <- rbindlist(lapply(split(d, d$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                   ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, calibration = NULL,
                                   condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
             b_gold = mp$p_gold, b_medal = mp$p_medal)
}))
z <- merge(d, base, by = c("race_id", "athlete_id"))

report <- function(pcol, bcol, ycol, label) {
  a <- z[, .(v = mean((get(pcol) - get(ycol))^2)), by = race_id]
  bq <- z[, .(v = mean((get(bcol) - get(ycol))^2)), by = race_id]
  mm <- merge(a, bq, by = "race_id")
  dif <- mm$v.x - mm$v.y
  n <- length(dif); sd_d <- sd(dif); se <- sd_d / sqrt(n)
  base_mean <- mean(mm$v.y)
  tt <- t.test(dif)
  # minimum detectable effect, two-sided alpha=.05, 80% power
  mde <- (1.96 + 0.84) * se
  cat(sprintf("\n---- %s ----\n", label))
  say("races %d | arm %.5f | base %.5f | diff %+.5f (%+.2f%%)",
      n, mean(mm$v.x), base_mean, mean(dif), 100 * mean(dif) / base_mean)
  say("95%% CI on the difference: [%+.5f, %+.5f] = [%+.2f%%, %+.2f%%]",
      tt$conf.int[1], tt$conf.int[2],
      100 * tt$conf.int[1] / base_mean, 100 * tt$conf.int[2] / base_mean)
  say("p = %.3g | per-race sd of the difference %.4f", tt$p.value, sd_d)
  say("MINIMUM DETECTABLE EFFECT at 80%% power: %.5f = %.2f%% of baseline",
      mde, 100 * mde / base_mean)
  say("races needed to detect the observed %+.2f%% at 80%% power: %s",
      100 * mean(dif) / base_mean,
      if (abs(mean(dif)) > 0) format(ceiling((2.8 * sd_d / abs(mean(dif)))^2), big.mark = ",") else "n/a")
}
report("p_medal", "b_medal", "hit_medal", "MEDAL Brier, arm vs last-5")
report("p_gold",  "b_gold",  "hit",       "GOLD Brier, arm vs last-5")

cat("\n==== WHAT THIS MEANS FOR THE GOAL ====\n")
say("If the MDE is larger than the gap being chased, the goal cannot be")
say("demonstrated on this sample no matter what is built: a fix that appears to")
say("close it is as likely to be sampling variation. In that case the honest")
say("options are (a) enlarge the population -- more races, or pool tiers -- or")
say("(b) switch to a metric with more power on the same data, e.g. logloss,")
say("which uses the whole probability rather than its squared error.")
