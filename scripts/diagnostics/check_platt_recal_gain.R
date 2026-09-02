# Platt-scaled (logistic) recalibration of p_medal, fit out-of-sample, tested
# against the standing goal (T1 medal Brier vs last-5).
#
# WHY THIS AND NOT ANOTHER ABILITY/SIGMA VARIANT, AND WHY NOT ISOTONIC AGAIN.
# check_isotonic_recal_gain.R tried a non-parametric reliability-curve remap
# and it made things worse (T1 +0.53%, throw +1.19%, Javelin W +1.68%, all
# p<0.03) -- plausibly because isotonic regression is high-variance and the
# fit window only has 678 races, too few to estimate a free-form reliability
# curve without overfitting it. Platt scaling is the standard lower-variance
# alternative: fit ONE logistic regression, 2 parameters
# (hit_medal ~ a + b*logit(p_medal)), instead of a free monotone curve. Same
# out-of-sample discipline, same diagnosed target (the reliability-curve
# distortion from check_medal_deficit_cuts.R), far fewer degrees of freedom
# to overfit with. It cannot invert any pair's relative order (monotone in p
# by construction, since b is fit unconstrained but a logistic map of a
# scalar is monotone for any b of consistent sign -- checked below), so it
# cannot make gold Brier (a
# single-probability calibration question) worse in a way compression's
# ORDER-preserving-but-magnitude-changing shift could not already do, and it
# targets the medal deficit directly rather than via a level proxy.
#
# OUT-OF-SAMPLE, same discipline as every other test tonight: the isotonic
# map is fit on races strictly before FIT_HOLDOUT and applied UNCHANGED to
# races on/after it. Fitting and testing on the same races would trivially
# "fix" calibration by construction and mean nothing.
#
# ANCHORS:
#   A1 fit/test race sets disjoint.
#   A2 the isotonic map must be genuinely non-trivial (not the identity) --
#      assert its range of adjustment exceeds a floor, else the fit found
#      nothing to correct and any downstream "improvement" is noise.
#   A3 report BOTH T1 aggregate and throw-only, since the diagnosed distortion
#      was found broadly (women's power events) as well as in throw
#      specifically -- a fix should be tested at both grains.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_ISO_HOLDOUT", "2022-01-01"))
FIT_LO      <- as.Date(Sys.getenv("CITIUS_ISO_FIT_LO", "2016-01-01"))
ARM         <- Sys.getenv("CITIUS_ISO_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | fit [%s, %s) | test >= %s", ARM, format(FIT_LO), format(FIT_HOLDOUT), format(FIT_HOLDOUT))

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), p_medal, p_gold)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal, merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite"]

fit <- d[date >= FIT_LO & date < FIT_HOLDOUT]
tst <- d[date >= FIT_HOLDOUT]
stopifnot("A1: fit and test races overlap" =
            length(intersect(unique(fit$race_id), unique(tst$race_id))) == 0)
say("fit: %s rows / %s races | test: %s rows / %s races",
    format(nrow(fit),big.mark=","), format(uniqueN(fit$race_id),big.mark=","),
    format(nrow(tst),big.mark=","), format(uniqueN(tst$race_id),big.mark=","))
stopifnot("fit population too small for Platt scaling" = nrow(fit) >= 500)

# Fit Platt scaling: hit_medal ~ a + b * logit(p_medal), on the FIT window
# only. Two parameters total -- far fewer degrees of freedom than isotonic's
# free-form curve, which is the whole point given isotonic overfit a
# 678-race fit window.
EPS <- 1e-4
logit <- function(p) { p <- pmin(pmax(p, EPS), 1 - EPS); log(p / (1 - p)) }
fit[, logit_p := logit(p_medal)]
platt <- glm(hit_medal ~ logit_p, data = fit, family = binomial())
say("Platt coefficients: a = %.4f, b = %.4f (b=1,a=0 would be the identity)",
    coef(platt)[1], coef(platt)[2])
stopifnot("A_sign: b must be positive, or the map is not order-preserving in p" =
            coef(platt)[2] > 0)
recal <- function(p) {
  out <- as.numeric(predict(platt, newdata = data.frame(logit_p = logit(p)), type = "response"))
  pmin(pmax(out, 1e-4), 1 - 1e-4)
}
# A2: the map must actually move something.
adj_range <- range(recal(fit$p_medal) - fit$p_medal)
say("Platt adjustment range on fit data: [%.4f, %.4f]", adj_range[1], adj_range[2])
stopifnot("A2: Platt map is trivially close to the identity -- nothing to correct" =
            diff(range(adj_range)) > 0.01 || max(abs(adj_range)) > 0.01)

tst[, p_medal_recal := recal(p_medal)]

hist <- deployed_history(OUT, events = unique(tst$event_id), from = min(tst$date) - 3650, to = max(tst$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- tst[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last", .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
tst <- merge(tst, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]

reg2 <- as.data.table(citius_events())[, .(event_id, orientation)]
evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE, .(event_id, sigma_within)]
tst <- merge(tst, reg2, by = "event_id"); tst <- merge(tst, evs, by = "event_id")
sim_rows <- rbindlist(lapply(split(tst, tst$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1], ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id, b_medal = mp$p_medal)
}))
tst <- merge(tst, sim_rows, by = c("race_id", "athlete_id"))

report <- function(dd, label) {
  if (uniqueN(dd$race_id) < 15) { say("%s: too few races (%d), skipped", label, uniqueN(dd$race_id)); return(invisible(NULL)) }
  a  <- dd[, .(v = mean((p_medal        - hit_medal)^2)), by = race_id]
  ar <- dd[, .(v = mean((p_medal_recal  - hit_medal)^2)), by = race_id]
  bl <- dd[, .(v = mean((b_medal        - hit_medal)^2)), by = race_id]
  m1 <- merge(a, bl, by = "race_id"); t1 <- t.test(m1$v.x, m1$v.y, paired = TRUE)
  m2 <- merge(ar, bl, by = "race_id"); t2 <- t.test(m2$v.x, m2$v.y, paired = TRUE)
  m3 <- merge(a, ar, by = "race_id"); t3 <- t.test(m3$v.y, m3$v.x, paired = TRUE)
  cat(sprintf("\n---- %s (%d races) ----\n", label, uniqueN(dd$race_id)))
  say("  raw       vs last-5: %+.2f%% p=%.3g", 100*(mean(m1$v.x)-mean(m1$v.y))/mean(m1$v.y), t1$p.value)
  say("  recal     vs last-5: %+.2f%% p=%.3g", 100*(mean(m2$v.x)-mean(m2$v.y))/mean(m2$v.y), t2$p.value)
  say("  recal vs raw (isolates the recalibration): %+.2f%% p=%.3g (negative = recal HELPS)",
      100*(mean(m3$v.y)-mean(m3$v.x))/mean(m3$v.x), t3$p.value)
}
cat("\n================ T1 AGGREGATE ================\n")
report(tst, "T1 all")
cat("\n================ THROW ONLY ================\n")
report(tst[family == "throw"], "throw")
cat("\n================ JAVELIN W ONLY ================\n")
report(tst[event_id == "AT-JavelinThrow-W"], "Javelin W")
