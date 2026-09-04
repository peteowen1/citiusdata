# Does shrinking ability toward the field mean help medal Brier when targeted
# at THROW specifically -- the family tonight's definitive-arm sweep actually
# found significant (+8.10%, p=0.023), not jump.
#
# WHY THIS IS A NEW TEST, NOT A REPEAT. check_ability_shrink_gain.R already
# refuted compression, but its "losing cells" (jump|W, sprint|W, hurdles|W)
# were drawn from an OLDER check (check_medal_deficit_cuts.R on
# backtest_tierctrl.rds, pre-sigma-scale, pre-marks-fix). Tonight's sweep on
# the actual definitive arm found a DIFFERENT significant family: throw
# (p=0.023), with Javelin Throw W the standout individual event (+25.33%,
# p=0.0044). Compression aimed at throw specifically has never been tested.
#
# CLEAN TEST, not entangled with the sigma-context scale. SIGMA_SCALE=1 (a
# no-op multiplier) so this isolates compression alone against the plain
# deployed baseline (tierctrl) -- the sigma scale itself is flagged NOT ready
# to deploy (real medal-logloss cost), so bundling it into this test would
# make a positive result unusable without first resolving that separately.
#
# OUT-OF-SAMPLE, same discipline as the original script: lambda chosen on
# [FIT_LO, FIT_HOLDOUT), applied unchanged to >= FIT_HOLDOUT. Wider fit window
# than the original (2016-2022 vs 2023-2025) because throw is a thinner cell
# and needs more races to pick a stable lambda.
#
# ANCHORS: A1 lambda=0 reproduces status quo, A2 fit/test disjoint, A3
# compression is order-preserving (assert rank correlation == 1).
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_TSHRINK_HOLDOUT", "2022-01-01"))
FIT_LO      <- as.Date(Sys.getenv("CITIUS_TSHRINK_FIT_LO", "2016-01-01"))
ARM         <- Sys.getenv("CITIUS_TSHRINK_ARM", "backtest_tierctrl.rds")
TARGET      <- Sys.getenv("CITIUS_TSHRINK_TARGET", "family")  # "family" or "javelinW"
NSIM_FIT <- 2000L; NSIM_TEST <- 4000L
LAMBDAS <- c(0, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60)
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | fit [%s, %s) | test >= %s | target %s | sigma scale 1.0 (no-op)",
    ARM, format(FIT_LO), format(FIT_HOLDOUT), format(FIT_HOLDOUT), TARGET)

b <- readRDS(file.path(OUT, ARM))
pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), median_mark)]
outc <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal, merged)]
d <- merge(pred, outc, by = c("race_id", "athlete_id"))[merged == FALSE]
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
d <- d[meet_tier == "T1_elite"]
d[, ability := orientation * log(median_mark)]
d <- d[is.finite(ability)]
d[, is_lose := if (TARGET == "javelinW") event_id == "AT-JavelinThrow-W" else family == "throw"]
say("target population (all dates): %s races", format(uniqueN(d[is_lose == TRUE]$race_id), big.mark=","))
d[, sigma_use := sigma_within]  # SIGMA_SCALE = 1, no-op, deliberately

fit <- d[date >= FIT_LO & date < FIT_HOLDOUT]
tst <- d[date >= FIT_HOLDOUT]
stopifnot("A2: fit and test races overlap" =
            length(intersect(unique(fit$race_id), unique(tst$race_id))) == 0)
say("fit: %s races (%s target) | test: %s races (%s target)",
    format(uniqueN(fit$race_id), big.mark=","), format(uniqueN(fit[is_lose==TRUE]$race_id), big.mark=","),
    format(uniqueN(tst$race_id), big.mark=","), format(uniqueN(tst[is_lose==TRUE]$race_id), big.mark=","))
stopifnot("target population too thin to fit a lambda" = uniqueN(fit[is_lose==TRUE]$race_id) >= 30)

chk <- copy(fit)
chk[, ab2 := mean(ability) + 0.8 * (ability - mean(ability)), by = race_id]
ordr <- chk[, .(rho = if (.N > 2 && uniqueN(ability) > 1)
                  suppressWarnings(cor(ability, ab2, method = "spearman")) else 1), by = race_id]
stopifnot("A3: compression changed the within-race order" =
            all(abs(ordr$rho - 1) < 1e-9, na.rm = TRUE))

sim_medal <- function(dd, lam_col, nsim) {
  dd <- copy(dd)
  dd[, ab_s := mean(ability) + (1 - get(lam_col)) * (ability - mean(ability)), by = race_id]
  rbindlist(lapply(split(dd, dd$race_id), function(r) {
    ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                     ability = r$ab_s, sigma = r$sigma_use)
    mp <- medal_probs(simulate_event(ab, n_sims = nsim, calibration = NULL,
                                     condition_sd = 0, seed = 11L))
    data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
               p_gold = mp$p_gold, p_medal = mp$p_medal)
  }))
}
brier_of <- function(sim, truth, pcol, ycol) {
  z <- merge(sim, truth[, .(race_id, athlete_id, y = get(ycol))], by = c("race_id", "athlete_id"))
  mean(z[, .(v = mean((get(pcol) - y)^2)), by = race_id]$v)
}

cat("\n==== lambda sweep, TARGET CELLS ONLY, fit window (medal Brier) ====\n")
fit_lose <- fit[is_lose == TRUE]
sweep_l <- rbindlist(lapply(LAMBDAS, function(l) {
  f2 <- copy(fit_lose)[, lam := l]
  s <- sim_medal(f2, "lam", NSIM_FIT)
  data.table(lambda = l, medal = round(brier_of(s, fit_lose, "p_medal", "hit_medal"), 5))
}))
print(sweep_l, row.names = FALSE)
best_lose <- sweep_l[which.min(medal)]$lambda
say("best lambda for the target cells on fit window: %.2f", best_lose)
if (best_lose == 0) say("NOTE: best lambda is 0 on the fit window -- compression does not help here either.")

hist <- deployed_history(OUT, events = unique(tst$event_id), from = min(tst$date) - 3650, to = max(tst$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- tst[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last", .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
tst <- merge(tst, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
tst_t <- tst[is_lose == TRUE]
say("\ntest window TARGET population with a last-5 baseline: %s races",
    format(uniqueN(tst_t$race_id), big.mark=","))
stopifnot("test target population too thin to score" = uniqueN(tst_t$race_id) >= 15)

tst_t[, lam0 := 0]; tst_t[, lam_b := best_lose]
S0 <- sim_medal(tst_t, "lam0", NSIM_TEST)
SB <- sim_medal(tst_t, "lam_b", NSIM_TEST)
tstL <- copy(tst_t)[, ability := l5][, lam0 := 0]
SL <- sim_medal(tstL, "lam0", NSIM_TEST)

report <- function(sim, label) {
  z <- merge(sim, tst_t[, .(race_id, athlete_id, hit_medal)], by = c("race_id","athlete_id"))
  zl <- merge(SL, tst_t[, .(race_id, athlete_id, hit_medal)], by = c("race_id","athlete_id"))
  a <- z[, .(v = mean((p_medal - hit_medal)^2)), by = race_id]
  bq <- zl[, .(v = mean((p_medal - hit_medal)^2)), by = race_id]
  mm <- merge(a, bq, by = "race_id")
  tt <- t.test(mm$v.x, mm$v.y, paired = TRUE)
  say("%-30s medal Brier %.5f | vs last-5 %+.2f%% p=%.3g", label, mean(mm$v.x),
      100*(mean(mm$v.x)-mean(mm$v.y))/mean(mm$v.y), tt$p.value)
}
cat("\n==== TEST WINDOW, target cells only, vs last-5 ====\n")
report(S0, "status quo (lambda=0)")
report(SB, sprintf("compressed (lambda=%.2f)", best_lose))

# direct paired comparison: did compression help vs status quo, on IDENTICAL rows
z0 <- merge(S0, tst_t[, .(race_id, athlete_id, hit_medal)], by=c("race_id","athlete_id"))
zb <- merge(SB, tst_t[, .(race_id, athlete_id, hit_medal)], by=c("race_id","athlete_id"))
a0 <- z0[, .(v=mean((p_medal-hit_medal)^2)), by=race_id]
ab_ <- zb[, .(v=mean((p_medal-hit_medal)^2)), by=race_id]
mm2 <- merge(a0, ab_, by="race_id")
tt2 <- t.test(mm2$v.y, mm2$v.x, paired = TRUE)
cat("\n==== VERDICT: compressed vs status quo, target cells, test window ====\n")
say("delta %+.2f%% p=%.3g (negative = compression HELPS)",
    100*(mean(mm2$v.y)-mean(mm2$v.x))/mean(mm2$v.x), tt2$p.value)
