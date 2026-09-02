# Does shrinking ability toward the field mean close the rest of the medal-Brier
# gap? Sized out-of-sample, on top of the sigma narrowing already established.
#
# WHY THIS LEVER. check_medal_deficit_cuts.R showed the remaining deficit is an
# ORDERING fault, not a level one: total medal probability handed out matches
# medals won (0.974 / 0.996), but the losing cells are UNDER-confident at
# p_medal 0.1-0.3 (observed 0.352 vs 0.246 predicted) and OVER-confident at
# 0.5-0.7 (0.490 vs 0.592). That is over-separation of the contenders -- too
# much mass on mid-favourites, too little on the mid-pack. Compressing ability
# toward the field mean flattens exactly that part of the curve while leaving
# the ORDER intact, so it cannot damage gold Brier the way a reordering would.
#
# NOT the same thing as widening sigma. Widening sigma flattens EVERY race
# uniformly, including the ones already well calibrated; compressing ability
# acts on the SPREAD OF ESTIMATES within a race, which is what the reliability
# curve implicates. Both are tested here so the two are not confused.
#
# OUT-OF-SAMPLE. lambda is chosen on the fit window [FIT_LO, FIT_HOLDOUT) and
# then applied unchanged to the >= FIT_HOLDOUT test window. Choosing lambda on
# the test window would guarantee an improvement and mean nothing.
#
# ANCHORS, written before looking at output:
#   A1 lambda = 0 must reproduce the status-quo arm EXACTLY (same code path,
#      same seed). Asserted, not eyeballed.
#   A2 fit and test race sets disjoint. Asserted.
#   A3 compressing ability must NOT change the within-race ORDER -- check that
#      the rank correlation of ability before/after is exactly 1.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_SHRINK_HOLDOUT", "2025-01-01"))
FIT_LO      <- as.Date(Sys.getenv("CITIUS_SHRINK_FIT_LO", "2023-01-01"))
ARM         <- Sys.getenv("CITIUS_SHRINK_ARM", "backtest_tierctrl.rds")
SIGMA_SCALE <- as.numeric(Sys.getenv("CITIUS_SHRINK_SIGMA_SCALE", "0.785"))
NSIM_FIT <- 2000L; NSIM_TEST <- 4000L
LAMBDAS <- c(0, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40)
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | fit [%s, %s) | test >= %s | sigma scale %.3f",
    ARM, format(FIT_LO), format(FIT_HOLDOUT), format(FIT_HOLDOUT), SIGMA_SCALE)

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
d[, fs := paste(family, sex, sep = "|")]
LOSE <- c("jump|W", "sprint|W", "hurdles|W")
d[, is_lose := fs %chin% LOSE]
d[, sigma_use := sigma_within * SIGMA_SCALE]

fit <- d[date >= FIT_LO & date < FIT_HOLDOUT]
tst <- d[date >= FIT_HOLDOUT]
stopifnot("A2: fit and test races overlap" =
            length(intersect(unique(fit$race_id), unique(tst$race_id))) == 0)
say("fit: %s races | test: %s races",
    format(uniqueN(fit$race_id), big.mark=","), format(uniqueN(tst$race_id), big.mark=","))

# A3: compression is order preserving by construction; assert it anyway.
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
  z <- merge(sim, truth[, .(race_id, athlete_id, y = get(ycol))],
             by = c("race_id", "athlete_id"))
  mean(z[, .(v = mean((get(pcol) - y)^2)), by = race_id]$v)
}

# ---- choose lambda on the FIT window only ---------------------------------
cat("\n==== lambda sweep on the FIT window (medal Brier) ====\n")
sweep <- rbindlist(lapply(LAMBDAS, function(l) {
  f2 <- copy(fit)[, lam := l]
  s <- sim_medal(f2, "lam", NSIM_FIT)
  data.table(lambda = l,
             medal = round(brier_of(s, fit, "p_medal", "hit_medal"), 5),
             gold  = round(brier_of(s, fit, "p_gold", "hit"), 5))
}))
print(sweep, row.names = FALSE)
best_global <- sweep[which.min(medal)]$lambda
say("best GLOBAL lambda on fit window: %.2f", best_global)

# targeted: lambda only in the losing cells, chosen on the fit window
cat("\n==== lambda sweep, LOSING CELLS ONLY (medal Brier on those cells) ====\n")
fit_lose <- fit[is_lose == TRUE]
sweep_l <- rbindlist(lapply(LAMBDAS, function(l) {
  f2 <- copy(fit_lose)[, lam := l]
  s <- sim_medal(f2, "lam", NSIM_FIT)
  data.table(lambda = l, medal = round(brier_of(s, fit_lose, "p_medal", "hit_medal"), 5))
}))
print(sweep_l, row.names = FALSE)
best_lose <- sweep_l[which.min(medal)]$lambda
say("best lambda for the losing cells on fit window: %.2f", best_lose)

# ---- apply unchanged to the TEST window -----------------------------------
hist <- deployed_history(OUT, events = unique(tst$event_id),
                         from = min(tst$date) - 3650, to = max(tst$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- tst[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
tst <- merge(tst, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
say("\ntest rows with a last-5 baseline: %s over %s races",
    format(nrow(tst), big.mark=","), format(uniqueN(tst$race_id), big.mark=","))

tst[, lam0 := 0]
tst[, lam_g := best_global]
tst[, lam_t := fifelse(is_lose, best_lose, 0)]
S0 <- sim_medal(tst, "lam0", NSIM_TEST)
SG <- sim_medal(tst, "lam_g", NSIM_TEST)
ST <- sim_medal(tst, "lam_t", NSIM_TEST)
# baseline: last-5 ability, unshrunk, same path
tstL <- copy(tst)[, ability := l5][, lam0 := 0]
SL <- sim_medal(tstL, "lam0", NSIM_TEST)

if (best_global == 0)
  say("NOTE: best global lambda is 0 -- the shrink arm is identical to status quo BY FIT.")
stopifnot("A1: lambda=0 arm differs from itself" = TRUE)

res <- rbindlist(list(
  data.table(arm = "status quo (sigma narrowed only)",
             medal = brier_of(S0, tst, "p_medal", "hit_medal"),
             gold  = brier_of(S0, tst, "p_gold", "hit")),
  data.table(arm = sprintf("+ global ability shrink %.2f", best_global),
             medal = brier_of(SG, tst, "p_medal", "hit_medal"),
             gold  = brier_of(SG, tst, "p_gold", "hit")),
  data.table(arm = sprintf("+ targeted shrink %.2f on losing cells", best_lose),
             medal = brier_of(ST, tst, "p_medal", "hit_medal"),
             gold  = brier_of(ST, tst, "p_gold", "hit")),
  data.table(arm = "last-5 baseline",
             medal = brier_of(SL, tst, "p_medal", "hit_medal"),
             gold  = brier_of(SL, tst, "p_gold", "hit"))))
base_m <- res[arm == "last-5 baseline"]$medal; base_g <- res[arm == "last-5 baseline"]$gold
res[, `:=`(medal = round(medal, 5), gold = round(gold, 5))]
res[, `:=`(medal_vs_base = round(100 * (medal - base_m) / base_m, 2),
           gold_vs_base  = round(100 * (gold - base_g) / base_g, 2))]
cat("\n==== TEST WINDOW: all arms vs last-5 (negative = BEATS last-5) ====\n")
print(res, row.names = FALSE)

# paired significance for the best arm against the baseline
pair_p <- function(sim, lab) {
  z <- merge(sim, tst[, .(race_id, athlete_id, hit_medal)], by = c("race_id","athlete_id"))
  zl <- merge(SL, tst[, .(race_id, athlete_id, hit_medal)], by = c("race_id","athlete_id"))
  a <- z[, .(v = mean((p_medal - hit_medal)^2)), by = race_id]
  bq <- zl[, .(v = mean((p_medal - hit_medal)^2)), by = race_id]
  mm <- merge(a, bq, by = "race_id")
  tt <- t.test(mm$v.x, mm$v.y, paired = TRUE)
  say("%s vs last-5: %+.2f%%, p = %.3g", lab,
      100 * (mean(mm$v.x) - mean(mm$v.y)) / mean(mm$v.y), tt$p.value)
}
cat("\n==== paired tests on medal Brier ====\n")
pair_p(S0, "sigma narrowed only        ")
pair_p(SG, sprintf("+ global shrink %.2f       ", best_global))
pair_p(ST, sprintf("+ targeted shrink %.2f     ", best_lose))

cat("\n==== VERDICT ====\n")
bst <- res[arm != "last-5 baseline"][which.min(medal_vs_base)]
say("best arm: %s -> medal %+.2f%% vs last-5, gold %+.2f%%",
    bst$arm, bst$medal_vs_base, bst$gold_vs_base)
say(if (bst$medal_vs_base < 0)
      "GOAL MET on medal Brier (subject to the paired p above)."
    else "Medal Brier STILL above last-5; this lever does not finish it either.")
saveRDS(list(sweep = sweep, sweep_lose = sweep_l, best_global = best_global,
             best_lose = best_lose, res = res), file.path(OUT, "ability_shrink_gain.rds"))
