# What is a finals-conditional sigma WORTH on medal Brier? Size it before
# building it.
#
# check_spread_vs_realised.R measured the simulator running ~17% wider than
# reality within T1 finals (pooled ratio 0.830; Discus 0.641, 800m 0.650, Shot
# Put 0.664, Pole Vault 0.666). `sigma_within` is fitted across an athlete's
# whole history; a T1 final is a peak-focus performance and more consistent
# than that implies. Narrowing the simulated spread is the only lever found so
# far that CAN move Brier -- every level correction is provably inert on it,
# because a uniform per-race shift to `ability` cannot change who beats whom.
#
# OUT-OF-SAMPLE BY CONSTRUCTION. The scale factor is fit on races strictly
# BEFORE FIT_HOLDOUT and applied only to races on/after it. Fitting the scale
# on the races it is then scored on would make it look good no matter what.
#
# WHY NOT ANCHOR TO THE ARM'S OWN 0.17118. A post-hoc re-simulation cannot
# reproduce the deployed number exactly: per-athlete sensitivity `s_i` is not
# carried in the predictions table, and `ability` has to be recovered from
# `median_mark`. So instead of comparing against a number produced by a
# DIFFERENT code path, all three arms below run through ONE path and differ
# only in the sigma they are handed:
#   A  sigma as-is           (scale 1.0)   -- the status quo, re-simulated
#   B  sigma x fitted scale  (narrowed)    -- the candidate
#   L  last-5 ability                       -- the baseline to beat
# A vs L reproduces the known deficit; B vs L is the question; A vs B isolates
# the sigma change with everything else held identical.
#
# ANCHORS, written before looking at output:
#   A1 A's medal Brier must land near the deployed arm's 0.17118. Not equal --
#      see above -- but a wild miss means the ability reconstruction is wrong
#      and nothing below is readable. Reported, not assumed.
#   A2 A vs L must reproduce the known sign: model WORSE on medal Brier.
#   A3 fit and test race sets must be disjoint. Asserted.
#   A4 the fitted scale must be < 1 (narrowing). If it comes back >= 1 the
#      premise is wrong and the script says so rather than proceeding.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_SIGMA_HOLDOUT", "2025-01-01"))
FIT_LO      <- as.Date(Sys.getenv("CITIUS_SIGMA_FIT_LO", "2023-01-01"))
ARM <- Sys.getenv("CITIUS_SIGMA_ARM", "backtest_tierctrl.rds")
NSIM <- 4000L
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | fit window [%s, %s) | test >= %s | n_sims %d",
    ARM, format(FIT_LO), format(FIT_HOLDOUT), format(FIT_HOLDOUT), NSIM)

b <- readRDS(file.path(OUT, ARM))
pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                         median_mark, p_medal, p_gold)]
outc <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal, merged)]
d <- merge(pred, outc, by = c("race_id", "athlete_id"))[merged == FALSE]

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date,
                   competition_id)], by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite"]

evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE,
                                                       .(event_id, sigma_within)]
d <- merge(d, evs, by = "event_id")
# `ability` is not carried in the predictions table; recover it from the median
# of the simulated mark distribution, which is what median_mark is.
d[, ability := orientation * log(median_mark)]
d[, resid_perf := orientation * log(actual) - ability]
d <- d[is.finite(resid_perf) & is.finite(ability)]

fit <- d[date >= FIT_LO & date < FIT_HOLDOUT]
tst <- d[date >= FIT_HOLDOUT]
stopifnot("fit window is empty" = nrow(fit) > 0, "test window is empty" = nrow(tst) > 0)
stopifnot("A3: fit and test races overlap" =
            length(intersect(unique(fit$race_id), unique(tst$race_id))) == 0)
say("fit: %s rows / %s races | test: %s rows / %s races",
    format(nrow(fit), big.mark=","), format(uniqueN(fit$race_id), big.mark=","),
    format(nrow(tst), big.mark=","), format(uniqueN(tst$race_id), big.mark=","))

# ---- fit the scale on the FIT window only ---------------------------------
# Per-race realised within-race sd against the sigma the simulator used, df
# corrected, then averaged per family. Family rather than event: the event-level
# cell is thin over a two-year window, and the measured pattern was a family
# one. Falls back to the global scale for a family absent from the fit.
rsd <- fit[, .(n = .N, sd_r = sd(resid_perf), sigma = sigma_within[1],
               family = family[1]), by = race_id][n >= 4 & is.finite(sd_r)]
rsd[, sd_r := sd_r * sqrt(n / pmax(n - 1, 1))]
rsd[, ratio := sd_r / sigma]
global_scale <- rsd[, mean(ratio)]
fam_scale <- rsd[, .(races = .N, scale = mean(ratio)), by = family][races >= 8L]
say("\nfitted GLOBAL scale: %.3f (from %d fit races)", global_scale, nrow(rsd))
print(fam_scale[order(scale)], row.names = FALSE)
if (global_scale >= 1) {
  say("\nA4 FAILED: fitted scale is >= 1, so the premise (simulator too wide) does")
  say("not hold on the fit window. Stopping rather than 'widening' on a whim.")
  quit(save = "no", status = 0)
}
scale_map <- setNames(fam_scale$scale, fam_scale$family)

# ---- better estimators of the same scale ----------------------------------
# The raw family mean is a poor estimator where a cell is thin or unstable:
# hurdles fits 0.714 on this window but measured 1.051 on the test window, so
# applying the raw value over-narrows it. Two alternatives, both fit on the
# SAME fit window, so the comparison is estimator-vs-estimator and nothing else:
#   pooled  two-level empirical-Bayes, event -> family -> global, same shrinkage
#           machinery as fit_family_pool_offsets.R
#   global  one constant for everything -- the simplest thing that could work,
#           and the honest control: if it matches the fancier estimators, the
#           per-event table is fitting noise and should not be built.
rsd_ev <- merge(rsd, unique(fit[, .(race_id, event_id)]), by = "race_id")
eb_shrink <- function(dd, group_col, parent_vals) {
  gs <- dd[, .(n = .N, mean_g = mean(ratio), var_g = var(ratio)), by = c(group_col)]
  gs[, se2 := ifelse(n > 1, var_g / n, NA_real_)]
  tau2 <- max(0, var(gs$mean_g, na.rm = TRUE) - mean(gs$se2, na.rm = TRUE), na.rm = TRUE)
  gs[, se2f := fifelse(is.na(se2), tau2, se2)]
  gs[, w := fifelse(is.finite(tau2 / (tau2 + se2f)), tau2 / (tau2 + se2f), 0)]
  gs[, parent := parent_vals[match(get(group_col), names(parent_vals))]]
  gs[!is.finite(parent), parent := global_scale]
  gs[, shrunk := w * mean_g + (1 - w) * parent]
  gs[]
}
fam_eb <- eb_shrink(rsd_ev, "family", setNames(rep(global_scale, uniqueN(rsd_ev$family)),
                                               unique(rsd_ev$family)))
fam_eb_map <- setNames(fam_eb$shrunk, fam_eb$family)
ev_parent <- unique(rsd_ev[, .(event_id, family)])
ev_parent_vals <- setNames(fam_eb_map[ev_parent$family], ev_parent$event_id)
ev_eb <- eb_shrink(rsd_ev, "event_id", ev_parent_vals)
ev_eb_map <- setNames(ev_eb$shrunk, ev_eb$event_id)
cat("\nEB-pooled family scales (shrunk toward global):\n")
print(fam_eb[order(shrunk), .(family, races = n, raw = round(mean_g, 3),
                              w = round(w, 2), shrunk = round(shrunk, 3))], row.names = FALSE)
say("EB event-level: %d events, shrinkage weight median %.2f (lower = more pooling)",
    nrow(ev_eb), median(ev_eb$w))
# Anchor: event-level must shrink at least as hard as family-level, since an
# event cell holds less data than its family. If it does not, the estimator is
# inverted -- the exact failure that exposed the T1-only fit problem before.
if (median(ev_eb$w) > median(fam_eb$w))
  say("WARNING: event level shrinks LESS than family level -- estimator suspect.")

# ---- last-5 baseline ability, same construction as check_brier_cuts_sweep --
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
say("test rows with a last-5 baseline: %s / %s races",
    format(nrow(tst), big.mark=","), format(uniqueN(tst$race_id), big.mark=","))

# ---- three arms, ONE code path --------------------------------------------
tst[, sigma_narrow := sigma_within * fifelse(family %chin% names(scale_map),
                                             scale_map[family], global_scale)]
sim_one <- function(r, ab_col, sg_col) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                   ability = r[[ab_col]], sigma = r[[sg_col]])
  mp <- medal_probs(simulate_event(ab, n_sims = NSIM, calibration = NULL,
                                   condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
             p_gold = mp$p_gold, p_medal = mp$p_medal)
}
tst[, sigma_pooled := sigma_within * fifelse(event_id %chin% names(ev_eb_map),
                                             ev_eb_map[event_id], global_scale)]
tst[, sigma_global := sigma_within * global_scale]
say("\nsimulating five arms over %s test races...", format(uniqueN(tst$race_id), big.mark=","))
parts <- split(tst, tst$race_id)
A <- rbindlist(lapply(parts, sim_one, ab_col = "ability", sg_col = "sigma_within"))
B <- rbindlist(lapply(parts, sim_one, ab_col = "ability", sg_col = "sigma_narrow"))
C <- rbindlist(lapply(parts, sim_one, ab_col = "ability", sg_col = "sigma_pooled"))
D <- rbindlist(lapply(parts, sim_one, ab_col = "ability", sg_col = "sigma_global"))
L <- rbindlist(lapply(parts, sim_one, ab_col = "l5",      sg_col = "sigma_within"))
setnames(A, c("p_gold","p_medal"), c("A_gold","A_medal"))
setnames(B, c("p_gold","p_medal"), c("B_gold","B_medal"))
setnames(C, c("p_gold","p_medal"), c("C_gold","C_medal"))
setnames(D, c("p_gold","p_medal"), c("D_gold","D_medal"))
setnames(L, c("p_gold","p_medal"), c("L_gold","L_medal"))
z <- Reduce(function(x, y) merge(x, y, by = c("race_id","athlete_id")), list(tst, A, B, C, D, L))
stopifnot("arms A and B are identical -- the sigma scale did not take effect" =
            mean(abs(z$A_medal - z$B_medal) > 1e-9) > 0.5)

byrace <- function(dd, pcol, y) dd[, .(v = mean((get(pcol) - get(y))^2)), by = race_id]
rep_pair <- function(p1, p2, y, lab) {
  r1 <- byrace(z, p1, y); r2 <- byrace(z, p2, y)
  mm <- merge(r1, r2, by = "race_id")
  tt <- t.test(mm$v.x, mm$v.y, paired = TRUE)
  data.table(comparison = lab, races = nrow(mm),
             brier_1 = round(mean(mm$v.x), 5), brier_2 = round(mean(mm$v.y), 5),
             rel_pct = round(100 * (mean(mm$v.x) - mean(mm$v.y)) / mean(mm$v.y), 2),
             p = signif(tt$p.value, 3))
}
cat("\n==== ANCHOR A1: does the re-simulated status quo land near the arm? ====\n")
say("re-simulated A medal Brier %.5f | deployed arm reference 0.17118",
    mean(byrace(z, "A_medal", "hit_medal")$v))
say("re-simulated A gold  Brier %.5f | deployed arm reference 0.07598",
    mean(byrace(z, "A_gold", "hit")$v))

cat("\n==== MEDAL BRIER (positive rel = first arm WORSE) ====\n")
res_m <- rbindlist(list(
  rep_pair("A_medal", "L_medal", "hit_medal", "A status quo  vs L last-5 (A2: expect WORSE)"),
  rep_pair("B_medal", "L_medal", "hit_medal", "B raw family  vs L last-5"),
  rep_pair("C_medal", "L_medal", "hit_medal", "C EB pooled   vs L last-5"),
  rep_pair("D_medal", "L_medal", "hit_medal", "D global only vs L last-5"),
  rep_pair("C_medal", "A_medal", "hit_medal", "C EB pooled   vs A status quo (isolates sigma)"),
  rep_pair("D_medal", "C_medal", "hit_medal", "D global      vs C EB pooled (is the table worth it?)")))
print(res_m, row.names = FALSE)

cat("\n==== GOLD BRIER (must not regress) ====\n")
res_g <- rbindlist(list(
  rep_pair("A_gold", "L_gold", "hit", "A status quo  vs L last-5"),
  rep_pair("B_gold", "L_gold", "hit", "B raw family  vs L last-5"),
  rep_pair("C_gold", "L_gold", "hit", "C EB pooled   vs L last-5"),
  rep_pair("D_gold", "L_gold", "hit", "D global only vs L last-5"),
  rep_pair("C_gold", "A_gold", "hit", "C EB pooled   vs A status quo")))
print(res_g, row.names = FALSE)

cat("\n==== VERDICT ====\n")
best <- res_m[comparison %like% "vs L last-5"][which.min(rel_pct)]
say("best estimator on medal Brier vs last-5: %s -> %+.2f%% (p=%.3g)",
    trimws(sub(" vs L last-5.*", "", best$comparison)), best$rel_pct, best$p)
say(if (best$rel_pct < 0) "NEGATIVE = the goal's medal-Brier half is MET by this change."
    else "STILL POSITIVE = narrowing sigma alone does not close the medal-Brier gap.")
gap_closed <- {
  a <- res_m[comparison %like% "^A status quo"]$rel_pct
  100 * (a - best$rel_pct) / a
}
say("closes %.0f%% of the status quo's %+.2f%% medal-Brier gap.", gap_closed,
    res_m[comparison %like% "^A status quo"]$rel_pct)
saveRDS(list(global_scale = global_scale, fam_scale = fam_scale,
             fam_eb = fam_eb, ev_eb = ev_eb,
             medal = res_m, gold = res_g), file.path(OUT, "finals_sigma_gain.rds"))
