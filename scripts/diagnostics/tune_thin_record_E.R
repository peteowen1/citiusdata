# Version E (Pete, 2026-11-09): "shrink twice" towards a REPLACEMENT LEVEL FOR AGE,
# with the decay speed tuned on a target that scores NO-SHOWS. Changes nothing published.
#
#   rating = a*R + c*L + b*F (+ one level shift c0), with
#   a = wR / (wR + mR*kR)                  recent level R, half-life = event's x hr
#   c = (1-a) * wL / (wL + m*kL)           own long record L, half-life H days
#   b = (1-a) * m*kL / (wL + m*kL)         floor F
#   wR, wL = total result weight of the recent / long refit; kR, kL = their fitted
#   kappa (race scatter^2 / between-athlete spread^2). a, b, c in [0,1], sum to 1.
# TUNED (Pete: "optimise weights of a b and c and decays of a and b"):
#   hr (recent half-life multiplier) {0.5,1,2}, mR (kappa_R multiplier) {0.5,1,2},
#   H (long half-life, days) {180,365,540,730}, m (kappa_L multiplier) {1,2,4,8,16}.
#   R comes from the raw refit (no neighbour_combine), so E at hr = mR = 1, m -> 0 is
#   close to but not identical with deployed A; A is reported from deployed_ability().
#   floor_age = median, over athletes aged age +-1 at the cut, of each athlete's MEAN
#         perf in the 12 months BEFORE the cut (no future data). Pete chose the median
#         same-age athlete (2026-11-09) over a top-10% level.
# Target (per rated athlete-event at a cut): MEAN perf over the next 12 months; an athlete
#   who did not race is scored at the median same-age (+-1) athlete who DID race in those
#   12 months. Why: the next-race target only ever scored comebacks, so any tuned decay
#   came back "none" (tune_thin_record_blend*.R).
# Tuned: H in {180,365,540,730} x m in {1,2,4,8,16}, on the 2024 cuts; verdict 2025 cuts.
# Every arm gets its own level shift c0 (median target-minus-prediction on 2024), so no
# arm is credited with the known level bias. a, b, c stay in [0, 1] by construction.
# Rows dropped (counted): no-shows with no age, or with < 20 same-age racers.
# Per-cut fits cached in data/flags/tune_E_cache/. Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "flags", "tune_E_cache"); dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
CUTS <- seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "3 months")
if (identical(Sys.getenv("SMOKE"), "1")) CUTS <- as.Date(c("2024-01-01", "2025-01-01"))
H_GRID <- c(180, 365, 540, 730); M_GRID <- c(1, 2, 4, 8, 16); HR_GRID <- c(0.5, 1, 2); MR_GRID <- c(0.5, 1, 2)
MIN_AGE_N <- 20L

co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
cat("rows", nrow(h), "athletes", uniqueN(h$athlete_id), "dates", format(range(h$date)), "\n")
cal <- deployed_calibration(OUT); ep <- deployed_event_params()

age_med <- function(tab, val) {   # median of `val` over same event, integer age +-1
  g <- unique(tab[, .(event_id, a0)])
  g[, {
    x <- tab[event_id == .BY$event_id & abs(a0 - .BY$a0) <= 1][[val]]
    .(n_age = length(x), med = if (length(x) >= MIN_AGE_N) median(x) else NA_real_)
  }, by = .(event_id, a0)]
}

res <- list()
for (i in seq_along(CUTS)) {
  cut <- CUTS[i]; f <- file.path(CACHE, sprintf("cut_%s.rds", format(cut)))
  if (file.exists(f)) { res[[i]] <- readRDS(f); cat("cut", format(cut), "cached\n"); next }
  t1 <- Sys.time(); past <- h[date < cut]
  a <- deployed_ability(past, as_of = cut - 1, calibration = cal, event_params = ep)
  a[, athlete_id := as.character(athlete_id)]
  d <- a[as.numeric(cut - last_date) <= 730,
         .(athlete_id, event_id, ability, raw = ability_raw, s = shrinkage, prior_mu, last_date)]
  for (hr in HR_GRID) {
    epr <- copy(ep)[event_id %in% EV, half_life := half_life * hr]
    l <- .deployed_ability_raw(past, cut - 1, cal, epr); l[, athlete_id := as.character(athlete_id)]
    l <- l[, .(athlete_id, event_id, rr = ability_raw, wr = w_total, kr = shrinkage * w_total / (1 - shrinkage))]
    setnames(l, c("rr", "wr", "kr"), paste0(c("R_", "wR_", "kR_"), hr))
    d <- merge(d, l, by = c("athlete_id", "event_id"), all.x = TRUE)
  }
  for (H in H_GRID) {
    eph <- copy(ep)[event_id %in% EV, half_life := H]
    l <- .deployed_ability_raw(past, cut - 1, cal, eph); l[, athlete_id := as.character(athlete_id)]
    l <- l[, .(athlete_id, event_id, lr = ability_raw, wl = w_total, kl = shrinkage * w_total / (1 - shrinkage))]
    setnames(l, c("lr", "wl", "kl"), paste0(c("long_", "wL_", "kL_"), H))
    d <- merge(d, l, by = c("athlete_id", "event_id"), all.x = TRUE)
  }
  ag <- past[!is.na(age)][order(date)][, .(age_cut = age[.N] + as.numeric(cut - date[.N]) / 365.25), by = athlete_id]
  ag <- ag[, .(age_cut = max(age_cut)), by = athlete_id]
  d <- ag[d, on = "athlete_id"][, a0 := floor(age_cut)]
  # floor: past 12 months, per athlete mean, median over same-age athletes
  pm <- past[date >= cut - 365, .(m = mean(perf)), by = .(athlete_id, event_id)]
  pm <- ag[pm, on = "athlete_id"][!is.na(age_cut)][, a0 := floor(age_cut)]
  fl <- age_med(pm, "m"); setnames(fl, c("n_age", "med"), c("n_age_floor", "floor_age"))
  d <- fl[d, on = c("event_id", "a0")]
  # target: next 12 months mean; no-shows at the median same-age racer
  nx <- h[date >= cut & date < cut + 365, .(next_mean = mean(perf), next_n = .N), by = .(athlete_id, event_id)]
  d <- nx[d, on = c("athlete_id", "event_id")][is.na(next_n), next_n := 0L]
  rp <- age_med(d[next_n > 0 & !is.na(a0), .(event_id, a0, next_mean)], "next_mean")
  setnames(rp, c("n_age", "med"), c("n_age_rep", "rep_age"))
  d <- rp[d, on = c("event_id", "a0")]
  d[, target := fifelse(next_n > 0, next_mean, rep_age)]
  d[, cut := cut]
  saveRDS(d, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f)
  res[[i]] <- d
  cat(sprintf("cut %s: rated %d, raced next 12m %d (%.1f%%), %.1f min\n", format(cut), nrow(d),
              d[next_n > 0, .N], 100 * d[, mean(next_n > 0)], as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  rm(past, a, d, nx, pm); invisible(gc())
}
r <- rbindlist(res, fill = TRUE)
n0 <- nrow(r)
r <- r[!is.na(target)]
cat(sprintf("rows %d; dropped %d with no target (no-show without age or < %d same-age racers)\n", n0, n0 - nrow(r), MIN_AGE_N))
r[is.na(floor_age), floor_age := prior_mu]   # no age / thin age group: the event mean, counted
cat(sprintf("floor fell back to the event mean on %d rows (%.1f%%)\n", r[is.na(a0) | is.na(n_age_floor) | n_age_floor < MIN_AGE_N, .N],
            100 * r[, mean(is.na(a0) | is.na(n_age_floor) | n_age_floor < MIN_AGE_N)]))
for (v in c(paste0("long_", H_GRID), paste0("R_", HR_GRID))) stopifnot(r[, mean(!is.na(get(v)))] > 0.99)
r <- r[complete.cases(r[, c(paste0("long_", H_GRID), paste0("R_", HR_GRID)), with = FALSE])]
r[, fit_set := year(cut) == 2024]
cat(sprintf("fit 2024 %d rows (no-shows %.1f%%); verdict 2025 %d rows (no-shows %.1f%%)\n",
            r[fit_set == TRUE, .N], 100 * r[fit_set == TRUE, mean(next_n == 0)],
            r[fit_set == FALSE, .N], 100 * r[fit_set == FALSE, mean(next_n == 0)]))

predE <- function(x, hr, mR, H, m) {
  wr <- x[[paste0("wR_", hr)]]; kr <- mR * x[[paste0("kR_", hr)]]; a <- wr / (wr + kr)
  wl <- x[[paste0("wL_", H)]]; kl <- m * x[[paste0("kL_", H)]]; sl <- kl / (wl + kl)
  a * x[[paste0("R_", hr)]] + (1 - a) * ((1 - sl) * x[[paste0("long_", H)]] + sl * x$floor_age)
}
mae <- function(p, y) 100 * mean(abs(y - p))
r[, base_A := ability]
r[, base_C365 := (1 - s) * raw + s * long_365]      # own-record pull, no floor (version-C-like, H 365)
grid <- CJ(hr = HR_GRID, mR = MR_GRID, H = H_GRID, m = M_GRID)
fit <- r[fit_set == TRUE]
grid[, fit_mae := mapply(function(hr, mR, H, m) { p <- predE(fit, hr, mR, H, m); mae(p + median(fit$target - p), fit$target) },
                         hr, mR, H, m)]
cat("\nTUNING ON 2024 (marks MAE % vs the 12-month target incl. no-shows, level-corrected; lower is better).\n")
cat("Best 10 of", nrow(grid), "settings:\n"); print(grid[order(fit_mae)][1:10][, fit_mae := round(fit_mae, 4)])
best <- grid[which.min(fit_mae)]
cat("H x m at the best hr, mR:\n")
print(dcast(grid[hr == best$hr & mR == best$mR], H ~ m, value.var = "fit_mae")[, lapply(.SD, function(v) round(v, 4))])
cat(sprintf("best on 2024: hr %g, mR %g, H %d, m %g\n", best$hr, best$mR, best$H, best$m))
r[, base_E1 := predE(r, 1, 1, 365, 1)]
r[, base_Ebest := predE(r, best$hr, best$mR, best$H, best$m)]
fit <- r[fit_set == TRUE]
for (b in grep("^base_", names(r), value = TRUE)) {
  c0 <- median(fit$target - fit[[b]]); r[, (sub("^base_", "pred_", b)) := get(b) + c0]
  cat(sprintf("level shift %-9s %+.3f%%\n", sub("^base_", "", b), 100 * c0))
}
boot_gap <- function(x, p1, p2) {
  g <- x[, .(d = sum(abs(target - get(p1)) - abs(target - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
V <- r[fit_set == FALSE]
arms <- c("pred_A", "pred_C365", "pred_E1", "pred_Ebest")
cat("\nVERDICT, 2025 cuts. MAE % vs target (lower is better). gap = MAE(A) - MAE(arm), 95% CI by athlete; > 0 = beats deployed.\n")
print(rbindlist(lapply(arms, function(p) {
  ci <- if (p == "pred_A") c(NA, NA) else boot_gap(V, "pred_A", p)
  data.table(arm = sub("pred_", "", p), all = mae(V[[p]], V$target),
             raced = mae(V[next_n > 0][[p]], V[next_n > 0]$target),
             no_show = mae(V[next_n == 0][[p]], V[next_n == 0]$target), gap_lo = ci[1], gap_hi = ci[2])
}))[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat(sprintf("rows: all %d, raced %d, no-show %d\n", nrow(V), V[next_n > 0, .N], V[next_n == 0, .N]))
cat("\nEbest vs C365 (own-record pull, no floor), gap CI:\n"); print(round(boot_gap(V, "pred_C365", "pred_Ebest"), 3))
V[, gapd := as.numeric(cut - last_date)]
V[, away := cut(gapd, c(-Inf, 60, 180, 365, Inf), c("<60d", "60-180d", "180-365d", ">365d"))]
cat("\nBy time since last race at the cut, 2025, MAE %:\n")
print(V[, .(n = .N, no_show_pct = round(100 * mean(next_n == 0), 1), A = round(mae(pred_A, target), 3),
            C365 = round(mae(pred_C365, target), 3), E1 = round(mae(pred_E1, target), 3),
            Ebest = round(mae(pred_Ebest, target), 3)), by = away][order(away)])
fwrite(grid, file.path(OUT, "flags", "tune_thin_record_E_grid.csv"))
fwrite(best, file.path(OUT, "flags", "tune_thin_record_E_best.csv"))
cat("total time", format(Sys.time() - t0), "\n")
