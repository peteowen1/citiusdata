# Version E tuning on R1 RACES (Pete, 2026-11-09: tune on R1, per
# docs/reference/modelling-traps.md "R1 vs M1"). Reads the per-cut fits cached by
# tune_thin_record_E.R (data/flags/tune_E_cache/); ratings are still built from ALL
# results (ability needs the club races); only the population, floor and target are R1.
#
# R1 = race_code OW/GW/GL/DF/A (the measured top group). Per cut, no future data:
#   population: athletes with >= 1 R1 result in the event in the 730 days before the cut;
#   floor F:    median same-age (+-1 y, widened to +-3 if < MIN_N) athlete's MEAN R1 perf
#               in the 12 months before the cut (Pete's median-same-age pick, R1 population);
#   target:     mean R1 perf in the next 12 months; an athlete with no R1 race in that
#               window is scored at the median same-age R1 racer of that window.
# Formula, knobs, level shifts, 2024-fit / 2025-verdict split: as tune_thin_record_E.R.
# Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A"); MIN_N <- 20L
fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE)
cat(sprintf("cuts %d: %s\n", length(fs), paste(sort(unique(format(r$cut))), collapse = " ")))
if (length(fs) < 8) cat("!!! fewer than 8 cuts cached -- PRELIMINARY\n")
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf) & toupper(trimws(race_code)) %in% R1]
h[, athlete_id := as.character(athlete_id)]
cat("R1 results in the 6 events:", nrow(h), "\n")

med_age <- function(tab, val) {   # median by event x integer age, +-1 widened to +-3 when thin
  g <- unique(tab[, .(event_id, a0)])
  g[, {
    x <- tab[event_id == .BY$event_id & abs(a0 - .BY$a0) <= 1][[val]]
    if (length(x) < MIN_N) x <- tab[event_id == .BY$event_id & abs(a0 - .BY$a0) <= 3][[val]]
    .(n = length(x), med = if (length(x) >= MIN_N) median(x) else NA_real_)
  }, by = .(event_id, a0)]
}
out <- list()
for (cu in sort(unique(r$cut))) {
  cu <- as.Date(cu, origin = "1970-01-01"); d <- r[cut == cu]
  pop <- unique(h[date < cu & date >= cu - 730, .(athlete_id, event_id)])
  d <- d[pop, on = c("athlete_id", "event_id"), nomatch = NULL]
  ages <- unique(d[!is.na(a0), .(athlete_id, a0)])
  pm <- h[date < cu & date >= cu - 365, .(m = mean(perf)), by = .(athlete_id, event_id)]
  pm <- ages[pm, on = "athlete_id", nomatch = NULL]
  fl <- med_age(pm, "m"); setnames(fl, c("n", "med"), c("n_floor", "floor_r1"))
  # Event-level R1 floors (Pete 2026-11-09: "80th pctile of R1 marks as replacement"),
  # from R1 MARKS in the 12 months before the cut, no age: perf = -log(mark), so the
  # mark faster than 80% of R1 marks is the 0.8 quantile of perf.
  fq <- h[date < cu & date >= cu - 365, .(floor_fast = quantile(perf, 0.8), floor_mid = quantile(perf, 0.5),
                                          floor_slow = quantile(perf, 0.2)), by = event_id]
  d <- fq[d, on = "event_id"]
  nx <- h[date >= cu & date < cu + 365, .(t_mean = mean(perf), t_n = .N), by = .(athlete_id, event_id)]
  d <- nx[d, on = c("athlete_id", "event_id")][is.na(t_n), t_n := 0L]
  rp <- med_age(d[t_n > 0 & !is.na(a0), .(event_id, a0, t_mean)], "t_mean"); setnames(rp, c("n", "med"), c("n_rep", "rep_r1"))
  d <- fl[d, on = c("event_id", "a0")]; d <- rp[d, on = c("event_id", "a0")]
  d[, target_r1 := fifelse(t_n > 0, t_mean, rep_r1)]
  out[[length(out) + 1]] <- d
}
r <- rbindlist(out, fill = TRUE)
n0 <- nrow(r); r <- r[!is.na(target_r1)]
# RACED_ONLY=1: ABILITY question only -- keep athletes who DID race R1 in the window
# (Pete, 2026-11-09). Every no-show-scored version tuned availability instead and
# made racers worse; availability goes on the ratings page as its own status.
RACED_ONLY <- identical(Sys.getenv("RACED_ONLY"), "1")
if (RACED_ONLY) { r <- r[t_n > 0]; cat("RACED_ONLY: athletes with >= 1 R1 race in the next 12 months\n") }
cat(sprintf("R1 population rows %d; dropped %d (no-show with no age or thin age group)\n", n0, n0 - nrow(r)))
nf <- r[is.na(floor_r1), .N]; r[is.na(floor_r1), floor_r1 := prior_mu]
cat(sprintf("floor fell back to the event mean on %d rows (%.1f%%)\n", nf, 100 * nf / nrow(r)))
HR <- c(0.5, 1, 2, 3, 4); HR <- HR[paste0("R_", HR) %in% names(r)]; HL <- c(180, 365, 540, 730)
FL <- c("floor_r1", "floor_fast", "floor_mid", "floor_slow")
cat("recent half-life multipliers available:", paste(HR, collapse = ", "), "
")
r <- r[complete.cases(r[, c(paste0("long_", HL), paste0("R_", HR)), with = FALSE])]
r[, fit_set := year(cut) == 2024]
cat(sprintf("fit 2024 %d rows (no R1 race next 12m %.1f%%); verdict 2025 %d rows (%.1f%%)\n",
            r[fit_set == TRUE, .N], 100 * r[fit_set == TRUE, mean(t_n == 0)],
            r[fit_set == FALSE, .N], 100 * r[fit_set == FALSE, mean(t_n == 0)]))

predE <- function(x, hr, mR, H, m, fl = "floor_r1") {
  wr <- x[[paste0("wR_", hr)]]; kr <- mR * x[[paste0("kR_", hr)]]; a <- wr / (wr + kr)
  wl <- x[[paste0("wL_", H)]]; kl <- m * x[[paste0("kL_", H)]]; sl <- kl / (wl + kl)
  a * x[[paste0("R_", hr)]] + (1 - a) * ((1 - sl) * x[[paste0("long_", H)]] + sl * x[[fl]])
}
mae <- function(p, y) 100 * mean(abs(y - p))
fit <- r[fit_set == TRUE]
grid <- CJ(fl = FL, hr = HR, mR = c(0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8), H = HL, m = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))
grid[, fit_mae := mapply(function(fl, hr, mR, H, m) { p <- predE(fit, hr, mR, H, m, fl); mae(p + median(fit$target_r1 - p), fit$target_r1) },
                         fl, hr, mR, H, m)]
cat("
Best setting per floor (2024 fit MAE %):
"); print(grid[, .SD[which.min(fit_mae)], by = fl][, fit_mae := round(fit_mae, 4)])
best <- grid[which.min(fit_mae)]
cat("\nTUNING ON 2024, R1 (marks MAE % vs R1 12-month target incl. no-shows, level-corrected; lower is better).\n")
cat("Best 10 of", nrow(grid), "settings:\n"); print(grid[order(fit_mae)][1:10][, fit_mae := round(fit_mae, 4)])
edge <- c(hr = best$hr == max(HR) || best$hr == min(HR), mR = best$mR %in% c(0.03125, 8), H = best$H %in% range(HL), m = best$m %in% c(0, 64))
if (any(edge)) cat("!!! best setting is on the grid edge for:", names(edge)[edge], "\n")
cat(sprintf("untuned (hr 1, mR 1, H 365, m 1) fit MAE: %.4f\n", grid[hr == 1 & mR == 1 & H == 365 & m == 1, fit_mae]))

r[, base_A := ability]
r[, base_U := predE(r, 1, 1, 365, 1)]
r[, base_Ebest := predE(r, best$hr, best$mR, best$H, best$m, best$fl)]
for (f in FL) { b <- grid[fl == f][which.min(fit_mae)]; r[, (paste0("base_F_", f)) := predE(r, b$hr, b$mR, b$H, b$m, f)] }
fit <- r[fit_set == TRUE]
for (b in grep("^base_", names(r), value = TRUE)) { c0 <- median(fit$target_r1 - fit[[b]]); r[, (sub("^base_", "pred_", b)) := get(b) + c0] }
boot_gap <- function(x, p1, p2) {
  g <- x[, .(d = sum(abs(target_r1 - get(p1)) - abs(target_r1 - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
V <- r[fit_set == FALSE]
cat("\nVERDICT, 2025 cuts, R1. MAE % vs target (lower is better). gap = MAE(A) - MAE(arm), 95% CI by athlete; > 0 = beats deployed.\n")
print(rbindlist(lapply(c("pred_A", "pred_U", "pred_Ebest", paste0("pred_F_", FL)), function(p) {
  ci <- if (p == "pred_A") c(NA, NA) else boot_gap(V, "pred_A", p)
  data.table(arm = sub("pred_", "", p), all = mae(V[[p]], V$target_r1), raced = mae(V[t_n > 0][[p]], V[t_n > 0]$target_r1),
             no_show = mae(V[t_n == 0][[p]], V[t_n == 0]$target_r1), gap_lo = ci[1], gap_hi = ci[2])
}))[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat(sprintf("rows: all %d, raced R1 %d, no R1 race %d\n", nrow(V), V[t_n > 0, .N], V[t_n == 0, .N]))
V[, away := cut(as.numeric(cut - last_date), c(-Inf, 60, 180, 365, Inf), c("<60d", "60-180d", "180-365d", ">365d"))]
cat("\nBy time since last race (any level) at the cut, 2025, R1, MAE %:\n")
print(V[, .(n = .N, no_r1_pct = round(100 * mean(t_n == 0), 1), A = round(mae(pred_A, target_r1), 3),
            U = round(mae(pred_U, target_r1), 3), Ebest = round(mae(pred_Ebest, target_r1), 3)), by = away][order(away)])
fwrite(best, file.path(OUT, "flags", if (RACED_ONLY) "tune_thin_record_E_R1_raced_best.csv" else "tune_thin_record_E_R1_best.csv"))
cat("total time", format(Sys.time() - t0), "\n")
