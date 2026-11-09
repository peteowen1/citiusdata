# Version E tuning restricted to ELITES. Reads the per-cut fits cached by
# tune_thin_record_E.R (data/flags/tune_E_cache/); fits nothing heavy.
#
# WHY. On all rated athletes the no-show-aware target is dominated by club runners who
# stop racing (41-45% no-shows, scored at the median same-age runner), so the tuning
# ran to every grid edge in the "collapse to the floor" direction and made athletes who
# DID race worse (2025 smoke: 2.031 -> 2.212). The thin-record question is about
# elites: the athletes heading the ratings page and entering championships.
# Population: rank <= TOP (default 200) within event at the cut by own long record
# (long_365, the 365-day refit), so the population is chosen without future data.
# Grid: hr {0.5,1,2} and H {180,365,540,730} are what the cache holds; mR and m are
# free here, so their grids are wider than the first run's (which hit its edges).
# Same formula, target, level shifts and 2024-fit / 2025-verdict split as the parent.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
TOP <- as.integer(Sys.getenv("TOP", "200"))
fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE)
cat(sprintf("cuts %d: %s\n", length(fs), paste(sort(unique(format(r$cut))), collapse = " ")))
if (length(fs) < 8) cat("!!! fewer than 8 cuts cached -- PRELIMINARY\n")
HR <- c(0.5, 1, 2); HL <- c(180, 365, 540, 730)
r <- r[complete.cases(r[, c(paste0("long_", HL), paste0("R_", HR)), with = FALSE])]
r[, elite_rank := frank(-long_365, ties.method = "min"), by = .(cut, event_id)]
r <- r[elite_rank <= TOP & !is.na(target)]
r[is.na(floor_age), floor_age := prior_mu]
r[, fit_set := year(cut) == 2024]
cat(sprintf("top %d per event per cut: fit 2024 %d rows (no-shows %.1f%%), verdict 2025 %d rows (no-shows %.1f%%)\n", TOP,
            r[fit_set == TRUE, .N], 100 * r[fit_set == TRUE, mean(next_n == 0)],
            r[fit_set == FALSE, .N], 100 * r[fit_set == FALSE, mean(next_n == 0)]))

predE <- function(x, hr, mR, H, m) {
  wr <- x[[paste0("wR_", hr)]]; kr <- mR * x[[paste0("kR_", hr)]]; a <- wr / (wr + kr)
  wl <- x[[paste0("wL_", H)]]; kl <- m * x[[paste0("kL_", H)]]; sl <- kl / (wl + kl)
  a * x[[paste0("R_", hr)]] + (1 - a) * ((1 - sl) * x[[paste0("long_", H)]] + sl * x$floor_age)
}
mae <- function(p, y) 100 * mean(abs(y - p))
fit <- r[fit_set == TRUE]
grid <- CJ(hr = HR, mR = c(0.25, 0.5, 1, 2, 4, 8), H = HL, m = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))
grid[, fit_mae := mapply(function(hr, mR, H, m) { p <- predE(fit, hr, mR, H, m); mae(p + median(fit$target - p), fit$target) },
                         hr, mR, H, m)]
best <- grid[which.min(fit_mae)]
cat("\nTUNING ON 2024, elites (marks MAE % vs 12-month target incl. no-shows, level-corrected; lower is better).\n")
cat("Best 10 of", nrow(grid), "settings:\n"); print(grid[order(fit_mae)][1:10][, fit_mae := round(fit_mae, 4)])
edge <- c(hr = best$hr %in% range(HR), mR = best$mR %in% c(0.25, 8), H = best$H %in% range(HL), m = best$m %in% c(0.25, 64))
if (any(edge)) cat("!!! best setting is on the grid edge for:", names(edge)[edge], "\n")

r[, base_A := ability]
r[, base_U := predE(r, 1, 1, 365, 1)]
r[, base_Ebest := predE(r, best$hr, best$mR, best$H, best$m)]
fit <- r[fit_set == TRUE]
for (b in grep("^base_", names(r), value = TRUE)) {
  c0 <- median(fit$target - fit[[b]]); r[, (sub("^base_", "pred_", b)) := get(b) + c0]
}
boot_gap <- function(x, p1, p2) {
  g <- x[, .(d = sum(abs(target - get(p1)) - abs(target - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
V <- r[fit_set == FALSE]
cat("\nVERDICT, 2025 cuts, elites. MAE % vs target (lower is better). gap = MAE(A) - MAE(arm), 95% CI by athlete; > 0 = beats deployed.\n")
print(rbindlist(lapply(c("pred_A", "pred_U", "pred_Ebest"), function(p) {
  ci <- if (p == "pred_A") c(NA, NA) else boot_gap(V, "pred_A", p)
  data.table(arm = sub("pred_", "", p), all = mae(V[[p]], V$target), raced = mae(V[next_n > 0][[p]], V[next_n > 0]$target),
             no_show = mae(V[next_n == 0][[p]], V[next_n == 0]$target), gap_lo = ci[1], gap_hi = ci[2])
}))[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat(sprintf("rows: all %d, raced %d, no-show %d\n", nrow(V), V[next_n > 0, .N], V[next_n == 0, .N]))
V[, away := cut(as.numeric(cut - last_date), c(-Inf, 60, 180, 365, Inf), c("<60d", "60-180d", "180-365d", ">365d"))]
cat("\nBy time since last race at the cut, 2025, elites, MAE %:\n")
print(V[, .(n = .N, no_show_pct = round(100 * mean(next_n == 0), 1), A = round(mae(pred_A, target), 3),
            U = round(mae(pred_U, target), 3), Ebest = round(mae(pred_Ebest, target), 3)), by = away][order(away)])
fwrite(best, file.path(OUT, "flags", sprintf("tune_thin_record_E_elite%d_best.csv", TOP)))
