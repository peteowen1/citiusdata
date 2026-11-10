# Level-corrected tuning of the thin-record blend. Reads the per-cut fits cached by
# tune_thin_record_blend.R (data/flags/tune_blend_cache/); fits nothing heavy.
#
# WHY. The first tuning (2026-11-09 smoke) fitted b1 = 0.77: it moved 23% of every
# athlete's weight onto the (slower) event mean, which mostly corrects the known level
# bias (every next race runs ~1.2% slower than rated). It improved marks most where
# there is NO thin record (pull < 0.05: 2.107 -> 1.841) and did not improve order.
# That is a level fix wearing a blend's clothes. Here EVERY arm, deployed included,
# gets its own fitted level shift c0 on the 2024 cuts, so the blend weights are judged
# only on what a level shift cannot do. Concordance is level-free by construction.
#
#   rating_E = floor + c0 + b1*(1-s)*(raw - floor) + b2*s*(1-sL)*(long - floor)
#   a = b1*(1-s), c = b2*s*(1-sL), b = 1 - a - c; c0 is a separate level term.
# Split: fit 2024 cuts, verdict 2025 cuts. Long half-life H from the same grid.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "flags", "tune_blend_cache")
fs <- list.files(CACHE, pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE)
HK <- sub("^long_", "", grep("^long_", names(r), value = TRUE))
r <- r[complete.cases(r[, c(paste0("long_", HK), paste0("sL_", HK)), with = FALSE])]
r[, fit_set := year(cut) == 2024]
cat(sprintf("cuts %d (%s); rows fit 2024 %d, verdict 2025 %d\n", length(fs),
            paste(format(sort(unique(r$cut))), collapse = " "), r[fit_set == TRUE, .N], r[fit_set == FALSE, .N]))
if (length(unique(r$cut)) < 8) cat("!!! fewer than 8 cuts cached -- PRELIMINARY\n")

mae <- function(p, y) 100 * mean(abs(y - p))
blend <- function(x, k, b1, b2) {
  L <- x[[paste0("long_", k)]]; sL <- x[[paste0("sL_", k)]]
  x$prior_mu + b1 * (1 - x$shrinkage) * (x$ability_raw - x$prior_mu) + b2 * x$shrinkage * (1 - sL) * (L - x$prior_mu)
}
fit <- r[fit_set == TRUE]
lvl <- function(p) median(fit$perf - p)            # the MAE-optimal constant shift
r[, base_A := ability]
r[, base_B := (1 - shrinkage) * ability_raw + shrinkage * long_x5]
for (k in HK) r[, (paste0("base_U_", k)) := blend(r, k, 1, 1)]
pars <- list()
for (k in HK) {
  o <- optim(c(0, 1, 1), function(p) mae(blend(fit, k, p[2], p[3]) + p[1], fit$perf), method = "Nelder-Mead",
             control = list(maxit = 2000, reltol = 1e-10))
  pars[[k]] <- data.table(H = k, c0_pct = 100 * o$par[1], b1 = o$par[2], b2 = o$par[3], fit_mae = o$value)
  r[, (paste0("pred_E_", k)) := blend(r, k, o$par[2], o$par[3]) + o$par[1]]
}
pars <- rbindlist(pars)
fit <- r[fit_set == TRUE]
for (b in grep("^base_", names(r), value = TRUE)) {
  c0 <- median(fit$perf - fit[[b]])
  r[, (sub("^base_", "pred_", b)) := get(b) + c0]
  pars <- rbind(pars, data.table(H = sub("^base_", "", b), c0_pct = 100 * c0, b1 = NA, b2 = NA,
                                 fit_mae = mae(fit[[b]] + c0, fit$perf)), fill = TRUE)
}
cat("\nFITTED ON 2024 (every arm has its own level shift c0, % of the mark; negative = predict slower).\n")
cat("E arms also fit b1, b2 (1, 1 = the untuned formula). fit_mae: marks MAE %, lower is better.\n")
print(pars[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)])

conc <- function(x, col) {
  x <- x[!is.na(race_key)][, if (.N >= 2) .SD, by = race_key]
  v <- x[, { p <- get(col); o <- perf; ii <- combn(.N, 2)
             d1 <- sign(p[ii[1, ]] - p[ii[2, ]]); d2 <- sign(o[ii[1, ]] - o[ii[2, ]])
             .(ok = sum(d1 == d2 & d2 != 0), n = sum(d2 != 0)) }, by = race_key]
  c(100 * sum(v$ok) / sum(v$n), sum(v$n))
}
boot_gap <- function(x, p1, p2) {
  g <- x[, .(d = sum(abs(perf - get(p1)) - abs(perf - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
V <- r[fit_set == FALSE]
arms <- grep("^pred_", names(r), value = TRUE)
cat("\nVERDICT, 2025 cuts, all arms level-corrected. marks MAE % (lower better), concordance % (higher better).\n")
cat("gap vs A = MAE(A) - MAE(arm), 95% CI by athlete; above 0 = arm beats deployed.\n")
out <- rbindlist(lapply(arms, function(p) {
  ci <- if (p == "pred_A") c(NA, NA) else boot_gap(V, "pred_A", p)
  data.table(arm = sub("pred_", "", p), mae = mae(V[[p]], V$perf), conc = conc(V, p)[1], gap_lo = ci[1], gap_hi = ci[2])
}))
print(out[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat(sprintf("pairs scored for concordance: %d\n", conc(V, "pred_A")[2]))
cat("\nTuned vs untuned at the same H (MAE gap U minus E, CI; above 0 = tuning helps beyond level):\n")
for (k in HK) { ci <- boot_gap(V, paste0("pred_U_", k), paste0("pred_E_", k)); cat(sprintf("  %-5s [%+.3f, %+.3f]\n", k, ci[1], ci[2])) }
V[, bucket := cut(shrinkage, c(-Inf, 0.05, 0.2, 0.5, Inf), c("<0.05", "0.05-0.2", "0.2-0.5", ">=0.5"))]
cat("\nBy pull at the cut, 2025, marks MAE % (level-corrected):\n")
print(V[, .(n = .N, A = round(mae(pred_A, perf), 3), B = round(mae(pred_B, perf), 3),
            U365 = round(mae(pred_U_h365, perf), 3), E365 = round(mae(pred_E_h365, perf), 3)), by = bucket][order(bucket)])
fwrite(pars, file.path(OUT, "flags", "tune_thin_record_blend_levelfree_params.csv"))
