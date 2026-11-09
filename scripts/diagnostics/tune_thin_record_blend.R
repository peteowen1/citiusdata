# TUNE the thin-record blend (Pete, 2026-11-09: "do the new formula but TUNE a b and c").
# Follows backtest_thin_record_prior.R, whose x5 long half-life was a prototype guess,
# never tuned. Changes nothing published.
#
# Formula ("shrink twice"): recent level pulled towards the athlete's own long record,
# the long record itself pulled towards the floor (prior_mu, the event mean):
#   rating = floor + b1*(1-s)*(raw - floor) + b2*s*(1-sL)*(long - floor)
#   => a = b1*(1-s), c = b2*s*(1-sL), b = 1 - a - c.
#   s  = deployed shrinkage (recent evidence), sL = shrinkage of the long refit,
#   raw/long = ability_raw of the deployed fit / of the long refit.
# Tuned knobs: H, the long refit's half-life (pre-declared grid below, flat days for
# all six events, plus the old x5 for reference), and b1, b2 (fitted per H by minimising
# marks MAE). b1 = b2 = 1 is the untuned formula, reported too.
# Split (fixed before running): fit on the 4 2024 cuts, verdict on the 4 2025 cuts.
# Target: each rated athlete-event's FIRST result in (cut, cut + 120 d]; history < cut.
# Same known leaks as the parent script (calibration/event params fitted on the full
# corpus; flag_implausible on the full corpus), same for every arm.
# Per-cut fits are cached in data/flags/tune_blend_cache/ so a killed run resumes.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "flags", "tune_blend_cache"); dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
CUTS <- seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "3 months")
if (identical(Sys.getenv("SMOKE"), "1")) CUTS <- as.Date(c("2024-01-01", "2025-01-01"))
H_GRID <- c("h180" = 180, "h365" = 365, "h540" = 540, "h730" = 730, "x5" = NA)
HORIZON <- 120

co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
cat("rows", nrow(h), "athletes", uniqueN(h$athlete_id), "dates", format(range(h$date)), "\n")
cal <- deployed_calibration(OUT); ep <- deployed_event_params()

res <- list()
for (i in seq_along(CUTS)) {
  cut <- CUTS[i]; f <- file.path(CACHE, sprintf("cut_%s.rds", format(cut)))
  if (file.exists(f)) { res[[i]] <- readRDS(f); cat("cut", format(cut), "cached\n"); next }
  t1 <- Sys.time(); past <- h[date < cut]
  a <- deployed_ability(past, as_of = cut - 1, calibration = cal, event_params = ep)
  a[, athlete_id := as.character(athlete_id)]
  ab <- a[as.numeric(cut - last_date) <= 730,
          .(athlete_id, event_id, ability, ability_raw, shrinkage, prior_mu, last_date)]
  for (k in names(H_GRID)) {
    epk <- copy(ep)
    if (is.na(H_GRID[[k]])) epk[, half_life := half_life * 5] else epk[event_id %in% EV, half_life := H_GRID[[k]]]
    l <- .deployed_ability_raw(past, cut - 1, cal, epk); l[, athlete_id := as.character(athlete_id)]
    l <- l[, .(athlete_id, event_id, lr = ability_raw, ls = shrinkage)]
    setnames(l, c("lr", "ls"), paste0(c("long_", "sL_"), k))
    ab <- merge(ab, l, by = c("athlete_id", "event_id"), all.x = TRUE)
  }
  nxt <- h[date >= cut & date < cut + HORIZON][order(date)]
  nxt <- nxt[, .SD[1], by = .(athlete_id, event_id)][, .(athlete_id, event_id, date, race_key, perf)]
  s <- merge(ab, nxt, by = c("athlete_id", "event_id")); s[, cut := cut]
  saveRDS(s, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f)
  res[[i]] <- s
  cat(sprintf("cut %s: rated %d, scored %d, %.1f min\n", format(cut), nrow(ab), nrow(s),
              as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  rm(past, a, ab, nxt); invisible(gc())
}
r <- rbindlist(res)
for (k in names(H_GRID)) stopifnot(r[, mean(!is.na(get(paste0("long_", k))))] > 0.99)
r <- r[complete.cases(r[, grep("^(long_|sL_)", names(r), value = TRUE), with = FALSE])]
r[, fit_set := year(cut) == 2024]
cat(sprintf("scored rows: fit (2024) %d, verdict (2025) %d\n", r[fit_set == TRUE, .N], r[fit_set == FALSE, .N]))

blend <- function(x, k, b1, b2) {
  L <- x[[paste0("long_", k)]]; sL <- x[[paste0("sL_", k)]]
  x$prior_mu + b1 * (1 - x$shrinkage) * (x$ability_raw - x$prior_mu) + b2 * x$shrinkage * (1 - sL) * (L - x$prior_mu)
}
mae <- function(p, y) 100 * mean(abs(y - p))
fit <- r[fit_set == TRUE]; ver <- r[fit_set == FALSE]
# Reference arms from the parent script: A deployed, B own-record pull with x5 (no offset).
r[, pred_A := ability]
r[, pred_B := (1 - shrinkage) * ability_raw + shrinkage * long_x5]
tuned <- list()
for (k in names(H_GRID)) {
  o <- optim(c(1, 1), function(b) mae(blend(fit, k, b[1], b[2]), fit$perf), method = "Nelder-Mead")
  tuned[[k]] <- data.table(H = k, b1 = o$par[1], b2 = o$par[2], fit_mae = o$value,
                           fit_mae_untuned = mae(blend(fit, k, 1, 1), fit$perf))
  r[, (paste0("pred_E_", k)) := blend(r, k, o$par[1], o$par[2])]
  r[, (paste0("pred_U_", k)) := blend(r, k, 1, 1)]
}
tuned <- rbindlist(tuned)
cat("\nFITTED ON 2024 CUTS (marks MAE %, lower is better). b1/b2 = 1 means the untuned formula.\n")
print(tuned[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)])

conc <- function(x, col) {   # share of within-race pairs ordered correctly
  x <- x[!is.na(race_key)][, if (.N >= 2) .SD, by = race_key]
  v <- x[, { p <- get(col); o <- perf; ii <- combn(.N, 2)
             d1 <- sign(p[ii[1, ]] - p[ii[2, ]]); d2 <- sign(o[ii[1, ]] - o[ii[2, ]])
             .(ok = sum(d1 == d2 & d2 != 0), n = sum(d2 != 0)) }, by = race_key]
  c(100 * sum(v$ok) / sum(v$n), sum(v$n))
}
boot_gap <- function(x, p1, p2) {   # paired by athlete: MAE(p1) - MAE(p2), > 0 = p2 better
  g <- x[, .(d = sum(abs(perf - get(p1)) - abs(perf - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
arms <- c("pred_A", "pred_B", paste0("pred_U_", names(H_GRID)), paste0("pred_E_", names(H_GRID)))
V <- r[fit_set == FALSE]
cat("\nVERDICT, 2025 cuts (held out). marks MAE %, lower is better; concordance %, higher is better.\n")
cat("vs A: MAE gap A minus arm, 95% CI by athlete; above 0 = arm beats deployed.\n")
out <- rbindlist(lapply(arms, function(p) {
  ci <- if (p == "pred_A") c(NA, NA) else boot_gap(V, "pred_A", p)
  data.table(arm = sub("pred_", "", p), mae = mae(V[[p]], V$perf), conc = conc(V, p)[1],
             gap_lo = ci[1], gap_hi = ci[2])
}))
print(out[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
best <- out[grepl("^E_", arm)][which.min(mae), arm]
cat(sprintf("\nBest tuned on 2025: %s. Its gap vs B (x5 own-record pull, untuned): ", best))
print(round(boot_gap(V, "pred_B", paste0("pred_", best)), 3))
V[, bucket := cut(shrinkage, c(-Inf, 0.05, 0.2, 0.5, Inf), c("<0.05", "0.05-0.2", "0.2-0.5", ">=0.5"))]
cat("\nBy pull at the cut, 2025 (marks MAE %):\n")
print(V[, .(n = .N, A = round(mae(pred_A, perf), 3), B = round(mae(pred_B, perf), 3),
            best = round(mae(get(paste0("pred_", best)), perf), 3)), by = bucket][order(bucket)])
fwrite(r, file.path(OUT, "flags", if (identical(Sys.getenv("SMOKE"), "1")) "tune_thin_record_blend_SMOKE.csv" else "tune_thin_record_blend.csv"))
fwrite(tuned, file.path(OUT, "flags", "tune_thin_record_blend_params.csv"))
cat("total time", format(Sys.time() - t0), "\n")
