# Every lever that raises the predicted LEVEL overfits. Can fit-year bias see
# it coming?
#
# THE PATTERN (2026-09-07). The coordinate descent in marks_fit.R walks up any
# parameter that makes predictions more optimistic, because on the fit years
# the optimism happens to match and it buys pooled MAE. Held out it does not
# match, and whole events flip to losing:
#
#   adj  1.0 -> 1.5   fit MAE better, held-out 32/35 -> 27/35
#   trim 0.4 -> 0.5   fit MAE better, held-out 32/35 -> 26/35
#
# Both 100m events break in both cases. Two levers, same mechanism, so the fix
# belongs in the OBJECTIVE rather than in the grid bounds -- capping ranges by
# hand just moves the problem to whichever lever is capped least tightly.
#
# THE QUESTION this answers: is the model's signed bias ON THE FIT YEARS a
# usable early warning? If fit-year bias rises with the lever while held-out
# events fall, then adding a bias penalty to the descent objective fixes the
# whole family of levers at once. If fit-year bias stays flat while held-out
# bias rises, it cannot -- and the honest answer is to bound the grids and say
# so.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_bias_lever.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))
BASE  <- list(blend = 0.7, hl = 90, trim = 0.4, shrink = 0, adj = 1)

frame <- function(p) {
  pairs[, w := w_static * 0.5^(age_days / p$hl)]
  pairs[, p_use := perf_raw + p$adj * (perf - perf_raw)]
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pairs)) else
    !(pairs$tactical & !is.na(pairs$rk) & pairs$rk <= floor(pairs$grp_n * p$trim))
  r <- pairs[keep, .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, ability := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  d <- merge(merge(test, m[, .(athlete_id, event_id, month, ability)],
                   by = c("athlete_id", "event_id", "month")),
             b5, by = c("athlete_id", "event_id", "date"))
  stopifnot("baseline missing on some rows" = all(is.finite(d$base)))
  d[, pred := (1 - p$blend) * ability + p$blend * base][]
}
ev <- function(d) {
  e <- d[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
             b = mean(100 * abs(base - act)), bias = mean(100 * (pred - act)),
             bias_b = mean(100 * (base - act))), by = .(event_id, family)][races >= 10]
  e[, beat := m < b][]
}
row_of <- function(p, lever, value) {
  d <- frame(p); f <- ev(d[date < SPLIT]); h <- ev(d[date >= SPLIT])
  data.table(lever = lever, value = value,
             fit_beat = sprintf("%d/%d", sum(f$beat), nrow(f)),
             fit_bias = round(weighted.mean(f$bias, f$n), 3),
             fit_bias_excess = round(weighted.mean(f$bias, f$n) - weighted.mean(f$bias_b, f$n), 3),
             hold_beat_n = sum(h$beat), hold_of = nrow(h),
             hold_bias = round(weighted.mean(h$bias, h$n), 3),
             hold_mae = round(weighted.mean(h$m, h$n), 3))
}
res <- rbindlist(c(
  lapply(c(0.5, 0.75, 1, 1.25, 1.5, 1.75), function(v)
    row_of(modifyList(BASE, list(adj = v)), "adj", v)),
  lapply(c(0, 0.15, 0.25, 0.4, 0.5, 0.6), function(v)
    row_of(modifyList(BASE, list(trim = v)), "trim", v)),
  lapply(c(30, 45, 60, 90, 180, 270, 365), function(v)
    row_of(modifyList(BASE, list(hl = v)), "half_life", v)),
  lapply(c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9), function(v)
    row_of(modifyList(BASE, list(blend = v)), "blend", v))))
for (lv in unique(res$lever)) {
  cat(sprintf("\n=== %s (all other parameters held at the 32/35 config) ===\n", lv))
  print(res[lever == lv])
}
cat("\nfit_bias_excess is the model's fit-year bias MINUS last-5's on the same\n")
cat("rows. That subtracts whatever level shift the era itself carries, which is\n")
cat("what defeated the per-event offset: the offset removed a period effect both\n")
cat("predictors shared, and helped last-5 just as much.\n")
cat(sprintf("\ncorrelation, fit_bias_excess vs held-out events beaten: %.3f\n",
            cor(res$fit_bias_excess, res$hold_beat_n)))
cat(sprintf("correlation, fit-year events beaten vs held-out events beaten: %.3f\n",
            cor(as.numeric(sub("/.*", "", res$fit_beat)), res$hold_beat_n)))
fwrite(res, file.path(OUT, "marks_bias_lever.csv"))
cat("\nwrote marks_bias_lever.csv\n")
