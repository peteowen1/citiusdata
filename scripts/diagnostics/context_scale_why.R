# Why does each family want a different amount of the context adjustment?
#
# Fitted per family, `context_scale` ranges from 0.47 (throw, walk, combined) to
# 0.60 (middle, road, distance). That is the one result from today with no
# mechanism attached, and an unexplained parameter is how the last three defects
# got in. Two questions, in order of how much they matter:
#
# 1. IS IT EVEN IDENTIFIED? If a family's error curve is flat across the grid,
#    the fitted value is whichever grid point noise favoured and the whole
#    finding is an artefact. This prints each family's curve and the depth of
#    its minimum relative to the spread of the curve, so a flat one is visible
#    rather than hidden behind a single chosen number.
#
# 2. IF IT IS, WHAT PREDICTS IT? The adjustment is an estimate with its own
#    error. Shrinking a noisy correction toward zero is the textbook response,
#    and the optimal amount is var(signal) / (var(signal) + var(noise)). We
#    cannot split those two directly, but we can test the implication: a family
#    whose adjustment is LARGE relative to its within-athlete spread should want
#    MORE of it, because the signal dominates. If that correlation is absent,
#    the story is not shrinkage and the values need another explanation before
#    they ship.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/context_scale_why.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
pairs[, tactical := tactical & family %in% citius:::.CITIUS_TACTICAL_FAMILIES]
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))
GRID  <- seq(0, 1.5, by = 0.25)

hl_default <- function(fam) {
  v <- rep(fit$hl, length(fam)); hv <- unlist(DEPLOYED$hl_family)
  if (length(hv)) { i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
frame_at <- function(cs) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / hl_default(pp$family))
  if (is.finite(fit$rhl) && fit$rhl > 0) w <- w * 0.5^(pp$.k / fit$rhl)
  p_use <- pp$perf_raw + cs * (pp$perf - pp$perf_raw)
  keep <- !(pp$tactical & !is.na(pp$rk) & fit$trim > 0 & pp$rk <= floor(pp$grp_n * fit$trim))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  merge(merge(test, m[, .(athlete_id, event_id, month, pred)],
              by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
}

# --- 1. is it identified? ----------------------------------------------------
say("sweeping context_scale over %d values", length(GRID))
curve <- rbindlist(lapply(GRID, function(v)
  frame_at(v)[date < SPLIT, .(cs = v, mae = mean(100 * abs(pred - act)), n = .N), by = family]))
cat("\n=== fit-year MAE by family and context_scale ===\n")
print(dcast(curve, family ~ cs, value.var = "mae"))

sharp <- curve[, {
  best <- cs[which.min(mae)]
  # Depth of the minimum as a share of the curve's own range. Near 1 the choice
  # is decisive; near 0 the curve is flat and the "fitted" value is noise.
  .(n = data.table::first(n), best = best,
    depth = (max(mae) - min(mae)) / max(mae),
    gap_to_next = {
      o <- sort(mae); (o[2] - o[1]) / o[1]
    })
}, by = family]
cat("\n=== how decisive is each family's optimum? ===\n")
print(sharp[order(-depth), .(family, n, best,
                             depth_pct = round(100 * depth, 2),
                             margin_over_2nd_pct = round(100 * gap_to_next, 3))])
cat("\ndepth_pct is how much worse the WORST grid value is than the best, as a\n")
cat("percentage. margin_over_2nd is how much better the winner is than the\n")
cat("runner-up -- under about 0.05% that is a coin toss between grid points.\n")

# --- 2. does adjustment size explain it? -------------------------------------
# The adjustment actually applied to each mark, and the athlete's own spread.
adj <- pairs[, .(adj_sd = stats::sd(perf - perf_raw, na.rm = TRUE),
                 adj_abs = mean(abs(perf - perf_raw), na.rm = TRUE),
                 spread = stats::sd(perf_raw, na.rm = TRUE),
                 marks = .N), by = family]
adj <- merge(adj, sharp[, .(family, best, depth)], by = "family")
adj[, ratio := adj_sd / spread]
cat("\n=== adjustment size against the fitted scale ===\n")
print(adj[order(-ratio), .(family, marks, adj_sd = round(adj_sd, 4),
                           spread = round(spread, 4), ratio = round(ratio, 3),
                           fitted_scale = best)])
if (nrow(adj) >= 4) {
  cat(sprintf("\nSpearman(adjustment size / spread, fitted scale) = %.3f over %d families\n",
              stats::cor(adj$ratio, adj$best, method = "spearman"), nrow(adj)))
  cat(sprintf("Spearman(mean |adjustment|, fitted scale)        = %.3f\n",
              stats::cor(adj$adj_abs, adj$best, method = "spearman")))
  cat("\nA POSITIVE correlation supports the shrinkage story: where the adjustment\n")
  cat("is large relative to the noise it is being asked to remove, more of it is\n")
  cat("worth keeping. Near zero, or negative, means something else is going on\n")
  cat("and these values should not ship on a story they do not support.\n")
}
fwrite(curve, file.path(OUT, "context_scale_curves.csv"))
say("wrote context_scale_curves.csv")
