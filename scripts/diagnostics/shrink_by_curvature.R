# Shrink by how well a unit's optimum is DETERMINED, not by how many rows it has.
#
# THE DEFECT. fit_event_params.R shrinks each family and event toward the level
# above in proportion to its row count:
#
#   value = (kappa * prior + n * own_fit) / (kappa + n)
#
# Row count is the wrong statistic. Measured on the context-adjustment scale,
# fit years:
#
#   family    marks     margin of the winner over the runner-up
#   hurdles    85,279   0.919%
#   walk        9,618   0.484%
#   jump      262,209   0.033%
#   throw     186,764   0.004%
#
# Jump has 262,209 marks and cannot tell 0.75 from 1.00; walk has a twenty-eighth
# as many and separates its winner cleanly. Weighting by n trusts jump and doubts
# walk, which is backwards. What matters is the PRECISION of the argmin, and for
# a smooth error curve that is its curvature: a sharp valley pins the minimum, a
# flat one does not, whatever the sample size.
#
# THE FIX. Fit a quadratic to each unit's (value, error) curve. Its second
# derivative `2a` is the curvature; the sampling variance of the argmin is
# proportional to 1/a, so `a` is the natural precision weight. Then shrink by
# precision instead of by n, with the scale set so the two are comparable:
#
#   value = (kappa * prior + prec * own_fit) / (kappa + prec)
#
# JUDGED HELD OUT, on events beaten and separated wins, against the count-weighted
# version it replaces. A cleverer weighting that does not measure better is not
# an improvement, and this one is only worth having if it earns its place.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/shrink_by_curvature.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
MINR  <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "5"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
pairs[, tactical := tactical & family %in% citius:::.CITIUS_TACTICAL_FAMILIES]
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))
GRID  <- seq(0, 1.5, by = 0.25)
GLOB  <- fit$adj

hl_default <- function(fam) {
  v <- rep(fit$hl, length(fam)); hv <- unlist(DEPLOYED$hl_family)
  if (length(hv)) { i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
frame_at <- function(csmap) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / hl_default(pp$family))
  if (is.finite(fit$rhl) && fit$rhl > 0) w <- w * 0.5^(pp$.k / fit$rhl)
  cs <- if (is.numeric(csmap) && length(csmap) == 1L) rep(csmap, nrow(pp)) else {
    i <- match(pp$event_id, names(csmap)); v <- rep(GLOB, nrow(pp))
    v[!is.na(i)] <- unname(csmap[i[!is.na(i)]]); v
  }
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
summarise <- function(d, label) {
  e <- d[, {
    dd <- 100 * (abs(pred - act) - abs(base_m - act))
    ci <- if (.N >= 5L && stats::sd(dd) > 0) stats::t.test(dd)$conf.int else c(NA_real_, NA_real_)
    .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
      b = mean(100 * abs(base_m - act)), lo = ci[1], hi = ci[2])
  }, by = .(event_id, family)][races >= MINR]
  data.table(config = label, beat = sum(e$m < e$b), of = nrow(e),
             won = sum(e$hi < 0, na.rm = TRUE), lost = sum(e$lo > 0, na.rm = TRUE),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}

# --- curves at both levels ---------------------------------------------------
say("sweeping the context scale over %d values", length(GRID))
curve <- rbindlist(lapply(GRID, function(v)
  frame_at(v)[date < SPLIT, .(cs = v, sae = sum(abs(pred - act)), n = .N),
              by = .(event_id, family)]))

# CURVATURE as precision. A quadratic through (value, error); `a` is half the
# second derivative, so a sharp valley gives a large `a`. Scaled by the unit's
# own error level, since a curve measured on a high-error event is not steeper
# just because its numbers are bigger.
prec_of <- function(v, err) {
  if (length(v) < 3L || !all(is.finite(err))) return(0)
  cf <- tryCatch(stats::coef(stats::lm(err ~ poly(v, 2, raw = TRUE))), error = function(e) NULL)
  if (is.null(cf) || !is.finite(cf[3]) || cf[3] <= 0) return(0)   # not convex: no information
  unname(cf[3]) / mean(err)
}
ev <- curve[, .(mae = sae / n, cs = cs, n = n), by = .(event_id, family, cs)]
ev_fit <- curve[, {
  m <- sae / n
  .(raw = cs[which.min(m)], n_e = data.table::first(n), prec_e = prec_of(cs, m))
}, by = .(event_id, family)]
fam_fit <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, cs)][, {
  m <- sae / n
  .(fam_raw = cs[which.min(m)], n_f = data.table::first(n), prec_f = prec_of(cs, m))
}, by = family]
cat("\n=== family: row count against curvature ===\n")
print(fam_fit[order(-prec_f), .(family, fam_raw, n_f,
                                precision = round(prec_f, 2),
                                rank_by_n = frank(-n_f), rank_by_prec = frank(-prec_f))])

compose <- function(weight, kf, ke) {
  f <- copy(fam_fit)
  wf <- if (weight == "n") f$n_f else f$prec_f
  f[, fam := (kf * GLOB + wf * fam_raw) / (kf + wf)]
  e <- merge(ev_fit, f[, .(family, fam)], by = "family", all.x = TRUE)
  e[is.na(fam), fam := GLOB]
  we <- if (weight == "n") e$n_e else e$prec_e
  e[, val := (ke * fam + we * raw) / (ke + we)]
  stats::setNames(e$val, e$event_id)
}
cat("\n=== held out: count-weighted against curvature-weighted ===\n")
# kappas on comparable scales: for counts the sweep chose 5000/1600; precision
# lives on a different scale entirely, so its kappas are swept rather than
# assumed, and the comparison is best-against-best.
rows <- list(summarise(frame_at(GLOB)[date >= SPLIT], sprintf("flat %.2f", GLOB)),
             summarise(frame_at(compose("n", 5000, 1600))[date >= SPLIT], "count-weighted 5000/1600"))
for (kf in c(1, 5, 25)) for (ke in c(1, 5, 25)) {
  rows[[length(rows) + 1]] <- summarise(
    frame_at(compose("prec", kf, ke))[date >= SPLIT],
    sprintf("curvature %g/%g", kf, ke))
}
res <- rbindlist(rows)
print(res[order(-won, mae)])
best_c <- res[config %like% "curvature"][order(-won, mae)][1]
base_n <- res[config %like% "count"][1]
cat(sprintf("\nbest curvature-weighted: %s -> %d beaten, %d separated wins, %+.2f%%\n",
            best_c$config, best_c$beat, best_c$won, best_c$vs_last5))
cat(sprintf("count-weighted:          %d beaten, %d separated wins, %+.2f%%\n",
            base_n$beat, base_n$won, base_n$vs_last5))
cat(if (best_c$won > base_n$won || (best_c$won == base_n$won && best_c$mae < base_n$mae))
  "=> curvature weighting EARNS its place; row count is the wrong statistic.\n"
  else
  "=> curvature weighting does NOT beat row count here. The diagnosis stands --\n   jump cannot tell 0.75 from 1.00 on 262k marks -- but fixing the weight does\n   not move the answer, so leave the simpler scheme in place.\n")
fwrite(res, file.path(OUT, "shrink_by_curvature.csv"))
say("wrote shrink_by_curvature.csv")
