# WHY does a plain mean of five raw marks beat the ability estimate?
#
# The blend is withdrawn (2026-09-07). Pete: "You can't blend with a baseline to
# beat a baseline cause then you're stealing the baseline's info." Right -- but
# the fact that blending WORKED is a measurement, and whatever defect it was
# papering over is still there. This script asks which part of the estimator is
# responsible, by switching each stage off in turn and re-scoring.
#
# Each stage is something `ability` does and last-5 does not:
#   context adjustment  puts marks on a neutral, final-equivalent footing
#   tactical trim       drops an athlete's worst marks in tactical events
#   shrinkage           pulls thin histories toward the event mean
#   recency decay       weights old marks less (last-5 is a hard 5-mark window)
#
# TWO BASELINES, and the difference between them is itself a finding.
#
#   base    last 5 raw marks before the RACE DATE. What every comparison so far
#           has used, including the one that licensed the blend.
#   base_m  last 5 raw marks before the MONTH START.
#
# They differ because marks_pairs.R cuts the model's history at `[date < month]`
# -- the lab estimates ability once per athlete-event-month, which is what makes
# it fast -- while base.rds cuts at `date < i.date`. So the race-date baseline
# has been seeing up to ~30 extra days of that athlete's racing, often their
# most recent race, that the model was never shown. Every number measured
# against `base` understates the model, and it specifically flattered the blend,
# because part of what blending bought was not modelling at all but fresher
# data. `base_m` is built from the pair table itself, so it sees exactly the
# information the model sees, and it is the honest comparison.
#
# THE LEVEL / SHAPE SPLIT is the other half of the report. For each variant it
# prints MAE, then MAE again after subtracting that variant's OWN mean error on
# the very rows being scored. That second number is an ORACLE -- it cannot ship,
# it is cheating by construction -- and that is exactly why it is useful: it is
# the best any pure level correction could ever do. If it closes the gap, the
# defect is LEVEL and the fix is to predict into the right frame. If it does
# not, the defect is SHAPE, the model weights an athlete's own marks wrongly,
# and no offset will ever fix it.
#
# GATE. A variant with no decay, no trim, no shrinkage, no adjustment and a hard
# 5-mark window IS last-5 arithmetically, so it must reproduce base_m exactly.
# It was this gate failing against `base` (max |diff| 0.156, larger than the
# whole effect under discussion) that surfaced the date mismatch above.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_why_last5.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
MINR  <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "5"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))

# The FAIR baseline: same estimator, same information cut-off as the model.
bm <- pairs[order(pid, age_days)][, .(base_m = mean(perf_raw[seq_len(min(.N, 5L))]),
                                      n_m = min(.N, 5L)), by = pid][n_m >= 3L]
bm <- merge(k[, .(pid, athlete_id, event_id, month)], bm, by = "pid")

# `last_n` keeps only an athlete's N most recent marks, the way last-5 does.
# NA means use them all, decayed, which is what the model does.
predict_at <- function(hl = DEPLOYED$half_life, trim = 0.25, shrink = 1, adj = 1,
                       last_n = NA_integer_, decay = TRUE) {
  p <- pairs
  if (!is.na(last_n)) {
    setorder(p, pid, age_days)
    p <- p[p[, .I[seq_len(min(.N, last_n))], by = pid]$V1]
  }
  w <- if (decay) p$w_static * 0.5^(p$age_days / hl) else rep(1, nrow(p))
  p_use <- p$perf_raw + adj * (p$perf - p$perf_raw)
  keep <- if (trim <= 0) rep(TRUE, nrow(p)) else
    !(p$tactical & !is.na(p$rk) & p$rk <= floor(p$grp_n * trim))
  r <- data.table(pid = p$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)],
             r, by = "pid")
  m[, kap := shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
frame <- function(...) {
  d <- merge(merge(test, predict_at(...), by = c("athlete_id", "event_id", "month")),
             b5, by = c("athlete_id", "event_id", "date"))
  d <- merge(d, bm[, .(athlete_id, event_id, month, base_m)],
             by = c("athlete_id", "event_id", "month"))
  stopifnot("a baseline is missing on some rows" = all(is.finite(d$base) & is.finite(d$base_m)))
  d[date >= SPLIT]
}

# --- GATE, and the size of the head start ------------------------------------
g <- frame(trim = 0, shrink = 0, adj = 0, last_n = 5L, decay = FALSE)
gate <- max(abs(g$pred - g$base_m))
say("GATE vs month-cut last-5: max |diff| %.3g over %s rows", gate, format(nrow(g), big.mark = ","))
if (!is.finite(gate) || gate > 1e-10) {
  cat("\nGATE FAILED against the month-cut baseline, which is built from the same\n",
      "pair table the model uses. That is not a date mismatch -- something in\n",
      "the variant machinery is wrong. Nothing below is interpretable.\n", sep = "")
  quit(status = 1L)
}
say("gate passed: the harness reproduces month-cut last-5 exactly")
say("HEAD START, race-date baseline over month-cut: mean |diff| %.4f of a mark",
    mean(abs(g$base - g$base_m)))
say("  race-date last-5 MAE %.4f%%  vs  month-cut last-5 MAE %.4f%%  (%.1f%% easier)",
    mean(100 * abs(g$base - g$act)), mean(100 * abs(g$base_m - g$act)),
    100 * (mean(abs(g$base_m - g$act)) - mean(abs(g$base - g$act))) / mean(abs(g$base_m - g$act)))

score <- function(label, note, ...) {
  d <- frame(...)
  e <- d[, {
    off <- mean(pred - act)                       # ORACLE: fitted on these rows
    .(races = uniqueN(race_key), n = .N,
      m = mean(100 * abs(pred - act)),
      m_lvl = mean(100 * abs(pred - off - act)),  # best a level fix could do
      b = mean(100 * abs(base - act)),            # race-date baseline (unfair)
      bm = mean(100 * abs(base_m - act)),         # month-cut baseline (fair)
      excess = mean(100 * (pred - act)) - mean(100 * (base_m - act)))
  }, by = .(event_id, family)][races >= MINR]
  rel <- function(x, y) round(100 * (weighted.mean(x, e$n) - weighted.mean(y, e$n)) /
                                weighted.mean(y, e$n), 2)
  data.table(variant = label, note = note, of = nrow(e),
             beat_fair = sum(e$m < e$bm), beat_unfair = sum(e$m < e$b),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_fair = rel(e$m, e$bm), vs_unfair = rel(e$m, e$b),
             mae_oracle = round(weighted.mean(e$m_lvl, e$n), 4),
             beat_oracle_fair = sum(e$m_lvl < e$bm),
             excess_bias = round(weighted.mean(e$excess, e$n), 3))
}

res <- rbindlist(list(
  score("deployed",          "everything on"),
  score("no adjustment",     "adj 0: raw marks",      adj = 0),
  score("half adjustment",   "adj 0.5",               adj = 0.5),
  score("no tactical trim",  "trim 0",                trim = 0),
  score("no shrinkage",      "shrink 0",              shrink = 0),
  score("fast decay",        "half-life 90d",         hl = 90),
  score("no decay",          "flat weights",          decay = FALSE),
  score("last 5, full model","5-mark window",         last_n = 5L),
  score("last 10, no adj",   "10-mark window, adj 0", last_n = 10L, adj = 0),
  score("BASELINE month-cut","the gate variant",      trim = 0, shrink = 0,
        adj = 0, last_n = 5L, decay = FALSE)))
cat("\n=== held out 2024+. beat_fair is the honest column. mae_oracle CANNOT ship ===\n")
print(res)

dep <- res[variant == "deployed"]; bas <- res[variant == "BASELINE month-cut"]
cat("\n=== reading it ===\n")
cat(sprintf("deployed beats %d of %d events against the FAIR baseline, %d of %d against\n",
            dep$beat_fair, dep$of, dep$beat_unfair, dep$of))
cat("the race-date one. That difference is a head start in information, not modelling.\n\n")
cat(sprintf("deployed MAE %.4f, fair baseline %.4f, gap %+.4f\n", dep$mae, bas$mae, dep$mae - bas$mae))
if (dep$mae > bas$mae) {
  lvl <- 100 * (dep$mae - dep$mae_oracle) / (dep$mae - bas$mae)
  cat(sprintf("a PERFECT level correction would close %.0f%% of that gap (to %.4f)\n",
              lvl, dep$mae_oracle))
  cat(if (lvl > 70)
    "=> mostly a LEVEL defect: the model predicts into the wrong frame. The fix is\n   to project the prediction into the target race's expected context.\n"
    else if (lvl < 30)
    "=> mostly a SHAPE defect: the model weights an athlete's own marks wrongly.\n   No offset fixes this; the weighting or the window is what is wrong.\n"
    else
    "=> BOTH, in comparable measure. Level and shape need separate fixes.\n")
} else {
  cat("=> the deployed model already beats the fair baseline on pooled MAE.\n")
}
fwrite(res, file.path(OUT, "marks_why_last5.csv"))

# --- THE WINDOW, swept ------------------------------------------------------
# A CAP ON HOW MANY RESULTS CONTRIBUTE is not the same lever as a faster decay,
# and the table above shows they behave completely differently: half-life 90d is
# +12.7% against the fair baseline, while keeping the 5 most recent marks with
# the SAME half-life is -4.2%. A decay crushes everything older than a few
# months even for an athlete who races twice a year; a cap keeps full weight on
# their last five whenever those were. Form is better described by an athlete's
# last N performances than by a fixed stretch of calendar.
#
# Everything else stays at deployed settings, so this is one lever.
cat("\n=== window sweep: cap on contributing marks, everything else deployed ===\n")
sw <- rbindlist(lapply(c(3, 4, 5, 6, 8, 10, 15, 20, 30, NA), function(n)
  score(if (is.na(n)) "uncapped (deployed)" else sprintf("last %d", n),
        "", last_n = if (is.na(n)) NA_integer_ else as.integer(n))))
print(sw[, .(variant, beat_fair, of, mae, vs_fair, excess_bias)])
best <- sw[which.min(mae)]
cat(sprintf("\nlowest MAE at %s (%.4f), most events beaten by %s\n",
            best$variant, best$mae, sw[which.max(beat_fair)]$variant))
if (best$variant %in% c("last 3", "last 30")) {
  cat("WARNING: the optimum is at the edge of the range swept, so the range is\n",
      "wrong and this number is not an optimum. Widen it before believing it.\n", sep = "")
}
fwrite(sw, file.path(OUT, "marks_window_sweep.csv"))
say("wrote marks_why_last5.csv and marks_window_sweep.csv")
