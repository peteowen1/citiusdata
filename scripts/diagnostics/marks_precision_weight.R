# How much should a Diamond League final outweigh a club heat?
#
# `result_weight()` gives every mark a static weight from its MEET TIER and its
# ROUND, both fitted by `calibrate()` as precision terms: a final is a cleaner
# read on ability than a heat, an elite meet than a development one. In the lab
# that lands in `w_static`, and it has never been questioned -- every parameter
# tuned so far has re-weighted marks by AGE or dropped them, never touched how
# much a mark's context says about how reliable it is.
#
# THE LEVER is an exponent:
#
#   w = w_static^lambda * (recency and races decay)
#
#   lambda 0   every mark counts the same whatever meet it was set at
#   lambda 1   the calibration's precision weights, as they are now
#   lambda 2   twice the spread on the log scale -- trust finals far more
#
# An exponent rather than a linear scale because these are PRECISIONS, which
# multiply. Halving lambda is halving the log-odds of trusting a final over a
# heat, which is the natural way to shrink a weight that is itself a ratio.
#
# WHY IT MIGHT MATTER. The precision weights were fitted to explain scatter, not
# to minimise prediction error, and those are different objectives. If elite
# meets are over-trusted, an athlete's estimate leans on a handful of peak
# performances -- which is exactly the systematic optimism that has been showing
# up all day.
#
# Fitted hierarchically, family then event, the same way the four parameters
# that came before it were.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_precision_weight.R'
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
ep    <- as.data.table(readRDS(file.path(OUT, "event_params.rds")))
# NEGATIVE VALUES ARE IN THE GRID DELIBERATELY. Zero is a boundary, and a
# minimum sitting on a boundary is the same warning as one sitting at a range
# edge: it may be the true optimum, or the grid may simply have stopped short.
# lambda < 0 INVERTS the weighting -- it trusts a heat more than a final, which
# is nonsense as a model -- so if the curve keeps falling below zero the lever is
# not measuring what it claims to and the whole result is suspect.
GRID  <- c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1, 1.5, 2)
GLOB  <- 1

cat("=== what the precision weights actually look like ===\n")
print(pairs[, .(marks = .N, min = round(min(w_static), 3), median = round(median(w_static), 3),
                max = round(max(w_static), 3),
                ratio_p90_p10 = round(quantile(w_static, .9) / quantile(w_static, .1), 2)),
            by = family][order(-ratio_p90_p10)])
cat("\nratio_p90_p10 is how much more a well-weighted mark counts than a poorly\n")
cat("weighted one within that family. Where it is near 1 the lever cannot do\n")
cat("anything, whatever the sweep says.\n")

i <- match(pairs$event_id, ep$event_id)
HLV <- fifelse(is.na(i), fit$hl,   ep$half_life[i])
RHV <- fifelse(is.na(i), fit$rhl,  ep$races_half_life[i])
CSV <- fifelse(is.na(i), fit$adj,  ep$context_scale[i])
TVV <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])

frame_at <- function(lam) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  lv <- if (length(lam) == 1L) rep(lam, nrow(pp)) else {
    j <- match(pp$event_id, names(lam)); v <- rep(GLOB, nrow(pp))
    v[!is.na(j)] <- unname(lam[j[!is.na(j)]]); v
  }
  w <- pp$w_static^lv * 0.5^(pp$age_days / HLV)
  w <- w * fifelse(is.finite(RHV) & RHV > 0, 0.5^(pp$.k / RHV), 1)
  p_use <- pp$perf_raw + CSV * (pp$perf - pp$perf_raw)
  keep <- !(pp$tactical & !is.na(pp$rk) & TVV > 0 & pp$rk <= floor(pp$grp_n * TVV))
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

cat("\n=== flat sweep, held out ===\n")
print(rbindlist(lapply(GRID, function(l)
  summarise(frame_at(l)[date >= SPLIT], sprintf("lambda %.2f", l)))))

# --- fitted hierarchically, family then event -------------------------------
say("fitting the hierarchy")
curve <- rbindlist(lapply(GRID, function(l)
  frame_at(l)[date < SPLIT, .(v = l, sae = sum(abs(pred - act)), n = .N), by = .(event_id, family)]))
ev <- curve[, .(sae = sum(sae), n = sum(n)), by = .(event_id, family, v)]
ev <- ev[ev[, .I[which.min(sae / n)], by = event_id]$V1][, .(event_id, family, raw = v, n_e = n)]
fm <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)]
fm <- fm[fm[, .I[which.min(sae / n)], by = family]$V1][, .(family, fam_raw = v, n_f = n)]
# margin over the runner-up, so a flat curve is visible rather than hidden
marg <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)][, {
  o <- sort(sae / n); .(margin_pct = round(100 * (o[2] - o[1]) / o[1], 3))
}, by = family]
cat("\n=== family fits, with how decisive each is ===\n")
print(merge(fm, marg, by = "family")[order(-margin_pct)])

compose <- function(kf, ke) {
  f <- copy(fm)[, fam := (kf * GLOB + n_f * fam_raw) / (kf + n_f)]
  e <- merge(ev, f[, .(family, fam)], by = "family", all.x = TRUE)
  e[is.na(fam), fam := GLOB]
  e[, val := (ke * fam + n_e * raw) / (ke + n_e)]
  stats::setNames(e$val, e$event_id)
}
cat("\n=== hierarchical, held out ===\n")
res <- rbindlist(c(list(summarise(frame_at(GLOB)[date >= SPLIT], "flat 1 (current)")),
  lapply(c(1000, 5000, 20000), function(kf)
    rbindlist(lapply(c(400, 1600, 6400), function(ke)
      summarise(frame_at(compose(kf, ke))[date >= SPLIT], sprintf("hier %g/%g", kf, ke)))))))
print(res[order(-won, mae)])
base <- res[config == "flat 1 (current)"]
best <- res[config != "flat 1 (current)"][order(-won, mae)][1]
cat(sprintf("\ncurrent: %d beaten, %d separated wins, MAE %.4f (%+.2f%%)\n",
            base$beat, base$won, base$mae, base$vs_last5))
cat(sprintf("best:    %s -> %d beaten, %d separated wins, MAE %.4f (%+.2f%%)\n",
            best$config, best$beat, best$won, best$mae, best$vs_last5))
cat(if (best$won > base$won || (best$won == base$won && best$mae < base$mae))
  "=> the precision weights are worth re-tuning.\n"
  else "=> no gain: the calibration's precision weights are already about right.\n")
fwrite(res, file.path(OUT, "marks_precision_weight.csv"))
say("wrote marks_precision_weight.csv")
