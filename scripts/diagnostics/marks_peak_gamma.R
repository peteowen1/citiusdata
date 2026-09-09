# `peak_gamma`: how far to lean toward an athlete's BEST marks rather than their
# typical ones.
#
#   q <- rank(perf) / n        within the athlete's own history, 1 = their best
#   w <- w * q^gamma
#
#   gamma 0    off. Every mark keeps the weight recency and context gave it.
#   gamma 1    weight proportional to rank: the best mark keeps full weight, the
#              worst keeps about 1/n of it.
#   gamma 2    sharper still -- the estimate becomes a read on peak form.
#
# It is off by default and has never been swept. It is also, transparently, an
# optimism dial: up-weighting an athlete's better marks predicts faster times.
# Every defect found today has been systematic optimism, so the expectation is
# that 0 wins, and the useful question is what happens either side of it.
#
# NEGATIVE VALUES ARE IN THE GRID, for the same reason they were for the
# precision weights. Zero is a boundary, and a minimum on a boundary is the same
# warning as one at a range edge: it might be the optimum, or the grid might have
# stopped short. gamma < 0 up-weights an athlete's WORST marks, which is not a
# model anyone would propose -- so if the curve keeps improving below zero, the
# lever is standing in for a level bias rather than measuring peak form. That is
# exactly what happened to `w_static^lambda`, and it is the single most useful
# thing that sweep produced.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_peak_gamma.R'
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
reg   <- as.data.table(citius_events())[, .(event_id, cv_prior)]
pairs <- merge(pairs, reg, by = "event_id", all.x = TRUE)
pairs[!is.finite(cv_prior) | cv_prior <= 0, cv_prior := citius:::.CITIUS_FALLBACK_CV]

i <- match(pairs$event_id, ep$event_id)
HLV <- fifelse(is.na(i), fit$hl,   ep$half_life[i])
RHV <- fifelse(is.na(i), fit$rhl,  ep$races_half_life[i])
CSV <- fifelse(is.na(i), fit$adj,  ep$context_scale[i])
TVV <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])
LAMBDA <- 0      # precision weighting off  (marks_precision_weight.R)
KHUB   <- 2.5    # asymmetric Huber cutoff  (marks_robust_location.R)
GRID   <- c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 2)

frame_at <- function(gam) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  pp[, w := w_static^LAMBDA * 0.5^(age_days / HLV)]
  pp[, w := w * fifelse(is.finite(RHV) & RHV > 0, 0.5^(.k / RHV), 1)]
  pp[, p_use := perf_raw + CSV * (perf - perf_raw)]
  pp <- pp[!(tactical & !is.na(rk) & TVV > 0 & rk <= floor(grp_n * TVV))]
  gv <- if (length(gam) == 1L) rep(gam, nrow(pp)) else {
    j <- match(pp$event_id, names(gam)); v <- rep(0, nrow(pp))
    v[!is.na(j)] <- unname(gam[j[!is.na(j)]]); v
  }
  if (any(gv != 0)) {
    # q in (0, 1], 1 = the athlete's best mark in that event-month
    pp[, .q := data.table::frank(p_use, ties.method = "first") / .N, by = pid]
    pp[, w := w * (.q^gv)]
  }
  # the robust step, at the settings already adopted
  pp[, mu1 := sum(w * p_use) / sum(w), by = pid]
  pp[, n_g := .N, by = pid]
  pp[, dev := p_use - mu1][, cut := KHUB * cv_prior]
  pp[, w_rob := w]
  pp[dev < -cut & n_g >= 3L, w_rob := w * (cut / abs(dev))]
  r <- pp[, .(ability_raw = sum(w_rob * p_use) / sum(w_rob), w_total = sum(w)), by = pid]
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
      b = mean(100 * abs(base_m - act)), bias = mean(100 * (pred - act)),
      bias_b = mean(100 * (base_m - act)), lo = ci[1], hi = ci[2])
  }, by = .(event_id, family)][races >= MINR]
  data.table(config = label, beat = sum(e$m < e$b), of = nrow(e),
             won = sum(e$hi < 0, na.rm = TRUE), lost = sum(e$lo > 0, na.rm = TRUE),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2),
             excess = round(weighted.mean(e$bias, e$n) - weighted.mean(e$bias_b, e$n), 3))
}
cat("=== flat sweep, held out. `excess` is optimism against the baseline ===\n")
res <- rbindlist(lapply(GRID, function(g)
  summarise(frame_at(g)[date >= SPLIT], sprintf("gamma %+.2f", g))))
print(res)

base <- res[config == sprintf("gamma %+.2f", 0)]
best <- res[order(-won, mae)][1]
cat(sprintf("\ngamma 0 (current): %d beaten, %d separated wins, MAE %.4f, excess %+.3f\n",
            base$beat, base$won, base$mae, base$excess))
cat(sprintf("best:              %s -> %d beaten, %d separated wins, MAE %.4f, excess %+.3f\n",
            best$config, best$beat, best$won, best$mae, best$excess))
neg <- res[config %in% sprintf("gamma %+.2f", c(-1, -0.5, -0.25))]
if (any(neg$mae < base$mae)) {
  cat("\nWARNING: the curve keeps improving BELOW zero, where gamma up-weights an\n")
  cat("athlete's WORST marks. That is not a model anyone would propose, so the\n")
  cat("lever is standing in for a level bias rather than measuring peak form --\n")
  cat("the same pattern as w_static^lambda. Do not read the negative rows as a\n")
  cat("setting; read them as a diagnosis.\n")
} else {
  cat("\nzero is a genuine optimum: the curve turns on both sides of it.\n")
}
fwrite(res, file.path(OUT, "marks_peak_gamma.csv"))
say("wrote marks_peak_gamma.csv")
