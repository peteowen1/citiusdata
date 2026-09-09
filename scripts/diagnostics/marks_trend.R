# Does an athlete's TREND carry information the weighted mean throws away?
#
# `ability` is a weighted average of past marks. An average has no direction: an
# athlete who has run 10.20, 10.15, 10.10, 10.05, 10.00 and one who has run
# 10.00, 10.05, 10.10, 10.15, 10.20 get estimates differing only by the recency
# weights, and the recency weighting is a blunt instrument for what is really a
# slope. Every parameter tuned so far has re-weighted information the model
# already had; this asks whether there is information it never had at all.
#
# THE TERM. For each athlete-event-month, fit a weighted least-squares slope of
# performance against time, using the same weights the ability estimate uses.
# Then project from the weighted CENTRE of the evidence forward to the target
# date:
#
#   centre     = sum(w * age_days) / sum(w)     days before the target
#   slope      = weighted regression of perf on -age_days, i.e. perf per day
#   prediction = ability + gamma * slope * centre
#
# `gamma` is how much of the trend to believe. 0 is the model as it stands. 1
# projects the fitted slope the whole way. Because the ability is already an
# average centred `centre` days in the past, gamma = 1 is not extrapolation
# beyond the data -- it is carrying the estimate forward to the day being
# predicted, which the current model simply does not do.
#
# WHY IT MIGHT NOT WORK, and the reason to measure rather than assume: a slope
# fitted on five noisy marks is mostly noise, and multiplying noise by a horizon
# amplifies it. That is exactly what shrinkage is for, so the slope is shrunk
# toward zero by its own evidence -- an athlete with two marks in a fortnight
# has no trend worth believing, one with twenty over two seasons does.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_trend.R'
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

# --- the ability estimate, and the trend alongside it ------------------------
pp <- data.table::copy(pairs)
data.table::setorder(pp, pid, age_days)
pp[, .k := seq_len(.N) - 1L, by = pid]
i <- match(pp$event_id, ep$event_id)
hlv <- fifelse(is.na(i), fit$hl,   ep$half_life[i])
rhv <- fifelse(is.na(i), fit$rhl,  ep$races_half_life[i])
csv <- fifelse(is.na(i), fit$adj,  ep$context_scale[i])
tvv <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])
pp[, w := w_static * 0.5^(age_days / hlv)]
pp[, w := w * fifelse(is.finite(rhv) & rhv > 0, 0.5^(.k / rhv), 1)]
pp[, p_use := perf_raw + csv * (perf - perf_raw)]
pp <- pp[!(tactical & !is.na(rk) & tvv > 0 & rk <= floor(grp_n * tvv))]

# Weighted mean, weighted centre, and the weighted slope, in one pass. `x` is
# days FORWARD (negative age), so a positive slope means improving on the
# oriented scale where higher is better.
tr <- pp[, {
  sw <- sum(w); xm <- sum(w * -age_days) / sw; ym <- sum(w * p_use) / sw
  sxx <- sum(w * (-age_days - xm)^2)
  sxy <- sum(w * (-age_days - xm) * (p_use - ym))
  .(ability_raw = ym, w_total = sw, centre = -xm, n_marks = .N,
    slope = if (sxx > 0) sxy / sxx else 0,
    sxx = sxx,
    # residual scatter about the fitted line, for shrinking the slope
    rss = { b <- if (sxx > 0) sxy / sxx else 0
            sum(w * (p_use - ym - b * (-age_days - xm))^2) })
}, by = pid]
tr[, se2 := fifelse(sxx > 0 & n_marks > 2, (rss / pmax(w_total, 1e-9)) / sxx, Inf)]

m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], tr, by = "pid")
m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
m[, ability := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]

say("slope fitted for %s athlete-months; median |slope| %.2e per day, median centre %.0f days",
    format(nrow(m), big.mark = ","), median(abs(m$slope)), median(m$centre))

score <- function(gamma, tau) {
  # Shrink the slope toward zero by its own precision: tau is the variance a
  # slope must beat to be believed. tau = 0 trusts every fitted slope.
  sh <- if (tau <= 0) 1 else 1 / (1 + tau * m$se2)
  d <- merge(merge(test, m[, .(athlete_id, event_id, month,
                               pred = ability + gamma * sh * slope * centre)],
                   by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))
  e <- d[date >= SPLIT, {
    dd <- 100 * (abs(pred - act) - abs(base_m - act))
    ci <- if (.N >= 5L && stats::sd(dd) > 0) stats::t.test(dd)$conf.int else c(NA_real_, NA_real_)
    .(races = uniqueN(race_key), n = .N, mm = mean(100 * abs(pred - act)),
      b = mean(100 * abs(base_m - act)), lo = ci[1], hi = ci[2])
  }, by = .(event_id, family)][races >= MINR]
  ef <- d[date < SPLIT, .(n = .N, mm = mean(100 * abs(pred - act)),
                          b = mean(100 * abs(base_m - act))), by = event_id]
  data.table(gamma = gamma, tau = tau,
             fit_vs = round(100 * (weighted.mean(ef$mm, ef$n) - weighted.mean(ef$b, ef$n)) /
                              weighted.mean(ef$b, ef$n), 2),
             beat = sum(e$mm < e$b), of = nrow(e),
             won = sum(e$hi < 0, na.rm = TRUE), lost = sum(e$lo > 0, na.rm = TRUE),
             mae = round(weighted.mean(e$mm, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$mm, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}
cat("\n=== trend term swept: gamma is how much of the slope to carry forward ===\n")
# TAU HAS TO BE SET ON THE SCALE OF `se2`, NOT GUESSED. A slope's variance is in
# per-day-squared units and lands around 1e-11, so the grid this script tried
# first -- running to 1e-4 -- multiplied to nothing and every row came back
# identical. That reads as "shrinkage makes no difference" when the truth is
# that the shrinkage never happened.
#
# Anchored on the observed distribution instead: tau = 1/quantile(se2) puts a
# slope at that quantile on half weight, so the grid spans "believe almost every
# slope" to "believe almost none".
q <- stats::quantile(m$se2[is.finite(m$se2)], c(0.9, 0.5, 0.1, 0.01), na.rm = TRUE)
TAUS <- c(0, 1 / q)
say("se2 quantiles 10/50/90%%: %.3g / %.3g / %.3g | tau grid %s",
    q[[3]], q[[2]], q[[1]], paste(signif(TAUS, 2), collapse = ", "))
res <- rbindlist(c(list(score(0, 0)),
                   lapply(c(0.1, 0.25, 0.5, 0.75, 1), function(g)
                     rbindlist(lapply(TAUS, function(t) score(g, t))))))
print(res[order(mae)][1:16])
base <- res[gamma == 0][1]
best <- res[order(-won, mae)][1]
cat(sprintf("\nno trend:   %d beaten, %d separated wins, MAE %.4f (%+.2f%%)\n",
            base$beat, base$won, base$mae, base$vs_last5))
cat(sprintf("best trend: gamma %.2f tau %g -> %d beaten, %d separated wins, MAE %.4f (%+.2f%%)\n",
            best$gamma, best$tau, best$beat, best$won, best$mae, best$vs_last5))
cat(if (best$won > base$won || (best$won == base$won && best$mae < base$mae))
  "=> the trend carries information the weighted mean was throwing away.\n"
  else
  "=> no gain: a slope fitted on this much data is noise, and the recency\n   weighting already captures what direction there is.\n")
fwrite(res, file.path(OUT, "marks_trend.csv"))
say("wrote marks_trend.csv")
