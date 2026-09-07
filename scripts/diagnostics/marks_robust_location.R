# The robust location estimator is ONE-SIDED, and k = 2.5 is a hand-set number.
#
# `.asymmetric_huber_mean()` downweights a mark only when it falls MORE than
# `k * sigma` BELOW the athlete's weighted mean:
#
#   cutoff <- -k * sig_target
#   bad    <- dev < cutoff
#   w[bad] <- w[bad] * (abs(cutoff) / abs(dev[bad]))
#
# An exceptional mark ABOVE the mean keeps its full weight. That asymmetry is
# deliberate -- a catastrophic race is usually injury or a fall, and says less
# about ability than a great one does -- but it is also, by construction, an
# upward bias: it trims the bad tail and keeps the good one. Given that every
# defect found today has turned out to be systematic optimism, it is the obvious
# place to look next.
#
# AND `k = 2.5` IS HAND-SET, which the package's own rule forbids: "no
# hand-tuned constants in the models; every quantity that affects an answer is
# estimated from data". It has never been measured.
#
# Three estimators, k swept across each:
#   mean        plain weighted mean, no trimming -- what the lab has used all day
#   asymmetric  the package's, downweighting the bad tail only
#   symmetric   downweighting both tails equally
#
# If asymmetric beats symmetric, the "a disaster says less than a triumph"
# argument is earning its place. If symmetric wins, the asymmetry has been
# buying optimism rather than robustness.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_robust_location.R'
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

# `sig_target` is the event's cv_prior, exactly as the package uses it.
reg <- as.data.table(citius_events())[, .(event_id, cv_prior)]
pairs <- merge(pairs, reg, by = "event_id", all.x = TRUE)
pairs[!is.finite(cv_prior) | cv_prior <= 0, cv_prior := citius:::.CITIUS_FALLBACK_CV]

i <- match(pairs$event_id, ep$event_id)
HLV <- fifelse(is.na(i), fit$hl,   ep$half_life[i])
RHV <- fifelse(is.na(i), fit$rhl,  ep$races_half_life[i])
CSV <- fifelse(is.na(i), fit$adj,  ep$context_scale[i])
TVV <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])
LAMBDA <- 0   # precision weighting off, the setting adopted from marks_precision_weight.R

# `mode` is "mean", "asym" or "sym"; k is the cutoff in sigmas.
frame_at <- function(mode, kk) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  pp[, w := w_static^LAMBDA * 0.5^(age_days / HLV)]
  pp[, w := w * fifelse(is.finite(RHV) & RHV > 0, 0.5^(.k / RHV), 1)]
  pp[, p_use := perf_raw + CSV * (perf - perf_raw)]
  pp <- pp[!(tactical & !is.na(rk) & TVV > 0 & rk <= floor(grp_n * TVV))]

  # Two vectorised passes, mirroring the package: a weighted mean, then a
  # reweight against deviations from it. Grouped arithmetic, no per-group R call.
  pp[, mu1 := sum(w * p_use) / sum(w), by = pid]
  pp[, n_g := .N, by = pid]
  if (mode == "mean") {
    pp[, w_rob := w]
  } else {
    pp[, dev := p_use - mu1]
    cut <- pp$kk_cut <- kk * pp$cv_prior
    bad <- if (mode == "asym") pp$dev < -cut else abs(pp$dev) > cut
    # groups under 3 marks are left alone, as the package does
    bad <- bad & pp$n_g >= 3L
    pp[, w_rob := w]
    pp[bad, w_rob := w * (kk_cut / abs(dev))]
  }
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
KS <- c(1, 1.5, 2, 2.5, 3, 4, 6)
say("sweeping k over %d values for two estimators", length(KS))
res <- rbindlist(c(
  list(summarise(frame_at("mean", NA)[date >= SPLIT], "plain weighted mean")),
  lapply(KS, function(x) summarise(frame_at("asym", x)[date >= SPLIT],
                                   sprintf("asymmetric k=%.1f", x))),
  lapply(KS, function(x) summarise(frame_at("sym", x)[date >= SPLIT],
                                   sprintf("symmetric  k=%.1f", x)))))
cat("\n=== held out, 44 events. `excess` is optimism against the baseline ===\n")
print(res[order(-won, mae)])

base <- res[config == "plain weighted mean"]
best <- res[order(-won, mae)][1]
asym25 <- res[config == "asymmetric k=2.5"]
cat(sprintf("\nplain mean (what the lab has used):  %d beaten, %d wins, MAE %.4f, excess %+.3f\n",
            base$beat, base$won, base$mae, base$excess))
cat(sprintf("the package's setting (asym k=2.5):  %d beaten, %d wins, MAE %.4f, excess %+.3f\n",
            asym25$beat, asym25$won, asym25$mae, asym25$excess))
cat(sprintf("best of everything:                  %s -> %d beaten, %d wins, MAE %.4f, excess %+.3f\n",
            best$config, best$beat, best$won, best$mae, best$excess))
# startsWith, NOT %like% "symmetric" -- that matches "asymmetric" as a substring,
# so both lines reported the asymmetric winner and the comparison was vacuous.
# Ordered on wins first, since that is the goal metric; MAE alone picks k = 1.0,
# which has the lowest error and five fewer separated wins.
bs <- res[startsWith(config, "symmetric")][order(-won, mae)][1]
ba <- res[startsWith(config, "asymmetric")][order(-won, mae)][1]
cat(sprintf("\nbest symmetric  %s: %d wins, MAE %.4f, excess %+.3f\n",
            bs$config, bs$won, bs$mae, bs$excess))
cat(sprintf("best asymmetric %s: %d wins, MAE %.4f, excess %+.3f\n",
            ba$config, ba$won, ba$mae, ba$excess))
cat(if (bs$won > ba$won || (bs$won == ba$won && bs$mae < ba$mae))
  "=> SYMMETRIC wins: the one-sided trim was buying optimism, not robustness.\n"
  else
  "=> asymmetric wins: trimming only the bad tail earns its place after all.\n")
# --- k FITTED HIERARCHICALLY, family then event ------------------------------
# A single global k assumes every family's bad tail looks the same. They do not:
# a thrower's series contains fouls and short attempts, a marathoner blows up or
# steps off, a sprinter gets disqualified or shuts down injured. Those are
# different distributions of "much worse than usual", so the cutoff that should
# stop trusting them may differ.
#
# Same construction as the four parameters already fitted this way: grid argmin
# per unit on the FIT YEARS, event shrunk toward family, family toward global,
# each by its own evidence.
say("fitting k hierarchically")
curve <- rbindlist(lapply(KS, function(x)
  frame_at("asym", x)[date < SPLIT, .(v = x, sae = sum(abs(pred - act)), n = .N),
                      by = .(event_id, family)]))
ev <- curve[, .(sae = sum(sae), n = sum(n)), by = .(event_id, family, v)]
ev <- ev[ev[, .I[which.min(sae / n)], by = event_id]$V1][, .(event_id, family, raw = v, n_e = n)]
fm <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)]
fm <- fm[fm[, .I[which.min(sae / n)], by = family]$V1][, .(family, fam_raw = v, n_f = n)]
# margin over the runner-up, so a family with a flat curve is visible
marg <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)][, {
  o <- sort(sae / n); .(margin_pct = round(100 * (o[2] - o[1]) / o[1], 3))
}, by = family]
cat("
=== k fitted per family, with how decisive each is ===
")
print(merge(fm, marg, by = "family")[order(-margin_pct)])

GLOBK <- 2.5
compose <- function(kf, ke) {
  f <- copy(fm)[, fam := (kf * GLOBK + n_f * fam_raw) / (kf + n_f)]
  e <- merge(ev, f[, .(family, fam)], by = "family", all.x = TRUE)
  e[is.na(fam), fam := GLOBK]
  e[, val := (ke * fam + n_e * raw) / (ke + n_e)]
  stats::setNames(e$val, e$event_id)
}
# frame_at takes a scalar k; a per-event k needs the vector form
frame_k <- function(kmap) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  pp[, w := w_static^LAMBDA * 0.5^(age_days / HLV)]
  pp[, w := w * fifelse(is.finite(RHV) & RHV > 0, 0.5^(.k / RHV), 1)]
  pp[, p_use := perf_raw + CSV * (perf - perf_raw)]
  pp <- pp[!(tactical & !is.na(rk) & TVV > 0 & rk <= floor(grp_n * TVV))]
  j <- match(pp$event_id, names(kmap))
  pp[, kv := fifelse(is.na(j), GLOBK, unname(kmap[j]))]
  pp[, mu1 := sum(w * p_use) / sum(w), by = pid]
  pp[, n_g := .N, by = pid]
  pp[, dev := p_use - mu1][, kk_cut := kv * cv_prior]
  pp[, w_rob := w]
  pp[dev < -kk_cut & n_g >= 3L, w_rob := w * (kk_cut / abs(dev))]
  r <- pp[, .(ability_raw = sum(w_rob * p_use) / sum(w_rob), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  merge(merge(test, m[, .(athlete_id, event_id, month, pred)],
              by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
}
cat("
=== hierarchical k, held out ===
")
hres <- rbindlist(c(
  list(res[config == "asymmetric k=2.5"][, .(config = "flat k=2.5 (current)", beat, of, won, lost, mae, vs_last5, excess)]),
  lapply(c(200, 1000, 5000), function(kf)
    rbindlist(lapply(c(100, 400, 1600), function(ke)
      summarise(frame_k(compose(kf, ke))[date >= SPLIT], sprintf("hier %g/%g", kf, ke)))))))
print(hres[order(-won, mae)])
hb <- hres[config != "flat k=2.5 (current)"][order(-won, mae)][1]
fb <- hres[config == "flat k=2.5 (current)"]
cat(sprintf("
flat k=2.5:   %d beaten, %d separated wins, MAE %.4f
", fb$beat, fb$won, fb$mae))
cat(sprintf("best hier:    %s -> %d beaten, %d separated wins, MAE %.4f
",
            hb$config, hb$beat, hb$won, hb$mae))
cat(if (hb$won > fb$won || (hb$won == fb$won && hb$mae < fb$mae))
  "=> a per-family k earns its place.
"
  else "=> a single global k is enough; the families do not want different cutoffs.
")
fwrite(hres, file.path(OUT, "marks_robust_hier.csv"))

fwrite(res, file.path(OUT, "marks_robust_location.csv"))
say("wrote marks_robust_location.csv")
