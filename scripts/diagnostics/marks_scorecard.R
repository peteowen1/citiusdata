# THE SCORECARD: every event, model against the fair last-5 baseline, held out.
#
# One question, answered plainly: in how many events do we beat a plain mean of
# the athlete's last five marks, and by how much? Everything else in the marks
# lab exists to choose parameters; this exists to report the result.
#
# The baseline is `base_m.rds` -- last five raw marks cut at the MONTH START,
# the same information the model has. `base.rds` cuts at the race date and gives
# the baseline up to 30 extra days of racing on 92% of rows, which is not a
# comparison, it is a handicap.
#
# Both race thresholds are printed. 10+ races per event is the strict reading
# and gives 35 events; 5+ gives 44 and includes the thin ones, which are exactly
# where a baseline is hardest to beat and so exactly what a headline number
# should not quietly exclude.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_scorecard.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
# WAC-class scoring weights. An Olympic final counts 10x a category F meet;
# see scripts/_score_weights.R for the table and why. Set every weight to 1 via
# CITIUS_SCORE_WEIGHTS to recover the unweighted numbers measured before
# 2026-09-07.
source(here::here("citiusdata", "scripts", "_score_weights.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
# Re-gate the cached tactical flag rather than trusting the cache to have
# been built after the family gate landed (2026-09-07 21:28). Correct today
# only because this defaults to marks_lab_cache_2020, which happens to be
# gated; a CITIUS_LAB_CACHE override pointing at an older cache would
# silently score sprints and throws as tactical. Added 2026-09-09.
pairs[, tactical := tactical & family %in% citius:::.CITIUS_TACTICAL_FAMILIES]
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- readRDS(file.path(OUT, "marks_fit_params.rds"))

# DEFAULT IS T1+T2 TOGETHER, WAC-WEIGHTED, AS OF 2026-09-08 -- reversed from
# the original T1-only default. The reasoning that made T1-only the headline
# was that T2 fields are shallower and weaker (7.5 athletes vs T1's 18.6), so
# scoring them together would mix two different populations. That reasoning
# is right about the population difference and wrong about the fix: WAC class
# weighting already discounts a weak field's races almost to nothing (a
# T1_elite/OW row outweighs a T2_strong/F row 5,454:1 -- see
# fit_event_params.R's TIER_W comment for the fit-side version of this same
# measurement), so a WAC-weighted score across T1+T2 is not "mixing two
# populations equally", it is scoring the SAME championship-weighted question
# fitting already asks, just with the T1-only population's crippling sample
# size problem removed: the men's 100m holdout goes from 55-60 T1-only races
# to 1,644 WAC-weighted T1+T2 races. The narrow population restriction bought
# nothing WAC weighting was not already buying, at a real cost in power.
#
# CITIUS_SCORE_ALL_TIERS=FALSE (or 0) restricts back to the old T1-only
# behaviour, for when the question is specifically and only about the
# T1_elite population.
#
# ACCEPTS "0"/"1" AS WELL AS "TRUE"/"FALSE", DELIBERATELY. R's as.logical()
# maps "1" and "0" to NA, not TRUE/FALSE -- which silently took the T1-only
# branch earlier today when this same script was called with
# CITIUS_SCORE_ALL_TIERS="1", because isTRUE(NA) is FALSE. Checking the
# literal string first avoids relying on every future caller to remember
# that "1" doesn't mean what it looks like it means.
.sat_raw <- Sys.getenv("CITIUS_SCORE_ALL_TIERS", "TRUE")
score_all_tiers <- toupper(.sat_raw) %in% c("1", "TRUE", "T", "YES")
if (score_all_tiers) {
  say("scoring every meet_tier in the cache, not T1 only (CITIUS_SCORE_ALL_TIERS default)")
} else if ("meet_tier" %in% names(test)) {
  n_all <- nrow(test)
  test <- test[meet_tier == "T1_elite"]
  if (nrow(test) < n_all)
    say("scoring T1_elite only: %s of %s rows kept",
        format(nrow(test), big.mark = ","), format(n_all, big.mark = ","))
}

hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) { hv <- unlist(hl_map); i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
predict_at <- function(p) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / hl_of(pp$family, p$hl, DEPLOYED$hl_family))
  if (is.finite(p$rhl) && p$rhl > 0) w <- w * 0.5^(pp$.k / p$rhl)
  p_use <- pp$perf_raw + p$adj * (pp$perf - pp$perf_raw)
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pp)) else
    !(pp$tactical & !is.na(pp$rk) & pp$rk <= floor(pp$grp_n * p$trim))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
# PREDICT AT THE PER-EVENT FITTED VALUES (event_params.rds), NOT A SINGLE
# GLOBAL SCALAR. `predict_at(p)` above applies one context_scale/trim/
# half_life/races_half_life to every event -- fine for "deployed" (which IS
# one global config) but wrong for "fitted", where fit_event_params.R chose a
# DIFFERENT value per event. Scoring the per-event fit with predict_at(fit)
# silently scores the OLD single-global config again: `fit` here is
# marks_fit_params.rds, a different, coarser artefact that predates the
# per-event hierarchy. Found 2026-09-08 when Pole Vault W's own sweep (12.86%
# held-out gap at its actual fitted values) did not match this script's
# "fitted" column (4.9%) for the same event -- they were two different models.
predict_at_event <- function(ep) {
  pp <- merge(data.table::copy(pairs), ep, by = c("event_id", "family"), all.x = TRUE)
  stopifnot("some events in pairs are missing from event_params.rds" =
              all(is.finite(pp$context_scale)))
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / pp$half_life)
  w <- w * data.table::fifelse(is.finite(pp$races_half_life) & pp$races_half_life > 0,
                                0.5^(pp$.k / pp$races_half_life), 1)
  p_use <- pp$perf_raw + pp$context_scale * (pp$perf - pp$perf_raw)
  keep <- !(pp$tactical & !is.na(pp$rk) & pp$trim_tactical > 0 &
              pp$rk <= floor(pp$grp_n * pp$trim_tactical))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
# PAIRED, because the model and the baseline predict the SAME rows. The per-row
# difference in absolute error has far less variance than either error alone, so
# an event scored on 17 races can still give a usable answer -- and more often
# tells you the apparent loss is not distinguishable from zero. Without it a
# +1.4% gap on 17 races and a +1.4% gap on 400 races read identically.
score_preds <- function(preds) {
  d <- merge(merge(test, preds, by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))[date >= SPLIT]
  stopifnot("no held-out rows" = nrow(d) > 0)
  d <- attach_score_weight(d, OUT, quiet = TRUE)
  d <- d[sw > 0]
  d[, {
    dd <- 100 * (abs(pred - act) - abs(base_m - act))
    # The interval is weighted too, or it would test a different population from
    # the one the point estimate reports.
    ci <- if (.N >= 5L && stats::sd(dd) > 0) {
      mu <- sum(sw * dd) / sum(sw)
      v  <- sum(sw * (dd - mu)^2) / sum(sw)
      ne <- sum(sw)^2 / sum(sw^2)            # Kish effective sample size
      se <- sqrt(v / ne)
      mu + c(-1, 1) * stats::qt(0.975, max(ne - 1, 1)) * se
    } else c(NA_real_, NA_real_)
    .(races = uniqueN(race_key), n = sum(sw),
      model = sum(sw * 100 * abs(pred - act)) / sum(sw),
      last5 = sum(sw * 100 * abs(base_m - act)) / sum(sw),
      lo = ci[1], hi = ci[2])
  }, by = .(event_id, family)][
    , `:=`(gap = 100 * (model - last5) / last5, beat = model < last5,
           verdict = data.table::fifelse(!is.finite(lo), "too few",
                     data.table::fifelse(hi < 0, "model better",
                     data.table::fifelse(lo > 0, "LAST-5 BETTER", "not separated"))))][]
}
score      <- function(p)  score_preds(predict_at(p))
score_event <- function(ep) score_preds(predict_at_event(ep))
cat("
"); invisible(attach_score_weight(
  merge(test[date >= SPLIT], bm, by = c("athlete_id", "event_id", "month")), OUT))

dep <- list(hl = DEPLOYED$half_life, trim = 0.25, shrink = 1, adj = 1, rhl = Inf)
ep  <- readRDS(file.path(OUT, "event_params.rds"))
e_dep <- score(dep); e_fit <- score_event(ep)

say("fitted config: PER-EVENT (event_params.rds) | half-life %g-%g | trim %.2f-%.2f | adjustment %.2f-%.2f | races %g-%g | shrink (global) %.2f",
    min(ep$half_life), max(ep$half_life), min(ep$trim_tactical), max(ep$trim_tactical),
    min(ep$context_scale), max(ep$context_scale), min(ep$races_half_life), max(ep$races_half_life),
    fit$shrink)
cat(sprintf("\nheld out from %s: %s predictions, %s races\n\n", format(SPLIT),
            format(sum(e_fit$n), big.mark = ","), format(sum(e_fit$races), big.mark = ",")))

hdr <- function(e, minr, label) {
  x <- e[races >= minr]
  cat(sprintf("%-10s events >= %2d races: beat %2d of %2d | model %.3f vs last-5 %.3f (%+.2f%%)\n",
              label, minr, sum(x$beat), nrow(x), weighted.mean(x$model, x$n),
              weighted.mean(x$last5, x$n),
              100 * (weighted.mean(x$model, x$n) - weighted.mean(x$last5, x$n)) /
                weighted.mean(x$last5, x$n)))
}
for (mr in c(10, 5, 1)) { hdr(e_dep, mr, "deployed"); hdr(e_fit, mr, "fitted"); cat("\n") }

cat("=== every event with 5+ held-out races, worst first ===\n")
tab <- e_fit[races >= 5][order(-gap), .(event_id, family, races,
                                        model = round(model, 3), last5 = round(last5, 3),
                                        gap = round(gap, 1))]
print(tab, nrows = 60)
cat(sprintf("\nlosing: %d of %d. worst is %+.1f%%.\n", sum(!e_fit[races >= 5]$beat),
            nrow(e_fit[races >= 5]), max(e_fit[races >= 5]$gap)))
cat("\n=== ARE THE LOSSES REAL? paired 95% intervals, losing events ===\n")
print(e_fit[races >= 5 & beat == FALSE][order(-gap),
      .(event_id, races, n, gap = round(gap, 1),
        ci95 = sprintf("[%+.3f, %+.3f]", lo, hi), verdict)])
cat("\n=== events we win that are ALSO not separated ===\n")
print(e_fit[races >= 5 & beat == TRUE & verdict == "not separated"][order(gap),
      .(event_id, races, gap = round(gap, 1), ci95 = sprintf("[%+.3f, %+.3f]", lo, hi))])
cat(sprintf("\nof %d scored events: %d separated in our favour, %d against, %d not separated.\n",
            nrow(e_fit[races >= 5]), sum(e_fit[races >= 5]$verdict == "model better"),
            sum(e_fit[races >= 5]$verdict == "LAST-5 BETTER"),
            sum(e_fit[races >= 5]$verdict == "not separated")))

cat("\n=== by family ===\n")
print(e_fit[races >= 5, .(events = .N, beat = sum(beat),
                          model = round(weighted.mean(model, n), 3),
                          last5 = round(weighted.mean(last5, n), 3),
                          gap = round(100 * (weighted.mean(model, n) - weighted.mean(last5, n)) /
                                        weighted.mean(last5, n), 1)),
            by = family][order(gap)])
fwrite(e_fit, file.path(OUT, "marks_scorecard.csv"))
say("wrote marks_scorecard.csv")
