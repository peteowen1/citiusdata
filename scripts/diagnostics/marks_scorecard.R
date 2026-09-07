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
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- readRDS(file.path(OUT, "marks_fit_params.rds"))

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
score <- function(p) {
  d <- merge(merge(test, predict_at(p), by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))[date >= SPLIT]
  stopifnot("no held-out rows" = nrow(d) > 0)
  d[, .(races = uniqueN(race_key), n = .N,
        model = mean(100 * abs(pred - act)), last5 = mean(100 * abs(base_m - act))),
    by = .(event_id, family)][, `:=`(gap = 100 * (model - last5) / last5,
                                     beat = model < last5)][]
}
dep <- list(hl = DEPLOYED$half_life, trim = 0.25, shrink = 1, adj = 1, rhl = Inf)
e_dep <- score(dep); e_fit <- score(fit)

say("fitted config: half-life %g | trim %.2f | shrink %.2f | adjustment %.2f | races %g",
    fit$hl, fit$trim, fit$shrink, fit$adj, fit$rhl)
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
cat("\n=== by family ===\n")
print(e_fit[races >= 5, .(events = .N, beat = sum(beat),
                          model = round(weighted.mean(model, n), 3),
                          last5 = round(weighted.mean(last5, n), 3),
                          gap = round(100 * (weighted.mean(model, n) - weighted.mean(last5, n)) /
                                        weighted.mean(last5, n), 1)),
            by = family][order(gap)])
fwrite(e_fit, file.path(OUT, "marks_scorecard.csv"))
say("wrote marks_scorecard.csv")
