# The 100m is only 0.5% ahead of last-5 where other events are 6-18% ahead, and
# it over-predicts by +0.64% where last-5 over-predicts by +0.19%. WHY?
#
# The cut analysis says the excess is FLAT across wind bins (+0.39 to +0.56), so
# it is not a conditions effect. It is a level, and something in the estimator
# is putting it there. Every stage that could is switched off here, one at a
# time, on the two 100m events only, held out.
#
# THE LEADING SUSPECT is the tactical trim. It drops an athlete's worst results,
# which raises their estimate by construction, and it is meant for events where
# a slow time reflects tactics rather than ability -- 800m and up. The 100m has
# no tactical racing. But the trim flag is a CALIBRATION output that OVERRIDES
# the registry (`tactical_index < -0.5` on calibrated events), and that override
# was never inspected: it turned 22 registry-flagged events into 55, including
# throws, where "tactical" cannot mean anything. If it has caught the 100m, the
# model has been trimming away exactly the slow races that tell it an athlete is
# not in the shape their best marks suggest.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_100m_optimism.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
EVS   <- trimws(strsplit(Sys.getenv("CITIUS_EVENTS", "AT-100Metres-M,AT-100Metres-W"), ",")[[1]])
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- readRDS(file.path(OUT, "marks_fit_params.rds"))

# --- IS THE 100m FLAGGED TACTICAL? ------------------------------------------
cat("=== the tactical flag, as the lab actually applies it ===\n")
flag <- pairs[, .(marks = .N, tactical = any(tactical)), by = event_id]
reg <- as.data.table(citius_events())[, .(event_id, registry_tactical = tactical, family)]
flag <- merge(flag, reg, by = "event_id", all.x = TRUE)
print(flag[event_id %in% EVS])
cat(sprintf("\nevents flagged tactical in the lab: %d of %d; registry says %d\n",
            sum(flag$tactical, na.rm = TRUE), nrow(flag), sum(flag$registry_tactical, na.rm = TRUE)))
cat("\nflagged tactical but NOT in the registry -- the calibration's override:\n")
odd <- flag[tactical == TRUE & registry_tactical == FALSE][order(family, event_id)]
print(odd[, .(event_id, family, marks)], nrows = 60)
cat("\nby family, how many events the override adds:\n")
print(odd[, .(events_added = .N), by = family][order(-events_added)])

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
score <- function(label, p, evs) {
  d <- merge(merge(test, predict_at(p), by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))[date >= SPLIT & event_id %in% evs]
  stopifnot("no rows" = nrow(d) > 0)
  dd <- 100 * (abs(d$pred - d$act) - abs(d$base_m - d$act))
  tt <- stats::t.test(dd)
  data.table(config = label, n = nrow(d),
             model = round(mean(100 * abs(d$pred - d$act)), 3),
             last5 = round(mean(100 * abs(d$base_m - d$act)), 3),
             gap = round(100 * (mean(abs(d$pred - d$act)) - mean(abs(d$base_m - d$act))) /
                           mean(abs(d$base_m - d$act)), 1),
             ci95 = sprintf("[%+.3f, %+.3f]", tt$conf.int[1], tt$conf.int[2]),
             excess = round(mean(100 * (d$pred - d$act)) - mean(100 * (d$base_m - d$act)), 3))
}
p <- function(...) modifyList(as.list(fit), list(...))
for (grp in list(list("the two 100m", EVS), list("all other events", setdiff(unique(test$event_id), EVS)))) {
  cat(sprintf("\n=== %s, held out ===\n", grp[[1]]))
  print(rbindlist(list(
    score("fitted",            p(),                grp[[2]]),
    score("no tactical trim",  p(trim = 0),        grp[[2]]),
    score("no adjustment",     p(adj = 0),         grp[[2]]),
    score("full adjustment",   p(adj = 1),         grp[[2]]),
    score("races decay off",   p(rhl = Inf),       grp[[2]]),
    score("shrink 1",          p(shrink = 1),      grp[[2]]))))
}
# --- A FAMILY-SPECIFIC ADJUSTMENT SCALE -------------------------------------
# The adjustment is monotone in optimism on the 100m (excess 0.050 / 0.453 /
# 0.856 at scale 0 / 0.5 / 1) and monotone the OTHER way on everything else,
# where more adjustment keeps improving MAE. So one global scale is being asked
# to serve two populations that want opposite things.
#
# This is a TWO-LEVEL split -- sprint against the rest -- motivated by a
# mechanism, not nine free per-family weights. Per-family fitting has overfit
# badly here before (16 of 35 held out against 28 for a flat value), so the
# sprint value is swept and the rest is pinned at the fitted 0.5, and both
# windows are printed so a fit-year-only gain is visible as one.
cat("
=== sprint-specific adjustment scale (others pinned at the fitted value) ===
")
predict_fam <- function(adj_sprint, p) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / hl_of(pp$family, p$hl, DEPLOYED$hl_family))
  if (is.finite(p$rhl) && p$rhl > 0) w <- w * 0.5^(pp$.k / p$rhl)
  a <- data.table::fifelse(pp$family == "sprint", adj_sprint, p$adj)
  p_use <- pp$perf_raw + a * (pp$perf - pp$perf_raw)
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pp)) else
    !(pp$tactical & !is.na(pp$rk) & pp$rk <= floor(pp$grp_n * p$trim))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
sprint_evs <- unique(pairs[family == "sprint"]$event_id)
row_fam <- function(a) {
  d0 <- merge(merge(test, predict_fam(a, as.list(fit)), by = c("athlete_id", "event_id", "month")),
              bm, by = c("athlete_id", "event_id", "month"))
  one <- function(x, lab) {
    e <- x[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
               b = mean(100 * abs(base_m - act))), by = .(event_id, family)][races >= 5]
    sprintf("%2d/%2d %+.2f%%", sum(e$m < e$b), nrow(e),
            100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) / weighted.mean(e$b, e$n))
  }
  sp <- d0[date >= SPLIT & event_id %in% sprint_evs]
  data.table(adj_sprint = a,
             fit_years = one(d0[date < SPLIT]), held_out = one(d0[date >= SPLIT]),
             sprint_gap = round(100 * (mean(abs(sp$pred - sp$act)) - mean(abs(sp$base_m - sp$act))) /
                                  mean(abs(sp$base_m - sp$act)), 1),
             sprint_excess = round(mean(100 * (sp$pred - sp$act)) - mean(100 * (sp$base_m - sp$act)), 3))
}
print(rbindlist(lapply(c(0, 0.15, 0.25, 0.35, 0.5, 0.75, 1), row_fam)))
say("done")
