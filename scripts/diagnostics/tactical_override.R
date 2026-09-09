# The calibration's tactical override flags 52 of 74 events. Should it?
#
# `estimate_ability()` overrides the registry's `tactical` flag with
# `tactical_index < -0.5`, and `tactical_index` is `.skewness(c_r)` -- the skew
# of the fitted RACE EFFECTS for that event. A strongly negative skew means some
# races come out much slower than typical.
#
# For a 1500m that is exactly right: championship finals are sit-and-kick and
# far slower than paced races, and a slow time there says nothing about ability,
# so dropping the worst marks is correct.
#
# For a shot put or a 100m it is not tactics at all. A negative skew there is
# WEATHER -- headwind, cold, a wet ring -- and the two demand opposite
# treatment. A tactically slow race should be DROPPED, because it does not
# measure the athlete. A weather-slowed race should be ADJUSTED, which
# `.adjust_history_to_target()` already does. Dropping it as well removes the
# athlete's genuine bad days and biases the estimate upward, which is exactly
# the systematic optimism the 100m has been carrying all day.
#
# The override currently adds, over the registry's 18: all 10 throws, all 8
# sprints, 6 hurdles, 5 jumps, 4 combined, 4 road, 3 walk.
#
# This tests three settings of the flag, everything else at the fitted config:
#   as deployed   whatever the calibration says
#   family-gated  the override may only flag families where slow races can
#                 plausibly BE tactical: middle, distance, road, walk, combined
#   registry only the override ignored entirely
#
# The flag is masked rather than recomputed, so the comparison isolates the flag
# and nothing else.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/tactical_override.R'
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
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))
ep    <- as.data.table(readRDS(file.path(OUT, "event_params.rds")))

# Families where a slow race can plausibly reflect RACING rather than weather.
# Middle and distance are the textbook case; road and walk races are won
# tactically over the closing kilometres; a combined event's individual marks
# are paced against the points table rather than run flat out.
TACTICAL_FAMILIES <- c("middle", "distance", "road", "walk", "combined")

reg <- as.data.table(citius_events())[, .(event_id, registry_tactical = tactical)]
pairs <- merge(pairs, reg, by = "event_id", all.x = TRUE)
pairs[is.na(registry_tactical), registry_tactical := FALSE]

hl_default <- function(fam) {
  v <- rep(fit$hl, length(fam)); hv <- unlist(DEPLOYED$hl_family)
  if (length(hv)) { i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
score <- function(label, flag, use_tables) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  pp[, tac := flag(pp)]
  if (use_tables) {
    i <- match(pp$event_id, ep$event_id)
    hlv <- fifelse(is.na(i), fit$hl, ep$half_life[i])
    rhv <- fifelse(is.na(i), fit$rhl, ep$races_half_life[i])
    csv <- fifelse(is.na(i), fit$adj, ep$context_scale[i])
    tvv <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])
  } else {
    hlv <- hl_default(pp$family); rhv <- rep(fit$rhl, nrow(pp))
    csv <- rep(fit$adj, nrow(pp)); tvv <- rep(fit$trim, nrow(pp))
  }
  w <- pp$w_static * 0.5^(pp$age_days / hlv)
  w <- w * fifelse(is.finite(rhv) & rhv > 0, 0.5^(pp$.k / rhv), 1)
  p_use <- pp$perf_raw + csv * (pp$perf - pp$perf_raw)
  keep <- !(pp$tac & !is.na(pp$rk) & tvv > 0 & pp$rk <= floor(pp$grp_n * tvv))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  d <- merge(merge(test, m[, .(athlete_id, event_id, month, pred)],
                   by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))[date >= SPLIT]
  # PAIRED per event, because "42 of 44" and "39 of 44" both include events
  # whose gap is inside its own interval. The count that means something is how
  # many are separated, and in which direction.
  e <- d[, {
    dd <- 100 * (abs(pred - act) - abs(base_m - act))
    ci <- if (.N >= 5L && stats::sd(dd) > 0) stats::t.test(dd)$conf.int else c(NA_real_, NA_real_)
    .(races = uniqueN(race_key), n = .N, mm = mean(100 * abs(pred - act)),
      b = mean(100 * abs(base_m - act)), lo = ci[1], hi = ci[2])
  }, by = .(event_id, family)][races >= MINR]
  data.table(flag = label, tables = use_tables,
             events_flagged = uniqueN(pp[tac == TRUE]$event_id),
             beat = sum(e$mm < e$b), of = nrow(e),
             won = sum(e$hi < 0, na.rm = TRUE), lost = sum(e$lo > 0, na.rm = TRUE),
             mae = round(weighted.mean(e$mm, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$mm, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}
FLAGS <- list(
  "as deployed"   = function(p) p$tactical,
  "family-gated"  = function(p) p$tactical & p$family %in% TACTICAL_FAMILIES,
  "registry only" = function(p) p$registry_tactical)

cat("=== which events each setting flags ===\n")
for (nm in names(FLAGS)) {
  f <- FLAGS[[nm]](pairs)
  cat(sprintf("%-14s %2d events | by family: %s\n", nm, uniqueN(pairs[f]$event_id),
              paste(sprintf("%s %d", names(table(unique(pairs[f, .(event_id, family)])$family)),
                            table(unique(pairs[f, .(event_id, family)])$family)), collapse = ", ")))
}
cat("\n=== held out, 44 events ===\n")
print(rbindlist(lapply(c(FALSE, TRUE), function(tb)
  rbindlist(lapply(names(FLAGS), function(nm) score(nm, FLAGS[[nm]], tb))))))
say("done")
