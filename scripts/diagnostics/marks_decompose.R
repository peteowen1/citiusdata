# The fitted config changes three things at once. Which of them can ship alone?
#
# WHY THIS EXISTS. The fit moved blend 0 -> 0.60, half-life 365 -> 270 and
# shrink 1 -> 0.25 together, and together they take held-out events beaten from
# 9 of 35 to 32 of 35. Shipping all three in one go is exactly the mistake the
# 2026-09-07 double-count review is about: two level-moving changes promoted the
# same day, each validated against a control lacking the other.
#
# The three are NOT equivalent in risk:
#
#   blend      MARKS ONLY. It is applied to the reported mark, never to the
#              ability the ranking is built from, so it provably cannot move a
#              finishing order or a medal probability. Shippable on marks
#              evidence alone.
#   half_life  Changes the ability itself. Moves ranking, so it needs a medal
#              backtest before promotion.
#   shrink     Same -- changes the ability.
#
# So this measures each lever alone, then in the combinations that matter, and
# reports held-out events beaten and excess optimism for each. What ships first
# is whatever the marks lab can license by itself.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_decompose.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
# How many held-out races an event needs before it is scored. 10 is the strict
# setting and gives 35 events; 5 gives more events at more noise per event. Both
# are worth printing -- "beat last-5 in every event" is a claim about the whole
# programme, and reporting only the events with the most data quietly excludes
# the thin ones, which are exactly where a baseline is hardest to beat.
MINR <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "10"))

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))
DEP <- list(blend = 0,   hl = DEPLOYED$half_life, trim = 0.25, shrink = 1,    adj = 1)
FIT <- list(blend = 0.6, hl = 270,                trim = 0.25, shrink = 0.25, adj = 1)

frame <- function(p) {
  pairs[, w := w_static * 0.5^(age_days / p$hl)]
  pairs[, p_use := perf_raw + p$adj * (perf - perf_raw)]
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pairs)) else
    !(pairs$tactical & !is.na(pairs$rk) & pairs$rk <= floor(pairs$grp_n * p$trim))
  r <- pairs[keep, .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, ability := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  d <- merge(merge(test, m[, .(athlete_id, event_id, month, ability)],
                   by = c("athlete_id", "event_id", "month")),
             b5, by = c("athlete_id", "event_id", "date"))
  stopifnot("baseline missing on some rows" = all(is.finite(d$base)))
  d[, pred := (1 - p$blend) * ability + p$blend * base][]
}
ev <- function(p) {
  d <- frame(p)[date >= SPLIT]
  e <- d[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
             b = mean(100 * abs(base - act)), bias = mean(100 * (pred - act)),
             bias_b = mean(100 * (base - act))), by = .(event_id, family)][races >= MINR]
  e[, beat := m < b][]
}
row <- function(label, p, touches) {
  e <- ev(p)
  data.table(config = label, touches = touches, beat = sum(e$beat), of = nrow(e),
             mae = round(weighted.mean(e$m, e$n), 3),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 1),
             excess_bias = round(weighted.mean(e$bias, e$n) - weighted.mean(e$bias_b, e$n), 3))
}
res <- rbindlist(list(
  row("deployed",                    DEP,                                            "-"),
  row("+ blend 0.6",                 modifyList(DEP, list(blend = 0.6)),             "marks only"),
  row("+ half-life 270",             modifyList(DEP, list(hl = 270)),                "ranking"),
  row("+ shrink 0.25",               modifyList(DEP, list(shrink = 0.25)),           "ranking"),
  row("+ half-life & shrink",        modifyList(DEP, list(hl = 270, shrink = 0.25)), "ranking"),
  row("all three (fitted)",          FIT,                                            "ranking")))
print(res)

cat("\n=== blend alone, per family (held out) ===\n")
eb <- ev(modifyList(DEP, list(blend = 0.6))); ed <- ev(DEP)
cmp <- merge(ed[, .(event_id, family, races, mae_dep = m, beat_dep = beat)],
             eb[, .(event_id, mae_bl = m, last5 = b, beat_bl = beat)], by = "event_id")
print(cmp[, .(events = .N, beat_deployed = sum(beat_dep), beat_blend = sum(beat_bl),
              deployed = round(weighted.mean(mae_dep, races), 3),
              blend = round(weighted.mean(mae_bl, races), 3),
              last5 = round(weighted.mean(last5, races), 3)), by = family][order(family)])
cat("\n=== events still losing with blend alone ===\n")
print(cmp[beat_bl == FALSE][order(-(mae_bl - last5) / last5),
      .(event_id, family, races, model = round(mae_bl, 3), last5 = round(last5, 3),
        gap = round(100 * (mae_bl - last5) / last5, 1))])
cat("\n=== blend sweep, held out (marks-only lever) ===\n")
#
# TWO numbers, because the goal names two things and they do not peak together:
#   pooled_mae     the out-of-sample mark MAE the goal says to minimise,
#                  weighted by rows, so high-volume events dominate it
#   per_event_gap  the mean of each event's own relative gap, weighting a 100m
#                  and a marathon equally -- the reading of "lower marks MAE FOR
#                  EACH EVENT", and what "beat last-5 in every event" tracks
# Printing only one of them hides the trade rather than resolving it.
sw <- rbindlist(lapply(seq(0, 0.8, by = 0.05), function(bl) {
  e <- ev(modifyList(DEP, list(blend = bl)))
  data.table(blend = bl, beat = sum(e$beat), of = nrow(e),
             pooled_mae = round(weighted.mean(e$m, e$n), 4),
             pooled_vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                       weighted.mean(e$b, e$n), 2),
             per_event_gap = round(mean(100 * (e$m - e$b) / e$b), 2),
             excess_bias = round(weighted.mean(e$bias, e$n) - weighted.mean(e$bias_b, e$n), 3))
}))
print(sw)
cat(sprintf("\nlowest pooled MAE at blend %.2f | most events beaten at %.2f | best per-event gap at %.2f\n",
            sw$blend[which.min(sw$pooled_mae)], sw$blend[which.max(sw$beat)],
            sw$blend[which.min(sw$per_event_gap)]))
fwrite(sw, file.path(OUT, "marks_blend_sweep.csv"))
fwrite(res, file.path(OUT, "marks_decompose.csv"))
cat("\nwrote marks_decompose.csv\n")
