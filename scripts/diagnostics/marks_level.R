# The 100m loses to last-5 on LEVEL, not on wind. Measure the level, and test
# the two things that could be causing it.
#
# THE FINDING that motivates this (marks_cuts.R, 2026-09-07). Cut by wind, the
# model's signed residual runs +1.31% in a headwind to -0.77% in a strong
# tailwind. That looks like a wind bug until you read last-5's residual on the
# SAME rows: +0.91% to -1.05%. Both predictors are blind to the target race's
# wind, so both slope. The model just sits about +0.4% optimistic in every wind
# bin. Headwind is where that offset hurts, because it adds to the error
# instead of cancelling it. On the two 100m events the offset is +0.51%, which
# is 40% of the 1.27% MAE -- easily enough to explain losing to last-5.
#
# TWO CANDIDATE CAUSES, both tested here:
#
#   A) adj_scale. marks_fit.R chose 1.5, the HIGHEST value in its grid, which
#      by itself means the grid was wrong (a swept optimum at the range edge is
#      not an optimum). The context adjustment stack mostly removes penalties,
#      so scaling it up scales the optimism up. This sweep extends past 1.5 and
#      reports MAE and BIAS together, because MAE alone is what selected 1.5.
#
#   B) A residual per-event level offset. Fitted on the fit years only, applied
#      to the held-out years. This is the refit the 2026-09-07 double-count
#      review left open: the old family debias was refuted ON TOP OF THE STRIP,
#      not in principle, and it was fitted against a model two calibrations old.
#      Here the strip is on and the debias is off, so what this measures is the
#      bias that genuinely remains.
#
# HONESTY CONTROL. The same offset trick would help last-5 too, so the report
# prints a debiased-last-5 column. Beating a naive baseline with a correction
# the baseline could also have is worth knowing about, not worth hiding.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_level.R'
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
b5    <- readRDS(file.path(CACHE, "base.rds"))
par   <- readRDS(file.path(OUT, "marks_fit_params.rds"))

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
# per-event summary; `off` is a named vector of level offsets in perf units
lookup <- function(off, ids) { if (is.null(off)) return(rep(0, length(ids)))
  v <- unname(off[match(ids, names(off))]); v[is.na(v)] <- 0; v }
ev <- function(d, off = NULL, off_b = NULL) {
  o  <- lookup(off,   d$event_id)
  ob <- lookup(off_b, d$event_id)
  e <- d[, .(races = uniqueN(race_key), n = .N,
             m = mean(100 * abs(pred - o - act)), b = mean(100 * abs(base - ob - act)),
             bias_m = mean(100 * (pred - o - act))), by = .(event_id, family)][races >= 10]
  e[, `:=`(gap = 100 * (m - b) / b, beat = m < b)][]
}
line <- function(e, label) cat(sprintf("%-26s beat %2d/%2d | model %.3f vs last5 %.3f (%+.1f%%) | bias %+.3f\n",
  label, sum(e$beat), nrow(e), weighted.mean(e$m, e$n), weighted.mean(e$b, e$n),
  100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) / weighted.mean(e$b, e$n),
  weighted.mean(e$bias_m, e$n)))

# --- A) does adj_scale buy MAE by injecting optimism? -------------------------
cat("\n=== A) adjustment scale: MAE and BIAS together, HELD OUT ===\n")
for (a in c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) {
  p <- modifyList(par, list(adj = a))
  e <- ev(frame(p)[date >= SPLIT])
  line(e, sprintf("adj %.2f", a))
}

# --- B) a per-event level offset, fitted out of sample ------------------------
d <- frame(par)
fit <- d[date < SPLIT]; hold <- d[date >= SPLIT]
off_m <- fit[, .(o = mean(pred - act)), by = event_id][, setNames(o, event_id)]
off_b <- fit[, .(o = mean(base - act)), by = event_id][, setNames(o, event_id)]
cat(sprintf("\n=== B) per-event level offset, fitted on %d rows before %s ===\n",
            nrow(fit), format(SPLIT)))
cat(sprintf("model offsets: median %+.3f%%, 5-95%% %+.3f to %+.3f, %d events\n",
            100 * median(off_m), 100 * quantile(off_m, .05), 100 * quantile(off_m, .95), length(off_m)))
e0 <- ev(hold); line(e0, "no offset (current)")
e1 <- ev(hold, off_m); line(e1, "model debiased")
e2 <- ev(hold, off_m, off_b); line(e2, "both debiased (control)")

cat("\n=== events that flip to beating last-5 with the offset ===\n")
fl <- merge(e0[, .(event_id, family, races, gap0 = gap, beat0 = beat)],
            e1[, .(event_id, gap1 = gap, beat1 = beat, off = 100 * off_m[event_id])], by = "event_id")
print(fl[beat1 == TRUE & beat0 == FALSE][order(gap1), .(event_id, family, races,
        gap0 = round(gap0, 1), gap1 = round(gap1, 1), offset = round(off, 3))])
cat("\n=== events still losing after the offset ===\n")
print(fl[beat1 == FALSE][order(-gap1), .(event_id, family, races,
        gap0 = round(gap0, 1), gap1 = round(gap1, 1), offset = round(off, 3))])
cat("\n=== events the offset BREAKS (were winning, now losing) ===\n")
print(fl[beat1 == FALSE & beat0 == TRUE][order(-gap1), .(event_id, gap0 = round(gap0, 1), gap1 = round(gap1, 1))])

cat("\n=== the two 100m, in detail ===\n")
print(fl[event_id %like% "AT-100Metres-", .(event_id, gap0 = round(gap0, 1),
        gap1 = round(gap1, 1), offset_pct = round(off, 3))])

# --- C) the adjustment scale, per event: what does adj 1.5 actually cost? -----
# A is the headline, so it gets the per-event detail. adj 1.0 is the DEFAULT,
# not a value chosen on this held-out set, which is the only reason comparing
# the two here is legitimate rather than a second round of the same overfit.
cat("
=== C) per-event, held out: adj 1.5 (fitted) vs adj 1.0 (default) ===
")
e15 <- ev(frame(par)[date >= SPLIT])
e10 <- ev(frame(modifyList(par, list(adj = 1)))[date >= SPLIT])
cmp <- merge(e15[, .(event_id, family, races, gap15 = gap, beat15 = beat)],
             e10[, .(event_id, gap10 = gap, beat10 = beat, bias10 = bias_m)], by = "event_id")
cat("
-- events adj 1.0 rescues --
")
print(cmp[beat10 == TRUE & beat15 == FALSE][order(gap10),
      .(event_id, family, races, gap15 = round(gap15, 1), gap10 = round(gap10, 1))])
cat("
-- events adj 1.0 breaks --
")
print(cmp[beat10 == FALSE & beat15 == TRUE][order(-gap10),
      .(event_id, family, races, gap15 = round(gap15, 1), gap10 = round(gap10, 1))])
cat("
-- events still losing at adj 1.0 --
")
print(cmp[beat10 == FALSE][order(-gap10),
      .(event_id, family, races, gap15 = round(gap15, 1), gap10 = round(gap10, 1),
        bias = round(bias10, 3))])
fwrite(data.table(event_id = names(off_m), offset = as.numeric(off_m)),
       file.path(OUT, "marks_level_offsets.csv"))
say("wrote marks_level_offsets.csv")
