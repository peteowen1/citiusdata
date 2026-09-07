# WHERE does the model lose to last-5? Cut the same predictions every way there
# is sample to ask.
#
# Pete, 2026-09-07: "surely the two 100m have the same issue wherever that is".
# Both 100m events are the worst remaining held-out losses (+4.3% W, +3.0% M)
# after the blend and the parameter fit. This slices those predictions -- and,
# with CITIUS_CUTS_EVENTS=all, every event -- by everything available:
#
#   evidence    how many prior marks the athlete had, and their total weight
#   span        how far back the athlete's history in this event reaches
#   freshness   days since their last race before this one
#   standard    how fast they are (their own last-5, ranked within the event)
#   age         the athlete's age on race day
#   season      calendar month
#   context     the race's tier, round class, indoor flag, legality
#   wind        the target race's reading, where the event records one
#   disagree    how far the model and last-5 disagreed going in
#
# Each cut reports model MAE, last-5 MAE and the gap in percent, so a bin where
# we lose badly is a lead rather than a curiosity. The model here is the FITTED
# config from marks_fit.R -- blend included -- not the deployed one.
#
# A CUT IS NOT A CAUSE. A bin can lose because the model is wrong there or
# because last-5 is unusually strong there. The tail of the report prints the
# SIGNED residual per bin alongside the MAE, because a level bias and a spread
# problem need different fixes and the MAE alone cannot tell them apart.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_cuts.R'
# Env: CITIUS_CUTS_EVENTS  "AT-100Metres-M,AT-100Metres-W" (default) or "all"
#      CITIUS_LAB_CACHE, CITIUS_FIT_SPLIT (held-out rows only if set)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
EVS   <- Sys.getenv("CITIUS_CUTS_EVENTS", "AT-100Metres-M,AT-100Metres-W")
MINN  <- as.integer(Sys.getenv("CITIUS_CUTS_MINN", "25"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))
par   <- readRDS(file.path(OUT, "marks_fit_params.rds"))
# CITIUS_CUTS_PARAMS overrides any of them, e.g. "blend=0.6,hl=365,shrink=1".
# Use it to cut the SHIPPED configuration rather than the last fitted one --
# they differ, and a cut of the wrong model answers the wrong question.
if (nzchar(Sys.getenv("CITIUS_CUTS_PARAMS", ""))) {
  for (kv in strsplit(Sys.getenv("CITIUS_CUTS_PARAMS"), ",")[[1]]) {
    p2 <- strsplit(trimws(kv), "=")[[1]]
    stopifnot("CITIUS_CUTS_PARAMS wants name=value pairs" = length(p2) == 2L)
    stopifnot("unknown parameter name" = p2[1] %in% names(par))
    par[[p2[1]]] <- as.numeric(p2[2])
  }
}
say("model = fitted config: blend %.2f, half-life %g, trim %.2f, shrink %.2f, adjustment %.2f",
    par$blend, par$hl, par$trim, par$shrink, par$adj)

# --- reproduce the fitted model, exactly as marks_fit.R scores it -------------
pairs[, w := w_static * 0.5^(age_days / par$hl)]
pairs[, p_use := perf_raw + par$adj * (perf - perf_raw)]
keep <- if (par$trim <= 0) rep(TRUE, nrow(pairs)) else
  !(pairs$tactical & !is.na(pairs$rk) & pairs$rk <= floor(pairs$grp_n * par$trim))
r <- pairs[keep, .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w),
                   n_hist = .N, freshness = min(age_days), span = max(age_days)), by = pid]
m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
m[, kap := par$shrink * (sigma^2 / sigma_between^2)]
m[, ability := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]

d <- merge(merge(test, m[, .(athlete_id, event_id, month, ability, w_total, n_hist, freshness, span)],
                 by = c("athlete_id", "event_id", "month")),
           b5, by = c("athlete_id", "event_id", "date"))
d[, pred := (1 - par$blend) * ability + par$blend * base]
stopifnot("baseline missing on some rows" = all(is.finite(d$base)))
if (nzchar(Sys.getenv("CITIUS_FIT_SPLIT", ""))) d <- d[date >= as.Date(Sys.getenv("CITIUS_FIT_SPLIT"))]

# --- race context, from the results table ------------------------------------
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
ctx <- unique(ch[, .(race_key, athlete_id, tier, round, wind, indoor, legal, age,
                     orientation)], by = c("race_key", "athlete_id"))
rm(ch); invisible(gc())
d <- merge(d, ctx, by = c("race_key", "athlete_id"), all.x = TRUE)
if (!identical(EVS, "all")) d <- d[event_id %in% trimws(strsplit(EVS, ",")[[1]])]
stopifnot("no rows after filtering" = nrow(d) > 0)

# error in PERCENT of a mark, the unit the whole lab reports in
d[, `:=`(ae_m = 100 * abs(pred - act), ae_b = 100 * abs(base - act),
         se_m = 100 * (pred - act), se_b = 100 * (base - act))]
say("%s predictions, %s races, %d events, %s to %s",
    format(nrow(d), big.mark = ","), format(uniqueN(d$race_key), big.mark = ","),
    uniqueN(d$event_id), format(min(d$date)), format(max(d$date)))

# --- the cuts ----------------------------------------------------------------
qb <- function(x, n = 4, lab = NULL) {
  br <- unique(stats::quantile(x, seq(0, 1, length.out = n + 1), na.rm = TRUE))
  if (length(br) < 3) return(factor(rep("all", length(x))))
  cut(x, br, include.lowest = TRUE, labels = if (length(br) == n + 1) lab else NULL)
}
d[, `:=`(
  cut_evidence  = qb(n_hist),
  cut_weight    = qb(w_total),
  cut_span      = cut(span, c(-1, 365, 730, 1460, 1e6),
                      labels = c("<1yr", "1-2yr", "2-4yr", ">4yr")),
  cut_freshness = cut(freshness, c(-1, 14, 30, 90, 365, 1e6),
                      labels = c("<2wk", "2-4wk", "1-3mo", "3-12mo", ">1yr")),
  cut_standard  = {v <- ave(base, event_id, FUN = function(z) rank(-z) / length(z))
                   cut(v, c(0, .25, .5, .75, 1),
                       labels = c("best 25%", "2nd", "3rd", "weakest 25%"))},
  cut_age       = cut(age, c(0, 21, 24, 27, 30, 99),
                      labels = c("<=21", "22-24", "25-27", "28-30", "31+")),
  cut_month     = factor(format(date, "%m")),
  cut_tier      = fifelse(is.na(tier), "NA", as.character(tier)),
  cut_round     = citius:::.round_class(round),
  cut_indoor    = fifelse(is.na(indoor), "NA", fifelse(indoor, "indoor", "outdoor")),
  cut_legal     = fifelse(is.na(legal), "NA", fifelse(legal, "legal", "not legal")),
  cut_wind      = cut(wind, c(-99, -1, 0, 1, 2, 99),
                      labels = c("<-1.0", "-1..0", "0..1", "1..2", ">2.0")),
  cut_disagree  = qb(abs(ability - base), 4, c("agree most", "2nd", "3rd", "disagree most"))
)]

summ <- function(by) {
  s <- d[, .(n = .N, athletes = uniqueN(athlete_id), model = mean(ae_m), last5 = mean(ae_b),
             bias_m = mean(se_m), bias_b = mean(se_b)), by = by]
  s[, gap := round(100 * (model - last5) / last5, 1)]
  for (cn in c("model", "last5", "bias_m", "bias_b")) set(s, j = cn, value = round(s[[cn]], 3))
  setnames(s, by, "bin")
  s[n >= MINN][order(-gap)]
}
cuts <- grep("^cut_", names(d), value = TRUE)
for (cn in cuts) {
  cat(sprintf("\n=== %s ===\n", sub("^cut_", "", cn)))
  print(summ(cn))
}

# --- which cut actually separates? -------------------------------------------
# The spread of the gap across a cut's bins says how much that dimension
# explains. A cut where every bin loses by the same amount is not the lead --
# it is the event-wide loss showing up again.
cat("\n=== how much each cut separates (gap spread across its bins) ===\n")
sep <- rbindlist(lapply(cuts, function(cn) {
  s <- summ(cn)
  if (nrow(s) < 2) return(NULL)
  data.table(cut = sub("^cut_", "", cn), bins = nrow(s), n = sum(s$n),
             worst_bin = as.character(s$bin[1]), worst_gap = s$gap[1],
             best_bin = as.character(s$bin[nrow(s)]), best_gap = s$gap[nrow(s)],
             spread = round(s$gap[1] - s$gap[nrow(s)], 1))
}))
print(sep[order(-spread)])
cat(sprintf("\noverall: model %.3f vs last-5 %.3f (%+.1f%%), model bias %+.3f, last-5 bias %+.3f\n",
            mean(d$ae_m), mean(d$ae_b), 100 * (mean(d$ae_m) - mean(d$ae_b)) / mean(d$ae_b),
            mean(d$se_m), mean(d$se_b)))

if (identical(EVS, "all")) {
  for (cn in c("cut_standard", "cut_evidence", "cut_freshness")) {
    cat(sprintf("\n=== worst 12 event x %s cells ===\n", sub("^cut_", "", cn)))
    s <- d[, .(n = .N, model = mean(ae_m), last5 = mean(ae_b)), by = c("event_id", cn)]
    s[, gap := round(100 * (model - last5) / last5, 1)]
    print(head(s[n >= MINN][order(-gap)], 12))
  }
}
fwrite(d[, .(event_id, athlete_id, race_key, date, act, pred, base, ae_m, ae_b, se_m, se_b,
             n_hist, w_total, freshness, span, age, tier, round, wind, indoor, legal)],
       file.path(OUT, "marks_cuts_rows.csv"))
say("wrote marks_cuts_rows.csv (%s rows)", format(nrow(d), big.mark = ","))
