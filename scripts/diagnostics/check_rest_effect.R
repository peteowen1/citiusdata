# DOES SHORT REST HURT PERFORMANCE, and should the model know about it?
#
# Pete's question: at a championship the heats, semi and final can fall within
# two or three days, sometimes two rounds on one day. The engine currently uses
# the gap between races only to DECAY a rating - how much to trust old evidence -
# and never to adjust the performance it EXPECTS. A same-day double is predicted
# as if the athlete were fresh.
#
# THE CONFOUND THAT WOULD FAKE THIS RESULT, and the reason for the control
# column below. Only athletes who run WELL advance to the next round. So every
# short-gap race is, by construction, preceded by an above-expectation run - and
# an above-expectation run regresses toward the mean next time out regardless of
# fatigue. Comparing raw residuals by gap would therefore show "fatigue" even if
# rest had no effect whatsoever. The fix is to condition on the previous race's
# residual: within a band of equally-good previous runs, does a shorter gap
# still cost time?
#
# Residual is (perf - r_pre) on the rating scale, oriented so HIGHER IS BETTER
# for every event. Negative means the athlete ran worse than their rating.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
TAG <- Sys.getenv("HIST_TAG", "final")
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","date","event_id","athlete_id",
                                       "r_pre","perf","place","rc","seen","n_eff")))
h <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf)]
cat(sprintf("starts with a prior rating: %s\n", format(nrow(h), big.mark = ",")))

setorder(h, athlete_id, date)
h[, resid := perf - r_pre]
# days since this athlete's previous race in ANY event, and that race's residual
h[, gap := as.integer(date - shift(date)), by = athlete_id]
h[, prev_resid := shift(resid), by = athlete_id]
h[, prev_rc := shift(rc), by = athlete_id]
g <- h[is.finite(gap) & gap >= 0 & gap <= 120 & is.finite(prev_resid)]
stopifnot("no rows with a previous race" = nrow(g) > 0)
cat(sprintf("starts with a previous race inside 120 days: %s\n",
            format(nrow(g), big.mark = ",")))

BK <- c(-1, 0, 1, 2, 4, 7, 14, 30, 60, 120)
LB <- c("same day", "1 day", "2 days", "3-4 days", "5-7 days", "8-14 days",
        "15-30 days", "31-60 days", "61-120 days")
g[, bucket := cut(gap, breaks = BK, labels = LB)]

cat("\n=== RAW residual by rest, which is the MISLEADING version ===\n")
cat("Short gaps look bad partly because advancing requires having run well.\n")
print(g[, .(starts = .N, mean_resid = round(mean(resid), 4),
            median_prev = round(median(prev_resid), 4)), by = bucket][order(bucket)])

cat("\n=== CONTROLLED: within bands of equally-good previous runs ===\n")
cat("If rest matters, short gaps stay negative INSIDE a band, not just across.\n")
g[, prev_band := cut(prev_resid, breaks = stats::quantile(prev_resid, 0:4/4, na.rm = TRUE),
                     labels = c("prev worst 25%","prev 25-50%","prev 50-75%","prev best 25%"),
                     include.lowest = TRUE)]
ct <- dcast(g[!is.na(prev_band)], bucket ~ prev_band,
            value.var = "resid", fun.aggregate = function(x) round(mean(x), 4))
print(ct)

cat("\n=== a regression that controls for it directly ===\n")
g[, short := as.integer(gap <= 2)]
m <- stats::lm(resid ~ short + prev_resid + factor(rc), data = g)
print(round(summary(m)$coefficients[, c(1, 2, 4)], 5))
cat("\n'short' is the effect of racing within 2 days, holding the previous\n")
cat("performance and the round type constant. Negative = short rest costs.\n")

cat("\n=== by family: who does it hurt? ===\n")
reg <- as.data.table(citius::citius_events())[, .(event_id, family, discipline, sex)]
gg <- merge(g, reg, by = "event_id")
fam <- gg[, {
  if (.N >= 200 && length(unique(short)) > 1) {
    mm <- stats::lm(resid ~ short + prev_resid)
    .(starts = .N, short_effect = round(stats::coef(mm)["short"], 4),
      se = round(summary(mm)$coefficients["short", "Std. Error"], 4))
  } else .(starts = .N, short_effect = NA_real_, se = NA_real_)
}, by = family]
setorder(fam, short_effect)
print(fam)
cat("\nA rest effect should be LARGEST where a race is most exhausting -\n")
cat("distance and middle - and smallest in the throws and jumps, where a\n")
cat("round costs a few seconds of effort. If it comes out the other way\n")
cat("round, the result is not fatigue and I should not call it that.\n")
