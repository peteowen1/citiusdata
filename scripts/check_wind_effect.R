# How much does wind actually move a mark, and does the effect taper?
#
# WHY A SMOOTH RATHER THAN A SLOPE. Everyone quotes wind as a linear rate - "about
# 0.05s per m/s in the 100m" - and the legality rule (+2.0) is a hard cliff. The
# physics says neither is right: drag benefit should SATURATE, because once you
# are being pushed, further push adds less and less, and a headwind should hurt
# more than the same tailwind helps. A GAM smooth can show that if it is there,
# and can show it is absent if it is not.
#
# THE IDENTIFICATION PROBLEM, and it is the whole design. Wind correlates with
# who is racing and where. So the outcome is not the mark - it is the mark
# relative to WHAT THAT ATHLETE USUALLY DOES in that event: perf minus the
# athlete-event mean. That absorbs ability entirely and leaves the within-athlete
# question, which is the one that matters: given this athlete, how much faster is
# this run because of the wind?
#
# WHAT MUST NOT BE DONE HERE. The engine's race shock removes what the field
# shared on the day - and wind is precisely that. Subtracting it would delete the
# effect being measured. This works from raw marks on purpose.
#
# KNOWN LIMITS, stated because they bound the conclusion:
#  - ALTITUDE is not in the corpus at all (we hold venue_city, not elevation), so
#    a high-altitude meet looks like a fast day. Altitude and tailwind are not
#    correlated, so this inflates the residual rather than biasing the smooth,
#    but it means the curve is "wind plus whatever else made that day fast".
#  - Wind is measured per RACE for track and per ATTEMPT for horizontal jumps.
#  - Nothing here corrects for the athlete's own trend across a career; the
#    demeaning is a level, not a trajectory.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
stopifnot("mgcv is required" = requireNamespace("mgcv", quietly = TRUE))
D      <- here::here("citiusdata", "data")
MINN   <- as.integer(Sys.getenv("WIND_MIN_MARKS", "3"))   # marks per athlete-event
WLO    <- as.numeric(Sys.getenv("WIND_LO", "-6"))
WHI    <- as.numeric(Sys.getenv("WIND_HI", "8"))
K      <- as.integer(Sys.getenv("WIND_K", "8"))

EVENTS <- c("AT-100Metres-M","AT-100Metres-W","AT-200Metres-M","AT-200Metres-W",
            "AT-110MetresHurdles-M","AT-100MetresHurdles-W",
            "AT-LongJump-M","AT-LongJump-W","AT-TripleJump-M","AT-TripleJump-W")
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, orientation, unit)]

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","mark","perf","date",
                                        "wind","legal","indoor","scoreable")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[event_id %chin% EVENTS & scoreable == TRUE & is.finite(mark) & mark > 0 &
         is.finite(perf) & is.finite(wind) & wind >= WLO & wind <= WHI &
         (is.na(indoor) | indoor == FALSE)]
c0[, n_ath := .N, by = .(athlete_id, event_id)]
c0 <- c0[n_ath >= MINN]
cat(sprintf("performances with a wind reading: %s over %s athlete-events, %d events\n",
            format(nrow(c0), big.mark = ","),
            format(uniqueN(paste(c0$athlete_id, c0$event_id)), big.mark = ","),
            uniqueN(c0$event_id)))
stopifnot("no usable performances" = nrow(c0) > 1000)

# THE OUTCOME: performance relative to this athlete's own level in this event.
c0[, y := perf - mean(perf), by = .(athlete_id, event_id)]
cat(sprintf("within-athlete deviation: sd %.4f, i.e. about %.2f%% of a mark\n",
            stats::sd(c0$y), 100 * (exp(stats::sd(c0$y)) - 1)))

# --- fit one smooth per event -------------------------------------------------
grid_w <- seq(-4, 6, by = 0.25)
curves <- list(); summ <- list()
for (EV in EVENTS) {
  dd <- c0[event_id == EV]
  if (nrow(dd) < 500) { cat(sprintf("SKIP %s: only %d rows\n", EV, nrow(dd))); next }
  m <- mgcv::gam(y ~ s(wind, k = K), data = dd, method = "REML")
  p <- stats::predict(m, newdata = data.frame(wind = grid_w), se.fit = TRUE)
  p0 <- stats::predict(m, newdata = data.frame(wind = 0))[[1]]
  o  <- reg[event_id == EV, orientation]
  un <- reg[event_id == EV, unit]
  # reference mark = the median mark in the event, so the curve is readable in
  # the units a reader thinks in
  ref <- stats::median(dd$mark)
  # y is in perf space (higher = better). Convert a perf gain to a mark change.
  eff <- p$fit - p0
  mk  <- ref * exp(if (o == -1) -eff else eff)
  curves[[EV]] <- data.table(
    event_id = EV, discipline = reg[event_id == EV, discipline],
    sex = reg[event_id == EV, sex], unit = un, ref_mark = ref,
    wind = grid_w,
    delta_pct = 100 * (exp(eff) - 1),
    mark = mk, delta_mark = mk - ref,
    lo = ref * exp(if (o == -1) -(eff + 1.96*p$se.fit) else (eff - 1.96*p$se.fit)),
    hi = ref * exp(if (o == -1) -(eff - 1.96*p$se.fit) else (eff + 1.96*p$se.fit)))
  # marginal effect per +1 m/s at several wind levels - this is where tapering
  # would show up
  at <- c(-2, -1, 0, 1, 2, 3, 4)
  pm <- stats::predict(m, newdata = data.frame(wind = c(at - 0.5, at + 0.5)))
  half <- length(at)
  slope_perf <- pm[(half+1):(2*half)] - pm[1:half]
  summ[[EV]] <- data.table(
    event_id = EV, discipline = reg[event_id == EV, discipline],
    sex = reg[event_id == EV, sex], n = nrow(dd), edf = round(sum(m$edf), 2),
    dev_expl = round(summary(m)$dev.expl, 4), ref_mark = round(ref, 2),
    wind = at,
    per_ms_mark = ref * (exp(if (o == -1) -slope_perf else slope_perf) - 1))
}
stopifnot("no event produced a fit" = length(curves) > 0)
cv <- rbindlist(curves); sm <- rbindlist(summ)

cat("\n=== how much does +1 m/s buy, at different wind levels? ===\n")
cat("(negative = faster for track, positive = further for jumps)\n")
w <- dcast(sm, discipline + sex + n + edf ~ wind, value.var = "per_ms_mark")
num <- setdiff(names(w), c("discipline","sex","n","edf"))
w[, (num) := lapply(.SD, function(x) round(x, 3)), .SDcols = num]
print(w)
cat("\nReading it: if the number SHRINKS as the wind column goes right, the\n")
cat("benefit is tapering. If it is flat, wind is linear and the usual single\n")
cat("coefficient is fine.\n")

cat("\n=== is a headwind worse than a tailwind is good? ===\n")
asym <- merge(cv[wind == -2, .(event_id, discipline, sex, head2 = delta_mark)],
              cv[wind ==  2, .(event_id, tail2 = delta_mark)], by = "event_id")
asym[, ratio := round(abs(head2) / abs(tail2), 2)]
print(asym[, .(discipline, sex, head_2ms = round(head2, 3),
               tail_2ms = round(tail2, 3), ratio)])
cat("ratio > 1 means a 2 m/s headwind costs more than a 2 m/s tailwind gives.\n")

f <- file.path(D, "wind_effect_curves.json")
writeLines(jsonlite::toJSON(list(
  curves = cv, marginal = sm,
  meta = list(rows = nrow(c0), min_marks = MINN, k = K,
              fitted = format(Sys.Date()))), dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d curve points over %d events)\n",
            basename(f), nrow(cv), uniqueN(cv$event_id)))
