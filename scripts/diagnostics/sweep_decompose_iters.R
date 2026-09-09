# Is 400 sweeps a monotone waypoint or an optimum? Measure the middle.
#
# The deployed decomposition stops at max_iter = 400 and does not converge; the
# effects keep growing (+117% from sweep 400 to 2000, per calibrate.R:147). Two
# endpoints cannot tell a monotone trend from an optimum near 400, and those
# imply opposite actions -- go further, versus you have already gone too far.
# (Framing owed to BOUNCER, 2026-09-05: 400 is a hyperparameter nobody chose.)
#
# CHEAPER THAN REBUILDING CALIBRATIONS. A full calibrate() is 1-3 hours; the
# question is only about c_r, so this calls decompose_races() directly at a
# range of iteration counts, on a few events rather than the whole corpus.
#
# THE YARDSTICK IS NOT CONVERGENCE. It is whether c_r is the right SIZE, and
# that is measurable out of sample: regress what a field ACTUALLY averaged in
# LATER races on the fitted c_r. Slope 1.0 means correctly sized; 0.65 means
# 1.5x too big. So the output is a curve of slope against sweep count, and the
# best sweep count is the one whose slope is nearest 1, not the largest one.
#
# Usage:
#   CITIUS_SWEEP_ITERS=100,200,400,800,1600,3000 Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
EVENTS <- trimws(strsplit(Sys.getenv("CITIUS_SWEEP_EVENTS",
                                     "AT-100Metres-M,AT-200Metres-M,AT-ShotPut-M"), ",")[[1]])
ITERS <- as.integer(trimws(strsplit(Sys.getenv("CITIUS_SWEEP_ITERS",
                                               "100,200,400,800,1600,3000"), ",")[[1]]))
CENTRE <- Sys.getenv("CITIUS_SWEEP_CENTRE", "always")
MINF <- as.integer(Sys.getenv("CITIUS_SWEEP_MINFIELD", "6"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]

res <- rbindlist(lapply(EVENTS, function(EV) {
  ORI <- as.data.table(citius_events())[event_id == EV]$orientation[1]
  d <- ch[event_id == EV & is.finite(mark) & !is.na(race_key) & !is.na(athlete_id),
          .(athlete_id, race_key, date, mark, event_id)]
  if (nrow(d) < 5000) { say(sprintf("%s: only %d rows, skipping", EV, nrow(d))); return(NULL) }
  d[, perf := ORI * log(mark)]
  say(sprintf("%s: %s rows, %s races", EV, format(nrow(d), big.mark = ","),
              format(uniqueN(d$race_key), big.mark = ",")))

  rbindlist(lapply(ITERS, function(it) {
    t0 <- Sys.time()
    dec <- decompose_races(d, max_iter = it, min_race_size = 2L, centre = CENTRE)
    r <- as.data.table(dec$race)
    if (is.null(r) || !nrow(r)) return(NULL)
    # Same out-of-sample yardstick as fit_race_effect_scale.R: what the field
    # actually averaged LATER, regressed on the fitted effect.
    big <- r[n_in_race >= MINF]
    set.seed(11L)
    if (nrow(big) > 800L) big <- big[sample(.N, 800L)]
    fld <- merge(d, big[, .(race_key, c_r, n_in_race)], by = "race_key")
    agg <- rbindlist(lapply(split(fld, fld$race_key), function(g) {
      lat <- d[athlete_id %chin% g$athlete_id & date > g$date[1], .(m = mean(perf)), by = athlete_id]
      if (!nrow(lat)) return(NULL)
      gg <- merge(g[, .(athlete_id, perf)], lat, by = "athlete_id")
      if (!nrow(gg)) return(NULL)
      data.table(c_r = g$c_r[1], observed = mean(gg$perf) - mean(gg$m))
    }))
    if (is.null(agg) || nrow(agg) < 50) return(NULL)
    fit <- stats::lm(observed ~ c_r, data = agg)
    data.table(event = EV, iters = it,
               converged = isTRUE(dec$converged),
               delta = signif(if (is.null(dec$delta)) NA_real_ else dec$delta, 3),
               sd_c_r = round(sd(r$c_r, na.rm = TRUE), 5),
               slope = round(unname(coef(fit)[2]), 3),
               r2 = round(summary(fit)$r.squared, 3),
               races_scored = nrow(agg),
               secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }))
}))

cat("\n\nSWEEP COUNT vs EFFECT SIZE AND CORRECTNESS\n")
cat("slope 1.0 = c_r correctly sized | slope 0.65 = 1.5x too big\n")
cat("(the best sweep count is the one nearest slope 1, NOT the largest)\n\n")
print(res)
if (nrow(res)) {
  cat("\nper event, is it monotone or is there an optimum?\n")
  for (ev in unique(res$event)) {
    s <- res[event == ev][order(iters)]
    best <- s[which.min(abs(slope - 1))]
    cat(sprintf("  %-18s slope %s | closest to 1.0 at %d sweeps (slope %.3f)\n",
                ev, paste(sprintf("%.2f", s$slope), collapse = " -> "),
                best$iters, best$slope))
  }
}
