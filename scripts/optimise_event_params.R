# Per-event parameter optimisation, with a guard against fitting noise.
#
# THE EFFICIENT TRICK. Ratings are built sequentially across all events at once,
# so a per-event value cannot be tested by running one event. But a single
# GLOBAL run scores every event, so N global runs at N candidate values give,
# for each event, its concordance at each value - and the per-event optimum can
# be assembled from runs that already happened. N runs, not N x 85.
#
# This is approximate: cross-event blending couples events slightly, so an
# assembled configuration is not exactly what any single run measured. Step 5
# therefore RE-RUNS with the assembled overrides and checks the gain survives.
# An assembled optimum that does not reproduce was arithmetic, not a model.
#
# THE GUARD, which is the whole reason this is safe to do at all. Eighty-five
# events x several values will hand back a "winner" per event whether or not one
# exists - the 10,000m has ~2,600 pairs and a 0.84 pp noise floor, so a 0.5 pp
# "improvement" there is meaningless. So a winner is kept ONLY if:
#   1. it beats the global default on 2025 by more than that event's noise, AND
#   2. the SAME value also beats the default on the sealed 2026 window.
# Everything else keeps the global value, and the script reports how many failed
# each test - if most do, the honest read is that per-event tuning is not
# supported for that parameter and the global value should stand.
#
# Usage:
#   ARMS="tag=value,tag=value,..."  PARAM=xblend  Rscript optimise_event_params.R
# e.g. ARMS="xbh_0=0,xbh_1=1" PARAM=xblend
# Each tag needs seqv3_history_<tag>.parquet, i.e. that arm ran with SEQ_HIST=1.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D     <- here::here("citiusdata", "data")
PARAM <- Sys.getenv("PARAM", "xblend")
MINP  <- as.integer(Sys.getenv("MIN_PAIRS", "800"))
OUT   <- Sys.getenv("EVPARAM_OUT", file.path(D, sprintf("event_params_%s.parquet", PARAM)))
spec  <- strsplit(strsplit(Sys.getenv("ARMS", ""), ",")[[1]], "=")
stopifnot("ARMS must be tag=value pairs" = length(spec) >= 2)
arms <- data.table(tag = vapply(spec, `[`, "", 1L),
                   value = as.numeric(vapply(spec, `[`, "", 2L)))
cat(sprintf("optimising %s over %d candidate values: %s\n",
            PARAM, nrow(arms), paste(arms$value, collapse = ", ")))

# --- per-event concordance for one arm ---------------------------------------
score_arm <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) stop(sprintf("missing history for arm '%s'", tag))
  h <- setDT(read_parquet(f))
  if (!"r_use" %in% names(h)) h[, r_use := r_pre]
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
         year(date) %in% c(2025L, 2026L)]
  h[, rid := .GRP, by = race_key]
  a <- h[, .(rid, event_id, yr = year(date), i = seq_len(.N), place, r = r_use)]
  m <- merge(a, a, by = c("rid", "event_id", "yr"), allow.cartesian = TRUE,
             suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  d <- m$r.x - m$r.y
  m[, cw := as.numeric((d > 0) == (place.x < place.y))]
  m[d == 0, cw := 0.5]
  m[, .(tag = tag, pairs = .N, conc = 100 * mean(cw)), by = .(event_id, yr)]
}
sc <- rbindlist(lapply(arms$tag, score_arm))
sc <- merge(sc, arms, by = "tag")
sc[, noise := 100 * sqrt(0.75 * 0.25 / pairs)]

# the default is the FIRST arm listed - the value the engine uses today
base_tag <- arms$tag[1]; base_val <- arms$value[1]
b <- sc[tag == base_tag, .(event_id, yr, base = conc, pairs, noise)]
# take only tag/value/conc from the candidate arms: `pairs` and `noise` must come
# from ONE side, or the merge yields pairs.x/pairs.y and a bare `pairs` silently
# resolves to base R's pairs() FUNCTION instead of erroring on a missing column.
# The pair count is a property of the races, not of the parameter, so the base
# arm's count is the right one for every arm.
x <- merge(sc[tag != base_tag, .(tag, event_id, yr, conc, value)], b,
           by = c("event_id", "yr"))
stopifnot("merge produced no rows - do the arms share events?" = nrow(x) > 0,
          "pairs/noise collided in the merge" =
            all(c("pairs", "noise") %in% names(x)) &&
            !any(grepl("\\.(x|y)$", names(x))))
x[, delta := conc - base]

w25 <- x[yr == 2025 & pairs >= MINP]
setorder(w25, event_id, -delta)
best <- w25[, .SD[1], by = event_id]                    # best value per event on 2025
best <- best[delta > noise]                             # test 1: beats its own noise
chk <- merge(best[, .(event_id, value, d25 = delta)],
             x[yr == 2026, .(event_id, value, d26 = delta)],
             by = c("event_id", "value"))
keep <- chk[d26 > 0]                                    # test 2: replicates, sealed

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
rep <- merge(keep, reg, by = "event_id")
setorder(rep, -d25)
cat(sprintf("\nevents scored (>= %d pairs): %d\n", MINP, uniqueN(w25$event_id)))
cat(sprintf("  beat the global value on 2025 by more than noise: %d\n", nrow(best)))
cat(sprintf("  of those, ALSO better on the sealed 2026 window:  %d\n", nrow(keep)))
cat(sprintf("  discarded as unreplicated: %d\n", nrow(best) - nrow(keep)))
if (nrow(rep)) {
  cat("\n=== per-event overrides that survive both tests ===\n")
  print(rep[, .(discipline, sex, family, value, `2025` = round(d25, 3), `2026` = round(d26, 3))])
} else {
  cat("\nNOTHING survives both tests. The honest conclusion is that per-event\n")
  cat("tuning of this parameter is not supported by the data - keep the global.\n")
}
if (nrow(keep)) {
  o <- keep[, .(event_id, value)]
  setnames(o, "value", PARAM)
  write_parquet(o, OUT)
  cat(sprintf("\nwrote %s (%d events)\n", basename(OUT), nrow(o)))
  cat("VERIFY IT: re-run the engine with SEQ_EVPARAM pointing at that file. The\n")
  cat("assembled configuration is not what any single arm measured, because\n")
  cat("cross-event blending couples events - if the gain does not reproduce,\n")
  cat("the assembly was arithmetic rather than a model.\n")
}
