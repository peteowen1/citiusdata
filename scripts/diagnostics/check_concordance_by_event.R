# Per-EVENT concordance for any set of arms.
#
# Pete: "concordance should be done per event I think - we've got good baselines
# - now we can do per event optimisation".
#
# The aggregate metric is ~98% track, and it has now hidden two real effects:
# cross-event pooling was refuted twice on it, and the seed half-life question
# was answered on it before anyone asked whether road events behaved
# differently. A knob that helps the marathon and nothing else moves the
# aggregate by roughly nothing.
#
# Usage: ARMS="tag1,tag2" Rscript check_concordance_by_event.R
# Each tag needs seqv3_history_<tag>.parquet, i.e. the arm ran with SEQ_HIST=1.
#
# CAUTION ON WHAT THIS LICENSES. Per-event scoring is the right lens; per-event
# free PARAMETERS are 85 knobs fitted against events some of which hold nine
# races, and would report success on noise. The discipline is: optimise per
# event on 2025, then require the winner to replicate on the sealed 2026 window.
# An event whose best value does not hold out of sample was noise, and the
# `pairs` and `noise` columns below are there to make that judgeable rather than
# assumed.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D <- here::here("citiusdata", "data")
ARMS <- strsplit(Sys.getenv("ARMS", "final"), ",")[[1]]
MINP <- .env_int("MIN_PAIRS", "500")
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]

one_arm <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) { cat(sprintf("  (no history for '%s')\n", tag)); return(NULL) }
  h <- setDT(read_parquet(f))
  if (!"r_use" %in% names(h)) h[, r_use := r_pre]
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12]
  rbindlist(lapply(c(2025L, 2026L), function(Y) {
    x <- h[year(date) == Y]
    if (!nrow(x)) return(NULL)
    x[, rid := .GRP, by = race_key]
    a <- x[, .(rid, event_id, i = seq_len(.N), place, r = r_use)]
    m <- merge(a, a, by = c("rid", "event_id"), allow.cartesian = TRUE,
               suffixes = c(".x", ".y"))
    m <- m[i.x < i.y & place.x != place.y]
    d <- m$r.x - m$r.y
    m[, cw := as.numeric((d > 0) == (place.x < place.y))]
    m[d == 0, cw := 0.5]
    m[, .(arm = tag, window = Y, pairs = .N,
          conc = round(100 * mean(cw), 3)), by = event_id]
  }))
}
res <- rbindlist(lapply(ARMS, one_arm))
if (!nrow(res)) { cat("nothing to score\n"); quit(status = 0) }
res <- merge(res, reg, by = "event_id", all.x = TRUE)
# an optimistic noise floor, to stop a 0.3pp swing on 800 pairs reading as real
res[, noise := round(100 * sqrt(0.75 * 0.25 / pairs), 3)]

w <- dcast(res[pairs >= MINP], discipline + sex + family + window ~ arm,
           value.var = "conc")
np <- res[pairs >= MINP, .(pairs = max(pairs), noise = max(noise)),
          by = .(discipline, sex, window)]
w <- merge(w, np, by = c("discipline", "sex", "window"))
if (length(ARMS) == 2L) {
  w[, delta := round(get(ARMS[2]) - get(ARMS[1]), 3)]
  w[, beats_noise := abs(delta) > noise]
}
setorder(w, window, family, discipline, sex)
cat(sprintf("\n=== PER-EVENT CONCORDANCE (events with >= %d pairs) ===\n", MINP))
print(w[window == 2025], nrows = 100)
if (length(ARMS) == 2L) {
  cat("\n=== where the arms differ by more than that event's noise floor ===\n")
  d <- w[beats_noise == TRUE]
  setorder(d, -delta)
  print(d[, .(discipline, sex, window, pairs, noise, delta)], nrows = 60)
  cat(sprintf("\n2025: %d events better, %d worse (of %d beating noise)\n",
      d[window == 2025 & delta > 0, .N], d[window == 2025 & delta < 0, .N],
      d[window == 2025, .N]))
  rep <- merge(d[window == 2025, .(discipline, sex, d25 = delta)],
               w[window == 2026, .(discipline, sex, d26 = delta)],
               by = c("discipline", "sex"))
  if (nrow(rep)) cat(sprintf("of those, %d of %d keep the same sign on the sealed window\n",
      rep[sign(d25) == sign(d26), .N], nrow(rep)))
}
