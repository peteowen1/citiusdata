# Which events have HEADROOM, as opposed to merely being hard?
#
# THE DISTINCTION THIS EXISTS TO MAKE. A low per-event concordance is not a
# to-do item. Some events are intrinsically less predictable - a tactical
# 1500m final is a coin-flip in a way a shot put is not - and no amount of
# modelling makes an unpredictable race predictable. What identifies real
# headroom is the model losing to, or barely beating, a SIMPLE alternative on
# that same event: season best, the mean of the last three, the last result, or
# the career best. If a stopwatch and a sort beat the model, that is a defect.
# If both are at 70% because the event is chaotic, that is the sport.
#
# SCORED ON r_use, the value the engine orders a field with, not on r_pre. That
# distinction inverted three published claims on 2026-08-21, including "the
# hurdles lose to season best" - which was the premise of an entire evening's
# work and was false.
#
# ON OVERFITTING, WHICH IS THE REAL RISK HERE. There are ~84 rated events. Under
# a null of no per-event effect, roughly a QUARTER of them look good on two
# independent windows by chance - about 21 events. The engine already records
# per-FAMILY parameters being adopted and reverted for exactly this reason at
# nine families, where 4 of 9 "survivors" sat against 2.25 expected. So this
# script reports both windows side by side and demands agreement in SIGN, and it
# reports the noise floor next to every number. An event is a candidate only if
# it trails on BOTH windows by more than its floor. Nothing here should be tuned
# per event on one window.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("HEADROOM_SEALED", "2026")
TUNE <- .env_int("HEADROOM_TUNE",   "2025")
MINP <- .env_int("HEADROOM_MIN_PAIRS", "400")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
h[, yr := year(date)]
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)

# merged races out - a shared place with different marks is parallel sections
dup <- h[, .(n = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
         n > 1 & marks > 1, unique(race_key)]
h <- h[!race_key %chin% dup]

# WALK-FORWARD BASELINES, each strictly from races before this one.
setorder(h, athlete_id, event_id, date, race_key)
h[, p_seasbest := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]
h[, p_best     := shift(cummax(perf)), by = .(athlete_id, event_id)]
h[, p_last     := shift(perf),         by = .(athlete_id, event_id)]
h[, p_mean3    := shift(frollmean(perf, 3, align = "right", na.rm = TRUE)),
  by = .(athlete_id, event_id)]

BASE <- c("p_seasbest", "p_best", "p_last", "p_mean3")

conc <- function(d, col) {
  a <- d[is.finite(get(col)), .(rid = .GRP, i = seq_len(.N), place,
                                m = r_use, b = get(col)), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (nrow(m) < MINP) return(NULL)
  won <- m$place.x < m$place.y
  cm <- fifelse(m$m.x == m$m.y, 0.5, as.numeric((m$m.x > m$m.y) == won))
  cb <- fifelse(m$b.x == m$b.y, 0.5, as.numeric((m$b.x > m$b.y) == won))
  # SAME PAIRS FOR BOTH, always - a baseline scored on a different population is
  # not a baseline. That is why `b` is carried into the pair table rather than
  # scored separately.
  data.table(pairs = nrow(m), model = 100 * mean(cm), base = 100 * mean(cb))
}

per_event <- function(yr_target) {
  d <- h[yr == yr_target]
  evs <- d[, .N, by = event_id][N >= 20, event_id]
  rbindlist(lapply(evs, function(ev) {
    x <- d[event_id == ev]
    rows <- rbindlist(lapply(BASE, function(b) {
      r <- conc(x, b); if (is.null(r)) NULL else cbind(baseline = b, r)
    }), fill = TRUE)
    if (!nrow(rows)) return(NULL)
    # the STRONGEST simple alternative for this event, which is what the model
    # has to beat to be worth having
    best <- rows[which.max(base)]
    data.table(event_id = ev, pairs = best$pairs,
               model = round(best$model, 2),
               best_baseline = best$baseline,
               base = round(best$base, 2),
               edge = round(best$model - best$base, 2),
               floor = round(100 * sqrt(0.25 / best$pairs), 2))
  }), fill = TRUE)
}

s <- per_event(SEAL); t <- per_event(TUNE)
stopifnot("no events scored on the sealed window" = !is.null(s) && nrow(s) > 20)
x <- merge(s, t, by = "event_id", suffixes = c("_seal", "_tune"))
x <- merge(x, reg, by = "event_id", all.x = TRUE)
cat(sprintf("events scored on both windows: %s\n", format(nrow(x), big.mark = ",")))

# A CANDIDATE trails on BOTH windows by more than its own floor. One window is
# not evidence - about a quarter of 84 events clear a single window by chance.
x[, candidate := edge_seal < -floor_seal & edge_tune < -floor_tune]
cat(sprintf("events where the model TRAILS its best simple baseline on both windows: %d\n",
            x[candidate == TRUE, .N]))
cat("\n=== the candidates, worst first ===\n")
if (x[candidate == TRUE, .N]) {
  print(x[candidate == TRUE][order(edge_seal),
        .(event_id, family, pairs_seal, model_seal, best_baseline_seal, base_seal,
          edge_seal, floor_seal, edge_tune, floor_tune)])
} else cat("  none. No event trails a simple baseline on both windows.\n")

cat("\n=== narrowest margins that are still positive (watch list) ===\n")
print(x[candidate == FALSE][order(edge_seal)][seq_len(min(12L, .N)),
      .(event_id, family, pairs_seal, model_seal, best_baseline_seal,
        edge_seal, floor_seal, edge_tune)])

cat("\n=== and the events where the model is furthest ahead, for contrast ===\n")
print(x[order(-edge_seal)][seq_len(min(6L, .N)),
      .(event_id, family, pairs_seal, model_seal, edge_seal, edge_tune)])

cat(sprintf("\nweighted mean edge over the best simple baseline: %.2f pp (sealed)\n",
            x[, stats::weighted.mean(edge_seal, pairs_seal)]))
f <- file.path(OUT, "event_headroom.json")
writeLines(jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("wrote %s\n", basename(f)))
