# Every event: the model against season best and personal best.
#
# SAME PAIRS FOR ALL THREE, always. A baseline scored on a different population
# is not a baseline - and the populations genuinely differ here, because a
# debutant has a rating but no season best and no personal best. Restricting to
# pairs where ALL THREE predictors exist for BOTH athletes is what makes the
# three columns comparable, and it is why these numbers are higher than the
# model's overall concordance: the pairs it finds hardest are exactly the ones
# with no baseline to compare against.
#
# WALK-FORWARD. Season best is the athlete's best mark EARLIER THIS SEASON;
# personal best is their best mark ever, before this race. Both lagged by one
# race, or the race being predicted enters its own predictor.
#
# SCORED ON r_use, the value the engine orders a field with. Scoring r_pre - the
# bare rating, before the ceiling and cross-event blends - is what made the
# hurdles look like a loss to season best on 2026-08-21 when they are a win.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("SBPB_SEALED", "2026")
TUNE <- .env_int("SBPB_TUNE",   "2025")
MINP <- .env_int("SBPB_MIN_PAIRS", "300")

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

setorder(h, athlete_id, event_id, date, race_key)
h[, sb := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]   # season best
h[, pb := shift(cummax(perf)), by = .(athlete_id, event_id)]       # personal best

score_event <- function(d) {
  d <- d[is.finite(sb) & is.finite(pb)]
  if (nrow(d) < 20) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, sb, pb), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (nrow(m) < MINP) return(NULL)
  won <- m$place.x < m$place.y
  cm <- fifelse(m$r_use.x == m$r_use.y, 0.5, as.numeric((m$r_use.x > m$r_use.y) == won))
  cs <- fifelse(m$sb.x    == m$sb.y,    0.5, as.numeric((m$sb.x    > m$sb.y)    == won))
  cp <- fifelse(m$pb.x    == m$pb.y,    0.5, as.numeric((m$pb.x    > m$pb.y)    == won))
  data.table(pairs = nrow(m),
             model = round(100 * mean(cm), 2),
             season_best = round(100 * mean(cs), 2),
             personal_best = round(100 * mean(cp), 2),
             vs_sb = round(100 * (mean(cm) - mean(cs)), 2),
             vs_pb = round(100 * (mean(cm) - mean(cp)), 2),
             floor = round(100 * sqrt(0.25 / nrow(m)), 2))
}

per <- function(y) {
  d <- h[yr == y]
  rbindlist(lapply(unique(d$event_id), function(ev) {
    r <- score_event(d[event_id == ev])
    if (is.null(r)) NULL else cbind(event_id = ev, r)
  }), fill = TRUE)
}

s <- per(SEAL); t <- per(TUNE)
stopifnot("nothing scored on the sealed window" = !is.null(s) && nrow(s) > 20)
x <- merge(s, t[, .(event_id, vs_sb_tune = vs_sb, vs_pb_tune = vs_pb)],
           by = "event_id", all.x = TRUE)
x <- merge(x, reg, by = "event_id", all.x = TRUE)
x[, event := sub("^AT-", "", event_id)]

cat(sprintf("%s events scored on %d, %s of them also on %d\n",
            format(nrow(x), big.mark = ","), SEAL,
            format(x[!is.na(vs_sb_tune), .N], big.mark = ","), TUNE))
cat(sprintf("\npooled over all events: model %.2f | season best %.2f | personal best %.2f\n",
            x[, stats::weighted.mean(model, pairs)],
            x[, stats::weighted.mean(season_best, pairs)],
            x[, stats::weighted.mean(personal_best, pairs)]))

cat("\n=== every event, sealed window, worst margin over season best first ===\n")
setorder(x, vs_sb)
print(x[, .(event, family, pairs, model, season_best, personal_best,
            vs_sb, vs_pb, floor, vs_sb_tune)], nrows = 100)

cat("\n=== by family ===\n")
print(x[, .(events = .N, pairs = sum(pairs),
            model = round(stats::weighted.mean(model, pairs), 2),
            season_best = round(stats::weighted.mean(season_best, pairs), 2),
            personal_best = round(stats::weighted.mean(personal_best, pairs), 2),
            vs_sb = round(stats::weighted.mean(vs_sb, pairs), 2),
            vs_pb = round(stats::weighted.mean(vs_pb, pairs), 2)),
        by = family][order(vs_sb)])

cat("\nvs_sb and vs_pb are the model MINUS that baseline, so positive is the\n")
cat("model winning. Compare against `floor`, and against vs_sb_tune - an event\n")
cat("that is negative on one window and positive on the other has told you\n")
cat("nothing except that it is small.\n")

f <- file.path(OUT, "event_vs_sb_pb.json")
writeLines(jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
