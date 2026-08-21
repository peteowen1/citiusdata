# WHERE does the model lose to season-best in the hurdles?
#
# The hurdles are the only family where sorting athletes by their season best
# beats the rating: 81.63% against 82.53% on 28,847 sealed-2026 pairs, a deficit
# of 0.90 points against a binomial floor around 0.25. Real, repeated across
# vintages, and unexplained.
#
# BEFORE PROPOSING A MECHANISM, LOCATE THE LOSS. A family-level number is an
# average over four events, three round types and every depth of record, and a
# deficit concentrated in one slice implies a completely different fix from one
# spread evenly. The project's own history is full of family-level numbers that
# hid opposite effects inside them.
#
# Five slices, each of which would point somewhere different:
#   event      - 110mH/100mH are one straight; 400mH is a full lap. If the loss
#                is only in the short hurdles it is about wind or stride pattern;
#                if only in the 400mH it is about pacing and lane draw.
#   depth      - the model is supposed to BEAT season-best on deep records and
#                lose on thin ones. If it loses on deep hurdles records too, the
#                usual explanation is gone.
#   round      - heats are jogged; if the deficit is all in heats, censoring is
#                under-doing its job here rather than the rating being wrong.
#   season     - season-best is a within-year statistic. Early-season pairs give
#                it little to work with, so a deficit that appears only late is a
#                different thing from one present in March.
#   field size - small fields are near-coin-flips for any method.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("HURDLE_SEALED_YEAR", "2026")

.hf <- file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))
stopifnot("no history for that FORM_TAG" = file.exists(.hf))
cat(sprintf("scoring %s\n", basename(.hf)))
h <- setDT(read_parquet(.hf))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
h[, yr := year(date)]

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
stopifnot("no family on the history rows" = h[!is.na(family), .N] > 0)

# --- season best, strictly walk-forward -------------------------------------
# The best mark this athlete has produced in this event EARLIER THIS SEASON.
# Anything else leaks the race being predicted into its own predictor.
setorder(h, athlete_id, event_id, date, race_key)
h[, sb := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]

hd <- h[family == "hurdles" & yr == SEAL & is.finite(sb)]
cat(sprintf("sealed %d hurdles rows with a prior season best: %s\n", SEAL,
            format(nrow(hd), big.mark = ",")))
stopifnot("no hurdles rows in the sealed window" = nrow(hd) > 1000)

# --- pairwise concordance, model against season best ------------------------
# Pairs are built INSIDE a race, and restricted to rows where BOTH predictors
# exist, or the two are being scored on different populations.
score <- function(d) {
  if (nrow(d) < 20) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_pre, sb), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  won <- m$place.x < m$place.y
  cm <- fifelse(m$r_pre.x == m$r_pre.y, 0.5, as.numeric((m$r_pre.x > m$r_pre.y) == won))
  cs <- fifelse(m$sb.x    == m$sb.y,    0.5, as.numeric((m$sb.x    > m$sb.y)    == won))
  n <- nrow(m)
  data.table(pairs = n,
             model = round(100 * mean(cm), 2),
             season_best = round(100 * mean(cs), 2),
             edge = round(100 * (mean(cm) - mean(cs)), 2),
             # the floor for a DIFFERENCE of two proportions on the same pairs is
             # narrower than for either alone, but quote the conservative one
             floor = round(100 * sqrt(0.25 / n), 2))
}

cat("\n=== overall, sealed hurdles ===\n")
print(score(hd))

cat("\n=== by event ===\n")
print(hd[, score(.SD), by = discipline][order(edge)])

cat("\n=== by depth of the THINNER athlete in the pair (n_eff bands) ===\n")
hd[, band := cut(n_eff, c(-Inf, 1, 3, 7, 15, Inf),
                 labels = c("<=1", "2-3", "4-7", "8-15", "16+"))]
print(hd[, score(.SD), by = band][order(band)])

cat("\n=== by round ===\n")
hd[, rnd := fifelse(grepl("final", rc, ignore.case = TRUE) &
                    !grepl("semi", rc, ignore.case = TRUE), "final",
                    fifelse(grepl("semi", rc, ignore.case = TRUE), "semi", "heat"))]
print(hd[, score(.SD), by = rnd][order(edge)])

cat("\n=== by part of season ===\n")
hd[, part := fifelse(month(date) <= 4, "Jan-Apr",
                     fifelse(month(date) <= 7, "May-Jul", "Aug-Dec"))]
print(hd[, score(.SD), by = part][order(part)])

cat("\n=== by field size ===\n")
hd[, fs := .N, by = race_key]
print(hd[, score(.SD), by = .(field = cut(fs, c(0, 4, 8, Inf),
                                          labels = c("2-4", "5-8", "9+")))][order(field)])

cat("\n=== the same cuts for SPRINT, as a control ===\n")
cat("Sprint is the next-weakest family (+0.57). If a slice behaves the same way\n")
cat("in both, it is not a hurdles fact - it is a fact about that slice.\n")
sp <- h[family == "sprint" & yr == SEAL & is.finite(sb)]
sp[, band := cut(n_eff, c(-Inf, 1, 3, 7, 15, Inf),
                 labels = c("<=1", "2-3", "4-7", "8-15", "16+"))]
print(sp[, score(.SD), by = band][order(band)])

f <- file.path(OUT, "hurdles_deficit.json")
writeLines(jsonlite::toJSON(list(
  tag = TAG, sealed = SEAL,
  overall  = score(hd),
  by_event = hd[, score(.SD), by = discipline],
  by_depth = hd[, score(.SD), by = band],
  by_round = hd[, score(.SD), by = rnd],
  sprint_by_depth = sp[, score(.SD), by = band]),
  dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
cat("\nRead `edge` against `floor`: negative and outside the floor is a real loss.\n")
