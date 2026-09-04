# The hurdles deficit by depth, with the pairing the first version claimed.
#
# WHAT WAS WRONG. check_hurdles_deficit.R banded rows by n_eff and THEN formed
# pairs inside each band, so every pair had two athletes of MATCHED experience -
# a thin athlete racing a deep one was dropped from every band rather than
# counted in the thinner one. Its own header said "by depth of the THINNER
# athlete in the pair", which is a different and much larger population. The
# numbers were not wrong about what they measured; they were wrong about what
# they said they measured, and it is the header that was quoted onward.
#
# THE CLAIM AT STAKE: the model is negative against season best at 8-15
# effective races (-1.64) and only turns positive at 16+ (+2.02), where the
# sprints turn positive at 8-15 (+2.31) - "same curve, shifted right". That
# shift is the entire argument that the hurdles need more evidence before a
# rating beats a maximum, and it is what the fat-tail mechanism was invoked to
# explain. If it does not survive correct pairing, the mechanism explains a
# fact that was never there.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("HURDLE_SEALED_YEAR", "2026")
TUNE <- .env_int("HURDLE_TUNE_YEAR",   "2025")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
h[, yr := year(date)]
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
stopifnot("registry join produced almost nothing" =
            h[!is.na(family), .N] > 0.9 * nrow(h))

# Walk-forward season best: the athlete's best EARLIER THIS SEASON, lagged so the
# race being predicted cannot enter its own predictor.
setorder(h, athlete_id, event_id, date, race_key)
h[, sb := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]

# PAIR FIRST, BAND SECOND. This is the whole correction.
# `target_yr`, NOT `yr`. `yr` is a COLUMN of h, so a parameter of that name is
# shadowed inside h[...] and `yr == yr` compares the column to itself, matching
# every row and silently pooling both windows. This repo's rules file documents
# the trap and it was written into the first draft of this file anyway.
pairs_for <- function(fam, target_yr) {
  d <- h[family == fam & yr == target_yr & is.finite(sb)]
  if (nrow(d) < 200) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, sb, n_eff), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  won <- m$place.x < m$place.y
  m[, `:=`(cm = fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won)),
           cs = fifelse(sb.x    == sb.y,    0.5, as.numeric((sb.x    > sb.y)    == won)),
           # the DEPTH OF THE THINNER ATHLETE, which is what the band is meant
           # to mean: how much evidence the weaker-known side of the comparison
           # has, since that is where a rating should struggle against a maximum
           nmin = pmin(n_eff.x, n_eff.y))]
  m[]
}

band_it <- function(m) {
  if (is.null(m) || !nrow(m)) return(NULL)
  m[, band := cut(nmin, c(-Inf, 1, 3, 7, 15, Inf),
                  labels = c("<=1","2-3","4-7","8-15","16+"))]
  m[, .(pairs = .N,
        model = round(100 * mean(cm), 2),
        season_best = round(100 * mean(cs), 2),
        edge = round(100 * (mean(cm) - mean(cs)), 2),
        floor = round(100 * sqrt(0.25 / .N), 2)), by = band][order(band)]
}

for (yr in c(TUNE, SEAL)) {
  cat(sprintf("\n=== %d, HURDLES, banded on the thinner athlete of each pair ===\n", yr))
  print(band_it(pairs_for("hurdles", yr)))
  cat(sprintf("\n=== %d, SPRINT, the same (this is the comparison) ===\n", yr))
  print(band_it(pairs_for("sprint", yr)))
}

cat("\nThe claim being re-tested: hurdles stay negative at 8-15 while sprints\n")
cat("turn positive there. If both families now cross at the same band, the\n")
cat("'same curve shifted right' reading does not survive correct pairing, and\n")
cat("the fat-tail mechanism is explaining something that was an artefact of\n")
cat("comparing only athletes of matched experience.\n")

f <- file.path(OUT, "hurdles_deficit_v2.json")
writeLines(jsonlite::toJSON(list(
  tag = TAG,
  hurdles_tune = band_it(pairs_for("hurdles", TUNE)),
  hurdles_seal = band_it(pairs_for("hurdles", SEAL)),
  sprint_tune  = band_it(pairs_for("sprint",  TUNE)),
  sprint_seal  = band_it(pairs_for("sprint",  SEAL))),
  dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
