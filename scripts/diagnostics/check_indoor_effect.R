# What is an indoor mark worth against an outdoor one?
#
# The engine does not reference `indoor` anywhere, and the registry has no
# separate indoor events - so an 800m run on a 200m banked oval and one run on a
# 400m outdoor track are currently the same event with no adjustment between
# them. World Athletics does not agree: it labels them "800 Metres Short Track"
# and ranks them separately.
#
# The direction is not obvious and differs by event, which is why this measures
# rather than assumes. Middle distance indoors is slower - twice the turns, tighter
# radius, and no chance of a tailwind on the straight. Sprints are murkier: 60m is
# indoor-only so there is nothing to compare, and a banked 200m may be quicker
# than an unbanked outdoor bend for some athletes. Jumps and throws should be
# close to neutral, and if they are not that is a finding in itself.
#
# METHOD, the same one used for wind and venue: within-athlete demeaning. The
# outcome is a performance relative to what THAT athlete usually does in THAT
# event, so ability cancels and what remains is the surface. Athletes who compete
# both indoors and outdoors carry the comparison; an athlete who only ever races
# indoors contributes nothing and should not, because they offer no contrast.
#
# WHAT WOULD MAKE THIS WRONG. Indoor season is Jan-Mar and outdoor is May-Sep, so
# a raw indoor/outdoor gap partly measures WHERE IN THE SEASON an athlete is
# rather than the surface. That confound is real and is why the seasonal control
# below exists - restricting to athletes who did both within a single year, and
# reporting the gap with and without that restriction. If the two disagree, the
# unrestricted number is measuring the calendar.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
MINA <- .env_int("INDOOR_MIN_ATH", 3)     # marks per athlete-event before it counts
MINC <- .env_int("INDOOR_MIN_CELL", 200)  # marks per side before an event reports

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","date","perf","mark",
                                        "indoor","scoreable","tier")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf) & !is.na(indoor)]
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family,
                                                  orientation, unit)]
c0 <- merge(c0, reg, by = "event_id")
c0[, yr := year(date)]
cat(sprintf("corpus with a known surface: %s marks | indoor %.1f%%\n",
            format(nrow(c0), big.mark = ","), 100 * mean(c0$indoor)))
stopifnot("no indoor marks at all - the flag may not be populated" =
            sum(c0$indoor) > 1000)

# --- how many events even have both surfaces? ---------------------------------
cov <- c0[, .(marks = .N, indoor = sum(indoor), outdoor = sum(!indoor)), by = event_id]
cov[, both := indoor >= MINC & outdoor >= MINC]
cat(sprintf("events with %d+ marks on BOTH surfaces: %d of %d\n",
            MINC, sum(cov$both), nrow(cov)))
cat("Events failing that are indoor-only (60m, 60mH) or outdoor-only (javelin,\n")
cat("hammer, the roads) and need no adjustment - there is nothing to convert.\n")

# --- the effect, within athlete ------------------------------------------------
est <- function(x, lab) {
  x <- copy(x)
  x[, n_ath := .N, by = .(athlete_id, event_id)]
  x <- x[n_ath >= MINA]
  # an athlete must actually have raced BOTH ways, or they contribute only their
  # own level to one side and the comparison is between different people
  x[, has_both := uniqueN(indoor) == 2, by = .(athlete_id, event_id)]
  x <- x[has_both == TRUE]
  if (!nrow(x)) return(data.table())
  x[, y := perf - mean(perf), by = .(athlete_id, event_id)]
  r <- x[, .(athletes = uniqueN(athlete_id), marks = .N,
             gap = mean(y[indoor == TRUE]) - mean(y[!indoor]),
             n_in = sum(indoor), n_out = sum(!indoor)), by = event_id]
  r <- r[n_in >= 30 & n_out >= 30]
  r[, arm := lab][]
}
# Unrestricted, then restricted to athletes who did both IN THE SAME YEAR.
all_yr  <- est(c0, "all years")
c0[, both_same_yr := uniqueN(indoor) == 2, by = .(athlete_id, event_id, yr)]
same_yr <- est(c0[both_same_yr == TRUE], "same season")

r <- merge(all_yr[, .(event_id, athletes, marks, gap_all = gap)],
           same_yr[, .(event_id, gap_season = gap)], by = "event_id", all.x = TRUE)
r <- merge(r, reg, by = "event_id")
# gap is in perf space, where sign already carries orientation. Convert to a
# percentage of a mark so a time and a distance read the same way: positive =
# indoor is WORSE.
r[, indoor_pct := 100 * (exp(-gap_all) - 1)]
r[, season_pct := 100 * (exp(-gap_season) - 1)]
setorder(r, -indoor_pct)
cat(sprintf("\n=== indoor vs outdoor, within athlete (%d events) ===\n", nrow(r)))
print(r[, .(discipline, sex, family, athletes, marks,
            indoor_pct = round(indoor_pct, 2), season_pct = round(season_pct, 2))],
      nrows = 40)
cat("\nindoor_pct > 0 means an indoor mark is WORSE than the same athlete's\n")
cat("outdoor form by that percentage. season_pct repeats it using only athletes\n")
cat("who raced both surfaces within one year - if the two disagree badly, the\n")
cat("unrestricted figure is measuring the calendar rather than the surface.\n")

cat("\n=== by family ===\n")
fam <- r[, .(events = .N, athletes = sum(athletes),
             indoor_pct = round(stats::median(indoor_pct), 2),
             season_pct = round(stats::median(season_pct, na.rm = TRUE), 2)), by = family]
setorder(fam, -indoor_pct)
print(fam)

f <- file.path(D, "indoor_effect.json")
writeLines(jsonlite::toJSON(list(by_event = r, by_family = fam),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d events)\n", basename(f), nrow(r)))
