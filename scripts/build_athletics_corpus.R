# Assemble the athletics corpus from both World Athletics routes.
#
# WHY: the model currently estimates ability from `championship_results.rds` --
# 308k performances, ~3.5 per athlete. The career sweep holds 4.98M for the same
# 85k athletes, 58 per athlete, back to 1974. Ability estimated from 3 results is
# almost entirely prior; the depth is the single largest untapped asset here.
#
# IDENTITY IS FREE, UNLIKE SWIMMING. Both routes are the same feed, so
# `athlete_id` is one namespace and no crosswalk step is needed -- 85,039 of the
# career sweep's 85,045 athletes already appear in the competition harvest.
#
# THE TRAP THIS SCRIPT EXISTS TO HANDLE: the athlete endpoint silently DROPS
# results with no valid mark (documented in citius/CLAUDE.md -- an elite pole
# vaulter's 209-result career contains zero NA marks, which cannot be true).
# Fouls, no-heights, DNF and DNS survive only on the competition route. So the
# union must record WHICH route each row came from, and `foul_rate` must be
# measured on competition rows alone. Pooled, the career rows act as a mass of
# clean denominators and drive every foul rate toward zero -- silently, since
# nothing errors and the output still looks like a rate.
#
# Usage:  Rscript scripts/build_athletics_corpus.R
VERSE <- here::here()
suppressMessages({library(citius); library(data.table)})
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

parts <- list()

f <- file.path(D, "championship_results.rds")
comp <- setDT(readRDS(f))
comp[, source := "competition"]
parts$comp <- comp
say("competition harvest: %s rows | %s athletes | %s competitions",
    format(nrow(comp), big.mark = ","), format(uniqueN(comp$athlete_id), big.mark = ","),
    format(uniqueN(comp$competition_id), big.mark = ","))

f <- file.path(D, "athletics_history.rds")
car <- setDT(readRDS(f))
car[, source := "career"]
parts$car <- car
say("career sweep:        %s rows | %s athletes | %s..%s",
    format(nrow(car), big.mark = ","), format(uniqueN(car$athlete_id), big.mark = ","),
    min(car$date, na.rm = TRUE), max(car$date, na.rm = TRUE))

keep <- c("source", "athlete_id", "event_id", "discipline", "date",
          "competition_id", "comp_name", "round", "tier",
          "value_raw", "mark_string", "mark", "place", "is_technical",
          "wind", "indoor", "legal", "venue_country", "venue_city",
          "venue_stadium", "age", "sex", "orientation", "perf")
na_for <- list(source = NA_character_, athlete_id = NA_character_,
               event_id = NA_character_, discipline = NA_character_,
               date = as.Date(NA), competition_id = NA_character_,
               comp_name = NA_character_, round = NA_character_,
               tier = NA_character_, value_raw = NA_real_,
               mark_string = NA_character_, mark = NA_real_, place = NA_integer_,
               is_technical = NA, wind = NA_real_, indoor = NA, legal = NA,
               venue_country = NA_character_, venue_city = NA_character_,
               venue_stadium = NA_character_, age = NA_real_, sex = NA_character_,
               orientation = NA_real_, perf = NA_real_)
all <- rbindlist(lapply(parts, function(p) {
  for (m in setdiff(keep, names(p))) p[[m]] <- na_for[[m]]
  d <- p[, ..keep]
  d[, competition_id := as.character(competition_id)]
  d
}), fill = TRUE)
say("\ncombined: %s rows", format(nrow(all), big.mark = ","))

# ---- one row per performance -----------------------------------------------
# Match on WHAT HAPPENED -- athlete, date, event, mark, place -- because the same
# performance genuinely arrives by both routes and counting it twice inflates
# w_total, which REDUCES shrinkage. The athletes we cover best would then be the
# least regressed to the mean, which is backwards.
#
# `place` is in the key deliberately: a heat and a final on the same day at the
# same mark are two performances, and they differ in place far more reliably than
# in round (the career route's round labels are sparser).
before <- nrow(all)
all[, mark_r := round(mark, 4)]
# Keep the RICHEST row when a performance arrives twice. The competition route
# carries the round label, the venue and the no-mark; the career route carries
# neither reliably. Sorting only by source would keep whichever sorted first and
# discard the round label the context offsets depend on.
all[, richness := (!is.na(round)) + (!is.na(place)) + (!is.na(tier)) +
                  (!is.na(venue_city)) + (source == "competition")]
setorder(all, athlete_id, date, event_id, mark_r, -richness)
all <- unique(all, by = c("athlete_id", "date", "event_id", "mark_r", "place"))
say("deduped: %s -> %s rows (%s duplicate performance%s removed)",
    format(before, big.mark = ","), format(nrow(all), big.mark = ","),
    format(before - nrow(all), big.mark = ","),
    if (before - nrow(all) == 1) "" else "s")
all[, c("mark_r", "richness") := NULL]

# ---- recover race groupings ------------------------------------------------
# The career route stores no race_key -- 98.8% of its rows have none -- but it
# keeps competition_id, event_id, round and date, which is exactly what
# add_race_key() needs. Rebuilding it turns a per-athlete history into whole
# (partial) fields and is what makes the depth usable for race effects rather
# than ability alone.
#
# Fields are PARTIAL: a race is recovered only for the athletes the sweep
# happened to include. That makes each race effect noisier, not biased, and
# decompose_races() already de-biases the shared-shock variance for field size.
all <- add_race_key(all)
all[is.na(competition_id) | is.na(event_id) | is.na(date), race_key := NA_character_]
fld <- all[!is.na(race_key), .(k = uniqueN(athlete_id)), by = race_key]
say("\nraces recovered: %s (was %s in the competition harvest alone)",
    format(nrow(fld), big.mark = ","), format(uniqueN(comp$race_key), big.mark = ","))
say("  singleton races: %.1f%% -- these keep c_r = 0 and contribute ability only",
    100 * mean(fld$k == 1))
say("  median field %s | races with 5+ athletes: %s",
    median(fld$k), format(sum(fld$k >= 5), big.mark = ","))

# ---- provenance the variance estimators need -------------------------------
# `has_nomark` marks the rows where an absent mark is INFORMATION rather than an
# absence of data. Only the competition route can say a mark was missing;
# on career rows a missing mark means the feed dropped it.
all[, nomark_observable := source == "competition"]
nm <- all[nomark_observable == TRUE, mean(is.na(mark) | !nzchar(trimws(mark_string %||% "")))]
say("\nno-mark rate on competition rows: %.2f%% (career rows cannot see these and\n  are excluded from foul_rate by `nomark_observable`)", 100 * nm)

all[, scoreable := !is.na(race_key) & !is.na(place)]
say("scoreable rows: %s of %s", format(sum(all$scoreable), big.mark = ","),
    format(nrow(all), big.mark = ","))

say("\nby source after dedupe:")
print(all[, .(rows = .N, athletes = uniqueN(athlete_id)), by = source][order(-rows)])
d <- all[, .N, by = athlete_id]
say("\nathletes: %s | events: %s | results per athlete: median %s, 90th pct %s",
    format(uniqueN(all$athlete_id), big.mark = ","), uniqueN(all$event_id),
    median(d$N), round(quantile(d$N, 0.9)))

saveRDS(all, file.path(D, "athletics_corpus.rds"))
arrow::write_parquet(all, file.path(D, "athletics_corpus.parquet"))
say("\nwrote athletics_corpus.{rds,parquet}")
