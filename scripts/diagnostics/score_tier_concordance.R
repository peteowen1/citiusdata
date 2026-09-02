# CONCORDANCE AT ELITE LEVEL ONLY - model against season best and personal best.
#
# WHY RESTRICT BY TIER. The pooled concordance figure is dominated by T3, which
# is ~27,000 of the ~32,000 competitions in the catalogue - domestic and
# development meets where fields are strung out and almost any predictor gets
# the ordering right. That inflates every method at once and compresses the gaps
# between them, so it measures the easy population and hides the one the project
# is actually for. T1 is 250 competitions: championships, Diamond League and the
# like, where the athletes are close together and ordering them is the real
# problem. LA 2028 projections live entirely in that population.
#
# EXPECT EVERY NUMBER TO BE LOWER HERE, INCLUDING THE MODEL'S. A T1-only figure
# is not the pooled figure with noise removed; it is a harder question. The thing
# to read is the GAP between the model and the simple baselines, not the level.
#
# SEASON BEST AND PERSONAL BEST ARE COMPUTED ON THE FULL HISTORY, then the
# SCORING is restricted to T1 races. An athlete's season best comes from every
# race they ran, not only their elite ones - computing it within the restricted
# set would invent a statistic nobody has and would flatter the model, since the
# baseline would be missing marks the model itself saw.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
TIER <- Sys.getenv("TIER_ONLY", "T1_elite")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
# r_use, not r_pre. r_pre is the bare rating; r_use is what the engine orders a
# field with, and scoring the wrong one has already inverted four conclusions
# in this project.
h <- h[is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
h[, date := as.Date(date)]
h[, yr := year(date)]

# --- walk-forward season best and personal best -----------------------------
# shift() BEFORE the race, so neither predictor contains the mark it is being
# used to predict. cummax is correct because perf is oriented log - higher is
# better in every event, including the throws and the timed ones.
setorder(h, athlete_id, event_id, date, race_key)
h[, sb := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]
h[, pb := shift(cummax(perf)), by = .(athlete_id, event_id)]

# --- tier ---------------------------------------------------------------
# competition_id is the first field of race_key (source_athletics.R builds it as
# competition_id|event|name|race|discriminator).
BAR <- "|"
h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h <- merge(h, cat0[, .(competition_id, meet_tier, is_major)],
           by = "competition_id", all.x = TRUE)

# THE TIER JOIN MUST ACTUALLY LAND. If race_key's first field stopped being the
# competition id, every row would come back with meet_tier NA, the T1 subset
# would be empty, and `all(logical(0))` style vacuity would let a downstream
# check pass on nothing. Assert a population before asserting a property - the
# Olympics-in-T2 anchor failed exactly this way.
cat(sprintf("tier join: %s of %s rows carry a tier (%.1f%%)\n",
            format(h[!is.na(meet_tier), .N], big.mark = ","),
            format(nrow(h), big.mark = ","),
            100 * h[, mean(!is.na(meet_tier))]))
stopifnot("the tier join landed on nothing - check the race_key format" =
            h[!is.na(meet_tier), .N] > 1000)
print(h[, .(rows = .N), by = meet_tier][order(meet_tier)])

# --- scoring ----------------------------------------------------------------
# SAME PAIRS FOR ALL THREE METHODS. Each is restricted to rows where all three
# predictors exist, so they are compared on one population. Scoring the model on
# every pair and season best only on pairs that have a season best would compare
# two methods on two different fields, and the model would win on the strength
# of the easier one.
score <- function(d) {
  d <- d[is.finite(sb) & is.finite(pb)]
  if (nrow(d) < 40) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, sb, pb), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (nrow(m) < 40) return(NULL)
  won <- m$place.x < m$place.y
  cc <- function(x, y) fifelse(x == y, 0.5, as.numeric((x > y) == won))
  cm <- cc(m$r_use.x, m$r_use.y); cs <- cc(m$sb.x, m$sb.y); cp <- cc(m$pb.x, m$pb.y)
  n <- nrow(m)
  data.table(pairs = n,
             model = round(100 * mean(cm), 2),
             seas_best = round(100 * mean(cs), 2),
             pers_best = round(100 * mean(cp), 2),
             vs_sb = round(100 * (mean(cm) - mean(cs)), 2),
             vs_pb = round(100 * (mean(cm) - mean(cp)), 2),
             floor = round(100 * sqrt(0.25 / n), 2))
}

cat(sprintf("\n=== %s only ===\n", TIER))
t1 <- h[meet_tier == TIER]
stopifnot("no rows at that tier" = nrow(t1) > 0)
cat(sprintf("%s scored rows across %s competitions\n",
            format(nrow(t1), big.mark = ","),
            format(t1[, uniqueN(competition_id)], big.mark = ",")))

cat("\n-- by season, so a single-window result cannot be read as a trend --\n")
print(t1[yr >= 2021, score(.SD), by = yr][order(yr)])

cat("\n-- pooled 2021+ --\n")
print(t1[yr >= 2021, score(.SD)])

cat("\n-- majors only, the hardest fields we have --\n")
mj <- h[is_major == TRUE & yr >= 2021]
if (nrow(mj) > 0) print(score(mj)) else cat("no major rows\n")

cat("\n-- for contrast, the other tiers pooled 2021+ --\n")
print(h[yr >= 2021 & !is.na(meet_tier), score(.SD), by = meet_tier][order(meet_tier)])

cat(sprintf("\n=== %s by event, smallest edge over season best first ===\n", TIER))
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline)]
t1 <- merge(t1, reg, by = "event_id", all.x = TRUE)
ev <- t1[yr >= 2021, score(.SD), by = .(discipline)][pairs >= 300][order(vs_sb)]
# 60+ disciplines against a one-sigma floor: several will sit beyond one floor
# on noise alone. Read the ordering, not any single row, and confirm anything
# interesting in a second window before acting on it.
print(head(ev, 15))

f <- file.path(D, "tier_concordance.json")
writeLines(jsonlite::toJSON(list(
  tag = TAG, tier = TIER,
  pooled = t1[yr >= 2021, score(.SD)],
  by_year = t1[yr >= 2021, score(.SD), by = yr],
  by_event = ev), dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
