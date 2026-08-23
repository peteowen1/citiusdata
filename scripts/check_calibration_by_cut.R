# CALIBRATION AND BIAS BY CUT - the check that would have caught the debutant
# problem without anyone having to think to look for it.
#
# WHY THIS EXISTS. The project is tuned against a tier-weighted concordance
# score, and a concordance score is an average taken across every pair the
# model saw. An average can look healthy while one slice underneath it is
# badly wrong, because the slices that are easy and numerous drown out the
# slice that is hard and rare. That happened for real on 2026-08-22: debutants
# were being seeded 1.553 standard deviations too high on 17% of rows, and it
# was invisible in every aggregate number the project had, because 83% of rows
# were unaffected and pulled the average back to looking fine. Cutting the same
# pairs by cold-versus-established showed the debut fix was worth +6.165 where
# the headline reported only +1.042. This script exists so that class of
# problem shows up on its own, every run, instead of depending on someone
# remembering to slice the data that particular way.
#
# WHAT A BAD RESULT MEANS. One cut sitting well below its neighbours, further
# than the noise floor printed next to it can explain, means the model is wrong
# in a specific population - a round, a tier, a season, an evidence band - not
# that the model is bad everywhere. The fix belongs there, not in a global
# retune that would move populations the aggregate never told you were fine.
#
# WHAT CONCORDANCE CANNOT SEE. Concordance only asks whether the model got the
# order of two athletes right. It cannot tell the difference between a model
# that is barely more sure than a coin flip and one that is wildly overconfident
# in the same direction, because both get full credit for getting the order
# right. The calibration section at the end is the check for that: it buckets
# pairs by how big a gap the model puts between two athletes, fits one global
# curve translating that gap into a predicted win rate, and compares the
# predicted rate against what the favourite actually did in each bucket. A
# model that is confident where it has not earned it will show up there even
# though it never reorders a single pair.
#
# WHY EVERY CELL BELOW 200 PAIRS IS SKIPPED AND COUNTED, NOT SILENTLY DROPPED.
# The noise floor at a handful of pairs is enormous, and a check that reports a
# number for a thin cell invites someone to read meaning into noise. Printing
# the skip count keeps that temptation out of the table instead of trusting
# whoever reads it to notice the cell was thin.
#
# SCORING USES r_use, NEVER r_pre. r_pre is the bare rating the engine learns
# from; r_use is what it actually orders a field with, and scoring r_pre by
# mistake has inverted four conclusions in this project already.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))

D        <- here::here("citiusdata", "data")
TAG      <- Sys.getenv("FORM_TAG", "final")
MIN_CELL <- 200L

f <- file.path(D, sprintf("seqv3_history_%s.parquet", TAG))
if (!file.exists(f))
  stop(sprintf("missing history file for FORM_TAG='%s': %s", TAG, f), call. = FALSE)

h <- setDT(read_parquet(f, col_select = c(
  "race_key", "date", "event_id", "athlete_id",
  "r_pre", "r_use", "perf", "place", "seen", "n_eff", "rc"
)))
stopifnot("history file loaded with zero rows - check FORM_TAG and the file path" =
            nrow(h) > 0)

if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]

h <- h[is.finite(r_use) & is.finite(place) & place > 0]
stopifnot("nothing survived the finite-rating / valid-place filter" = nrow(h) > 0)
h[, date := as.Date(date)]
h[, yr := year(date)]

cat(sprintf("=== calibration and bias by cut: FORM_TAG='%s', %s rows, %s to %s ===\n",
            TAG, format(nrow(h), big.mark = ","),
            as.character(min(h$date)), as.character(max(h$date))))

# --- tier, via competition_catalogue joined on the first field of race_key --
# race_key is built as competition_id|event|name|race|discriminator
# (source_athletics.R), so competition_id is always its first "|"-delimited
# field. If that stops being true the join lands on nothing and the check
# below catches it - the same failure mode documented in
# score_tier_concordance.R and in the Olympics-in-T2 anchor incident.
BAR <- "|"
h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h <- merge(h, cat0[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
cat(sprintf("tier join: %s of %s rows carry a tier (%.1f%%)\n",
            format(h[!is.na(meet_tier), .N], big.mark = ","),
            format(nrow(h), big.mark = ","), 100 * h[, mean(!is.na(meet_tier))]))
stopifnot("the tier join landed on almost nothing - check the race_key format" =
            h[!is.na(meet_tier), .N] > 1000)

# --- round, derived from rc the same way score_vs_wa_ranking.R does it ------
h[, rnd := fifelse(grepl("final", rc, ignore.case = TRUE) &
                     !grepl("semi", rc, ignore.case = TRUE), "final",
                   fifelse(grepl("semi", rc, ignore.case = TRUE), "semi", "heat"))]

# --- field size, starters in the race ---------------------------------------
h[, field_n := .N, by = race_key]
h[, field_band := cut(field_n, breaks = c(1, 4, 8, 16, Inf),
                       labels = c("2-4", "5-8", "9-16", "17+"), right = TRUE)]

# --- event family, via the canonical registry -------------------------------
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
cat(sprintf("event registry join: %s of %s rows matched a family (%.1f%%)\n",
            format(h[!is.na(family), .N], big.mark = ","),
            format(nrow(h), big.mark = ","), 100 * h[, mean(!is.na(family))]))
stopifnot("the event registry join landed on almost nothing - check event_id" =
            h[!is.na(family), .N] > 1000)

# --- race-level attributes must not vary within a race_key ------------------
# meet_tier, rnd, yr, family and field_band are all facts about the RACE, not
# the athlete, so every row sharing a race_key must carry the same value. If
# any of them varies within a race, the per-race join below would silently
# pick one value at random and mislabel pairs - check it rather than assume it.
chk <- h[, .(nt = uniqueN(meet_tier), nr = uniqueN(rnd), ny = uniqueN(yr),
             nf = uniqueN(family), nb = uniqueN(field_band)), by = race_key]
if (!all(chk$nt == 1 & chk$nr == 1 & chk$ny == 1 & chk$nf == 1 & chk$nb == 1))
  stop("a race-level attribute (tier / round / year / family / field size) ",
       "varies within a single race_key - the per-race join is not safe to use",
       call. = FALSE)

races <- unique(h[, .(race_key, meet_tier, rnd, field_band, yr, family, field_n)])
stopifnot("race-level table does not have one row per race" =
            nrow(races) == uniqueN(h$race_key))

# --- build every pair once, then cut it every way below ---------------------
# SAME PAIR CONSTRUCTION AS score_tier_concordance.R and score_debut_prior.R:
# one row per athlete within a race, self-joined by race, kept only where
# i.x < i.y (so each pair counts once) and place.x != place.y (drops ties on
# place, which carry no order information).
a <- h[, .(rid = .GRP, i = seq_len(.N), place, r_use, seen, n_eff, athlete_id),
       by = race_key]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
if (nrow(m) == 0)
  stop("pair construction produced zero pairs across the whole history", call. = FALSE)

m[, race_key := race_key.x]
m[, c("race_key.x", "race_key.y") := NULL]
m <- merge(m, races, by = "race_key")
stopifnot("race-level merge dropped rows unexpectedly" = nrow(m) > 0)

won <- m$place.x < m$place.y
m[, c := fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won))]

# how much of the pair is cold (0, 1 or 2 debutant sides)
m[, ncold := (!seen.x) + (!seen.y)]

# evidence carried by the pair, WEAKEST SIDE FIRST. A pair is only as reliable
# as its least-evidenced athlete: if either side has never been seen, the pair
# is labelled "not seen"; otherwise it is bucketed by the SMALLER of the two
# n_eff values, because that is the side driving how little the model
# actually knows about this comparison.
m[, min_neff := pmin(n_eff.x, n_eff.y)]
m[, evid := fcase(
  !(seen.x & seen.y), "not seen",
  min_neff <= 1,      "n_eff<=1",
  min_neff <= 3,       "n_eff 2-3",
  min_neff <= 7,       "n_eff 4-7",
  min_neff <= 15,      "n_eff 8-15",
  default = "n_eff 16+"
)]
m[, evid := factor(evid, levels = c("not seen", "n_eff<=1", "n_eff 2-3",
                                     "n_eff 4-7", "n_eff 8-15", "n_eff 16+"))]

cat(sprintf("\n%s pairs built from %s races\n",
            format(nrow(m), big.mark = ","), format(uniqueN(m$race_key), big.mark = ",")))

# --- reporting helper --------------------------------------------------------
# Groups by `by_col`, prints concordance and the binomial noise floor
# (100 * sqrt(0.25 / n)) beside every row, and SPLITS OFF any cell under
# MIN_CELL pairs rather than printing a number nobody should trust. Asserts a
# population before returning anything, per the "all(logical(0)) is TRUE"
# trap this project has hit four times: an empty or all-NA cut must stop the
# script, not print a table with nothing in it.
cut_table <- function(d, by_col, label) {
  tab <- d[, .(pairs = .N, conc = round(100 * mean(c), 3)), by = by_col]
  if (nrow(tab) == 0)
    stop(sprintf("cut '%s' produced no groups at all - check the join or derivation that feeds it",
                 label), call. = FALSE)
  tab[, floor := round(100 * sqrt(0.25 / pairs), 3)]
  setorderv(tab, by_col)
  skip <- tab[pairs < MIN_CELL]
  keep <- tab[pairs >= MIN_CELL]
  cat(sprintf("\n-- %s --\n", label))
  if (nrow(keep) > 0) print(keep) else cat("  no cell reached the 200-pair minimum\n")
  if (nrow(skip) > 0) {
    lbl <- do.call(paste, c(as.list(skip[, ..by_col]), sep = "/"))
    cat(sprintf("  skipped %d cell(s) below %d pairs: %s\n", nrow(skip), MIN_CELL,
                paste(sprintf("%s (n=%d)", lbl, skip$pairs), collapse = ", ")))
  }
  list(label = label, kept = keep, skipped_cells = nrow(skip))
}

res <- list(
  evidence_carried = cut_table(m, "evid",       "evidence carried (seen / n_eff of the weaker side)"),
  cold_count       = cut_table(m, "ncold",      "how much of the pair is cold (0, 1 or 2 debutant sides)"),
  meet_tier        = cut_table(m, "meet_tier",  "meet tier"),
  round            = cut_table(m, "rnd",        "round"),
  field_size       = cut_table(m, "field_band", "field size"),
  season           = cut_table(m, "yr",         "season"),
  event_family     = cut_table(m, "family",     "event family")
)

# --- calibration: does the predicted gap track the actual win rate? --------
# Concordance cannot see this, because a barely-confident correct call and a
# wildly overconfident correct call score identically. Instead: take the
# absolute rating gap between the two athletes, fit ONE logistic curve mapping
# that gap onto a win probability (this is the standard way to turn a raw
# score into a probability, and using a single global curve is deliberate - it
# does not let each bucket fit its own slope, so a bucket that disagrees with
# the curve is telling you something the curve's shape cannot absorb), then
# bucket pairs by gap and compare the curve's predicted win rate against what
# the higher-rated athlete actually achieved in that bucket. A well calibrated
# model tracks the diagonal (predicted approx actual) in every bucket; a model
# that is more confident than it has earned will show large gaps predicting
# a near-certainty that the data does not support.
cat("\n=== calibration: predicted win rate implied by the rating gap, vs actual ===\n")
cal <- m[r_use.x != r_use.y]
if (nrow(cal) == 0)
  stop("no pairs carry distinct ratings - cannot check calibration", call. = FALSE)
cal[, gap := abs(r_use.x - r_use.y)]

fit <- suppressWarnings(glm(c ~ gap, data = cal, family = binomial()))
cal[, pred := predict(fit, type = "response")]

brk <- unique(stats::quantile(cal$gap, probs = seq(0, 1, length.out = 9), na.rm = TRUE))
if (length(brk) < 3)
  stop("the rating gap has almost no spread across pairs - cannot form quantile buckets",
       call. = FALSE)
cal[, gbucket := cut(gap, breaks = brk, include.lowest = TRUE)]

calib <- cal[, .(pairs = .N,
                  mean_gap = round(mean(gap), 4),
                  predicted_win_rate = round(100 * mean(pred), 2),
                  actual_win_rate = round(100 * mean(c), 2)), by = gbucket]
if (nrow(calib) == 0)
  stop("calibration bucketing produced no groups", call. = FALSE)
calib[, diff := round(actual_win_rate - predicted_win_rate, 2)]
calib[, floor := round(100 * sqrt(0.25 / pairs), 3)]
setorder(calib, mean_gap)

skip_cal <- calib[pairs < MIN_CELL]
keep_cal <- calib[pairs >= MIN_CELL]
print(keep_cal)
if (nrow(skip_cal) > 0)
  cat(sprintf("  skipped %d bucket(s) below %d pairs\n", nrow(skip_cal), MIN_CELL))

# --- write results ------------------------------------------------------
out <- list(
  tag = TAG,
  min_cell = MIN_CELL,
  n_pairs_total = nrow(m),
  cuts = lapply(res, function(r) list(label = r$label, kept = r$kept,
                                       skipped_cells = r$skipped_cells)),
  calibration = list(kept = keep_cal, skipped_cells = nrow(skip_cal))
)
out_f <- file.path(D, "calibration_by_cut.json")
writeLines(jsonlite::toJSON(out, dataframe = "rows", auto_unbox = TRUE, na = "null"), out_f)
cat(sprintf("\nwrote %s\n", basename(out_f)))
