# IS THE DEPLOYED WIN PROBABILITY CALIBRATED? Read it off the backtest.
#
# check_calibration_by_cut.R can only approximate this. It derives the model's
# implied probability as pnorm(gap / sqrt(v_x + v_y + 2 * vp_event)), where
# vp_event is the median within-race variance for the event. That is a stand-in
# for the PER-ATHLETE sigma the deployed simulate_event actually uses, and a
# within-race variance mixes an individual's noise together with the ability
# spread between the athletes in that race, so it is plausibly too large. It
# reported the model underconfident by up to 4.53 points, which is exactly the
# shape an inflated variance produces, so the finding could not be separated
# from the approximation.
#
# The backtest does not have that problem. It runs the real simulate_event on
# real fields with the real per-athlete sigma, under a strict temporal cut, and
# stores the resulting probabilities next to what actually happened. So the
# honest calibration answer is already on disk and does not need the expensive
# machinery re-run.
#
# WHAT A BAD RESULT LOOKS LIKE. Probabilities that are systematically too close
# to the base rate mean the spread is too wide and the model is underconfident;
# probabilities too far from it mean the opposite. Either way medal projections
# for LA 2028 are probability statements, so this is the number that governs
# them - concordance cannot see it at all, because a barely-right call and a
# wildly overconfident right call score identically.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
BT  <- Sys.getenv("CITIUS_BT_OUT", "backtest.rds")

f <- file.path(OUT, BT)
stopifnot("no backtest on disk - set CITIUS_BT_OUT" = file.exists(f))
b <- readRDS(f)
cat(sprintf("reading %s, written %s\n", BT,
            format(file.info(f)$mtime, "%Y-%m-%d %H:%M")))
cat(sprintf("top-level elements: %s\n", paste(names(b), collapse = ", ")))

# Find the per-prediction table rather than assuming its name. A backtest that
# changed shape would otherwise be read as an empty result.
cand <- Filter(function(x) is.data.frame(x) && nrow(x) > 100, b)
stopifnot("no per-prediction table found in the backtest object" = length(cand) > 0)
cat(sprintf("candidate tables: %s\n",
            paste(sprintf("%s (%s rows, %d cols)", names(cand),
                          format(vapply(cand, nrow, 1L), big.mark = ","),
                          vapply(cand, ncol, 1L)), collapse = " | ")))

# NAMED EXPLICITLY, NOT GUESSED. p_gold pairs with `hit`, p_medal with
# `hit_medal`. Pairing a probability with the wrong outcome flag would produce a
# calibration curve about nothing, and both flags are 0/1 so nothing would error.
pred <- as.data.table(b$predictions)
outc <- as.data.table(b$outcomes)
stopifnot("predictions and outcomes are different sizes" = nrow(pred) == nrow(outc))

d <- merge(pred, outc, by = c("race_id", "athlete_id"), all = FALSE)
cat(sprintf("%s predictions joined to %s outcomes -> %s rows\n",
            format(nrow(pred), big.mark = ","), format(nrow(outc), big.mark = ","),
            format(nrow(d), big.mark = ",")))
# A LOSSY JOIN WOULD SILENTLY CHANGE THE BASE RATE, which is the one number this
# whole check rests on. Assert it kept everything.
stopifnot("the join lost rows - the keys do not line up" = nrow(d) == nrow(pred))
stopifnot("outcome flags are not 0/1" =
            all(d$hit %in% c(0, 1)) && all(d$hit_medal %in% c(0, 1)))

# ---- EXCLUDE MERGED RACES BEFORE SCORING ANYTHING --------------------------
#
# The engine's own test for a merged race is a place shared by DIFFERENT MARKS,
# which is right because a shared place alone is usually a legitimate tie. The
# backtest outcomes carry no marks, so that test is unavailable here and the
# count has to stand in for it: a race recording more than four medallists
# cannot be explained by ties, since the most a tie can produce is four - two
# silvers and no bronze, or two bronzes.
#
# Four is therefore kept and five or more is dropped. Races recording FEWER than
# three are dropped too: they are incomplete rather than merged, and scoring
# three units of model probability against two awarded medals biases the same
# way, just in the other direction.
#
# This is a filter on a stored artefact, not a fix. The real fix is to apply the
# mark-based test inside backtest_athletics.R where outcomes are built, which
# needs a full re-run because the per-competition cache holds outcomes without
# it. Set BT_KEEP_MERGED=1 to reproduce the uncorrected numbers.
KEEP_MERGED <- Sys.getenv("BT_KEEP_MERGED", "0") != "0"
per_race <- d[, .(medals = sum(hit_medal)), by = race_id]
drop_ids <- per_race[medals > 4L | medals < 3L, race_id]
cat(sprintf("\nraces: %s total | %s awarding 3 or 4 medals | %s dropped as merged or incomplete\n",
            format(nrow(per_race), big.mark = ","),
            format(per_race[medals %in% c(3L, 4L), .N], big.mark = ","),
            format(length(drop_ids), big.mark = ",")))
if (length(drop_ids))
  cat(sprintf("dropped races carry %s medals against %d expected by the three-per-race rule\n",
              format(per_race[race_id %chin% drop_ids, sum(medals)], big.mark = ","),
              3L * length(drop_ids)))
if (!KEEP_MERGED && length(drop_ids)) {
  d <- d[!race_id %chin% drop_ids]
  stopifnot("every row was dropped as merged" = nrow(d) > 0)
  cat(sprintf("scoring %s entrant-rows after the filter\n",
              format(nrow(d), big.mark = ",")))
} else if (KEEP_MERGED) {
  cat("BT_KEEP_MERGED is set - scoring the uncorrected outcomes\n")
}

report <- function(pcol, ocol, label) {
  x <- d[is.finite(get(pcol))]
  if (!nrow(x)) { cat(sprintf("%s: no finite probabilities\n", label)); return(NULL) }
  tot_p <- sum(x[[pcol]]); tot_a <- sum(x[[ocol]])
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("%s rows | predicted total %.1f | actual total %d | ratio %.3f\n",
              format(nrow(x), big.mark = ","), tot_p, tot_a, tot_p / max(tot_a, 1)))
  cat("A ratio above 1 means the model spends more probability than the events it\n")
  cat("is spending it on, i.e. it is over-forecasting; below 1, under.\n")

  # Buckets on the PREDICTED value, which is how a reliability curve is read:
  # of the rows we said were 20% likely, how many happened.
  brk <- unique(stats::quantile(x[[pcol]], probs = seq(0, 1, length.out = 11),
                                na.rm = TRUE))
  if (length(brk) < 3) { cat("  probability has too little spread to bucket\n"); return(NULL) }
  x[, bk := cut(get(pcol), breaks = brk, include.lowest = TRUE)]
  r <- x[, .(n = .N,
             predicted = round(100 * mean(get(pcol)), 2),
             actual    = round(100 * mean(get(ocol)), 2)), by = bk][order(bk)]
  r[, diff := round(actual - predicted, 2)]
  # floor on the ACTUAL rate in each bucket, which is what the comparison needs
  r[, floor := round(100 * sqrt(pmax(actual/100, 1e-6) * (1 - actual/100) / n), 2)]
  r[, ratio := round(diff / pmax(floor, 1e-9), 1)]
  # THIN BUCKETS SAY NOTHING. Print them but mark them, rather than letting a
  # 12-row cell with a wild ratio read as a finding.
  r[, thin := n < 200]
  print(r)
  cat("diff is actual minus predicted, in percentage points. ratio is diff over\n")
  cat("one standard error of the actual rate. Ignore rows marked thin.\n")
  invisible(r)
}

g <- report("p_gold",  "hit",       "GOLD: p_gold against actually winning")
m <- report("p_medal", "hit_medal", "MEDAL: p_medal against actually medalling")

# RETRACTED 2026-08-23, SAME DAY. The finding recorded below is wrong. Read
# this block before the numbers under it.
#
# The model spends EXACTLY three medals of probability per race - median, min
# and max of sum(p_medal) by race are all 3.000 - so it cannot be short in
# aggregate. It was scored against outcome data recording 4,185 medals in 1,346
# races, which is 3.11 per race. The excess is not ties. 34 races record more
# than three medallists, including single races recording 20, 17, 16, 15, 14, 13
# and 12, which is the merged-race corruption seen elsewhere in this project
# (duplicated place values), not dead heats. Those 34 races carry 182 unwinnable
# medals; 29 other races record fewer than three, missing 35. Net +147, which is
# the entire "shortfall".
#
# Re-scored on the 1,283 races that awarded exactly three medals:
#   total expected 3848.9 | total actual 3849 | net +0.1
# The bucket pattern collapses with it - the bottom four buckets go from
# +10.9, +20.0, +31.5, +45.3 to +4.9, +8.5, +4.9, +1.1, and the residual is
# noisy and sums to zero, with +26.6 in one bucket against -21.0 in its
# neighbour. There is no medal miscalibration to explain.
#
# HOW IT WAS MISSED. The reliability table compared a predicted rate against an
# actual rate per bucket and never checked that the two sides were counting the
# same thing. Three units of probability per race were being scored against a
# world that handed out 3.11, so the model looked short everywhere, and worst in
# the buckets holding the most entrants per medal - the longshots. That
# manufactures precisely the pattern that was reported. The check that would
# have caught it in ten seconds is the one that eventually did: sum both sides
# and see whether the totals can even agree. Do that before reading any
# reliability curve.
#
# check_medal_count_per_race.R holds the counts and the clean re-score.
#
# ---------------------------------------------------------------------------
# The superseded finding, kept so the retraction is legible:
#
# MEASURED 2026-08-23 on backtest.rds, which was written 2026-08-12 and so
# predates the replacement-level debut prior. It describes the model as it stood
# then, not as it stands now.
#
# GOLD IS FINE. Predicted total 1346.0 against 1317 actual, ratio 1.022, and no
# bucket further than 2.4 standard errors from its actual rate.
#
# MEDAL UNDERPREDICTS LONGSHOTS, consistently and well outside noise:
#     bucket 1  1,473 rows   predicted  0.08   actual  0.81   +0.73   3.2 se
#     bucket 2  1,436         1.18              2.58          +1.40   3.3
#     bucket 3  1,449         4.10              6.28          +2.18   3.4
#     bucket 4  1,453         9.06             12.18          +3.12   3.6
#     bucket 5  1,452        15.68             16.94          +1.26   1.3
# Four consecutive buckets, same sign, then it closes. Total medal probability
# is 3.5% short. An athlete given a 4% medal chance medals 6.3% of the time.
#
# DO NOT ATTRIBUTE THE MECHANISM YET. Three things would each produce this and
# each wants a different fix: a performance tail that is too thin, a per-athlete
# sigma too small for weaker athletes, or a shared condition shock that is too
# narrow. That last one is easy to overlook because the shock cancels out of a
# pairwise comparison, which is the verse rule everyone remembers - but it does
# NOT cancel out of whether an athlete reaches the top three of a field, and
# that is exactly what p_medal asks.
#
# It also disagrees with check_calibration_by_cut.R, which suggested the spread
# was too WIDE in head-to-head terms. Both can hold: a pairwise comparison and a
# top-three-of-a-field question load different parts of the distribution. The
# medal figure is the one measured on the deployed simulator with its own sigma,
# so it is the one to trust of the two.
cat("\n=== what this settles ===\n")
cat("These probabilities come from the deployed simulate_event running on real\n")
cat("fields with the real per-athlete sigma, under a strict temporal cut. So\n")
cat("unlike check_calibration_by_cut.R, the variance is the model's own and not\n")
cat("an approximation, and a bias here is a bias in the thing that produces\n")
cat("medal projections.\n")

f2 <- file.path(OUT, "backtest_calibration.json")
writeLines(jsonlite::toJSON(list(source = BT, gold = g, medal = m),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f2)
cat(sprintf("\nwrote %s\n", basename(f2)))
