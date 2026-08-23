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
