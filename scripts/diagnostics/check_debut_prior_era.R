# THE DEBUT PRIOR IS AN ALL-TIME EVENT MEAN, AND THE SPORT MOVES.
#
# form_ratings.R:1200 computes MU as mean(perf) per event over the WHOLE loaded
# corpus, and line 1764 hands exactly that to any athlete with no prior rating
# in the event:
#
#     if (is.null(v)) { r_pre[m] <- mu; n_eff[m] <- 0; next }
#
# So a debutant's starting rating is the average of every mark in the corpus,
# including ones set years before they raced. If performance drifts over time -
# and in athletics it does - that prior is stale by construction, and it gets
# staler the further back the corpus reaches.
#
# THIS IS WHY THE from2010 ARM LOST. Extending to 2010 pulled every event mean
# toward an older, slower era, so every modern debutant was seeded too low. The
# damage landed on athletes with NO pre-2020 history (-0.412, 17 floors) and not
# on those carrying it (-0.046, inside the floor) - backwards from stale-form
# decay, and exactly what an era-biased debut prior predicts, because the
# established athletes never touch the prior at all.
#
# So the from2010 result is not evidence that older data is worthless. It is
# evidence that the debut prior is era-blind. That distinction decides whether
# the fix is to throw the data away or to date the prior.
#
# What this measures: how far the prior moves with the window, and how much the
# per-event mean drifts per year, both in units of the within-race spread that
# the ratings actually live on.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

h <- setDT(read_parquet(file.path(D, "seqv3_history_from2010.parquet"),
                        col_select = c("race_key", "date", "event_id", "perf", "place")))
h <- h[is.finite(perf) & is.finite(place) & place > 0]
h[, date := as.Date(date)][, yr := year(date)]

# the scale ratings live on: typical within-race spread per event
sp <- h[, .(v = var(perf)), by = .(event_id, race_key)][is.finite(v),
        .(sd_race = sqrt(median(v))), by = event_id]

mu10 <- h[, .(mu_2010 = mean(perf)), by = event_id]
mu20 <- h[yr >= 2020, .(mu_2020 = mean(perf)), by = event_id]
mu24 <- h[yr >= 2024, .(mu_2024 = mean(perf)), by = event_id]
n    <- h[, .(rows = .N), by = event_id]

m <- Reduce(function(a, b) merge(a, b, by = "event_id"),
            list(mu10, mu20, mu24, sp, n))
m <- m[rows >= 2000]
m[, shift_10_to_20 := mu_2020 - mu_2010]
m[, shift_20_to_24 := mu_2024 - mu_2020]
m[, shift_in_sd    := shift_10_to_20 / sd_race]

cat(sprintf("events with >=2,000 rows: %d\n", nrow(m)))
cat(sprintf("\nmean shift moving the window from 2010+ to 2020+: %+.4f in perf units\n",
            mean(m$shift_10_to_20)))
cat(sprintf("as a fraction of within-race spread: %+.3f sd (median %+.3f)\n",
            mean(m$shift_in_sd), median(m$shift_in_sd)))
cat(sprintf("events where the 2020+ mean is HIGHER (sport got faster): %d of %d\n",
            m[shift_10_to_20 > 0, .N], nrow(m)))

cat("\n=== the 12 events whose prior moves most, in sd of within-race spread ===\n")
print(head(m[order(-abs(shift_in_sd)),
             .(event_id, rows, sd_race = round(sd_race, 4),
               mu_2010 = round(mu_2010, 4), mu_2020 = round(mu_2020, 4),
               shift = round(shift_10_to_20, 4), in_sd = round(shift_in_sd, 3))], 12))

# AND THE SAME PROBLEM EXISTS AT THE DEPLOYED SETTING. Even from 2020 the prior
# is a six-year average handed to an athlete debuting in 2026, so measure the
# drift that remains inside the deployed window.
cat("\n=== drift WITHIN the deployed 2020+ window: 2024+ mean against 2020+ mean ===\n")
cat(sprintf("mean shift: %+.4f | as sd of within-race spread: %+.3f\n",
            mean(m$shift_20_to_24), mean(m$shift_20_to_24 / m$sd_race)))
cat("If this is non-zero the deployed debut prior is stale too, just less so -\n")
cat("which makes dating the prior a fix for the current model, not only a\n")
cat("precondition for using older data.\n")

cat("\n=== how much of the corpus is a debut, so how much the prior touches ===\n")
if ("seen" %chin% names(setDT(read_parquet(file.path(D, "seqv3_history_from2010.parquet"),
                                           col_select = "seen")))) {
  s <- setDT(read_parquet(file.path(D, "seqv3_history_from2010.parquet"),
                          col_select = c("seen", "date")))
  s[, yr := year(as.Date(date))]
  print(s[yr >= 2021, .(rows = .N, debuts = sum(!seen),
                        pct = round(100 * mean(!seen), 1)), by = yr][order(yr)])
}
