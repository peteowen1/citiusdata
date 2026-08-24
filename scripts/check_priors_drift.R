# WHICH OF THE ENGINE'S CORPUS-ESTIMATED QUANTITIES DRIFT, AND SHOULD THEY?
#
# MU, the debut prior mean, drifts with the corpus window (0.135 sd of
# within-race spread from 2010+ to 2020+, and a further 0.154 sd from 2020+ to
# 2024+). It is handed to 12-20% of rows a year as their starting rating. That
# raises the obvious question: what else is estimated this way?
#
# THE PRINCIPLED SPLIT IS NOT "does it change over time" - almost everything
# does a little. It is what kind of quantity it is:
#
#   PRIORS - what we believe about an athlete before seeing them. MU (the mean)
#     and VP (the variance) are exactly this. They describe the CURRENT state of
#     an event's population, so a contemporary estimate is the correct one and a
#     pooled all-time estimate is simply wrong, not merely noisier.
#
#   STRUCTURAL RELATIONSHIPS - how a covariate maps to performance. The per-event
#     wind coefficients and the aging curves are these. A 2 m/s tailwind helps a
#     100m by the same physics in 2012 and 2026, so pooling across time buys
#     precision at no cost in bias. Dating them would add variance for nothing.
#     Aging is the arguable one: training methods move, but slowly, and the
#     curve is shared across a whole family.
#
#   HYPERPARAMETERS - k0, kappa, the decay half-life, the ceiling. These are
#     control settings chosen to make the filter behave, not estimates of the
#     world. Letting them vary by era is a licence to overfit.
#
# So the candidates for dating are the two priors, and this measures whether the
# second one, VP, drifts enough to matter the way MU does.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

h <- setDT(read_parquet(file.path(D, "seqv3_history_from2010.parquet"),
                        col_select = c("race_key", "date", "event_id", "perf", "place")))
h <- h[is.finite(perf) & is.finite(place) & place > 0]
h[, yr := year(as.Date(date))]

# VP is the MEDIAN OF RACE-LEVEL VARIANCE per event - rebuilt here exactly as
# form_ratings.R:1237 builds it, so the drift measured is the drift of the value
# the engine actually uses, not of a similar-looking statistic.
vp_for <- function(dd) dd[, .(v = var(perf)), by = .(event_id, race_key)][
  is.finite(v), .(vp = stats::median(v)), by = event_id]

v10 <- vp_for(h)[,            .(event_id, vp_2010 = vp)]
v20 <- vp_for(h[yr >= 2020])[, .(event_id, vp_2020 = vp)]
v24 <- vp_for(h[yr >= 2024])[, .(event_id, vp_2024 = vp)]
n   <- h[, .(rows = .N), by = event_id]

m <- Reduce(function(a, b) merge(a, b, by = "event_id"), list(v10, v20, v24, n))
m <- m[rows >= 2000 & is.finite(vp_2010) & vp_2010 > 0]

# ratios, because a variance prior is scale-free in a way a mean is not: what
# matters is whether the starting uncertainty is 20% too wide, not its units.
m[, r_10_20 := vp_2020 / vp_2010]
m[, r_20_24 := vp_2024 / vp_2020]

cat(sprintf("events with >=2,000 rows: %d\n\n", nrow(m)))
cat(sprintf("VP moving the window 2010+ -> 2020+ : median ratio %.3f (%.1f%% change)\n",
            median(m$r_10_20), 100 * (median(m$r_10_20) - 1)))
cat(sprintf("VP moving 2020+ -> 2024+           : median ratio %.3f (%.1f%% change)\n",
            median(m$r_20_24), 100 * (median(m$r_20_24) - 1)))
cat(sprintf("events where VP moves more than 20%% between 2010+ and 2020+: %d of %d\n",
            m[abs(r_10_20 - 1) > 0.2, .N], nrow(m)))

cat("\n=== the 12 events whose VARIANCE prior moves most (2010+ -> 2020+) ===\n")
print(head(m[order(-abs(r_10_20 - 1)),
             .(event_id, rows, vp_2010 = signif(vp_2010, 3),
               vp_2020 = signif(vp_2020, 3), ratio = round(r_10_20, 3))], 12))

cat("\nA variance prior that is too WIDE makes a debutant's rating move too fast\n")
cat("on their first result; too NARROW and it moves too slowly. Unlike the mean,\n")
cat("this biases the SPEED of learning rather than the starting point, so it\n")
cat("shows up as mis-rated athletes two or three races in rather than on debut.\n")
