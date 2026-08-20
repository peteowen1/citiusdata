# Does the wind smooth have data where it is being asked to predict?
#
# The correction is clamped at the stored curve's ends, so no race extrapolates
# past it at APPLICATION time. But the curves themselves run -4 to +6 m/s, which
# means the GAM was EVALUATED there when they were built - and a smooth predicts
# happily in regions with no data. The race page shows a +5.6 m/s 100m, so this
# is not hypothetical.
#
# The stored curves carry `lo` and `hi`, so the question can be answered from the
# artefact: a band that blows out at the edges is the model saying it does not
# know, and a band that stays tight where there is no data would be the worrying
# case.
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
j <- jsonlite::fromJSON(file.path(D, "wind_effect_curves.json"))
cu <- as.data.table(j$curves)
stopifnot("curve file has no confidence band" = all(c("lo","hi") %chin% names(cu)))
cu[, width := hi - lo]
ref <- cu[abs(wind) <= 1, .(w0 = stats::median(width)), by = event_id]
cu <- merge(cu, ref, by = "event_id")
cu[, rel := width / w0]
cat("=== how much wider is the band away from zero wind? ===\n")
b <- cu[, .(events = uniqueN(event_id), median_rel_width = round(stats::median(rel), 2)),
        by = .(wind = round(abs(wind)))][order(wind)]
print(b)
cat("\nrel = band width at that wind, over the same event's width near zero.\n")
cat("A smooth with no data at the edge should widen sharply. If it does not,\n")
cat("the model is confident where it has nothing to be confident about.\n")

cat("\n=== the deployed curve at its extremes, men's 100m ===\n")
print(cu[event_id == "AT-100Metres-M" & wind %in% c(-4,-2,0,2,4,6),
         .(wind, delta_pct = round(delta_pct, 3), lo = round(lo, 4),
           hi = round(hi, 4), rel_width = round(rel, 2))])
cat("\nThe applied correction is clamped to this range, so +5.6 m/s takes the\n")
cat("value at +5.6 and nothing beyond it - the risk is the curve out here, not\n")
cat("the clamping.\n")
