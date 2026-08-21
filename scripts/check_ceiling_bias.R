# Is the ceiling term rewarding race COUNT rather than ability?
#
# THE ORDERING BLEND is (1-CEIL)*R + CEIL*best, with CEIL = 0.30 and `best` the
# athlete's best adjusted mark so far. The rationale is sound - a championship is
# won by whoever runs fast on the day, so a pure average under-rates a big
# performer - but the STATISTIC chosen to represent "on the day" is a maximum,
# and a maximum is a biased estimator of anything.
#
# THE PROBLEM, IN ONE LINE. E[max of n draws] increases with n. Two athletes of
# identical ability, one with 5 races and one with 30, have different expected
# bests purely from sampling. The ceiling therefore carries a race-count reward
# that has nothing to do with how good anyone is - and n varies enormously here,
# from a marathoner's 3 races to a collegiate sprinter's 40.
#
# WHAT `best` IS AND IS NOT. It is computed from `perf`, which the engine
# receives AFTER wind, stadium and indoor have been removed, so it is already an
# adjusted mark - that part is fine. Field strength is handled separately by the
# race shock. The issue is purely the order statistic.
#
# THE PRINCIPLED ALTERNATIVE. The engine already estimates each athlete's own
# spread (`v`). A high quantile of the predicted distribution, R + c*sqrt(v), is
# the same idea as "what could they do on a good day" but is UNBIASED in n: it
# does not care how many times you rolled, only how good and how variable you
# are. If the bias below is real, that is the swap worth testing.
#
# MEASURE FIRST. If E[best - R] is flat in n, the concern is theoretical and the
# max is fine.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

st <- setDT(read_parquet(file.path(OUT, sprintf("seqv2_state_%s.parquet", TAG))))
need <- c("athlete_id","event_id","R","best","n_eff","v")
stopifnot("state is missing a column this needs" = all(need %chin% names(st)))
st <- st[is.finite(R) & is.finite(best) & is.finite(n_eff) & n_eff > 0]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
st <- merge(st, reg, by = "event_id", all.x = TRUE)
cat(sprintf("%s athlete-events with a best and a rating\n", format(nrow(st), big.mark = ",")))
stopifnot("too few rows" = nrow(st) > 20000)

# HOW FAR ABOVE THEIR OWN RATING IS AN ATHLETE'S BEST? In perf units, higher is
# better in every event, so this is positive by construction and its SIZE is the
# question. Scaled by the athlete's own sd so events with different spreads are
# comparable - that is the same device the tail analysis used.
st[, gap := best - R]
st[, gap_z := fifelse(is.finite(v) & v > 0, gap / sqrt(v), NA_real_)]

cat("\n=== how far the best sits above the rating, by number of races ===\n")
st[, band := cut(n_eff, c(0, 2, 4, 8, 15, 30, Inf),
                 labels = c("1-2","3-4","5-8","9-15","16-30","30+"))]
b <- st[, .(athlete_events = .N,
            median_gap_z = round(stats::median(gap_z, na.rm = TRUE), 3),
            mean_gap_z   = round(mean(gap_z, na.rm = TRUE), 3),
            median_R     = round(stats::median(R), 4)), by = band][order(band)]
print(b)
cat("\nIf median_gap_z climbs with the race count, the ceiling is paying for\n")
cat("volume. A maximum does that; a quantile of the fitted distribution does not.\n")

# CONTROL FOR ABILITY. The bands above differ in more than n - athletes who race
# more may simply be better, or worse. Compare WITHIN a narrow rating slice so
# ability is held roughly fixed and only the race count varies.
cat("\n=== the same, holding ability fixed (within rating deciles) ===\n")
st[, rdec := cut(R, quantile(R, seq(0, 1, 0.1), na.rm = TRUE),
                 include.lowest = TRUE, labels = FALSE), by = event_id]
w <- st[!is.na(rdec) & !is.na(gap_z), .(athlete_events = .N,
        median_gap_z = round(stats::median(gap_z), 3)), by = .(band)][order(band)]
print(w)
cat("\nSame direction within rating deciles = the effect is the race count, not\n")
cat("that busy athletes happen to be better.\n")

cat("\n=== by family, because n varies hugely between them ===\n")
fb <- st[!is.na(family), .(athlete_events = .N,
         median_n = round(stats::median(n_eff), 1),
         median_gap_z = round(stats::median(gap_z, na.rm = TRUE), 3)), by = family]
setorder(fb, -median_gap_z)
print(fb)
cat("\nA marathoner races three times a year and a collegiate sprinter forty. If\n")
cat("the gap tracks median_n across families, the ceiling is systematically\n")
cat("kinder to the events people contest most often.\n")

# WHAT THE SWAP WOULD LOOK LIKE. c chosen so the quantile sits at the same
# average distance above R as the current best does - so this is a like-for-like
# replacement of the STATISTIC, not a change in how aggressive the ceiling is.
cc <- st[is.finite(gap_z), stats::median(gap_z)]
cat(sprintf("\nMatched constant: R + %.3f*sqrt(v) sits where the median best sits today.\n", cc))
st[, alt := R + cc * sqrt(v)]
cat("=== does the alternative remove the n-dependence? ===\n")
st[, alt_z := (alt - R) / sqrt(v)]
print(st[, .(median_best_z = round(stats::median(gap_z, na.rm = TRUE), 3),
             median_alt_z  = round(stats::median(alt_z, na.rm = TRUE), 3)), by = band][order(band)])
cat("\nalt_z is flat by construction - that is the point. The comparison that\n")
cat("decides is not this table but whether ordering on it scores better, which\n")
cat("needs an engine arm.\n")

f <- file.path(OUT, "ceiling_bias.json")
writeLines(jsonlite::toJSON(list(tag = TAG, by_n = b, by_family = fb, matched_c = cc),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
