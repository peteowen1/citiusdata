# Does head-to-head history carry signal the ratings do not?
#
# The ceiling analysis found the model gets 7% of WIDE-gap pairs wrong (4%+
# apart), where race noise alone should cost ~1%. That gap is rating error, and
# a direct H2H record is the obvious thing a single-number rating compresses
# away: A may have beaten B eight times out of ten despite similar ratings.
#
# SIZE IT BEFORE BUILDING IT. The binding constraint is coverage - if almost no
# pair has met before, the mechanism can be real and still be worth nothing.
# So this measures coverage first and the signal second.
#
# Everything is strictly walk-forward: a pair's record counts only meetings
# BEFORE the race being scored.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_use) & place <= 12]
h[, nf := .N, by = race_key]; h <- h[nf >= 2]
setorder(h, date, race_key)
h[, rid := .GRP, by = race_key]
h[, yr := year(date)]
cat(sprintf("athlete-races: %s across %s races\n",
            format(nrow(h), big.mark = ","), format(uniqueN(h$rid), big.mark = ",")))

# --- every pair in every race, 2020 on, so the record is built from the start --
a <- h[, .(rid, yr, event_id, i = seq_len(.N), athlete_id, place, r = r_use), by = race_key]
m <- merge(a, a, by = c("race_key", "rid", "yr", "event_id"),
           allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
cat(sprintf("pairs in all years: %s\n", format(nrow(m), big.mark = ",")))

# order-independent pair key, so A-vs-B and B-vs-A accumulate together
m[, `:=`(lo = pmin(athlete_id.x, athlete_id.y), hi = pmax(athlete_id.x, athlete_id.y))]
m[, pk := paste0(lo, "|", hi, "|", event_id)]
# did the LOW-id athlete win this meeting?
m[, lo_won := as.integer(fifelse(athlete_id.x == lo, place.x < place.y, place.y < place.x))]
setorder(m, rid)
# strictly prior record for this pair
m[, n_prior  := seq_len(.N) - 1L,            by = pk]
m[, lo_prior := shift(cumsum(lo_won), 1L, 0L), by = pk]

s <- m[yr %in% c(2025, 2026)]
cat(sprintf("\nscored pairs 2025-26: %s\n", format(nrow(s), big.mark = ",")))
cat("\n=== COVERAGE: how many scored pairs have met before? ===\n")
s[, band := cut(n_prior, c(-1, 0, 1, 2, 4, 9, Inf),
                labels = c("never met", "1", "2", "3-4", "5-9", "10+"))]
print(s[, .(pairs = .N, share = round(100 * .N / nrow(s), 1)), by = band][order(band)])

# --- SIGNAL: among pairs that HAVE met, does the record add anything? --------
p <- s[n_prior >= 1]
p[, lo_rate := lo_prior / n_prior]                       # prior win rate of lo
p[, lo_is_x := athlete_id.x == lo]
p[, rdiff := r.x - r.y]                                  # rating edge to x
# H2H edge to x, shrunk toward 0 by evidence: a 1-0 record says far less than 8-2
p[, h2h := fifelse(lo_is_x, lo_rate - 0.5, 0.5 - lo_rate) * (n_prior / (n_prior + 3))]
p[, x_won := place.x < place.y]
cat(sprintf("\npairs with a prior meeting: %s (%.1f%% of scored pairs)\n",
            format(nrow(p), big.mark = ","), 100 * nrow(p) / nrow(s)))
cat("\n=== does H2H predict the winner BEYOND the rating? ===\n")
cat("concordance on those pairs, blending rating with the H2H edge:\n\n")
sc <- rbindlist(lapply(c(0, 0.002, 0.005, 0.01, 0.02, 0.05), function(L) {
  v <- p$rdiff + L * p$h2h
  ok <- v != 0
  data.table(lambda = L, pairs = sum(ok),
             concordance = round(100 * mean((v[ok] > 0) == p$x_won[ok]), 3))
}))
print(sc)
cat("\nlambda 0 is the model as it stands. The H2H edge is in log-mark units,\n")
cat("so lambda 0.01 means a 10-0 record is worth about 0.5% of a mark.\n")
# is the signal there at all, independent of the blend?
cat(sprintf("\nraw check: among pairs where H2H and rating DISAGREE (%s pairs),\n",
            format(p[sign(h2h) != 0 & sign(h2h) != sign(rdiff), .N], big.mark = ",")))
dis <- p[sign(h2h) != 0 & sign(h2h) != sign(rdiff)]
cat(sprintf("  the rating is right %.1f%% of the time, H2H right %.1f%%\n",
            100 * mean((dis$rdiff > 0) == dis$x_won),
            100 * mean((dis$h2h > 0) == dis$x_won)))
