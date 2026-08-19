# How much of the scored metric is thin-record and cold-start pairs?
#
# This sets the HARD CEILING on what better priors can buy. A cold-start athlete
# carries the event mean, so two of them have IDENTICAL ratings and the engine's
# `r_pre[i] > r_pre[j]` comparison is a coin flip by construction. If those pairs
# are 5% of the metric, better priors cannot matter; if they are a third, that
# is where the remaining points are.
#
# Scored exactly as the engine scores: 2026, finishers placing <= 12, races with
# >= 3 athletes (the loop skips smaller ones entirely).
suppressMessages(library(arrow)); suppressMessages(library(data.table))
h <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/data/seqv3_history_final.parquet"))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
setorder(h, athlete_id, event_id, date, race_key)
h[, n_prior := seq_len(.N) - 1L, by = .(athlete_id, event_id)]
setorder(h, date, race_key)
s <- h[year(date) == 2026 & place <= 12]
s[, nf := .N, by = race_key]; s <- s[nf >= 3]
s[, rid := .GRP, by = race_key]
s[, depth := fifelse(!seen, 0L, n_prior)]

a <- s[, .(rid, i = seq_len(.N), place, r_pre, depth)]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
m[, thin := pmin(depth.x, depth.y)]
m[, correct := (r_pre.x > r_pre.y) == (place.x < place.y)]   # engine's own rule
m[, band := cut(thin, c(-1, 0, 1, 3, 7, 15, Inf),
                labels = c("0 cold start", "1", "2-3", "4-7", "8-15", "16+"))]

tot <- nrow(m); base <- 100 * mean(m$correct)
r <- m[, .(pairs = .N, share = round(100 * .N / tot, 1),
           conc = round(100 * mean(correct), 2),
           tied = round(100 * mean(r_pre.x == r_pre.y), 1)), by = band][order(band)]
# headroom: what the overall metric becomes if a band were scored perfectly
r[, if_perfect := round(base + share/100 * (100 - conc), 2)]
cat(sprintf("2026 scored pairs: %s | overall concordance %.2f%%\n\n",
            format(tot, big.mark = ","), base))
cat("banded by the THINNER athlete's prior races in that event\n")
cat("tied = %% of pairs where both carry an identical rating (a coin flip)\n\n")
print(r)
cat(sprintf("\nboth athletes cold: %s pairs (%.1f%% of the metric), concordance %.2f%%\n",
            format(m[depth.x == 0 & depth.y == 0, .N], big.mark = ","),
            100 * m[, mean(depth.x == 0 & depth.y == 0)],
            100 * m[depth.x == 0 & depth.y == 0, mean(correct)]))
cat(sprintf("at least one cold:  %s pairs (%.1f%%), concordance %.2f%%\n",
            format(m[depth.x == 0 | depth.y == 0, .N], big.mark = ","),
            100 * m[, mean(depth.x == 0 | depth.y == 0)],
            100 * m[depth.x == 0 | depth.y == 0, mean(correct)]))
cat("\nif_perfect = the OVERALL metric if that band alone were scored 100%.\n")
cat("It is an upper bound nothing can reach, not a target.\n")
