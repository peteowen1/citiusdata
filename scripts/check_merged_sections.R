# race_key is competition|event|round|date, which has no section identifier - so
# parallel sections of the same round on the same day collapse into one "race".
# Concordance is pairwise WITHIN a race, so those pairs compare athletes who
# never met, and the race shock is estimated across sections that had different
# conditions.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                        col_select=c("race_key","event_id","place","seen","perf","r_use","date")))
h <- h[seen == TRUE & is.finite(place) & place > 0 & place <= 12]
# a duplicated finishing position inside one race_key is unambiguous: two
# athletes cannot both finish 3rd in the same race
r <- h[, .(n = .N, dup = .N - uniqueN(place)), by = race_key]
cat(sprintf("scored races: %s | containing a duplicate finishing place: %s (%.2f%%)\n",
            format(nrow(r), big.mark=","), format(sum(r$dup>0), big.mark=","),
            100*mean(r$dup>0)))
cat(sprintf("scored rows inside those races: %s of %s (%.2f%%)\n",
            format(sum(r[dup>0, n]), big.mark=","), format(sum(r$n), big.mark=","),
            100*sum(r[dup>0, n])/sum(r$n)))

# how many CONCORDANCE PAIRS come from them - the quantity that matters, since a
# merged race contributes pairs quadratically in its size
r[, pairs := n*(n-1)/2]
cat(sprintf("\nconcordance pairs from merged races: %s of %s (%.2f%%)\n",
            format(sum(r[dup>0, pairs]), big.mark=","), format(sum(r$pairs), big.mark=","),
            100*sum(r[dup>0, pairs])/sum(r$pairs)))
cat("Quadratic in race size, so a few large merged races carry more weight than\n")
cat("their row count suggests - which is why pairs is the number to quote.\n")
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h2 <- merge(h, r[, .(race_key, dup)], by="race_key")
h2 <- merge(h2, reg, by="event_id")
print(h2[, .(rows=.N, pct_in_merged=round(100*mean(dup>0),2)), by=family][order(-pct_in_merged)])
