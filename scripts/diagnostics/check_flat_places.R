# Races where every athlete shares one finishing place but the marks differ.
# Split by key provenance: an AUTHORITATIVE key comes from the API's own
# raceId/raceNumber, so we know those athletes really were one race and the
# order can be recovered from the marks. A DERIVED key
# (competition|AT-event|round|date) cannot rule out merged sections.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select=c("race_key","event_id","athlete_id","place","perf",
                                      "scoreable","source")))
c0 <- c0[scoreable==TRUE & is.finite(perf) & !is.na(place) & place>0 & !is.na(race_key)]
c0[, athlete_id := as.character(athlete_id)]
r <- c0[, .(n=.N, ath=uniqueN(athlete_id), places=uniqueN(place),
            marks=uniqueN(round(perf,9))), by=race_key]
flat <- r[places==1L & ath>1L & marks>1L]      # one place, several athletes, several marks
flat[, derived := grepl("|AT-", race_key, fixed=TRUE)]
cat(sprintf("races where ALL places are identical but marks differ: %s\n",
            format(nrow(flat), big.mark=",")))
print(flat[, .(races=.N, rows=sum(n), median_field=as.numeric(stats::median(n)),
               max_field=max(n)), by=.(key = fifelse(derived, "derived (unsafe)", "authoritative (fixable)"))])
cat("\n=== the biggest authoritative ones ===\n")
print(flat[derived==FALSE][order(-n)][seq_len(min(8L,.N)), .(race_key, athletes=ath, marks)])
cat(sprintf("\ntotal rows recoverable: %s\n",
            format(sum(flat[derived==FALSE, n]), big.mark=",")))
