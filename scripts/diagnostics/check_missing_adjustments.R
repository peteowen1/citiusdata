# What other systematic effects are sitting in the corpus unmodelled?
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select=c("athlete_id","event_id","date","mark","mark_string",
                                      "perf","scoreable","legal","wind","indoor",
                                      "venue_city","venue_stadium","is_technical")))
c0 <- c0[scoreable == TRUE & is.finite(perf)]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
c0 <- merge(c0, reg, by="event_id")
cat(sprintf("scoreable marks: %s\n\n", format(nrow(c0), big.mark=",")))

cat("=== 1. TIMING PRECISION (hand vs electronic) ===\n")
tr <- c0[family %chin% c("sprint","hurdles","middle") & !is.na(mark_string)]
# decimals after the final "." in the cleaned numeric string
tr[, clean := gsub("[^0-9.]", "", mark_string)]
tr[, dp := nchar(clean) - as.integer(regexpr("[.][^.]*$", clean)) ]
tr[!grepl("[.]", clean), dp := 0L]
tr[, hand_like := dp <= 1]
print(tr[, .(marks=.N, one_dp=sum(hand_like), pct=round(100*mean(hand_like),2)), by=family][order(-pct)])
cat("A time given to ONE decimal is almost always hand-timed. Hand timing reads\n")
cat("about 0.2s fast over 100m because a human starts the watch on the smoke.\n")

cat("\n=== 2. venue_stadium: finer than the city we correct on ===\n")
v <- c0[!is.na(venue_city) & nzchar(venue_city)]
cat(sprintf("venue_stadium populated on %.1f%% of marks\n", 100*mean(!is.na(v$venue_stadium) & nzchar(v$venue_stadium))))
multi <- v[!is.na(venue_stadium) & nzchar(venue_stadium),
           .(stadiums = uniqueN(venue_stadium), marks=.N), by=venue_city][stadiums > 1]
cat(sprintf("cities with more than one stadium: %d (covering %s marks)\n",
            nrow(multi), format(sum(multi$marks), big.mark=",")))
print(multi[order(-marks)][seq_len(min(5L,.N))])

cat("\n=== 3. LEGAL flag: is it doing anything? ===\n")
print(c0[family %chin% c("sprint","jump"), .(marks=.N), by=.(legal=as.character(legal))][order(-marks)])

cat("\n=== 4. ERA: has the distance-running level stepped? ===\n")
d <- c0[family %chin% c("distance","road") & year(date) >= 2015]
d[, yr := year(date)]
d[, n_ath := .N, by=.(athlete_id, event_id)]
dd <- d[n_ath >= 4]
dd[, y := perf - mean(perf), by=.(athlete_id, event_id)]
print(dd[, .(marks=.N, athletes=uniqueN(athlete_id), rel_pct=round(100*(exp(mean(y))-1),3)), by=yr][order(yr)])
cat("Within-athlete, so ability cancels. A step rather than a drift would be the\n")
cat("shoe-technology change; a drift is just the sport getting faster.\n")
