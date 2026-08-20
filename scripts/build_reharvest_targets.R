# Which competitions are worth pulling through the competition endpoint?
#
# 24,089 competitions have no competition-route coverage, so their races carry a
# DERIVED key (competition|event|round|date) which cannot separate age divisions -
# a U18 400m and a senior 400m at the same meet on the same day become one race.
# Re-harvesting all of them is roughly seven hours at the documented rate limit.
#
# Most of them are not worth it. A competition only suffers if it actually RAN
# more than one division of the same event, and the visible symptom is a
# finishing place held by two athletes with different marks inside one derived
# race. Ranking by how many rows that touches puts the seven hours where it buys
# something.
#
# Deliberately NOT ranked by meet tier. A club meet with six age divisions is
# more corrupted than a championship with none, and tier would invert that.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
OUT <- Sys.getenv("REHARVEST_OUT", "reharvest_targets.csv")
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("race_key","competition_id","athlete_id",
                                        "place","perf","scoreable","source","date")))
c0 <- c0[scoreable == TRUE & is.finite(perf) & !is.na(race_key)]
c0[, athlete_id := as.character(athlete_id)]
stopifnot("corpus came back empty" = nrow(c0) > 100000)

# competitions we have never pulled through the competition route
cov <- c0[, .(rows = .N, has_comp = any(source == "competition"),
              last = max(date)), by = competition_id]
cand <- cov[has_comp == FALSE]
cat(sprintf("competitions with no competition-route coverage: %s (%s rows)\n",
            format(nrow(cand), big.mark = ","), format(sum(cand$rows), big.mark = ",")))

# the symptom: one place, two athletes, two different marks, inside a derived key
dk <- c0[competition_id %chin% cand$competition_id & !is.na(place) & place > 0]
bad <- dk[, .(ath = uniqueN(athlete_id), marks = uniqueN(round(perf, 9))),
          by = .(competition_id, race_key, place)][ath > 1 & marks > 1]
hit <- unique(bad[, .(competition_id, race_key)])
aff <- dk[hit, on = .(competition_id, race_key), .N, by = competition_id]
setnames(aff, "N", "rows_in_merged_races")
t <- merge(cand[, .(competition_id, rows, last)], aff, by = "competition_id")
setorder(t, -rows_in_merged_races)
cat(sprintf("of those, showing a detectable merge: %s competitions, %s affected rows\n",
            format(nrow(t), big.mark = ","),
            format(sum(t$rows_in_merged_races), big.mark = ",")))
cat("\n=== how far down do you have to go? ===\n")
t[, cum := cumsum(rows_in_merged_races)]
tot <- sum(t$rows_in_merged_races)
for (n in c(50, 100, 250, 500, 1000, 2000)) {
  if (n > nrow(t)) next
  cat(sprintf("  top %5d competitions -> %5.1f%% of affected rows\n", n, 100 * t$cum[n] / tot))
}
cat("\n=== the twenty worst ===\n")
print(t[seq_len(min(20L, .N)), .(competition_id, total_rows = rows,
                                 rows_in_merged_races, last)])
f <- file.path(D, OUT)
fwrite(t[, .(competition_id, total_rows = rows, rows_in_merged_races, last)], f)
cat(sprintf("\nwrote %s (%s competitions, ranked)\n", OUT, format(nrow(t), big.mark = ",")))
cat("Re-harvest from the top down; each one pulled gains an authoritative key\n")
cat("carrying eventName, which is what separates the age divisions.\n")
