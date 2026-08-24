# Can ATHLETE AGE separate the age divisions we merged?
#
# The WA page for Ypsilanti shows Alena Riva (b. 1995) winning the senior 400m
# and Alice Bucher (b. 2008) winning the U18 400m - both stored as "1st" in one
# race. Thirteen years apart. If the corpus carries usable ages, the divisions
# may be separable without re-harvesting anything, which would reach the 20,815
# competitions the targeted harvest deliberately leaves alone.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("race_key","athlete_id","place","perf","age",
                                        "scoreable","source")))
c0 <- c0[scoreable == TRUE & is.finite(perf) & !is.na(place) & place > 0 & !is.na(race_key)]
c0[, athlete_id := as.character(athlete_id)]
cat(sprintf("scoreable rows: %s | age populated on %.1f%%\n",
            format(nrow(c0), big.mark = ","), 100 * mean(is.finite(c0$age))))

# the merged set: a place held by different athletes with different marks
bad <- c0[, .(ath = uniqueN(athlete_id), marks = uniqueN(round(perf, 9))),
          by = .(race_key, place)][ath > 1 & marks > 1, unique(race_key)]
b <- c0[race_key %chin% bad]
cat(sprintf("merged races: %s | rows: %s | age populated on %.1f%% of them\n",
            format(length(bad), big.mark = ","), format(nrow(b), big.mark = ","),
            100 * mean(is.finite(b$age))))

# Within a merged race, does age spread the way separate divisions would?
# A single senior field is tight; senior + U18 is bimodal with a real gap.
g <- b[is.finite(age), .(n = .N, ages = uniqueN(age), span = max(age) - min(age),
                         gap = { a <- sort(unique(age)); if (length(a) > 1) max(diff(a)) else 0 }),
       by = race_key][n >= 4]
cat(sprintf("\nmerged races with 4+ aged rows: %s\n", format(nrow(g), big.mark = ",")))
print(g[, .(races = .N, median_span = as.numeric(stats::median(span)),
            median_largest_gap = as.numeric(stats::median(gap)),
            pct_gap_3plus = round(100 * mean(gap >= 3), 1))])
cat("\nspan = oldest minus youngest in the race. gap = the biggest jump between\n")
cat("consecutive ages present. A genuine single race has a smooth spread; two\n")
cat("divisions stacked together leave a hole where neither field has athletes.\n")

# a control: the SAME statistics on races we know are clean
ok <- c0[!race_key %chin% bad & is.finite(age),
         .(n = .N, span = max(age) - min(age),
           gap = { a <- sort(unique(age)); if (length(a) > 1) max(diff(a)) else 0 }),
       by = race_key][n >= 4]
cat("\n=== control: races with no sign of a merge ===\n")
print(ok[, .(races = .N, median_span = as.numeric(stats::median(span)),
             median_largest_gap = as.numeric(stats::median(gap)),
             pct_gap_3plus = round(100 * mean(gap >= 3), 1))])
cat("\nIf the merged set does not separate from the control, age cannot do this\n")
cat("job and the harvest is the only route.\n")
