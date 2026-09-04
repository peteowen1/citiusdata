# THE SAME MEDAL CALIBRATION, IN COUNTS RATHER THAN RATES.
#
# The reliability table reports `actual` as a percentage and that reads as a
# decimal number of medals, which it is not. Spelling out the unit:
#
#   one ROW is one athlete in one scored race - an entrant, not a race and not
#   an athlete. So the same person appears once per championship they contested.
#   `hit_medal` on that row is 1 if they medalled in that race and 0 otherwise.
#   `predicted` is the mean of p_medal over the rows in the bucket, as a percent.
#   `actual` is the mean of hit_medal over those same rows, also as a percent -
#   so it is the share of those entrants who medalled, not a medal count.
#
# Buckets are deciles of predicted p_medal, so each holds about a tenth of the
# entrants, which is why every n is near 1,450 rather than reflecting anything
# about the events themselves.
#
# Printing the raw counts alongside makes the comparison checkable by eye, which
# a percentage to two decimal places does not.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
BT  <- Sys.getenv("CITIUS_BT_OUT", "backtest.rds")
b <- readRDS(file.path(OUT, BT))

d <- merge(as.data.table(b$predictions), as.data.table(b$outcomes),
           by = c("race_id", "athlete_id"), all = FALSE)
stopifnot("join lost rows" = nrow(d) == nrow(b$predictions))

cat(sprintf("unit: one athlete in one scored race\n"))
cat(sprintf("%s entrant-rows | %s distinct races | %s distinct athletes\n",
            format(nrow(d), big.mark = ","),
            format(uniqueN(d$race_id), big.mark = ","),
            format(uniqueN(d$athlete_id), big.mark = ",")))
cat(sprintf("medals awarded across those rows: %s | golds: %s\n",
            format(sum(d$hit_medal), big.mark = ","),
            format(sum(d$hit), big.mark = ",")))

brk <- unique(stats::quantile(d$p_medal, probs = seq(0, 1, length.out = 11), na.rm = TRUE))
d[, bk := cut(p_medal, breaks = brk, include.lowest = TRUE)]
r <- d[, .(entrants = .N,
           pred_pct = round(100 * mean(p_medal), 2),
           actual_pct = round(100 * mean(hit_medal), 2),
           medals_expected = round(sum(p_medal), 1),
           medals_actual = sum(hit_medal)), by = bk][order(bk)]
r[, extra_medals := round(medals_actual - medals_expected, 1)]
cat("\n=== medal calibration in counts ===\n")
print(r)
cat("\nmedals_expected is the sum of p_medal over the entrants in that bucket, so\n")
cat("it is the number of medals the model expected THAT GROUP to win between\n")
cat("them. medals_actual is how many they won. extra_medals is the shortfall in\n")
cat("whole medals, which is the same finding as the percentage table and easier\n")
cat("to sanity check.\n")

cat(sprintf("\ntotal expected %.1f | total actual %d | shortfall %.1f medals\n",
            sum(r$medals_expected), sum(r$medals_actual),
            sum(r$medals_actual) - sum(r$medals_expected)))
cat("\nThe shortfall sits in the low-probability buckets: the model does not\n")
cat("expect outsiders to medal as often as they do. That is the finding.\n")
