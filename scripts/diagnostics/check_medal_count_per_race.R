# WHERE DO 4,185 MEDALS COME FROM IN 1,346 RACES?
#
# The medal reliability table showed the model short by 147 medals overall and
# short in most buckets, which was read as the model underpredicting outsiders.
# The totals say that reading cannot be right on its own:
#
#   model expected 4,038.0 medals   and   1,346 races x 3 = 4,038 exactly
#   actually awarded 4,185
#
# So p_medal sums to exactly three per race by construction - the model cannot
# be short in aggregate, because the probability it has to spend is fixed. Every
# medal beyond 4,038 is a race where the OUTCOME data records more than three
# medallists. That is ties: dead heats, shared bronze, and any race where the
# place column repeats.
#
# This matters because those extra medals are unwinnable by construction. A
# model that allocates three medals per race is scored against a world that
# handed out 3.11, so it looks short everywhere, and the shortfall lands hardest
# in the buckets with the most entrants per medal - the longshots. That would
# manufacture exactly the pattern reported, without the model being wrong at all.
#
# So count them before believing anything about calibration.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
BT  <- Sys.getenv("CITIUS_BT_OUT", "backtest.rds")
b <- readRDS(file.path(OUT, BT))

d <- merge(as.data.table(b$predictions), as.data.table(b$outcomes),
           by = c("race_id", "athlete_id"), all = FALSE)
stopifnot("join lost rows" = nrow(d) == nrow(b$predictions))

per <- d[, .(medals = sum(hit_medal), golds = sum(hit),
             entrants = .N, p_sum = sum(p_medal)), by = race_id]
cat(sprintf("%s races | %s medals | %s golds\n",
            format(nrow(per), big.mark = ","),
            format(sum(per$medals), big.mark = ","),
            format(sum(per$golds), big.mark = ",")))
cat(sprintf("model probability spent per race: median %.3f, min %.3f, max %.3f\n",
            median(per$p_sum), min(per$p_sum), max(per$p_sum)))

cat("\n=== medals actually recorded per race ===\n")
print(per[, .(races = .N, medals_total = sum(medals)), by = medals][order(medals)])

cat("\n=== golds actually recorded per race ===\n")
print(per[, .(races = .N), by = golds][order(golds)])

ex <- per[medals > 3]
cat(sprintf("\nraces recording MORE than 3 medals: %s of %s (%.1f%%), carrying %s extra medals\n",
            format(nrow(ex), big.mark = ","), format(nrow(per), big.mark = ","),
            100 * nrow(ex) / nrow(per),
            format(sum(ex$medals) - 3 * nrow(ex), big.mark = ",")))
short <- per[medals < 3]
cat(sprintf("races recording FEWER than 3: %s, %s medals missing\n",
            format(nrow(short), big.mark = ","),
            format(3 * nrow(short) - sum(short$medals), big.mark = ",")))
cat(sprintf("net against 3 per race: %+d medals\n",
            sum(per$medals) - 3L * nrow(per)))

# THE HONEST RE-TEST. Score only races that awarded exactly three medals, where
# the model's fixed three units of probability and the world's three medals are
# the same quantity. If the bucket pattern survives that, it is about the model;
# if it collapses, it was about the ties.
ok <- per[medals == 3L, race_id]
cat(sprintf("\n=== re-scored on the %s races awarding exactly 3 medals ===\n",
            format(length(ok), big.mark = ",")))
k <- d[race_id %chin% ok | race_id %in% ok]
stopifnot("no rows survived the exactly-three filter" = nrow(k) > 0)
brk <- unique(stats::quantile(k$p_medal, probs = seq(0, 1, length.out = 11), na.rm = TRUE))
k[, bk := cut(p_medal, breaks = brk, include.lowest = TRUE)]
r <- k[, .(entrants = .N,
           expected = round(sum(p_medal), 1),
           actual = sum(hit_medal)), by = bk][order(bk)]
r[, extra := round(actual - expected, 1)]
print(r)
cat(sprintf("\ntotal expected %.1f | total actual %d | net %+.1f\n",
            sum(r$expected), sum(r$actual), sum(r$actual) - sum(r$expected)))
cat("\nIf net is near zero here, the 147-medal shortfall was ties, and any\n")
cat("remaining bucket pattern is the real calibration signal.\n")
