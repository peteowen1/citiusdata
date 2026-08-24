# WHY DO ONLY 88 OF THE 250 T1 COMPETITIONS APPEAR IN THE SCORED HISTORY?
#
# The catalogue holds 250 T1_elite competitions. The scored history contains
# rows from 88 of them. Before quoting a T1-only concordance as the headline
# metric, that gap has to be explained: if 162 elite meets are silently absent,
# the metric describes a third of the population it claims to.
#
# Three explanations, and they have completely different consequences:
#   out of range  - the meet predates the corpus. Benign, and the honest fix is
#                   to state the metric's span rather than to widen it.
#   no scored rows - harvested, in range, but produced nothing the engine scores
#                   (no comparable pairs, unmodelled events, missing places).
#                   Benign per meet but worth knowing the size of.
#   unharvested   - in range and simply not fetched. That is a data gap in the
#                   most important tier, and it is work, not a caveat.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
h[, date := as.Date(date)]
BAR <- "|"
h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
span <- range(h$date)
cat(sprintf("scored history spans %s to %s\n", span[1], span[2]))

c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
t1 <- c0[meet_tier == "T1_elite"]
t1[, first_date := as.Date(first_date)]
cat(sprintf("catalogue T1 competitions: %s, spanning %s to %s\n",
            format(nrow(t1), big.mark = ","),
            min(t1$first_date, na.rm = TRUE), max(t1$first_date, na.rm = TRUE)))

scored <- unique(h$competition_id)
t1[, has_rows := competition_id %chin% scored]
t1[, in_range := !is.na(first_date) & first_date >= span[1] & first_date <= span[2]]

cat("\n=== T1 competitions by whether they reach the scored history ===\n")
print(t1[, .(comps = .N, results = sum(results, na.rm = TRUE)),
         by = .(in_range, has_rows)][order(-in_range, -has_rows)])

cat("\n=== T1 by year, scored against catalogued ===\n")
print(t1[, .(catalogued = .N, scored = sum(has_rows)),
         by = .(yr = year(first_date))][order(yr)])

# THE ONE THAT MATTERS: in range, holds results, and still absent.
gap <- t1[in_range == TRUE & has_rows == FALSE]
cat(sprintf("\n=== IN RANGE BUT NOT SCORED: %d competitions ===\n", nrow(gap)))
if (nrow(gap)) {
  cat(sprintf("they hold %s catalogued results between them\n",
              format(sum(gap$results, na.rm = TRUE), big.mark = ",")))
  print(head(gap[order(-results),
                 .(comp_name, yr = year(first_date), results, races, athletes)], 25))
} else cat("none - every in-range T1 meet reaches the metric\n")
