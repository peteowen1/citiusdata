# DOES ANY RACE IN THE CURRENT DATA STILL AWARD MORE THAN THREE MEDALS?
#
# 34 races in the stored backtest awarded up to 20 medals, which is what made a
# perfectly calibrated model look 3.5 standard errors miscalibrated. Only 2 of
# those 34 race_ids still exist in championship_results.rds, and the first one
# reads
#
#     7104835|10229607|Split times|10|f318127400n22
#
# with "Split times" sitting in the event-name field. So they are pre-keyfix
# keys from a backtest written 2026-08-12, before the race key was corrected,
# and split-time rows were being treated as finishing positions.
#
# That makes the stale backtest a dead end and raises the only question worth
# answering: is the corruption still there NOW. A backtest is regenerable; a
# corrupt shared outcomes file is not, and it feeds every arm.
#
# THE TEST IS A CONSERVATION LAW. Exactly three medals per race is not a
# convention, it is a fact about the sport, so it can be asserted rather than
# hoped for. Count distinct athletes at places 1 to 3 per race and find every
# race that cannot be right. Ties legitimately produce four - two silvers and no
# bronze, or two bronzes - so the signal to chase is not "more than three", it is
# "many more than three", and relays produce a whole team per position.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
cat(sprintf("championship_results.rds: %s rows\n", format(nrow(ch), big.mark = ",")))
pcol <- intersect(c("place", "position", "rank"), names(ch))[1]
if (is.na(pcol)) {
  cat("no place column; columns are:\n"); print(names(ch)); quit(status = 0)
}
ch <- ch[is.finite(get(pcol)) & get(pcol) > 0]
cat(sprintf("with a usable %s: %s rows | %s races\n", pcol,
            format(nrow(ch), big.mark = ","),
            format(uniqueN(ch$race_key), big.mark = ",")))

pod <- ch[get(pcol) <= 3, .(podium_rows = .N,
                            podium_athletes = uniqueN(athlete_id),
                            at1 = sum(get(pcol) == 1),
                            at2 = sum(get(pcol) == 2),
                            at3 = sum(get(pcol) == 3)), by = .(race_key, event_id)]
cat(sprintf("\nraces with any podium row: %s\n", format(nrow(pod), big.mark = ",")))
cat("\n=== distribution of podium athletes per race ===\n")
print(pod[, .(races = .N), by = podium_athletes][order(podium_athletes)])

bad <- pod[podium_athletes > 4][order(-podium_athletes)]
cat(sprintf("\nraces with MORE THAN 4 podium athletes (beyond what a tie explains): %s\n",
            format(nrow(bad), big.mark = ",")))
if (nrow(bad)) {
  cat("\n=== the worst 15 ===\n")
  print(utils::head(bad[, .(race_key = substr(race_key, 1, 46), event_id,
                            podium_athletes, at1, at2, at3)], 15))
  cat("\n=== which events do they sit in ===\n")
  print(bad[, .(races = .N, worst = max(podium_athletes)), by = event_id][order(-races)][1:12])
  # Is the event-name field of the race key the tell, as it was for the stale
  # backtest? Third pipe-separated field.
  bad[, ename := tstrsplit(race_key, "|", fixed = TRUE, keep = 3L)[[1]]]
  cat("\n=== event-name field of the race key, which is where 'Split times' lived ===\n")
  print(bad[, .(races = .N), by = ename][order(-races)][1:12])
} else {
  cat("\nNone. Every race in the current file has a podium a tie could explain,\n")
  cat("so the corruption that broke the medal calibration is not live - it was\n")
  cat("in the pre-keyfix data the 2026-08-12 backtest was built on, and\n")
  cat("regenerating the backtest is what clears it.\n")
}
