# Junior and youth implements are DIFFERENT EVENTS. A 6kg shot and a 7.26kg shot
# are not comparable marks, and 99.0cm hurdles are not 106.7cm hurdles. If those
# rows carry the senior event_id, every junior mark is inflating a senior rating.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("event_id","discipline","athlete_id","mark",
                                        "date","round","scoreable","age")))
c0 <- c0[is.finite(mark) & mark > 0]
c0[, qualified := grepl("(", discipline, fixed = TRUE)]
cat(sprintf("corpus rows with a qualified discipline: %s of %s (%.2f%%)\n",
            format(sum(c0$qualified), big.mark = ","),
            format(nrow(c0), big.mark = ","), 100 * mean(c0$qualified)))

cat("\n=== every qualified discipline, and the event_id it is filed under ===\n")
q <- c0[qualified == TRUE, .(rows = .N, athletes = uniqueN(athlete_id),
                             scoreable = sum(scoreable, na.rm = TRUE),
                             median_mark = round(stats::median(mark), 2),
                             median_age = round(stats::median(age, na.rm = TRUE), 1)),
        by = .(discipline, event_id)]
setorder(q, -rows)
print(head(q, 20))

cat("\n=== THE QUESTION: do junior marks share an event_id with senior ones? ===\n")
mapped <- q[!is.na(event_id)]
cat(sprintf("qualified disciplines mapped to ANY event_id: %d of %d\n",
            nrow(mapped), nrow(q)))
cat(sprintf("scoreable rows among qualified disciplines: %s\n",
            format(sum(c0[qualified == TRUE, scoreable], na.rm = TRUE), big.mark = ",")))
if (!nrow(mapped)) {
  cat("\nRESOLVED: every junior/youth implement is unmapped and unscored, so none\n")
  cat("of them can reach a senior rating. 110mH (99.0cm) is not filed under\n")
  cat("AT-110MetresHurdles-M, a 6kg shot is not filed under AT-ShotPut-M.\n")
} else {
  sen <- c0[qualified == FALSE & !is.na(event_id),
            .(srows = .N, smed = round(stats::median(mark), 2)), by = event_id]
  shared <- merge(mapped, sen, by = "event_id")
  if (nrow(shared)) {
    shared[, `:=`(pct_junior = round(100 * rows / (rows + srows), 2),
                  gap_pct = round(100 * (median_mark - smed) / smed, 1))]
    setorder(shared, -rows)
    print(head(shared[, .(event_id, discipline, rows, median_mark, srows, smed,
                          pct_junior, gap_pct)], 15))
  }
}
# A guard, so a future registry change that starts mapping these is caught.
stopifnot("a junior-implement discipline is now SCOREABLE under some event_id - it
would put 6kg shot marks into the senior rating" =
            sum(c0[qualified == TRUE, scoreable], na.rm = TRUE) == 0)
cat("\nguard passes: no qualified-implement row is scoreable\n")

cat("\n=== what is being left on the table ===\n")
cat(sprintf("%s unused rows over %s athletes, median age %.1f. These are real\n",
            format(sum(c0$qualified), big.mark = ","),
            format(uniqueN(c0[qualified == TRUE, athlete_id]), big.mark = ","),
            stats::median(c0[qualified == TRUE, age], na.rm = TRUE)))
cat("performances by young athletes that carry no weight anywhere. Rating them\n")
cat("would need an implement-adjustment factor per discipline - a separate job,\n")
cat("not a defect. See docs/backlog/.\n")
