# WHAT WOULD AN EARLIER ENGINE START DATE ADMIT?
#
# form_ratings.R cuts the corpus at 2020-01-01 with a bare constant and no
# comment. The data behind that cut is not missing - championship_results.rds
# spans 1982 to 2026 and the corpus store reaches 1974 - so the cut is a choice
# nobody has recently re-examined.
#
# It matters because the T1 metric's noise floor is set by how many elite pairs
# exist, and six seasons is all the current cut allows. But more history is not
# free: it is sequential work for the engine, older results are thinner and
# noisier, and the per-event wind and aging fits were estimated on the current
# span. So measure what each candidate start actually buys - in T1 rows, T1
# competitions and total rows - before running anything.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
tier <- c0[, .(competition_id, meet_tier)]

ds <- open_dataset(file.path(D, "athletics_corpus_store"))
d <- setDT(as.data.frame(ds |> dplyr::select(competition_id, date, perf, place)))
d[, competition_id := as.character(competition_id)]
d <- d[!is.na(date) & !is.na(perf) & !is.na(place) & place > 0]
d <- merge(d, tier, by = "competition_id", all.x = TRUE)
d[, yr := year(as.Date(date))]
cat(sprintf("corpus store, scoreable rows: %s | %s to %s\n",
            format(nrow(d), big.mark = ","), min(d$yr), max(d$yr)))

cat("\n=== what each candidate start date admits ===\n")
res <- rbindlist(lapply(c(2005, 2010, 2012, 2014, 2016, 2018, 2020), function(y) {
  s <- d[yr >= y]
  data.table(from = y,
             total_rows = nrow(s),
             t1_rows    = s[meet_tier == "T1_elite", .N],
             t1_comps   = s[meet_tier == "T1_elite", uniqueN(competition_id)],
             t2_rows    = s[meet_tier == "T2_strong", .N])
}))
res[, t1_vs_2020 := round(t1_rows / res[from == 2020, t1_rows], 2)]
res[, rows_vs_2020 := round(total_rows / res[from == 2020, total_rows], 2)]
print(res)

cat("\n=== T1 rows by year, so the thin early years are visible ===\n")
print(d[meet_tier == "T1_elite" & yr >= 2000,
        .(comps = uniqueN(competition_id), rows = .N), by = yr][order(yr)])

cat("\nt1_vs_2020 is the multiplier on elite sample. The T1 concordance floor\n")
cat("falls as 1/sqrt of it, so 4x the rows roughly halves the floor - but\n")
cat("rows_vs_2020 is the multiplier on sequential engine work, and the early\n")
cat("years are thin enough that the two ratios are not the same.\n")
