# ARE THE PRE-2020 T1 MEETS ALREADY ON DISK?
#
# The catalogue reports 119,909 results across the 155 pre-2020 T1 competitions.
# It could only know those counts from somewhere, and there are two very
# different possibilities:
#
#   already harvested, excluded downstream - the results are cached and the
#     corpus simply starts in 2020. Then this is a one-line date change plus a
#     rebuild, not a scrape, and harvesting would re-fetch data we hold.
#   catalogue built from feed metadata - the counts come from the competition
#     listing without the results ever being fetched. Then it is a real scrape.
#
# Getting this wrong in the expensive direction means hours of rate-limited
# requests for data already on disk, so check before fetching anything.
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

c0 <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
c0[, first_date := as.Date(first_date)]
pre <- c0[meet_tier == "T1_elite" & first_date < as.Date("2020-01-01")]
cat(sprintf("pre-2020 T1 competitions in the catalogue: %d, %s catalogued results\n",
            nrow(pre), format(sum(pre$results, na.rm = TRUE), big.mark = ",")))

# every per-competition cache we know about
CACHES <- c("ath_comp_cache", "ath_comp_cache_majors")
for (cn in CACHES) {
  p <- file.path(D, cn)
  if (!dir.exists(p)) { cat(sprintf("%-24s absent\n", cn)); next }
  ids <- sub("\\.rds$", "", list.files(p, pattern = "\\.rds$"))
  cat(sprintf("%-24s %s cached competitions, %d of our 155 present\n",
              cn, format(length(ids), big.mark = ","),
              sum(pre$competition_id %chin% ids)))
}

# and the assembled results files
for (fn in c("championship_results.rds", "championship_results_majors.rds")) {
  f <- file.path(D, fn)
  if (!file.exists(f)) { cat(sprintf("%-34s absent\n", fn)); next }
  r <- setDT(readRDS(f))
  r[, competition_id := as.character(competition_id)]
  hit <- pre$competition_id %chin% unique(r$competition_id)
  cat(sprintf("%-34s %s rows | holds %d of our 155 | its date range %s..%s\n",
              fn, format(nrow(r), big.mark = ","), sum(hit),
              min(as.Date(r$date), na.rm = TRUE), max(as.Date(r$date), na.rm = TRUE)))
}

# the corpus store, which is what the engine actually reads
st <- file.path(D, "athletics_corpus_store")
if (dir.exists(st)) {
  ds <- arrow::open_dataset(st)
  dr <- setDT(as.data.frame(ds |> dplyr::select(date) |> dplyr::filter(!is.na(date))))
  cat(sprintf("%-34s %s rows | %s..%s\n", "athletics_corpus_store",
              format(nrow(dr), big.mark = ","), min(dr$date), max(dr$date)))
}
cat("\nIf a cache already holds most of the 155, this is a rebuild, not a scrape.\n")
