# SUPERSEDED along with check_era_effect.R -- built on the same broken
# meet_tier/wrong-tier-code population (see that file's header). The athlete-
# concentration/turnover METHOD here is still sound and worth reusing if
# check_era_effect3.R's corrected population ever shows a real level shift to
# investigate; the specific Shot Put numbers below are not trustworthy.
#
# Follow-up: is the Shot Put "control" shift a field-wide improvement or a
# handful of athletes racking up repeat top-20 results? Distinguishes "the
# population got better" from "one or two athletes got better," which the
# top-20-marks method from check_era_effect.R cannot tell apart on its own.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

x <- readRDS(file.path(D, "era_effect_check.rds"))
annual <- x$annual

corpus <- setDT(readRDS(file.path(D, "athletics_corpus.rds")))
cat_tbl <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
corpus[, competition_id := as.character(competition_id)]
corpus <- merge(corpus, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
t1 <- corpus[meet_tier == "T1_elite" & !is.na(mark) & !is.na(date)]
t1[, year := data.table::year(date)]
t1[, oriented := mark * orientation]

top20_athletes <- function(disc, sx, yrs) {
  d <- t1[discipline == disc & sex == sx & year %in% yrs]
  data.table::setorder(d, year, -oriented)
  d[, rk := seq_len(.N), by = year]
  d <- d[rk <= 20]
  data.table(discipline = disc, sex = sx,
             window = paste(min(yrs), max(yrs), sep = "-"),
             n_marks = nrow(d), n_distinct_athletes = uniqueN(d$athlete_id),
             top_athlete_share = if (nrow(d)) max(table(d$athlete_id)) / nrow(d) else NA)
}

checks <- rbindlist(list(
  top20_athletes("Shot Put", "M", 2012:2016), top20_athletes("Shot Put", "M", 2017:2023),
  top20_athletes("Shot Put", "W", 2012:2016), top20_athletes("Shot Put", "W", 2017:2023),
  top20_athletes("Marathon", "M", 2012:2016), top20_athletes("Marathon", "M", 2017:2023),
  top20_athletes("800 Metres", "M", 2016:2020), top20_athletes("800 Metres", "M", 2021:2026),
  top20_athletes("Discus Throw", "M", 2012:2016), top20_athletes("Discus Throw", "M", 2017:2023)
))
cat("=== distinct-athlete concentration in the top-20 pool, pre vs post ===\n")
print(checks)

# Same question a different way: does the SAME set of athletes dominate both
# windows (survivorship / a talent generation), or did the population turn
# over (consistent with a field-wide tech effect lifting whoever is racing)?
overlap <- function(disc, sx, pre, post) {
  a <- unique(t1[discipline == disc & sex == sx & year %in% pre][order(-oriented)][1:min(30,.N)]$athlete_id)
  b <- unique(t1[discipline == disc & sex == sx & year %in% post][order(-oriented)][1:min(30,.N)]$athlete_id)
  data.table(discipline = disc, sex = sx, pre_n = length(a), post_n = length(b),
             overlap_n = length(intersect(a, b)), overlap_pct = 100*length(intersect(a,b))/length(union(a,b)))
}
cat("\n=== athlete-set overlap, top-30-by-mark pre vs post (low overlap = turnover, not the same stars) ===\n")
print(rbindlist(list(
  overlap("Shot Put", "M", 2012:2016, 2019:2023),
  overlap("Marathon", "M", 2012:2016, 2019:2023),
  overlap("800 Metres", "M", 2016:2020, 2022:2026),
  overlap("Discus Throw", "M", 2012:2016, 2019:2023)
)))
