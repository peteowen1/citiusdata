# WHY DID THE DEBUT PRIOR CHANGE NOTHING ON THE WA BENCHMARK?
#
# dp_id and dp_rep returned identical figures to every decimal across all five
# championship windows. Identical numbers from two arms that are supposed to
# differ is the signal that has cost this project the most - a scorer reading
# the deployed model while being asked about an arm produced exactly this, and
# identical-to-the-digit was the only tell.
#
# The tag mechanism is not the problem here: `final` returns 76.35 pooled where
# dp_id returns 76.37, so the scorer demonstrably reads different files for
# different tags. That leaves a claim about the POPULATION - that WA-ranked
# athletes in championship fields are essentially never debutants - which is
# plausible, because reaching WA's top 200 in an event requires a season of
# results. Plausible is not measured. Count them.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

MAP <- data.table(
  event_slug = c("100m","200m","400m","800m","1500m","5000m","10000m","marathon",
                 "110mh","100mh","400mh","high-jump","pole-vault","long-jump",
                 "triple-jump","shot-put","discus-throw","hammer-throw","javelin-throw"),
  stem = c("100Metres","200Metres","400Metres","800Metres","1500Metres","5000Metres",
           "10000Metres","Marathon","110MetresHurdles","100MetresHurdles",
           "400MetresHurdles","HighJump","PoleVault","LongJump","TripleJump",
           "ShotPut","DiscusThrow","HammerThrow","JavelinThrow"))

w <- setDT(read_parquet(file.path(D, "wa_rankings_dated.parquet"), mmap = FALSE))
w[, athlete_id := as.character(athlete_id)]
w <- merge(w, MAP, by = "event_slug")
w[, event_id := sprintf("AT-%s-%s", stem, fifelse(sex == "men", "M", "W"))]
w[, rank_date := as.Date(rank_date)]

h <- setDT(read_parquet(file.path(D, "seqv3_history_dp_id.parquet"), mmap = FALSE))
h[, athlete_id := as.character(athlete_id)]
h[, date := as.Date(date)]
h <- h[is.finite(place) & place > 0]

MEETS <- data.table(
  meet = c("Tokyo 2020","Eugene 2022","Budapest 2023","Paris 2024","Tokyo 2025"),
  from = as.Date(c("2021-07-30","2022-07-15","2023-08-19","2024-08-01","2025-09-13")),
  to   = as.Date(c("2021-08-08","2022-07-24","2023-08-27","2024-08-11","2025-09-21")))
avail <- sort(unique(w$rank_date))
MEETS[, rank_date := as.Date(vapply(from, function(f) as.character(max(avail[avail < f])),
                                    character(1)))]

out <- rbindlist(lapply(seq_len(nrow(MEETS)), function(i) {
  M <- MEETS[i]
  d <- h[date >= M$from & date <= M$to]
  wk <- w[rank_date == M$rank_date, .(athlete_id, event_id, wa_place)]
  d <- merge(d, wk, by = c("athlete_id","event_id"), all.x = TRUE)
  data.table(meet = M$meet,
             rows = nrow(d),
             cold_all = sum(!d$seen),
             ranked = sum(!is.na(d$wa_place)),
             cold_and_ranked = sum(!is.na(d$wa_place) & !d$seen))
}))
out[, pct_cold_all    := round(100 * cold_all / rows, 1)]
out[, pct_cold_ranked := round(100 * cold_and_ranked / pmax(ranked, 1), 2)]
print(out)

tot <- out[, .(rows = sum(rows), ranked = sum(ranked),
               cold_ranked = sum(cold_and_ranked))]
cat(sprintf("\nacross all five meets: %s WA-ranked rows, of which %s are cold (%.2f%%)\n",
            format(tot$ranked, big.mark = ","), format(tot$cold_ranked, big.mark = ","),
            100 * tot$cold_ranked / tot$ranked))
cat("\nThe debut prior can only change a row where the athlete has no prior\n")
cat("rating in the event. If that count is zero or near it among WA-ranked\n")
cat("rows, two arms differing ONLY in that prior must return identical figures,\n")
cat("and the identity is a fact about who WA ranks rather than a broken scorer.\n")
cat("Being in a top-200 world ranking requires a season of results, so the\n")
cat("benchmark population is by construction the one this fix cannot reach.\n")
