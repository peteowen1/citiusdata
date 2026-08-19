# 27.9% of cold starts have held prior results — but a 2016 mark is worth far
# less than a 2025 one, and the corpus lead-in starts 2020-01-01, so the priors
# could be mostly pre-lead-in and stale. That decides whether this is worth
# building, so measure it before recommending it.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
ca <- setDT(as.data.frame(open_dataset(file.path(D, "athletics_careers_store")) |>
       dplyr::select(athlete_id, date, perf, discipline, sex, tier)))
ca <- ca[!is.na(perf) & is.finite(perf) & !is.na(date)]
ca[, event_id := paste0("AT-", gsub("[^A-Za-z0-9]", "", discipline), "-", sex)]
ca[, athlete_id := as.character(athlete_id)]
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
h[, athlete_id := as.character(athlete_id)]
cold <- h[year(date) == 2026 & place <= 12 & seen == FALSE,
          .(first_seen = min(date)), by = .(athlete_id, event_id)]
m <- ca[cold, on = .(athlete_id, event_id), allow.cartesian = TRUE][date < first_seen]
m[, gap_d := as.numeric(first_seen - date)]

per <- m[, .(n = .N, newest_gap = min(gap_d), best = max(perf)),
         by = .(athlete_id, event_id)]
cat(sprintf("cold-start athlete-events with a held prior: %s\n", format(nrow(per), big.mark=",")))
cat("\nhow stale is the MOST RECENT held prior?\n")
per[, band := cut(newest_gap, c(-1, 180, 365, 730, 1460, Inf),
                  labels = c("< 6 months","6-12 months","1-2 years","2-4 years","4+ years"))]
b <- per[, .(athlete_events = .N, share = round(100*.N/nrow(per),1),
             median_n_prior = as.numeric(median(n))), by = band][order(band)]
print(b)
cat(sprintf("\nusable within 2 years: %s (%.1f%% of those with a prior, %.1f%% of ALL cold starts)\n",
            format(per[newest_gap <= 730, .N], big.mark=","),
            100*per[newest_gap <= 730, .N]/nrow(per),
            100*per[newest_gap <= 730, .N]/nrow(cold)))
cat(sprintf("\ntotal 2026 cold-start athlete-events: %s\n", format(nrow(cold), big.mark=",")))
