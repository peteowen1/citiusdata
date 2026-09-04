# Can the cold starts be seeded from data we ALREADY HOLD?
#
# The corpus is 1,225,339 rows. The careers store is far larger, so the corpus
# is a filtered subset (tier / date / event). If an athlete the corpus treats as
# a debutant already has results in the careers store, a debut prior costs a
# join rather than a scraper — and cold starts are 28.7% of the metric.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")


ds <- open_dataset(file.path(D, "athletics_careers_store"))
cat(sprintf("careers store: %s rows\n", format(nrow(ds), big.mark = ",")))
ca <- setDT(as.data.frame(ds |> dplyr::select(athlete_id, date, perf, discipline,
                                              sex, tier)))
ca <- ca[!is.na(perf) & is.finite(perf) & !is.na(date)]
ca[, event_id := paste0("AT-", gsub("[^A-Za-z0-9]", "", discipline), "-", sex)]
ca[, athlete_id := as.character(athlete_id)]
cat(sprintf("usable career rows: %s | distinct athlete-events: %s\n",
            format(nrow(ca), big.mark = ","),
            format(uniqueN(ca[, .(athlete_id, event_id)]), big.mark = ",")))

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h[, athlete_id := as.character(athlete_id)]
cold <- h[year(date) == 2026 & place <= 12 & seen == FALSE,
          .(first_seen = min(date)), by = .(athlete_id, event_id)]
cat(sprintf("\n2026 cold-start athlete-events in the scored metric: %s\n",
            format(nrow(cold), big.mark = ",")))

# does the careers store hold a PRIOR result for that athlete-event?
setkey(ca, athlete_id, event_id)
m <- ca[cold, on = .(athlete_id, event_id), allow.cartesian = TRUE]
m <- m[!is.na(date) & date < first_seen]
cov_ev <- m[, .(prior = .N, best = max(perf)), by = .(athlete_id, event_id)]
cat(sprintf("of those, HAVE prior results in the careers store: %s (%.1f%%)\n",
            format(nrow(cov_ev), big.mark = ","),
            100 * nrow(cov_ev) / nrow(cold)))
cat(sprintf("median prior results available: %.0f | 25th pct %.0f | 75th pct %.0f\n",
            median(cov_ev$prior), quantile(cov_ev$prior, .25), quantile(cov_ev$prior, .75)))

# same athlete, ANY event (a cross-event prior from held data)
ac <- unique(ca[, .(athlete_id, date, perf, event_id)])
m2 <- ac[cold[, .(athlete_id, ev_target = event_id, first_seen)],
         on = .(athlete_id), allow.cartesian = TRUE]
m2 <- m2[!is.na(date) & date < first_seen]
cat(sprintf("\nhave prior results in ANY event: %s (%.1f%%)\n",
            format(uniqueN(m2[, .(athlete_id, ev_target)]), big.mark = ","),
            100 * uniqueN(m2[, .(athlete_id, ev_target)]) / nrow(cold)))
cat("\nSAME-EVENT prior is the one that seeds a rating directly.\n")
