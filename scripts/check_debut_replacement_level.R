# SHOULD A DEBUTANT BE SEEDED AT THE POPULATION MEAN, OR AT REPLACEMENT LEVEL?
#
# form_ratings.R seeds any athlete with no prior rating in an event at MU, the
# mean of EVERY row in the corpus for that event. That mean is dominated by
# established athletes, who race repeatedly and are good enough to keep being
# selected. A debutant is not a draw from that distribution - they are a draw
# from the distribution of people arriving in the event.
#
# If those two differ, the current prior is not merely stale, it is BIASED, and
# in a predictable direction: debutants are seeded as though they were an
# average established athlete, so they are systematically overrated on entry.
#
# Three candidate priors, each pre-race and so all legitimate:
#   MU              mean of all rows - what the engine does now
#   replacement     mean of DEBUT rows only - what a new arrival actually runs
#   replacement|tier  the same, conditioned on the tier of the meet they debut
#                   at. An athlete first appearing in a Diamond League field is
#                   a different prospect from one first appearing at a domestic
#                   meet, and the tier is known before the race.
#
# This measures the gap between them, in units of the within-race spread the
# ratings live on, because that is what decides whether it is worth building.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
BAR <- "|"

h <- setDT(read_parquet(file.path(D, "seqv3_history_from2020.parquet"),
                        col_select = c("race_key","date","event_id","athlete_id",
                                       "perf","place","seen")))
h <- h[is.finite(perf) & is.finite(place) & place > 0]
h[, yr := year(as.Date(date))]

c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
h <- merge(h, c0[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

# the scale: typical within-race spread per event
sp <- h[, .(v = var(perf)), by = .(event_id, race_key)][is.finite(v),
        .(sd_race = sqrt(median(v))), by = event_id]

mu   <- h[,            .(mu_all = mean(perf), n_all = .N), by = event_id]
rep0 <- h[seen == FALSE, .(mu_debut = mean(perf), n_debut = .N), by = event_id]

m <- Reduce(function(a, b) merge(a, b, by = "event_id"), list(mu, rep0, sp))
m <- m[n_all >= 2000 & n_debut >= 200]
m[, gap := mu_debut - mu_all]
m[, gap_sd := gap / sd_race]

cat(sprintf("events with >=2,000 rows and >=200 debuts: %d\n", nrow(m)))
cat(sprintf("debut rows are %.1f%% of the corpus\n", 100 * h[, mean(seen == FALSE)]))
cat(sprintf("\nmean gap (debut mean - population mean): %+.4f perf units\n", mean(m$gap)))
cat(sprintf("in units of within-race spread: %+.3f sd (median %+.3f)\n",
            mean(m$gap_sd), median(m$gap_sd)))
cat(sprintf("events where debutants are WORSE than the population mean: %d of %d\n",
            m[gap < 0, .N], nrow(m)))
cat("\nCompare against the staleness effect measured separately: MU moves 0.135 sd\n")
cat("when the corpus window changes. If the gap above is larger than that, the\n")
cat("BIAS in the prior matters more than its date.\n")

cat("\n=== the 12 events where seeding at the population mean flatters debutants most ===\n")
print(head(m[order(gap_sd), .(event_id, n_debut, sd_race = round(sd_race, 4),
                              mu_all = round(mu_all, 4), mu_debut = round(mu_debut, 4),
                              gap_sd = round(gap_sd, 3))], 12))

# --- does the tier of the debut meet carry information? ----------------------
# Known before the race, so a legitimate conditioning variable.
cat("\n=== replacement level BY TIER OF THE DEBUT MEET ===\n")
d0 <- h[seen == FALSE & !is.na(meet_tier)]
d0 <- merge(d0, m[, .(event_id, mu_all, sd_race)], by = "event_id")
d0[, z := (perf - mu_all) / sd_race]
print(d0[, .(debuts = .N,
             mean_z_vs_population = round(mean(z), 3),
             sd_z = round(sd(z), 3)), by = meet_tier][order(meet_tier)])
cat("\nmean_z_vs_population is how far a debutant at that tier sits from the\n")
cat("prior the engine currently gives them, in within-race spreads. A tier that\n")
cat("is far from zero is one where the single flat prior is most wrong - and\n")
cat("the spread BETWEEN tiers is what a conditional prior would recover.\n")

cat("\n=== and by how strong the debut field is, which is also known pre-race ===\n")
d0[, fld := cut(d0[, .N, by = race_key][d0, on = "race_key", x.N],
                c(0, 4, 8, 16, Inf), labels = c("2-4", "5-8", "9-16", "17+"))]
print(d0[!is.na(fld), .(debuts = .N, mean_z = round(mean(z), 3)), by = fld][order(fld)])
