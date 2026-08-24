# WHAT IS THE T1 POPULATION, EXACTLY? Span, size, and which meets.
#
# A tier-restricted metric is only readable if you know what it covers. 250
# competitions could be 25 years of Olympics or three seasons of Diamond League,
# and those imply completely different things about how fast it can iterate and
# how much of it is recent enough to matter for LA 2028.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
h <- h[is.finite(place) & place > 0]
h[, date := as.Date(date)]
BAR <- "|"
h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
h <- merge(h, c0[, .(competition_id, comp_name, meet_tier, is_major)],
           by = "competition_id", all.x = TRUE)

t1 <- h[meet_tier == "T1_elite"]
cat(sprintf("T1 scored rows: %s across %s competitions\n",
            format(nrow(t1), big.mark = ","),
            format(t1[, uniqueN(competition_id)], big.mark = ",")))
cat(sprintf("date span: %s to %s\n", min(t1$date), max(t1$date)))

cat("\n=== T1 rows by year ===\n")
print(t1[, .(comps = uniqueN(competition_id), rows = .N,
             races = uniqueN(race_key)), by = .(yr = year(date))][order(yr)])

cat("\n=== the 20 largest T1 meets ===\n")
print(head(t1[, .(rows = .N, yr = year(min(date))), by = comp_name][order(-rows)], 20))

cat("\n=== how much of T1 is a major ===\n")
print(t1[, .(comps = uniqueN(competition_id), rows = .N), by = is_major])
