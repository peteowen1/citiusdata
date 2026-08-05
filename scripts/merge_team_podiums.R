# Fold the team-sport podiums into sport_medal_tables.
#
# Team sports have no NOC medal table on Wikipedia, so the sport-level harvest
# recorded nothing for them and the `opponent` class was the least reliable of
# the three -- only two team sports reached the host-effect regression at all.
# `harvest_team_podiums.R` recovers gold/silver/bronze from the medallists
# table, the infobox medal rows or a final-standings table; this binds the
# result on and re-runs the edition reconciliation.
#
# Podiums are only added for sport-editions the main harvest did NOT already
# cover, so nothing is double-counted.

library(data.table)
OUT <- "C:/dev/citiusverse/citiusdata/data"
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))

base <- as.data.table(readRDS(file.path(OUT, "sport_medal_tables.rds")))
pods <- as.data.table(readRDS(file.path(OUT, "team_sport_podiums.rds")))

base_keys <- unique(base[, .(games, year, sport)])
base_keys[, k := paste(games, year, sport)]
pods[, k := paste(games, year, sport)]

overlap <- pods[k %in% base_keys$k]
if (nrow(overlap)) {
  cat(sprintf("Dropping %d podium rows for sport-editions the main harvest already has:\n",
              nrow(overlap)))
  print(unique(overlap[, .(games, year, sport)]))
  pods <- pods[!k %in% base_keys$k]
}
pods[, k := NULL]

pods[, source := "podium"]
if (!"source" %in% names(base)) base[, source := "noc_table"]
merged <- rbindlist(list(base, pods), fill = TRUE)

cat(sprintf("\nsport_medal_tables: %d -> %d rows; %d -> %d sport-editions\n",
            nrow(base), nrow(merged),
            uniqueN(base[, .(games, year, sport)]),
            uniqueN(merged[, .(games, year, sport)])))

# --- reconciliation against the official gold totals ------------------------
med <- unique(as.data.table(readRDS(file.path(OUT, "multisport_medal_tables.rds")))[
  , .(games, year, official = total_golds_in_games)])
recon <- function(dt, label) {
  e <- dt[, .(sport_golds = sum(gold), sports = uniqueN(sport)), by = .(games, year)]
  e <- merge(e, med, by = c("games", "year"))
  e[, pct := 100 * sport_golds / official]
  cat(sprintf("%-8s editions within 2%% of official: %d/%d   median coverage %.1f%%\n",
              label, sum(abs(e$pct - 100) <= 2), nrow(e), median(e$pct)))
  e
}
before <- recon(base, "before")
after  <- recon(merged, "after")

cmp <- merge(before[, .(games, year, pct_before = pct)],
             after[, .(games, year, pct_after = pct, official)],
             by = c("games", "year"))
cmp[, gained := round(pct_after - pct_before, 1)]
cat("\nEditions where coverage improved most:\n")
print(head(cmp[gained > 0][order(-gained),
      .(games, year, official, before = round(pct_before, 1),
        after = round(pct_after, 1), gained)], 15))

over <- after[pct > 102]
if (nrow(over)) {
  cat("\nWARNING -- editions now OVER 102% of their official gold total:\n")
  print(over[order(-pct)])
  cat("A podium was added for an event the official table does not count.\n")
}

# Team sports specifically, which is the point of the exercise.
merged[, is_team := sport %in% sport_subjectivity()[team_sport == TRUE, sport]]
cat(sprintf("\nTeam sport-editions: %d -> %d\n",
            uniqueN(base[sport %in% sport_subjectivity()[team_sport == TRUE, sport],
                         .(games, year, sport)]),
            uniqueN(merged[is_team == TRUE, .(games, year, sport)])))
cat("distinct team sports represented:",
    uniqueN(merged[is_team == TRUE, sport]), "\n")
print(merged[is_team == TRUE, .(sport_editions = uniqueN(paste(games, year)),
                                golds = sum(gold)), by = sport][order(-sport_editions)])

merged[, is_team := NULL]
saveRDS(merged, file.path(OUT, "sport_medal_tables_with_podiums.rds"))
fwrite(cmp, file.path(OUT, "sport_medal_tables_coverage_change.csv"))
cat("\nSaved sport_medal_tables_with_podiums.rds\n")
