# Should road races be promoted into T2, or is the problem upstream of tiering?
#
# Half Marathon M holds 28,095 corpus rows: 999 T1, 675 T2, and 24,679 with NO
# CATALOGUE ENTRY AT ALL. An uncatalogued row cannot be promoted, because the
# engine's inner join to the catalogue drops it before any tier is consulted.
# So the question is not "which T3 road races deserve T2" but "what is sitting
# in the uncatalogued bucket, and is any of it elite?"
#
# The answer decides the fix. If the majors are in there, this is a cataloguing
# gap worth closing. If it is mass-participation fun runs, they should stay out
# and the thin road ratings are simply honest.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
ROAD <- reg[family %chin% c("road", "walk"), event_id]

x <- rbindlist(lapply(ROAD, function(EV) {
  f <- file.path(D, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(NULL)
  y <- tryCatch(setDT(read_parquet(f, col_select =
        c("athlete_id","competition_id","date","perf","comp_name","place"))),
        error = function(e) NULL)
  if (is.null(y)) return(NULL)
  y[, event_id := EV][]
}), fill = TRUE)
x[, competition_id := as.character(competition_id)]
x <- merge(x, cat0[, .(competition_id, meet_tier, class)], by = "competition_id", all.x = TRUE)
x[is.na(meet_tier), meet_tier := "(uncatalogued)"]
cat(sprintf("road + walk rows in the corpus store: %s\n\n", format(nrow(x), big.mark = ",")))
print(x[, .(rows = .N, competitions = uniqueN(competition_id),
            athletes = uniqueN(athlete_id)), by = meet_tier][order(-rows)])

# Is the uncatalogued bucket elite? Judge by the marks, not the names: a race
# whose WINNER is world class is a race we want, whatever it is called.
cat("\n=== uncatalogued road competitions, ranked by winning standard ===\n")
u <- x[meet_tier == "(uncatalogued)" & is.finite(perf) & place == 1]
top <- u[, .(best = max(perf), n_rows = .N,
             comp = comp_name[1], date = max(date)), by = .(competition_id, event_id)]
# marathon only, so the marks are comparable
mar <- top[event_id == "AT-Marathon-M"][order(-best)]
if (nrow(mar)) {
  mar[, winner := sprintf("%d:%02d:%02d", floor(exp(-best)/3600),
        floor((exp(-best) %% 3600)/60), round(exp(-best) %% 60))]
  cat("\nMEN'S MARATHON, fastest uncatalogued winners:\n")
  print(utils::head(mar[, .(comp = substr(comp, 1, 44), date, winner, n_rows)], 15))
}
cat(sprintf("\nuncatalogued marathon competitions with a sub-2:10 winner: %d\n",
            nrow(mar[exp(-best) < 7800])))
cat("A sub-2:10 winner is an elite field by any definition. If those are\n")
cat("uncatalogued, the fix is to CATALOGUE them, not to reclassify T3.\n")

cat("\n=== for contrast, what IS catalogued for the marathon ===\n")
print(x[event_id == "AT-Marathon-M", .(rows = .N, comps = uniqueN(competition_id)),
        by = meet_tier][order(-rows)])
