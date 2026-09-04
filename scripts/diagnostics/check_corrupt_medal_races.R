# WHAT ARE THE 34 RACES THAT AWARD MORE THAN THREE MEDALS?
#
# A race cannot award 20 medals. These are in championship_results.rds, which is
# the shared outcome file every backtest arm on disk was scored against, so
# whatever they are they have been corrupting medal and gold hit rates for every
# arm, not just the one that surfaced them.
#
# Three candidate explanations, each wanting a different fix:
#
#   merged races   several heats, or several age divisions, sharing one race
#                  identifier, so places 1, 2 and 3 appear once per merged heat.
#                  This is the failure that produced 46-athlete "finals" with six
#                  athletes at each placing earlier in this project, and the fix
#                  is in the race key rather than in the outcomes.
#   relays         a team event where three TEAMS medal but the results list one
#                  row per athlete, so a medal position legitimately repeats four
#                  or more times. Not corruption at all - it means relays should
#                  be excluded from an athlete-level medal count.
#   duplicate rows the same athlete-place recorded more than once from an
#                  overlapping harvest, which inflates the count without any
#                  structural cause.
#
# They are distinguishable: count DISTINCT athletes per medal position, look at
# whether the event is a relay, and check whether the extra rows carry distinct
# marks. Do that rather than assuming, because the cheap assumption here is
# "ties", and ties cannot produce twenty.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
BT  <- Sys.getenv("CITIUS_BT_OUT", "backtest.rds")

b <- readRDS(file.path(OUT, BT))
d <- merge(as.data.table(b$predictions), as.data.table(b$outcomes),
           by = c("race_id", "athlete_id"), all = FALSE)
per <- d[, .(medals = sum(hit_medal), entrants = .N), by = race_id]
bad <- per[medals > 3][order(-medals)]
stopifnot("no over-medalled races found" = nrow(bad) > 0)
cat(sprintf("%d races award more than 3 medals, %d extra medals in total\n",
            nrow(bad), sum(bad$medals) - 3L * nrow(bad)))
cat(sprintf("race_id looks like: %s\n", bad$race_id[1]))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
cat(sprintf("championship_results.rds: %s rows | columns: %s\n",
            format(nrow(ch), big.mark = ","),
            paste(utils::head(names(ch), 14), collapse = ", ")))

# The backtest's race_id must be matched back to the source. Try the obvious
# key rather than assuming a name: if race_key exists and matches, use it.
key <- if ("race_key" %chin% names(ch) &&
           any(bad$race_id %chin% ch$race_key)) "race_key" else NA_character_
if (is.na(key)) {
  cat("\nCould not match backtest race_id back to championship_results.rds on\n")
  cat("race_key. Showing the backtest side only - the source join needs the\n")
  cat("actual key naming, which must be read rather than guessed.\n")
  print(utils::head(bad, 20))
  quit(status = 0)
}
cat(sprintf("matched backtest race_id to championship_results$%s\n", key))

src <- ch[get(key) %chin% bad$race_id]
stopifnot("no source rows for the bad races" = nrow(src) > 0)

pcol <- intersect(c("place", "position", "rank"), names(src))[1]
acol <- intersect(c("athlete_id", "athlete"), names(src))[1]
ecol <- intersect(c("event_id", "discipline", "event"), names(src))[1]
stopifnot("cannot find place/athlete columns in the source" =
            !is.na(pcol) && !is.na(acol))

sm <- src[, .(rows = .N,
              distinct_athletes = uniqueN(get(acol)),
              distinct_places = uniqueN(get(pcol)),
              at_place_1 = sum(get(pcol) == 1, na.rm = TRUE),
              at_place_3 = sum(get(pcol) == 3, na.rm = TRUE),
              distinct_marks_top3 = uniqueN(mark[get(pcol) <= 3]),
              event = get(ecol)[1]), by = c(key)]
setnames(sm, key, "race_id")
sm <- merge(sm, bad, by = "race_id")[order(-medals)]
cat("\n=== the over-medalled races, from the source ===\n")
print(utils::head(sm, 20))

cat("\n=== what kind of event are they? ===\n")
print(sm[, .(races = .N, extra = sum(medals) - 3L * .N), by = event][order(-races)])

cat("\n=== reading it ===\n")
cat("at_place_1 far above 1 with several distinct marks means several races were\n")
cat("merged under one identifier - the fix belongs in the race key.\n")
cat("A relay event with 4 athletes at each of places 1 to 3 is not corruption,\n")
cat("it means relays must be dropped from an athlete-level medal count.\n")
cat("Duplicate rows carrying the SAME mark for the same athlete are a harvest\n")
cat("overlap and should be deduplicated at source.\n")
