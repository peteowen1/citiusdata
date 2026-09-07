# How much data does each event actually have, and which ones are worth rating?
#
# Pete: "what even is WeightThrow M? should we remove it as an event? what's
# total number of meets / races / marks per event".
#
# The registry carries 90-odd events because it was built to cover what the
# feeds return, not what the project forecasts. Some of those are Olympic
# programme events with decades of depth; some exist because a handful of US
# indoor meets contest them. The weight throw is the clearest case: a 35lb /
# 20lb throw contested at NCAA and USATF indoors, in neither the World
# Athletics championship programme nor the Olympics.
#
# The cost of carrying a near-empty event is not zero. It lands in every
# per-event fit as a unit with its own parameter, it appears in "events beaten"
# counts as though it were comparable to the 100m, and its noise gets
# hierarchically averaged into its family's prior.
#
# Counts are over the WHOLE corpus (what the model trains on) and again over the
# held-out window (what any score is actually measured on), because an event can
# look substantial in the corpus and still be scored on four races.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/event_inventory.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, date := as.Date(date)]
inv <- ch[, .(marks = .N,
              races = uniqueN(race_key),
              meets = uniqueN(competition_id),
              athletes = uniqueN(athlete_id),
              first = min(date, na.rm = TRUE), last = max(date, na.rm = TRUE),
              pct_indoor = round(100 * mean(indoor %in% TRUE), 1),
              t1_races = uniqueN(race_key[tier %in% c("OW", "GW", "DF", "GL")])),
          by = event_id]
reg <- as.data.table(citius_events())[, .(event_id, family, discipline, sex, technical)]
inv <- merge(inv, reg, by = "event_id", all.x = TRUE)

# What the scorecard actually scores: held-out races per event.
sc <- if (file.exists(file.path(OUT, "marks_scorecard.csv")))
  fread(file.path(OUT, "marks_scorecard.csv"))[, .(event_id, held_out_races = races, beat)] else NULL
if (!is.null(sc)) inv <- merge(inv, sc, by = "event_id", all.x = TRUE)
inv[is.na(held_out_races), held_out_races := 0L]
setorder(inv, -marks)

cat(sprintf("\n%d events in the corpus; %d have 5+ held-out races and are scored.\n",
            nrow(inv), sum(inv$held_out_races >= 5)))
cols <- c("event_id", "family", "meets", "races", "marks", "athletes",
          "held_out_races", "pct_indoor")
cat("\n=== TOP 10 by marks ===\n"); print(head(inv[, ..cols], 10))
cat("\n=== BOTTOM 10 by marks ===\n"); print(tail(inv[, ..cols], 10))

cat("\n=== the 44 scored events, smallest first ===\n")
print(inv[held_out_races >= 5][order(marks), ..cols], nrows = 50)

cat("\n=== events in the corpus but NOT scored (under 5 held-out races) ===\n")
print(inv[held_out_races < 5][order(-marks), ..cols], nrows = 60)

# The specific question. An event contested almost entirely indoors, at few
# meets, is a different kind of object from one with a global outdoor season.
cat("\n=== weight throw, in context ===\n")
print(inv[event_id %like% "WeightThrow" | event_id %like% "ShotPut",
          .(event_id, meets, races, marks, athletes, held_out_races, pct_indoor, first, last)])
fwrite(inv, file.path(OUT, "event_inventory.csv"))
say("wrote event_inventory.csv")
