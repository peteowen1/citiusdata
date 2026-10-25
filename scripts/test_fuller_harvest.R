# Synthetic check for citius_fuller_harvest() (citiusdata#37). Run:
#   Rscript scripts/test_fuller_harvest.R     (stops on the first failure)
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
mk <- function(keys, n) rbindlist(Map(function(k, m) data.table(race_key = k, athlete_id = seq_len(m)), keys, n))

saved <- mk(c("a", "b", "c"), c(8, 8, 8))
thin  <- mk(c("a", "b"), c(8, 3))          # c missing, b cut short
m <- citius_fuller_harvest(thin, saved)
stopifnot(nrow(m) == 24, attr(m, "from_saved") == 2L, !anyDuplicated(m[, .(race_key, athlete_id)]))

full <- mk(c("a", "b", "c", "d"), c(8, 8, 8, 8))   # live fuller and with a new race
m <- citius_fuller_harvest(full, saved)
stopifnot(nrow(m) == 32, attr(m, "from_saved") == 0L)

stopifnot(nrow(citius_fuller_harvest(thin, NULL)) == 11, attr(citius_fuller_harvest(thin, NULL), "from_saved") == 0L)
# Hash-keyed race re-keyed between harvests (athlete set changed): live must win, no duplicates
mk2 <- function(key, ids) data.table(race_key = key, event_id = 1L, athlete_id = ids, date = as.Date("2026-07-29"), round = "F")
m <- citius_fuller_harvest(mk2("x|f101n7", 1:7), mk2("x|f100n8", 1:8))
stopifnot(nrow(m) == 7, all(m$race_key == "x|f101n7"), attr(m, "from_saved") == 0L)
# a genuinely missing race (different athletes, other event) is still rescued, with its date
miss <- mk2("y|f5n8", 21:28); miss[, `:=`(event_id = 2L, date = as.Date("2026-08-01"))]
m <- citius_fuller_harvest(mk2("x|f101n7", 1:7), rbind(mk2("x|f100n8", 1:8), miss))
stopifnot(nrow(m) == 15, attr(m, "from_saved") == 1L, identical(attr(m, "saved_dates"), as.Date("2026-08-01")))
cat("citius_fuller_harvest: all checks passed\n")
