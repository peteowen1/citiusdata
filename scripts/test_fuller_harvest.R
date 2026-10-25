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
cat("citius_fuller_harvest: all checks passed\n")
