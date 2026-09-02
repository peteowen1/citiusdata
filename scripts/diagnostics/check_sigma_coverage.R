# Assert that the deployed calibration carries a usable sigma_within for every
# event the selection-shrinkage arm will actually score.
#
# WHY THIS EXISTS AS A SEPARATE PRE-RUN CHECK. Inside the backtest loop an event
# with no fitted sigma shrinks by zero -- deliberately, so one thin event cannot
# abort a 200-meet run. But that is exactly the shape of failure this repo keeps
# being bitten by: an arm that is quietly PARTIAL looks identical to an arm that
# completed, and the scorecard cannot tell you which you got. So the precondition
# is asserted once, loudly, before any compute is spent, rather than counted in a
# hot loop nobody reads.
#
#   Rscript citiusdata/scripts/check_sigma_coverage.R
#
# Exit 0 = every scored event has a finite, positive sigma_within.
# Exit 1 = coverage gap; the arm would be partial. Names the events.
suppressMessages(library(data.table))
OUT <- file.path("citiusdata", "data")
CAL <- Sys.getenv("CITIUS_BT_CALIBRATION", "calibration_corpus_csigma_coast.rds")
REF <- Sys.getenv("CITIUS_SIGMA_REF_ARM", "backtest_ctrl_now.rds")

cal <- readRDS(file.path(OUT, CAL))
ev  <- as.data.table(cal$events)
if (!all(c("event_id", "sigma_within") %chin% names(ev)))
  stop("calibration$events lacks event_id/sigma_within: ", CAL)

# The events that actually get scored, taken from a real prior arm rather than
# assumed -- the registry lists events this population never contests.
#
# b$predictions/b$outcomes carry only race_id/athlete_id -- backtest_athletics.R
# never stamps event_id onto them -- so it has to come via race_id from
# championship_results.rds, the same join every sibling check in this session
# uses. A first run of this script (2026-09-01) tried b$predictions$event_id
# directly; that column does not exist, so `scored` came back NULL and the
# ev[...] join failed with "non-existing column(s): cols[1]='event_id'".
b <- readRDS(file.path(OUT, REF))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
scored_races <- unique(as.data.table(b$predictions)$race_id)
scored <- unique(ch[race_key %chin% scored_races]$event_id)
scored <- scored[!is.na(scored)]

cov <- ev[data.table(event_id = scored), on = "event_id"]
bad <- cov[!is.finite(sigma_within) | sigma_within <= 0]

cat(sprintf("calibration : %s\n", CAL))
cat(sprintf("scored events: %d (from %s)\n", length(scored), REF))
cat(sprintf("sigma_within : min %.5f  median %.5f  max %.5f\n",
            min(cov$sigma_within, na.rm = TRUE),
            median(cov$sigma_within, na.rm = TRUE),
            max(cov$sigma_within, na.rm = TRUE)))

if (nrow(bad)) {
  cat(sprintf("\nFAIL: %d of %d scored events have no usable sigma_within.\n",
              nrow(bad), length(scored)))
  print(bad[, .(event_id, sigma_within)])
  cat("\nThese would shrink by ZERO, making the arm partial while looking complete.\n")
  quit(save = "no", status = 1L)
}
cat(sprintf("\nPASS: all %d scored events carry a usable sigma_within.\n", length(scored)))
