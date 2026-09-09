# A calibration whose race effects are FILTERED BY FIELD SIZE.
#
# WHY. calibration$race holds 663,716 fitted per-race effects and the deployed
# model ignores all of them (adjust_race defaults off), so wind, altitude,
# track and weather flow straight into every athlete's ability estimate --
# ability.R:722 says so in as many words. That is the leading explanation for
# why per-athlete sigma correlates only 0.30 with actual consistency
# (docs/incidents/sigma-does-not-track-consistency-2026-09-05.md).
#
# But turning them ALL on is worse, not better: measured on Lyles, sigma went
# 0.0110 -> 0.0153. The reason is in the data. Field sizes:
#
#   40% of races have < 5 athletes, 72% have < 8, the median is 5.
#
# A two-athlete race cannot separate "the race was fast" from "the athlete was
# fast", so its c_r is mostly the athlete, and subtracting it removes the
# signal the model is trying to measure. This keeps only races big enough for
# the decomposition to identify a shared effect, and zeroes the rest -- which
# leaves those rows exactly where the deployed model already has them.
#
# Usage:
#   CITIUS_RACEFILT_MIN=8 Rscript citiusdata/scripts/build_calibration_racefilt.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
SRC  <- Sys.getenv("CITIUS_RACEFILT_SRC", "calibration_corpus_wac_coast_0904.rds")
MIN  <- as.integer(Sys.getenv("CITIUS_RACEFILT_MIN", "8"))
DEST <- Sys.getenv("CITIUS_RACEFILT_OUT", sprintf("calibration_racefilt_min%d.rds", MIN))

cal <- readRDS(file.path(OUT, SRC))
r <- as.data.table(cal$race)
stopifnot("no n_in_race on calibration$race" = "n_in_race" %in% names(r))
before <- nrow(r)
keep <- r[n_in_race >= MIN]
cat(sprintf("race effects: %s of %s kept at n_in_race >= %d (%.1f%%)\n",
            format(nrow(keep), big.mark = ","), format(before, big.mark = ","),
            MIN, 100 * nrow(keep) / before))
# An empty or near-empty table would make adjust_race a silent no-op, which
# looks exactly like "the fix did nothing".
if (nrow(keep) < 0.05 * before) cli::cli_abort(
  "Filter kept only {nrow(keep)} of {before} races; adjust_race would be near-inert.")
cat(sprintf("kept effects: mean %.6f, sd %.5f (dropped: mean %.6f, sd %.5f)\n",
            mean(keep$c_r, na.rm = TRUE), sd(keep$c_r, na.rm = TRUE),
            mean(r[n_in_race < MIN]$c_r, na.rm = TRUE),
            sd(r[n_in_race < MIN]$c_r, na.rm = TRUE)))
cal$race <- keep[]
cal$race_filter_min <- MIN
cal$provenance$race_filter <- sprintf("n_in_race >= %d, %d of %d kept", MIN, nrow(keep), before)
saveRDS(cal, file.path(OUT, DEST))
cat(sprintf("wrote %s\n", DEST))
