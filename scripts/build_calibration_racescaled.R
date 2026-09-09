# Scale each race effect by the fraction of it that is actually REAL.
#
# THE MEASUREMENT THIS IMPLEMENTS (2026-09-05). calibration$race fits a shared
# per-race effect c_r. Regressing what a field ACTUALLY averaged in later races
# on the fitted c_r, across 8,717 races in six events, gives a slope of 0.647
# pooled -- so about 65% of the fitted effect shows up out of sample and the
# rest is over-correction. Per event: 100m 0.686, 200m 0.717, 400m 0.559,
# 800m 0.619, long jump 0.676, shot put 0.346.
#
# Independently corroborated on a single race: the Gout Gout 200m where 5 of 6
# athletes PB'd. Fitted c_r said +1.013 s; the field has averaged +0.587 s
# slower since. 0.587/1.013 = 0.58, inside the measured range.
#
# WHY THIS MATTERS RATHER THAN JUST TURNING adjust_race ON. Raw c_r was tried
# on 2026-08-13 and failed its anchor immediately -- it predicted 2:01 for the
# world's best 800m women (ability.R:736). That is what a 1.5x over-correction
# does to elite athletes, who race in genuinely fast races. The mechanism was
# never wrong; the magnitude was.
#
# Throws take the smallest scale by a wide margin (0.346), which is consistent
# with far more idiosyncratic variation being absorbed into c_r as if shared.
# A single global scale would over-correct exactly the family that is already
# worst on marks.
#
# Usage:
#   CITIUS_RACESCALE=0.647 Rscript citiusdata/scripts/build_calibration_racescaled.R
#   CITIUS_RACESCALE_TABLE=race_effect_scales.csv Rscript ...   # per-event
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT   <- here::here("citiusdata", "data")
SRC   <- Sys.getenv("CITIUS_RACESCALE_SRC", "calibration_corpus_wac_coast_0904.rds")
SCALE <- as.numeric(Sys.getenv("CITIUS_RACESCALE", "0.647"))
TABLE <- Sys.getenv("CITIUS_RACESCALE_TABLE", "")
MINF  <- as.integer(Sys.getenv("CITIUS_RACESCALE_MINFIELD", "5"))
DEST  <- Sys.getenv("CITIUS_RACESCALE_OUT", "calibration_racescaled.rds")

cal <- readRDS(file.path(OUT, SRC))
r <- as.data.table(cal$race)
stopifnot("no c_r on calibration$race" = "c_r" %in% names(r))
before_sd <- sd(r$c_r, na.rm = TRUE)

if (nzchar(TABLE) && file.exists(file.path(OUT, TABLE))) {
  tb <- fread(file.path(OUT, TABLE))
  stopifnot("scale table needs event_id and slope" = all(c("event_id","slope") %in% names(tb)))
  r <- merge(r, tb[, .(event_id, slope)], by = "event_id", all.x = TRUE)
  n_missing <- sum(is.na(r$slope))
  r[is.na(slope), slope := SCALE]
  cat(sprintf("per-event scales from %s; %s of %s races fell back to the pooled %.3f\n",
              TABLE, format(n_missing, big.mark = ","), format(nrow(r), big.mark = ","), SCALE))
} else {
  r[, slope := SCALE]
  cat(sprintf("single pooled scale %.3f applied to all %s races\n",
              SCALE, format(nrow(r), big.mark = ",")))
}

# Races too small to identify a shared effect keep a scale of ZERO rather than
# being scaled down: 40% of races have fewer than 5 athletes, and in those c_r
# is mostly the athlete, so shrinking it still removes real signal. Zero leaves
# those rows exactly where the deployed model already has them.
n_small <- sum(r$n_in_race < MINF, na.rm = TRUE)
r[n_in_race < MINF, slope := 0]
cat(sprintf("%s of %s races (%.1f%%) have n_in_race < %d and keep c_r = 0\n",
            format(n_small, big.mark = ","), format(nrow(r), big.mark = ","),
            100*n_small/nrow(r), MINF))

r[, c_r := c_r * slope]
r[, slope := NULL]
cal$race <- r[]
cal$race_scale <- if (nzchar(TABLE)) paste0("per-event:", TABLE) else SCALE
cal$race_scale_minfield <- MINF
saveRDS(cal, file.path(OUT, DEST))
cat(sprintf("\nc_r sd %.5f -> %.5f\nwrote %s\n", before_sd, sd(r$c_r, na.rm = TRUE), DEST))
