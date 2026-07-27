# Recalibrate athletics on the v2 harvest (heats separated by raceNumber).
#
# Writes *_v2.rds alongside the v1 artefacts rather than over them, so the two
# can be compared and so nothing downstream breaks mid-run. Promote by renaming
# once the comparison looks right.
#
# What to expect if the race_key fix did what it should: heats were previously
# pooled into one race per round, so between-heat variation had nowhere to go
# but the residual. Separating them should RAISE condition_sd and LOWER
# sigma_within. If neither moves, the pooling was immaterial and that is worth
# knowing too.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
champs <- setDT(readRDS(file.path(OUT, "championship_results_v2.rds")))[!is.na(date)]
cli::cli_alert_info(
  "{format(nrow(champs), big.mark=',')} results | {uniqueN(champs$competition_id)} meets | {format(uniqueN(champs$race_key), big.mark=',')} races"
)

clean <- flag_implausible(champs)
cal2 <- calibrate(clean, min_races = 30L)
hl2  <- fit_half_life(clean[!is.na(perf) & !is.na(event_id)])
ag2  <- fit_aging_curve(clean[!is.na(perf) & !is.na(event_id)])

saveRDS(cal2, file.path(OUT, "calibration_v2.rds"))
saveRDS(hl2,  file.path(OUT, "half_life_v2.rds"))
saveRDS(ag2,  file.path(OUT, "aging_v2.rds"))

cal1 <- readRDS(file.path(OUT, "calibration.rds"))

cli::cli_h2("v1 vs v2 calibration")
cat("tail_df      :", cal1$tail_df, "->", cal2$tail_df, "\n")
cat("events calib.:", sum(cal1$events$calibrated, na.rm = TRUE), "->",
    sum(cal2$events$calibrated, na.rm = TRUE), "\n")

cat("\n--- round (the open question: did pooling semis inflate the final precision?) ---\n")
cat("v1:\n"); print(cal1$round)
cat("v2:\n"); print(cal2$round)

cat("\n--- tier ---\n")
cat("v1:\n"); print(cal1$tier)
cat("v2:\n"); print(cal2$tier)

# The headline test. Pooled heats should have deflated condition_sd and inflated
# sigma_within; separating them should reverse both.
cat("\n--- per-event sigma_within and condition_sd ---\n")
e1 <- as.data.table(cal1$events)[, .(event_id, s1 = sigma_within, c1 = condition_sd,
                                     f1 = foul_rate, n1 = n_races)]
e2 <- as.data.table(cal2$events)[, .(event_id, s2 = sigma_within, c2 = condition_sd,
                                     f2 = foul_rate, n2 = n_races)]
m <- merge(e1, e2, by = "event_id")
cat(sprintf("events in both: %d\n", nrow(m)))
cat(sprintf("  sigma_within  median %.5f -> %.5f  (%+.1f%%)\n",
            median(m$s1, na.rm = TRUE), median(m$s2, na.rm = TRUE),
            100 * (median(m$s2, na.rm = TRUE) / median(m$s1, na.rm = TRUE) - 1)))
cat(sprintf("  condition_sd  median %.5f -> %.5f  (%+.1f%%)\n",
            median(m$c1, na.rm = TRUE), median(m$c2, na.rm = TRUE),
            100 * (median(m$c2, na.rm = TRUE) / median(m$c1, na.rm = TRUE) - 1)))
cat(sprintf("  races/event   median %.0f -> %.0f\n",
            median(m$n1, na.rm = TRUE), median(m$n2, na.rm = TRUE)))
cat(sprintf("  events where sigma_within FELL: %d of %d (%.0f%%)\n",
            sum(m$s2 < m$s1, na.rm = TRUE), nrow(m),
            100 * mean(m$s2 < m$s1, na.rm = TRUE)))
cat(sprintf("  events where condition_sd ROSE: %d of %d (%.0f%%)\n",
            sum(m$c2 > m$c1, na.rm = TRUE), nrow(m),
            100 * mean(m$c2 > m$c1, na.rm = TRUE)))

cat("\nbiggest condition_sd movers:\n")
m[, d := c2 - c1]
print(head(m[order(-abs(d)), .(event_id, c1 = round(c1, 4), c2 = round(c2, 4),
                               s1 = round(s1, 4), s2 = round(s2, 4), n2)], 12))

cat("\n--- half-lives ---\n")
cat("v1:\n"); print(readRDS(file.path(OUT, "half_life.rds")))
cat("v2:\n"); print(hl2)

cat("\n--- aging peaks ---\n")
print(ag2$peaks)
