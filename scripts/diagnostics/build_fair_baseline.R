# Write the FAIR last-5 baseline into the lab cache, once, so every scorer uses
# the same one.
#
# WHY THIS EXISTS. `base.rds` takes an athlete's last five raw marks before the
# RACE DATE. The pair table cuts their history at `[date < month]`, because the
# lab estimates ability once per athlete-event-month and that is what makes it
# fast. So the baseline had up to ~30 extra days of that athlete's racing --
# often their most recent race -- that the model was never shown.
#
# That does NOT affect a parameter chosen by minimising the model's own MAE: the
# baseline cancels out of that comparison entirely. It DOES affect anything
# chosen by "events beaten" or by excess bias over the baseline, which is
# precisely what marks_fit.R's objective is built from. So the parameters that
# fit selected have to be re-checked, and every scorer needs to read the same
# fair baseline rather than each rebuilding its own.
#
# base_m.rds is built FROM the pair table, so it cannot drift away from the
# model's history window by construction.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/build_fair_baseline.R'
suppressMessages(library(data.table))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))

# Same estimator as base.rds -- unweighted mean of the five most recent RAW
# marks, minimum three -- on the model's own information cut-off.
bm <- pairs[order(pid, age_days)][, .(base_m = mean(perf_raw[seq_len(min(.N, 5L))]),
                                      n_m = min(.N, 5L)), by = pid][n_m >= 3L]
bm <- merge(k[, .(pid, athlete_id, event_id, month)], bm, by = "pid")
stopifnot("fair baseline is empty" = nrow(bm) > 0,
          "fair baseline has non-finite values" = all(is.finite(bm$base_m)))
saveRDS(bm[, .(athlete_id, event_id, month, base_m, n_m)], file.path(CACHE, "base_m.rds"))
say("wrote base_m.rds: %s rows, %d athletes, %d events",
    format(nrow(bm), big.mark = ","), uniqueN(bm$athlete_id), uniqueN(bm$event_id))

# Report the size of the head start, so the number is on the record rather than
# rediscovered. Joined on athlete-event-month, which is what the scorers use.
cmp <- merge(b5, k[, .(athlete_id, event_id, month)], by = c("athlete_id", "event_id"),
             allow.cartesian = TRUE)
cmp <- merge(cmp, bm[, .(athlete_id, event_id, month, base_m)],
             by = c("athlete_id", "event_id", "month"))
say("head start: %s overlapping rows, mean |race-date minus month-cut| = %.4f of a mark",
    format(nrow(cmp), big.mark = ","), mean(abs(cmp$base - cmp$base_m)))
say("  identical on %.1f%% of rows -- the rest are athletes who raced inside the month",
    100 * mean(abs(cmp$base - cmp$base_m) < 1e-12))
