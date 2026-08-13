# Recalibrate athletics from the unified corpus (competition + career routes).
#
# Separate from recalibrate.R because the corpus needs two things the
# single-source path does not: the `nomark_observable` provenance column must
# survive into calibrate(), and the table is 16x larger, so it is narrowed to the
# columns the estimators actually read before any of them run.
#
# Narrowing is not cosmetic here. A bracket filter on a wide table copies every
# column of every passing row, and the decomposition brackets this table once per
# sweep. Carrying 28 columns when 14 are read is roughly twice the allocation per
# sweep, and R's gc() does not see the growth -- only the OS does.
#
# Usage:  Rscript scripts/recalibrate_corpus.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
IN   <- Sys.getenv("CITIUS_CAL_INPUT", "athletics_corpus.rds")
PREV <- Sys.getenv("CITIUS_CAL_BASELINE", "calibration.rds")
SUF  <- Sys.getenv("CITIUS_CAL_SUFFIX", "_corpus")

rss <- function(tag) {
  mb <- tryCatch(round(as.numeric(system2("powershell", c("-NoProfile", "-Command",
    "(Get-Process -Id $PID).WorkingSet64"), stdout = TRUE)) / 1e6), error = function(e) NA)
  cat(sprintf("[%s] %s  R heap %.0f MB\n", format(Sys.time(), "%H:%M:%S"), tag,
              sum(gc()[, 2])))
  invisible(NULL)
}

x <- setDT(readRDS(file.path(OUT, IN)))[!is.na(date)]
cat(sprintf("%s: %s results | %s meets | %s races\n", IN,
            format(nrow(x), big.mark = ","), format(uniqueN(x$competition_id), big.mark = ","),
            format(uniqueN(x$race_key), big.mark = ",")))

# Everything the estimators read, and nothing else. `nomark_observable` is
# load-bearing: without it the career rows dilute every foul rate toward zero.
keep <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "sex",
          "round", "tier", "race_key", "competition_id", "discipline",
          "orientation", "is_technical", "nomark_observable", "source")
x <- x[, intersect(keep, names(x)), with = FALSE]
cat(sprintf("narrowed to %d columns (%s)\n", ncol(x),
            format(object.size(x), units = "GB")))
rss("loaded")

clean <- flag_implausible(x)
rm(x); invisible(gc())
rss("flagged")

cal <- calibrate(clean, min_races = 30L)
rss("calibrated")
saveRDS(cal, file.path(OUT, paste0("calibration", SUF, ".rds")))

fit <- clean[!is.na(perf) & !is.na(event_id)]
hl <- fit_half_life(fit)
rss("half-life")
saveRDS(hl, file.path(OUT, paste0("half_life", SUF, ".rds")))

ag <- fit_aging_curve(fit)
rss("aging")
saveRDS(ag, file.path(OUT, paste0("aging", SUF, ".rds")))

# ---- compare against the single-source baseline -----------------------------
old <- readRDS(file.path(OUT, PREV))
cat("\n=== calibration: baseline -> corpus ===\n")
cat(sprintf("tail_df       : %s -> %s\n", old$tail_df, cal$tail_df))
cat(sprintf("events calib. : %d -> %d\n", sum(old$events$calibrated, na.rm = TRUE),
            sum(cal$events$calibrated, na.rm = TRUE)))
cat(sprintf("converged     : %s\n", cal$converged))

cat("\n--- round ---\nbaseline:\n"); print(old$round)
cat("corpus:\n"); print(cal$round)
cat("\n--- tier ---\nbaseline:\n"); print(old$tier)
cat("corpus:\n"); print(cal$tier)

e1 <- as.data.table(old$events)[, .(event_id, s1 = sigma_within, c1 = condition_sd,
                                    f1 = foul_rate, n1 = n_races)]
e2 <- as.data.table(cal$events)[, .(event_id, s2 = sigma_within, c2 = condition_sd,
                                    f2 = foul_rate, n2 = n_races)]
m <- merge(e1, e2, by = "event_id")
cat(sprintf("\nevents in both: %d\n", nrow(m)))
cat(sprintf("  sigma_within median %.5f -> %.5f (%+.1f%%)\n",
            median(m$s1, na.rm = TRUE), median(m$s2, na.rm = TRUE),
            100 * (median(m$s2, na.rm = TRUE) / median(m$s1, na.rm = TRUE) - 1)))
cat(sprintf("  condition_sd median %.5f -> %.5f (%+.1f%%)\n",
            median(m$c1, na.rm = TRUE), median(m$c2, na.rm = TRUE),
            100 * (median(m$c2, na.rm = TRUE) / median(m$c1, na.rm = TRUE) - 1)))
cat(sprintf("  foul_rate    median %.5f -> %.5f  (must NOT collapse toward 0)\n",
            median(m$f1, na.rm = TRUE), median(m$f2, na.rm = TRUE)))
cat(sprintf("  races/event  median %.0f -> %.0f\n",
            median(m$n1, na.rm = TRUE), median(m$n2, na.rm = TRUE)))

cat("\n--- half-lives ---\n"); print(hl)
cat("\n--- aging peaks ---\n"); print(ag$peaks)
cat("\nDONE\n")
