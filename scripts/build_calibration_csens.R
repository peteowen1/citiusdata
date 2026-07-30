# Calibration for the `csens` arm: csigma, with condition sensitivity actually
# identified.
#
# `sensitivity` was exactly 1.000 for all 84,362 athletes on every calibration
# built before 2026-07-31 -- sd 3.4e-05 against sd(sensitivity_raw) of 1.60. A
# constant sensitivity makes `s_i * c` degenerate to `c`, which cancels from
# every pairwise comparison, so the shared race shock was INERT for placings and
# moved only marks. That is the single mechanism by which race conditions can
# reorder a field.
#
# Cause: `between <- max(var(slope_adj) - mean(noise_var), 1e-6)` subtracted an
# unweighted mean over a quantity with an unbounded tail. Measured on the corpus,
# median(noise_var) 0.107 against mean 147,000 and max 5.6e9 -- the population
# noise level was set by athletes with sxx ~ 1e-14, seen twice in races where
# nothing happened. Replaced with DerSimonian-Laird, which weights each athlete
# by their own precision.
#
# ONE-VARIABLE A/B. `$athlete` is a leaf of the calibration -- nothing else reads
# it -- so only that table is rebuilt and swapped into the csigma calibration.
# Every other slot is byte-identical to the baseline by construction.
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius"))
library(data.table)

OUT <- "C:/dev/citiusverse/citiusdata/data"
BASE <- "calibration_corpus_csigma.rds"

x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[!is.na(date)]
cat(sprintf("corpus: %s results | %s races\n", format(nrow(x), big.mark = ","),
            format(uniqueN(x$race_key), big.mark = ",")))
keep <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "sex",
          "round", "tier", "race_key", "competition_id", "discipline",
          "orientation", "is_technical", "nomark_observable", "source")
x <- x[, intersect(keep, names(x)), with = FALSE]
clean <- flag_implausible(x)
rm(x); invisible(gc())

cat("calibrating...\n")
t0 <- Sys.time()
cal_new <- calibrate(clean, min_races = 30L)
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

a <- as.data.table(cal_new$athlete)
cat("\n=== SENSITIVITY, REBUILT ===\n")
cat("athletes            :", nrow(a), "\n")
cat("sd(sensitivity)     :", signif(sd(a$sensitivity, na.rm = TRUE), 4), "\n")
cat("5-95%%               :", paste(signif(quantile(a$sensitivity, c(.05, .95), na.rm = TRUE), 3),
                                    collapse = " .. "), "\n")
cat("range               :", paste(signif(range(a$sensitivity, na.rm = TRUE), 3), collapse = " .. "), "\n")
cat("TARGET (58k harvest): sd 0.235, 5-95%% 0.63 .. 1.39\n")

base <- readRDS(file.path(OUT, BASE))
old <- as.data.table(base$athlete)
cat("\nbaseline sd         :", signif(sd(old$sensitivity, na.rm = TRUE), 4), "\n")

base$athlete <- cal_new$athlete
saveRDS(base, file.path(OUT, "calibration_corpus_csens.rds"))
cat("\nwrote calibration_corpus_csens.rds\n")
