# Paired forecast-error comparison between form-engine arms.
#
# The adjusted-marks arms of 2026-09-18/19 were judged on this and the table
# was assembled by hand; this makes it one command. Rows are paired per
# (race_key, athlete_id, event_id) on the rows EVERY named arm holds with a
# finite error and seen == TRUE, restricted to 2025-26 (the tune + confirm
# window the review quotes). Per family and pooled:
#   sd %      sd of the error in % of mark (lower is better)
#   MAE %     mean |error| in % of mark (lower is better)
#   d MAE     arm MAE minus the comparator's, paired per row, in pp of mark;
#             negative = the arm beats the comparator; t from the paired test
#
#   CITIUS_FA_ARM=adjv9 CITIUS_FA_VS=adjold,adjv7 \
#     Rscript citiusdata/scripts/diagnostics/compare_forecast_arms.R
suppressMessages({ library(arrow); library(data.table) })
D   <- here::here("citiusdata", "data")
ARM <- Sys.getenv("CITIUS_FA_ARM", "")
VS  <- strsplit(Sys.getenv("CITIUS_FA_VS", "adjold,adjv7"), ",")[[1]]
FROM <- as.Date(Sys.getenv("CITIUS_FA_FROM", "2025-01-01"))
stopifnot("CITIUS_FA_ARM names the arm to judge" = nzchar(ARM))
key <- c("race_key", "athlete_id", "event_id")
load1 <- function(tag) {
  f <- file.path(D, sprintf("forecast_marks_%s.parquet", tag))
  stopifnot(file.exists(f))
  x <- setDT(read_parquet(f, col_select = dplyr::all_of(c(key, "family", "date", "indoor", "error", "seen"))))
  x[, athlete_id := as.character(athlete_id)]
  x <- x[seen == TRUE & is.finite(error) & date >= FROM]
  cat(sprintf("%-8s %s rows %s..%s\n", tag, format(nrow(x), big.mark = ","), min(x$date), max(x$date)))
  unique(x, by = key)
}
a <- load1(ARM)
for (v in VS) {
  b <- load1(v)
  m <- merge(a, b[, c(key, "error"), with = FALSE], by = key, suffixes = c("", "_vs"))
  m[, `:=`(ae = abs(error), ae_vs = abs(error_vs))]
  m[, d := ae - ae_vs]
  cat(sprintf("\n=== %s vs %s: %s paired rows, %s..%s ===\n", ARM, v, format(nrow(m), big.mark = ","), min(m$date), max(m$date)))
  cat("sd/MAE in % of mark, lower is better; d_mae_pp = arm minus comparator (negative = arm wins), t paired\n")
  fam <- m[, .(rows = .N, sd_arm = round(100 * sd(error), 3), sd_vs = round(100 * sd(error_vs), 3),
               mae_arm = round(100 * mean(ae), 3), mae_vs = round(100 * mean(ae_vs), 3),
               d_mae_pp = round(100 * mean(d), 4), t = round(mean(d) / (sd(d) / sqrt(.N)), 2)), by = family][order(family)]
  pooled <- m[, .(family = "ALL", rows = .N, sd_arm = round(100 * sd(error), 3), sd_vs = round(100 * sd(error_vs), 3),
                  mae_arm = round(100 * mean(ae), 3), mae_vs = round(100 * mean(ae_vs), 3),
                  d_mae_pp = round(100 * mean(d), 4), t = round(mean(d) / (sd(d) / sqrt(.N)), 2))]
  print(rbind(fam, pooled))
  cat("middle distance split by indoor (same columns):\n")
  print(m[family == "middle", .(rows = .N, mae_arm = round(100 * mean(ae), 3), mae_vs = round(100 * mean(ae_vs), 3),
                                d_mae_pp = round(100 * mean(d), 4), t = round(mean(d) / (sd(d) / sqrt(.N)), 2)),
          by = .(indoor = indoor %in% TRUE)][order(indoor)])
}
invisible(NULL)
