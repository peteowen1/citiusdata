# Progress, rate and ETA for every running harvest, read from the caches
# themselves rather than from a log.
#
# File mtimes ARE the log: each cached item is stamped when it landed, so rate
# and ETA come from the data with nothing to keep in sync. It also survives a
# killed job, which a log written by the job does not.
#
# Reports RECENT rate separately from average, because harvest cost drifts:
# athlete careers vary in length, and the athletics backtest slowed 3x through a
# run as later meets accumulated deeper history. "Remaining x average" therefore
# understates the tail, and the recent figure is the honest one.

library(data.table)
OUT <- here::here("citiusdata", "data")

report <- function(label, dir, total, unit = "item") {
  if (!dir.exists(dir)) { cat(sprintf("\n%-22s not started\n", label)); return(invisible()) }
  fi <- file.info(list.files(dir, full.names = TRUE))
  n <- nrow(fi)
  if (!n) { cat(sprintf("\n%-22s 0 cached\n", label)); return(invisible()) }
  fi <- fi[order(fi$mtime), ]
  span <- as.numeric(difftime(max(fi$mtime), min(fi$mtime), units = "mins"))
  age <- as.numeric(difftime(Sys.time(), max(fi$mtime), units = "mins"))
  cat(sprintf("\n%s\n", label))
  cat(sprintf("  cached      : %s of %s (%.1f%%)\n", format(n, big.mark = ","),
              format(total, big.mark = ","), 100 * n / total))
  if (span > 0.1 && n > 1) {
    avg <- n / span
    cat(sprintf("  average rate: %.1f %ss/min over %.0f min\n", avg, unit, span))
    if (n >= 30) {
      recent <- tail(fi, 30)
      rspan <- as.numeric(difftime(max(recent$mtime), min(recent$mtime), units = "mins"))
      rate <- if (rspan > 0) 30 / rspan else NA_real_
      if (is.finite(rate)) {
        cat(sprintf("  recent rate : %.1f %ss/min (last 30)\n", rate, unit))
        cat(sprintf("  ETA         : %.0f min (%.1f h) at recent rate\n",
                    (total - n) / rate, (total - n) / rate / 60))
        cat(sprintf("  drift       : %s\n",
                    if (rate < avg * 0.8) "SLOWING - cost per item rising"
                    else if (rate > avg * 1.2) "speeding up" else "steady"))
      }
    }
    # Empty cache files are recorded misses, not failures, but a high rate means
    # the filter is fetching things that hold nothing.
    empty <- sum(fi$size < 200)
    cat(sprintf("  empty files : %s (%.0f%%) — recorded misses\n",
                format(empty, big.mark = ","), 100 * empty / n))
  }
  cat(sprintf("  last write  : %.1f min ago%s\n", age,
              if (age > 5) "  <- STALLED?" else ""))
  invisible()
}

cli::cli_h2(format(Sys.time(), "%H:%M:%S"))
report("Swimming (full pool sweep)", file.path(OUT, "swim_cache_full"), 2272, "comp")
report("Athletics athlete careers", file.path(OUT, "ath_athlete_cache"), 87119, "athlete")
report("Athletics competitions", file.path(OUT, "ath_comp_cache_v3"), 1120, "comp")

cat("\n--- where the time goes ---\n")
cat("  Both feeds are rate limited at ~0.25s/request (citius_get_json throttle).\n")
cat("  Swimming costs 1 + n_disciplines requests per competition, so a big meet\n")
cat("  is 30+ requests; athletics costs exactly 1 per athlete.\n")
cat("  That makes the athlete sweep cheap per RESULT and the swimming sweep\n")
cat("  expensive per COMPETITION.\n")
