# The elite panel printed in REAL MARKS — times as m:ss.xx, field as metres.
#
# Same panel and same selection rule as check_elite_panel.R (each event's #1 by
# the rating carried into their first race of the test year, ranked by gap to
# the event's #2, outcome-independent). Only the presentation changes.
#
# Orientation is not stored in the history file, so it is DERIVED per event by
# checking which of exp(R+offset) / exp(-(R+offset)) reproduces the display's
# own pred_mark, then asserted. Guessing from the unit string would be a silent
# bug the moment a unit is spelled differently.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
h[, resid := perf - r_pre]
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
nm <- unique(d[, .(athlete_id, athlete_name)])

# --- derive orientation per event, and check it ---
o <- d[is.finite(pred_mark) & pred_mark > 0 & is.finite(R) & is.finite(offset)]
o[, e_pos := abs(exp(R + offset) - pred_mark)]
o[, e_neg := abs(exp(-(R + offset)) - pred_mark)]
ori <- o[, .(orient = if (median(e_neg) < median(e_pos)) -1L else 1L,
             err = min(median(e_neg), median(e_pos)),
             unit = unit[1]), by = event_id]
stopifnot(nrow(ori) > 0, max(ori$err / 1) < 1e-6)   # must reproduce pred_mark
cat(sprintf("orientation derived for %d events, max reconstruction error %.2e\n",
            nrow(ori), max(ori$err)))

fmt <- function(mark, orient) {
  if (is.na(mark)) return("     -")
  if (orient > 0) return(sprintf("%.2f m", mark))
  if (mark >= 60) return(sprintf("%d:%05.2f", floor(mark/60), mark %% 60))
  sprintf("%.2f s", mark)
}

cohort <- function(yr, n_panel = 10, min_finals = 6) {
  st <- h[year(date) == yr][order(date), .SD[1], by = .(athlete_id, event_id)]
  st <- st[n_eff >= 5, .(athlete_id, event_id, r0 = r_pre)]
  setorder(st, event_id, -r0)
  st[, rk := seq_len(.N), by = event_id]
  st[, gap := r0 - shift(r0, -1), by = event_id]
  p <- st[rk == 1 & is.finite(gap)][order(-gap)]
  # mean predicted and mean actual, in log space then exponentiated so the
  # ratio of the two IS the reported % bias (a mean of marks would not be)
  tested <- h[year(date) == yr & rc == "final",
              .(finals = .N, mr = mean(r_pre), mp = mean(perf),
                mean_pct = 100*mean(resid),
                p = tryCatch(stats::t.test(resid)$p.value, error = function(e) NA_real_)),
              by = .(athlete_id, event_id)]
  p <- merge(p, tested, by = c("athlete_id","event_id"))[finals >= min_finals]
  p <- merge(p, nm, by = "athlete_id", all.x = TRUE)
  p <- merge(p, ori[, .(event_id, orient)], by = "event_id", all.x = TRUE)
  setorder(p, -gap)
  p[1:min(n_panel, .N)]
}

show <- function(p, yr) {
  cat(sprintf("\n===== SELECTED as at end-%d, TESTED on %d finals =====\n", yr-1, yr))
  cat(sprintf("%-3s %-24s %-22s %6s  %10s  %10s  %8s  %7s\n",
              "#","athlete","event","finals","predicted","actual","diff","p"))
  cat(strrep("-", 100), "\n")
  for (i in seq_len(nrow(p))) {
    or <- p$orient[i]
    pred <- exp(or * p$mr[i]); act <- exp(or * p$mp[i])
    cat(sprintf("%-3d %-24s %-22s %6d  %10s  %10s  %+7.2f%%  %7.4f\n",
        i, substr(ifelse(is.na(p$athlete_name[i]),"?",p$athlete_name[i]),1,24),
        substr(sub("^AT-","",p$event_id[i]),1,22), p$finals[i],
        fmt(pred, or), fmt(act, or), p$mean_pct[i], p$p[i]))
  }
  cat(sprintf("\n  panel mean %+.3f%%   (negative = ran WORSE than the model predicted)\n",
              mean(p$mean_pct)))
}
show(cohort(2025), 2025)
show(cohort(2026), 2026)
cat("\npredicted = the rating each athlete carried INTO each final, averaged in log\n")
cat("space then converted back, so the diff column is exactly the % bias reported.\n")
