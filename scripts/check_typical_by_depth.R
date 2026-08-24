# The 'typical' mark is right on average and wrong for everybody.
#
# It is beaten 50.34% of the time overall, against a 50% target - which looks
# perfect and hides that the aggregate is an average of two opposite errors:
#
#   n_eff < 1    beaten 53.6%     the line is too SLOW for thin records
#   n_eff 15+    beaten 43.9%     the line is too FAST for deep records
#
# The offset is fitted pooled per event, so it carries the average depth of the
# fit population and fits nobody at the extremes. This is the same class of
# defect as the good-day column being a 1-in-12.4 event labelled 1-in-10, and it
# is user-facing in the same way: `pred_mark` is the number a reader treats as
# "what this athlete usually runs".
#
# WHY IT HAS NOT BEEN FIXED. form_display_marks.R guarantees in its own header
# that it "changes what is shown, never the rank", and a depth-dependent offset
# applied to `rank_mark` WOULD reorder athletes within an event, because two
# athletes on the same rating with different evidence would get different shifts.
# That guarantee is deliberate and worth keeping.
#
# THE SEPARATION THIS TESTS. `pred_mark` (what an athlete typically runs) and
# `rank_mark` (the sort key) are different questions and do not have to share an
# offset. A depth-aware offset on pred_mark ALONE fixes the displayed number and
# leaves the ordering untouched, satisfying the guarantee rather than breaking
# it. This script measures whether that actually works before anything ships.
#
# FITTED BEFORE 2025, CHECKED ON 2025 AND 2026 SEPARATELY, so an improvement has
# to hold on a window that is neither fitted nor sealed.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
FIT_BEFORE <- as.Date("2025-01-01")
MIN_N <- 200L

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & rc == "final" & is.finite(perf) & is.finite(r_pre) & is.finite(n_eff)]
stopifnot("no finals in history" = nrow(h) > 50000)
h[, resid := perf - r_pre]
h[, yr := year(date)]
h[, band := cut(n_eff, c(-Inf, 1, 2, 3, 5, 8, 15, Inf),
                labels = c("<1", "1-2", "2-3", "3-5", "5-8", "8-15", "15+"))]

fit <- h[date < FIT_BEFORE]
stopifnot("fit window is empty" = nrow(fit) > 10000)

# --- the offset as it is now: one median per event ----------------------------
off0 <- fit[, .(offset = stats::median(resid), n_fit = .N), by = event_id]
pooled <- fit[, stats::median(resid)]
off0[n_fit < MIN_N, offset := pooled]

# --- a depth-aware offset -----------------------------------------------------
# Per event AND evidence band would be far too thin - 86 events x 7 bands. So:
# one pooled median per BAND as a correction on top of the per-event offset. That
# keeps the per-event level (which is real and varies in sign) and adds a single
# depth term shared across events, which is the smallest change that can work.
adj <- fit[, .(band_adj = stats::median(resid - pooled), n = .N), by = band]
adj[n < 500, band_adj := 0]
cat("=== the depth correction, fitted before 2025 ===\n")
print(adj[order(band)])
cat("\nband_adj is how much a record of that depth typically runs ABOVE the\n")
cat("pooled line, in log-perf units. Positive = the pooled line is too slow.\n")

# --- does it hold out of sample? ----------------------------------------------
score <- function(yr_keep, lab) {
  x <- h[yr %in% yr_keep]
  x <- merge(x, off0[, .(event_id, offset)], by = "event_id", all.x = TRUE)
  x[is.na(offset), offset := pooled]
  x <- merge(x, adj[, .(band, band_adj)], by = "band", all.x = TRUE)
  x[is.na(band_adj), band_adj := 0]
  r <- x[, .(finals = .N,
             now_pct   = round(100 * mean(perf > r_pre + offset), 1),
             fixed_pct = round(100 * mean(perf > r_pre + offset + band_adj), 1)),
         by = band]
  setorder(r, band)
  cat(sprintf("\n=== %s: %% of finals beating the 'typical' line (target 50) ===\n", lab))
  print(r)
  # the honest summary is the WORST band, not the average - the average is what
  # already looks fine while hiding both errors
  cat(sprintf("worst deviation from 50: now %.1f pp | fixed %.1f pp\n",
              max(abs(r$now_pct - 50)), max(abs(r$fixed_pct - 50))))
  cbind(window = lab, r)
}
v25 <- score(2025, "VALIDATION 2025 (neither fitted nor sealed)")
v26 <- score(2026, "SEALED 2026")

cat("\nA fix has to improve the worst band on BOTH windows. Improving 2026 alone\n")
cat("would mean it was fitted to the sealed data by accident.\n")
f <- file.path(D, "typical_by_depth.json")
writeLines(jsonlite::toJSON(list(tag = TAG, depth_adj = adj,
                                 validation = v25, sealed = v26),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
