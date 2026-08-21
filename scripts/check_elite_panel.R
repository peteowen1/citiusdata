# Persistent per-athlete bias among the genuinely dominant — TWO COHORTS.
#
# Pete's design: select as at end-2024 and test 2025; select as at end-2025 and
# test 2026. Two independent looks, and any athlete in both gives a replication
# check, which is far stronger than one cohort.
#
# SELECTION IS PRE-SPECIFIED AND OUTCOME-INDEPENDENT in each cohort: the event's
# #1 by the rating carried into their first race of the test year, ranked by the
# gap to their event's #2. Nothing conditions on the test year's results.
# Searching all athletes for the most significant would be dredging; this is a
# fixed panel, and p-values are Holm-corrected for its size.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
h[, resid := perf - r_pre]
d <- setDT(read_parquet(file.path(OUT, sprintf("form_display_%s.parquet", TAG))))
nm <- unique(d[, .(athlete_id, athlete_name)])

cohort <- function(yr, n_panel = 15, min_finals = 6) {
  st <- h[year(date) == yr][order(date), .SD[1], by = .(athlete_id, event_id)]
  st <- st[n_eff >= 5, .(athlete_id, event_id, r0 = r_pre)]
  setorder(st, event_id, -r0)
  st[, rk := seq_len(.N), by = event_id]
  st[, gap := r0 - shift(r0, -1), by = event_id]
  p <- st[rk == 1 & is.finite(gap)][order(-gap)]
  tested <- h[year(date) == yr & rc == "final",
              .(finals = .N, mean_pct = 100*mean(resid),
                sd_pct = 100*stats::sd(resid),
                p = tryCatch(stats::t.test(resid)$p.value, error = function(e) NA_real_)),
              by = .(athlete_id, event_id)]
  p <- merge(p, tested, by = c("athlete_id","event_id"))[finals >= min_finals]
  p <- merge(p, nm, by = "athlete_id", all.x = TRUE)
  setorder(p, -gap)
  p <- p[seq_len(min(n_panel, .N))]
  p[, p_holm := stats::p.adjust(p, method = "holm")]
  p[, cohort := yr][]
}
show <- function(p, yr) {
  cat(sprintf("\n=== SELECTED at end-%d, TESTED on %d — %d athletes ===\n", yr-1, yr, nrow(p)))
  cat(sprintf("%-24s %-18s %5s %6s %8s %6s %8s %8s\n",
              "athlete","event","gap%","finals","mean%","sd%","p","p(Holm)"))
  for (i in seq_len(nrow(p)))
    cat(sprintf("%-24s %-18s %5.2f %6d %+8.3f %6.2f %8.4f %8.4f\n",
                substr(ifelse(is.na(p$athlete_name[i]),"?",p$athlete_name[i]),1,24),
                substr(sub("^AT-","",p$event_id[i]),1,18), 100*p$gap[i], p$finals[i],
                p$mean_pct[i], p$sd_pct[i], p$p[i], p$p_holm[i]))
  cat(sprintf("  panel mean bias %+.3f%% | clearing p<0.05 uncorrected: %d (expect ~%.1f by chance) | surviving Holm: %d\n",
              mean(p$mean_pct), p[p < 0.05, .N], 0.05*nrow(p), p[p_holm < 0.05, .N]))
}
c25 <- cohort(2025); c26 <- cohort(2026)
show(c25, 2025); show(c26, 2026)

cat("\n=== REPLICATION: athletes in BOTH panels ===\n")
both <- merge(c25[, .(athlete_id, event_id, athlete_name, y25 = mean_pct, p25 = p)],
              c26[, .(athlete_id, event_id, y26 = mean_pct, p26 = p)],
              by = c("athlete_id","event_id"))
if (nrow(both)) {
  setorder(both, -y26)
  for (i in seq_len(nrow(both)))
    cat(sprintf("  %-24s %-18s 2025 %+7.3f%% (p=%.3f)   2026 %+7.3f%% (p=%.3f)  %s\n",
                substr(both$athlete_name[i],1,24), substr(sub("^AT-","",both$event_id[i]),1,18),
                both$y25[i], both$p25[i], both$y26[i], both$p26[i],
                if (sign(both$y25[i]) == sign(both$y26[i])) "same sign" else "FLIPS"))
  cat(sprintf("\n  same sign in both years: %d of %d\n",
              both[sign(y25) == sign(y26), .N], nrow(both)))
} else cat("  none in both panels\n")
