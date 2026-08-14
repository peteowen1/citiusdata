# Analysis of the per-race form history (form_ratings.R with SEQ_HIST=1).
#
# The point of this file is that r_pre is the rating an athlete CARRIED INTO a
# race, so every question below is answered OUT OF SAMPLE. The end-of-season
# state in seqv2_state_*.parquet cannot answer any of them: it has already
# absorbed the races you would test it against, which is how a pooled finals
# bias of -0.005% over 126,669 finals was produced on 2026-08-14 and briefly
# mistaken for a result.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
TAG <- Sys.getenv("FORM_TAG", "final")
SC  <- Sys.getenv("FORM_OUT", "C:/dev/citiusverse/citiusdata/data")
f <- file.path(SC, sprintf("seqv3_history_%s.parquet", TAG))
if (!file.exists(f)) stop("no history file at ", f,
                          " -- run form_ratings.R with SEQ_HIST=1")
h <- setDT(read_parquet(f))
stopifnot(nrow(h) > 1e5, all(c("r_pre","perf","rc","seen","n_eff") %in% names(h)))
cat(sprintf("history: %s athlete-races, %s races, %s to %s\n",
            format(nrow(h), big.mark=","), format(uniqueN(h$race_key), big.mark=","),
            min(h$date), max(h$date)))

# Only races the athlete came in WITH a rating are a forecast; a cold start is
# fitted to the race itself and would flatter every number here.
p <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf)]
p[, resid := perf - r_pre]
cat(sprintf("forecastable rows (seen): %s of %s (%.1f%%)\n",
            format(nrow(p), big.mark=","), format(nrow(h), big.mark=","),
            100*nrow(p)/nrow(h)))

cat("\n== 1. OUT-OF-SAMPLE LEVEL BIAS by round class ==\n")
cat("   (positive = ran better than the rating carried in)\n")
print(p[date >= as.Date("2026-01-01"),
        .(n = .N, bias_pct = round(100*mean(resid), 4),
          sd_pct = round(100*stats::sd(resid), 3)), by = rc][order(rc)])

cat("\n== 2. THE DISPLAY OFFSET: finals only, per event (top 12 by n) ==\n")
ev <- p[date >= as.Date("2026-01-01") & rc == "final",
        .(n = .N, bias_pct = round(100*mean(resid), 4)), by = event_id][order(-n)]
print(utils::head(ev, 12))
cat(sprintf("\n   pooled finals bias: %.4f%% over %s finals rows\n",
            100*p[date >= as.Date("2026-01-01") & rc == "final", mean(resid)],
            format(p[date >= as.Date("2026-01-01") & rc == "final", .N], big.mark=",")))
cat("   THIS is the number the ratings page needs, and it is out of sample.\n")

cat("\n== 3. DOES THE LEARNING-RATE FLOOR BIND, AND ON WHOM? ==\n")
K0 <- as.numeric(Sys.getenv("SEQ_K0","0.95")); KAPPA <- as.numeric(Sys.getenv("SEQ_KAPPA","3"))
KFLOOR <- as.numeric(Sys.getenv("SEQ_KFLOOR","0.18"))
p[, k_decay := K0 * KAPPA / (n_eff + KAPPA)]
p[, floored := k_decay < KFLOOR]
cat(sprintf("   floor %.2f binds on %.1f%% of forecastable rows (n_eff > %.1f)\n",
            KFLOOR, 100*mean(p$floored), max(0, KAPPA*(K0/KFLOOR - 1))))
print(p[, .(rows = .N, mean_n_eff = round(mean(n_eff),1),
            bias_pct = round(100*mean(resid),4)), by = floored])

cat("\n== 4. RATING TRAJECTORY for a named athlete-event ==\n")
AID <- Sys.getenv("FORM_ATHLETE", "")
if (nzchar(AID)) {
  tr <- h[athlete_id == AID][order(date)]
  print(tr[, .(date, event_id, rc, n_eff = round(n_eff,1),
               r_pre = round(r_pre,4), perf = round(perf,4), place)])
} else {
  cat("   set FORM_ATHLETE=<athlete_id> to print one athlete's rating path\n")
}
