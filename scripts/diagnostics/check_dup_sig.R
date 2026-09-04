# Is Duplantis's +1.003% a real individual effect, or 7 races of noise?
# His race-by-race swings are large (+4.20 to -4.20), so test rather than assume.
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
chk <- function(pat, yr_from = 2026) {
  aid <- d[grepl(pat, athlete_name), athlete_id][1]
  x <- h[athlete_id == aid & year(date) >= yr_from & rc == "final"]
  if (nrow(x) < 3) return(NULL)
  t <- stats::t.test(x$resid)
  data.table(athlete = pat, finals = nrow(x),
             mean_pct = round(100*mean(x$resid), 3),
             sd_pct = round(100*stats::sd(x$resid), 2),
             t = round(unname(t$statistic), 2),
             p = signif(t$p.value, 2),
             ci = sprintf("%+.2f to %+.2f", 100*t$conf.int[1], 100*t$conf.int[2]))
}
cat("2026 FINALS, per athlete — is the individual effect distinguishable from 0?\n\n")
print(rbindlist(lapply(c("Duplantis","Hodgkinson","Werro","Lyles","Tentoglou"), chk)))
cat("\nsame athletes with 2024+ included, for a longer record:\n\n")
print(rbindlist(lapply(c("Duplantis","Hodgkinson","Werro","Lyles","Tentoglou"),
                       function(p) chk(p, 2024))))
cat("\nA wide CI spanning 0 means the apparent individual effect is not\n")
cat("distinguishable from noise at this sample size.\n")
