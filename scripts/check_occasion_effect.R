# Are the occasion effects significantly different from zero? I called them
# "fractions of a percent" and moved on, which conflates SMALL with NOT REAL.
# Test them, and put every number on the same scale (percent, and seconds on a
# 1:58 800m) so the comparison with Birmingham's 3.07 s is like for like.
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
cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h[, competition_id := tstrsplit(race_key, "|", fixed = TRUE)[[1]]]
h <- merge(h, cat0[, .(competition_id, class)], by = "competition_id", all.x = TRUE)
p <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre) & rc == "final" & !is.na(class)]
p[, resid := perf - r_pre]
MAJ <- c("olympics","world_champs","european_champs","commonwealth","world_indoor",
         "continental","world_other")
p[, occasion := fifelse(class %chin% MAJ, "championship", "ordinary")]
REF <- 118  # seconds, a 1:58 800m, for converting % to something readable

tt <- function(x, label) {
  t <- stats::t.test(x)
  data.table(group = label, n = length(x),
             pct = round(100*mean(x), 3),
             secs_on_118 = round(REF*(exp(mean(x))-1), 2),
             se_pct = round(100*(t$conf.int[2]-t$conf.int[1])/3.92, 4),
             t = round(unname(t$statistic), 1),
             p = signif(t$p.value, 2))
}
res <- rbindlist(list(
  tt(p[occasion=="ordinary"]$resid, "ordinary finals"),
  tt(p[occasion=="championship"]$resid, "championship finals"),
  tt(p[class=="european_champs"]$resid, "  european_champs"),
  tt(p[class=="world_champs"]$resid, "  world_champs"),
  tt(p[class=="olympics"]$resid, "  olympics")))
print(res)

cat("\nchampionship vs ordinary, two-sample:\n")
tt2 <- stats::t.test(p[occasion=="championship"]$resid, p[occasion=="ordinary"]$resid)
gap <- diff(rev(c(mean(p[occasion=="championship"]$resid), mean(p[occasion=="ordinary"]$resid))))
cat(sprintf("  gap %.3f pp  (%.2f s on a 1:58)   t = %.1f   p = %s\n",
            -100*gap, -REF*(exp(gap)-1), unname(tt2$statistic), signif(tt2$p.value, 2)))

cat(sprintf("\nBirmingham medallists ran %.2f s faster than TYPICAL = %.2f%%\n",
            3.07, 100*log(REF/(REF-3.07))))
cat("Historical championship MEDALLISTS (selection-confounded) ran +0.789%\n")
cat(sprintf("  = %.2f s on a 1:58. Birmingham's medallists beat that by ~3x.\n",
            REF*(exp(0.00789)-1)))
