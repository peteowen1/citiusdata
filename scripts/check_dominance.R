# Does the model struggle with DOMINANT athletes — the elite of the elite?
#
# Mechanism to test: the race shock is anchored on the field. If an athlete is
# far better than everyone else, the field's performance carries little
# information about theirs, and their own margin may not be credited.
#
# Dominance is computed from PRE-RACE ratings only (their r_pre minus the best
# OTHER r_pre in the same race), so nothing conditions on the outcome.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
h[, resid := perf - r_pre]
f <- h[year(date) == 2026 & rc == "final"]
f[, nf := .N, by = race_key]
f <- f[nf >= 4]
# best OTHER rating in the race
f[, top_other := {
  o <- max(r_pre); s <- sort(r_pre, decreasing = TRUE)
  fifelse(r_pre >= o, if (length(s) > 1) s[2] else NA_real_, o)
}, by = race_key]
f <- f[is.finite(top_other)]
f[, dominance := 100 * (r_pre - top_other)]   # % better than the next best
f[, dband := cut(dominance, c(-Inf, 0, 0.5, 1, 2, Inf),
                 labels = c("not favourite","0-0.5% ahead","0.5-1%","1-2%","2%+ ahead"))]
cat("2026 FINALS: bias by how far AHEAD of the field the athlete was rated\n")
cat("positive = ran better than their rating; dominance is pre-race only\n\n")
print(f[, .(athlete_finals = .N, athletes = uniqueN(athlete_id),
            bias_pct = round(100*mean(resid), 3)), by = dband][order(dband)])

cat("\nDUPLANTIS, race by race in 2026 (pole vault, mark in metres):\n")
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
aid <- d[grepl("Duplantis", athlete_name), athlete_id][1]
du <- h[athlete_id == aid & year(date) == 2026][order(date)]
for (i in seq_len(nrow(du)))
  cat(sprintf("  %s %-6s cleared %.2f m  rating implied %.2f m  %+6.2f%%\n",
              as.character(du$date[i]), substr(du$rc[i],1,6),
              exp(du$perf[i]), exp(du$r_pre[i]), 100*du$resid[i]))
cat(sprintf("\n  his 2026 mean: %+.3f%% over %d races\n",
            100*mean(du$resid), nrow(du)))
cat("\nFor contrast, the most dominant athlete-races in 2026 (2%+ ahead):\n")
nm <- unique(d[, .(athlete_id, athlete_name)])
dm <- merge(f[dband == "2%+ ahead"], nm, by = "athlete_id", all.x = TRUE)
print(dm[, .(finals = .N, bias = round(100*mean(resid), 2)),
         by = athlete_name][order(-finals)][1:10])
