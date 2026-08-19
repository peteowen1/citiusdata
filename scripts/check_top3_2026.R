# Is Werro unusual, or does the model under-rate the best athletes generally?
#
# TAUTOLOGY GUARD (Pete's design): pick each event's top 3 using ONLY the rating
# they carried into their FIRST 2026 race — i.e. their end-of-2025 standing,
# fixed before any 2026 result exists. Then measure how they did in 2026 finals.
# Selecting on 2026 form would guarantee overperformance by construction; this
# cannot.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
h[, resid := perf - r_pre]

# standing as at end-2025 = the rating carried into the first 2026 appearance
first26 <- h[year(date) == 2026][order(date), .SD[1], by = .(athlete_id, event_id)]
first26 <- first26[, .(athlete_id, event_id, r_end2025 = r_pre, n_end2025 = n_eff)]
# require some evidence behind the standing, else "top 3" is noise
first26 <- first26[n_end2025 >= 3]
setorder(first26, event_id, -r_end2025)
first26[, standing := seq_len(.N), by = event_id]

f26 <- h[year(date) == 2026 & rc == "final"]
f26 <- merge(f26, first26, by = c("athlete_id","event_id"), all.x = TRUE)
f26[, grp := fifelse(is.na(standing), "unranked",
              fifelse(standing <= 3, "top 3 at end-2025",
              fifelse(standing <= 10, "4-10", "11+")))]
cat("2026 FINALS: how the pre-selected groups actually performed\n")
cat("positive = ran BETTER than the rating they carried in\n\n")
g <- f26[grp != "unranked", .(athletes = uniqueN(athlete_id), finals = .N,
                              bias_pct = round(100*mean(resid), 3),
                              secs_118 = round(118*(exp(mean(resid))-1), 2)),
         by = grp]
setorder(g, -bias_pct)
print(g)

cat("\nper-event top 3 only, the named cases:\n")
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
nm <- unique(d[, .(athlete_id, athlete_name)])
t3 <- f26[grp == "top 3 at end-2025"]
t3 <- merge(t3, nm, by = "athlete_id", all.x = TRUE)
who <- t3[, .(finals = .N, bias_pct = round(100*mean(resid), 3)),
          by = .(athlete_name, event_id)][finals >= 2]
for (pat in c("Werro","Hodgkinson","Lyles","Duplantis","Tentoglou","Bol")) {
  r <- who[grepl(pat, athlete_name)]
  if (nrow(r)) for (i in seq_len(nrow(r)))
    cat(sprintf("  %-22s %-22s %2d finals  %+7.3f%%\n", r$athlete_name[i],
                sub("^AT-","",r$event_id[i]), r$finals[i], r$bias_pct[i]))
}
cat(sprintf("\ntop-3 group spread: %d athlete-events, %.1f%% of them positive\n",
            nrow(who), 100*mean(who$bias_pct > 0)))
cat(sprintf("median %+.3f%%, IQR %+.3f%% to %+.3f%%\n",
            stats::median(who$bias_pct), stats::quantile(who$bias_pct, .25),
            stats::quantile(who$bias_pct, .75)))
