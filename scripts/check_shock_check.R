# Is the ~2.5% shock in Werro's Diamond League races REAL?
#
# If her rivals also outran their ratings by ~2.5%, the shock is doing its job
# and the problem is that the whole elite 800m population is under-rated. If
# they did not, the shock is mis-estimated and it is eating her signal.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
aid <- d[event_id == "AT-800Metres-W" & grepl("Werro", athlete_name), athlete_id][1]
keys <- h[athlete_id == aid & event_id == "AT-800Metres-W" &
          date %in% as.Date(c("2026-06-07","2026-06-16","2026-06-28")), race_key]
nm <- unique(d[, .(athlete_id, athlete_name)])
for (k in keys) {
  f <- h[race_key == k & seen == TRUE]
  f <- merge(f, nm, by = "athlete_id", all.x = TRUE)
  f[, surprise := 100*(perf - r_pre)]
  setorder(f, place)
  cat(sprintf("\n== %s  (%s, %d finishers rated) ==\n", as.character(f$date[1]), k, nrow(f)))
  cat(sprintf("%-24s %8s %10s %9s\n", "athlete", "mark", "rating in", "beat by"))
  for (i in seq_len(min(8, nrow(f))))
    cat(sprintf("%-24s %8s %10s %+8.2f%%\n",
                substr(ifelse(is.na(f$athlete_name[i]),"?",f$athlete_name[i]),1,24),
                sprintf("%d:%05.2f", floor(exp(-f$perf[i])/60), exp(-f$perf[i]) %% 60),
                sprintf("%d:%05.2f", floor(exp(-f$r_pre[i])/60), exp(-f$r_pre[i]) %% 60),
                f$surprise[i]))
  est <- f[n_eff >= 2]
  cat(sprintf("  FIELD MEAN (established, n=%d): %+.2f%%  <- this is the shock\n",
              nrow(est), mean(est$surprise)))
}
cat("\nIf the field mean is genuinely ~+2.5%, the shock is right and the whole\n")
cat("elite population is under-rated. That is a LEVEL problem, not a Werro one.\n")
