suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
d <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
h[, athlete_id := as.character(athlete_id)]; d[, athlete_id := as.character(athlete_id)]
nm <- unique(d[, .(athlete_id, athlete_name)])
jk <- nm[athlete_name %like% "^Josh.*Kerr|^Joshua.*Kerr", athlete_id]
cat(sprintf("Josh Kerr athlete_id(s): %s\n\n", paste(jk, collapse=", ")))
x <- h[athlete_id %chin% jk][order(date)]
tm <- function(p){s<-exp(-p); if (s>=60) sprintf("%d:%05.2f", floor(s/60), s%%60) else sprintf("%.2f", s)}
cat(sprintf("=== every race in the corpus (%d) ===\n", nrow(x)))
for (i in seq_len(nrow(x)))
  cat(sprintf("  %s %-16s %-6s ran %-9s rating in %-9s n_eff %.1f\n",
      as.character(x$date[i]), sub("^AT-","",x$event_id[i]), x$rc[i],
      tm(x$perf[i]), tm(x$r_pre[i]), x$n_eff[i]))
cat("\n=== where he sits in each event's ranking ===\n")
print(d[athlete_id %chin% jk, .(event = sub("^AT-","",event_id), rk,
        typical = round(pred_mark,2), n_eff = round(n_eff,1), last)][order(event)])
cat("\n=== who IS in the 1500m and Mile top 10, and how thin are they? ===\n")
for (EV in c("AT-1500Metres-M","AT-Mile-M")) {
  cat(sprintf("\n%s:\n", sub("^AT-","",EV)))
  print(d[event_id == EV & rk <= 10, .(rk, athlete_name,
          typical = round(pred_mark,2), n_eff = round(n_eff,1))])
}
