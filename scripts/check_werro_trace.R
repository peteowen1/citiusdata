# Werro's rating trajectory, race by race, to see WHICH races drag it to 1:58
# when her elite finals cluster at 1:53-1:54.5.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
aid <- d[event_id == "AT-800Metres-W" & grepl("Werro", athlete_name), athlete_id][1]
cat(sprintf("Werro athlete_id %s\n\n", aid))
w <- h[athlete_id == aid & event_id == "AT-800Metres-W"][order(date)]
cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
w[, competition_id := tstrsplit(race_key, "|", fixed = TRUE)[[1]]]
w <- merge(w, cat0[, .(competition_id, comp_name, class, meet_tier)],
           by = "competition_id", all.x = TRUE)
setorder(w, date)
fm <- function(s) sprintf("%d:%05.2f", floor(s/60), s %% 60)
w[, mark := exp(-perf)]
w[, implied := exp(-r_pre)]          # what the rating implied going in
cat(sprintf("%-11s %-7s %9s %11s %9s %6s %-22s\n",
            "date","round","mark","rating in","surprise","n_eff","meet"))
for (i in seq_len(nrow(w)))
  cat(sprintf("%-11s %-7s %9s %11s %+9.2f%% %6.1f %-22s\n",
              as.character(w$date[i]), substr(w$rc[i],1,7), fm(w$mark[i]),
              fm(w$implied[i]), 100*(w$perf[i]-w$r_pre[i]), w$n_eff[i],
              substr(ifelse(is.na(w$comp_name[i]), "?", w$comp_name[i]), 1, 22)))
cat(sprintf("\nraces in the model: %d | 2026 only: %d\n", nrow(w), w[date >= as.Date("2026-01-01"), .N]))
cat("\nher 2026 marks by round:\n")
print(w[date >= as.Date("2026-01-01"), .(n = .N, best = fm(min(mark)), worst = fm(max(mark)),
                                          median = fm(stats::median(mark))), by = rc])
