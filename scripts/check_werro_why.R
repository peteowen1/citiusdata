# Her rating should converge: k is pinned at the 0.32 floor and she surprises
# +3% every final. It does not. Where does the update go?
#
# The update is R_next = R + k * ((perf - R) - S), so backing out the ACTUAL
# movement between consecutive races reveals how much the race shock S ate.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
d <- setDT(read_parquet(file.path(OUT, "form_display_final.parquet")))
aid <- d[event_id == "AT-800Metres-W" & grepl("Werro", athlete_name), athlete_id][1]
w <- h[athlete_id == aid & event_id == "AT-800Metres-W"][order(date)]
K0 <- 0.95; KAPPA <- 3; KFLOOR <- 0.32
w[, k := pmax(K0 * KAPPA / (n_eff + KAPPA), KFLOOR)]
w[, raw_surprise := perf - r_pre]
w[, r_next := shift(r_pre, -1)]
w[, actual_move := r_next - r_pre]
w[, eff_surprise := actual_move / k]     # what the model ACTED on
w[, shock_ate := raw_surprise - eff_surprise]
x <- w[date >= as.Date("2026-05-01") & !is.na(r_next)]
fm <- function(s) sprintf("%d:%05.2f", floor(s/60), s %% 60)
cat("Werro 2026, what the model DID with each race\n")
cat("raw = how much she beat her rating; acted-on = after the race shock was removed\n\n")
cat(sprintf("%-11s %-6s %8s %8s %9s %11s %10s\n",
            "date","round","mark","raw %","acted-on %","shock ate %","rating after"))
for (i in seq_len(nrow(x)))
  cat(sprintf("%-11s %-6s %8s %+8.2f %+10.2f %+11.2f %10s\n",
              as.character(x$date[i]), substr(x$rc[i],1,6), fm(exp(-x$perf[i])),
              100*x$raw_surprise[i], 100*x$eff_surprise[i], 100*x$shock_ate[i],
              fm(exp(-x$r_next[i]))))
cat(sprintf("\nOver 2026: mean raw surprise %+.2f%%, mean acted-on %+.2f%%\n",
            100*mean(w[date >= as.Date("2026-01-01") & !is.na(eff_surprise)]$raw_surprise),
            100*mean(w[date >= as.Date("2026-01-01") & !is.na(eff_surprise)]$eff_surprise)))
cat(sprintf("So the race shock absorbed %.0f%% of her outperformance.\n",
            100*(1 - mean(w[date >= as.Date("2026-01-01") & !is.na(eff_surprise)]$eff_surprise) /
                     mean(w[date >= as.Date("2026-01-01") & !is.na(eff_surprise)]$raw_surprise))))
