# Athletes away 365+ days from ANY race who then raced R1: which version predicts them best?
# (Pete 2026-11-10.) 105 rows / 73 athletes over 8 cuts: own level (version E) 2.84%; any real
# decay to the R1 median is worse (half-life 365 d: 4.92%). Returners ran ~0.45% FASTER than rated.
suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
EV <- c("AT-800Metres-M","AT-800Metres-W","AT-1500Metres-M","AT-1500Metres-W","AT-5000Metres-M","AT-5000Metres-W")
R1 <- c("OW","GW","GL","DF","A")
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[, athlete_id := as.character(athlete_id)]
anyd <- co[!is.na(date), .(athlete_id, date)]
h <- co[event_id %in% EV & !is.na(perf)]; rm(co); invisible(gc())
h[, r1 := toupper(trimws(race_code)) %in% R1]
r <- rbindlist(lapply(list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*rds$", full.names = TRUE), readRDS), fill = TRUE)
out <- list()
for (cu in sort(unique(r$cut))) { cu <- as.Date(cu, origin = "1970-01-01"); d <- r[cut == cu]
  fl <- h[r1 == TRUE & date < cu & date >= cu - 730, .(floor = median(perf)), by = event_id]
  la <- anyd[date < cu, .(last_any = max(date)), by = athlete_id]
  nx <- h[r1 == TRUE & date >= cu & date < cu + 365, .(t_mean = mean(perf)), by = .(athlete_id, event_id)]
  d <- fl[d, on = "event_id"]; d <- la[d, on = "athlete_id"]; out[[length(out)+1]] <- nx[d, on = c("athlete_id","event_id"), nomatch = NULL] }
s <- rbindlist(out, fill = TRUE)
s[, a := wR_2 / (wR_2 + 0.5 * kR_2)][, E := a * R_2 + (1 - a) * long_365]
s <- s[is.finite(E) & is.finite(floor)]
s[, gap_any := as.numeric(cut - last_any)]
lv <- median(s[year(cut) == 2024, t_mean - E])            # level shift from all 2024 R1 racers
ret <- s[gap_any > 365]
cat(sprintf("R1 racers away 365+ days from ANY race at the cut, who raced R1 in the next 12 months: %d rows, %d athletes (all 8 cuts)\n",
            nrow(ret), uniqueN(ret$athlete_id)))
cat(sprintf("their median days away: %.0f\n", median(ret$gap_any)))
mae <- function(p) 100 * mean(abs(ret$t_mean - (p + lv)))
cat("\nMarks error % for returners (lower is better), every version given the same level shift:\n")
res <- data.table(version = "E: own level, no decay", mae = mae(ret$E))
for (f in c(0.1, 0.25, 0.5)) res <- rbind(res, data.table(version = sprintf("E pulled %d%% towards R1 median", 100 * f), mae = mae((1 - f) * ret$E + f * ret$floor)))
for (hl in c(180, 365, 730)) { b <- 1 - 0.5^(ret$gap_any / hl); res <- rbind(res, data.table(version = sprintf("decay to R1 median, half-life %d d", hl), mae = mae((1 - b) * ret$E + b * ret$floor))) }
res <- rbind(res, data.table(version = "R1 median only", mae = mae(ret$floor)))
print(res[, mae := round(mae, 3)])
cat(sprintf("\nsigned error of E for returners (actual - predicted, %%; negative = ran slower than rated): median %.2f, mean %.2f\n",
            100 * median(ret$t_mean - (ret$E + lv)), 100 * mean(ret$t_mean - (ret$E + lv))))
