derr <- function(e) { cat("ERROR:", conditionMessage(e), "\n"); quit(status = 1) }
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
SP  <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-citiusverse/9dc3e857-04af-4b60-bd2c-0b14804a5738/scratchpad"

st <- setDT(read_parquet(file.path(SP, "seqv2_state_combo.parquet")))
st[, athlete_id := as.character(athlete_id)]
stopifnot(nrow(st) > 1e5)

# How far is a form rating from what the athlete actually runs in a FINAL?
# The rating is on the oriented log-perf scale; compare it to the athlete's
# 2026 finals performances only, since that is what a ratings page implies.
evs <- c("AT-100Metres-M","AT-200Metres-M","AT-400Metres-M","AT-800Metres-M",
         "AT-1500Metres-M","AT-5000Metres-M","AT-100Metres-W","AT-800Metres-W",
         "AT-1500Metres-W","AT-110MetresHurdles-M")
res <- rbindlist(lapply(evs, function(EV) {
  f <- file.path(OUT, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(NULL)
  x <- setDT(read_parquet(f, col_select = c("athlete_id","date","perf","place","round")))
  x[, athlete_id := as.character(athlete_id)]
  x <- x[!is.na(perf) & !is.na(place) & place > 0 & date >= as.Date("2026-01-01")]
  if (!nrow(x)) return(NULL)
  x[, rc := fifelse(grepl("semi", round, ignore.case=TRUE), "semi",
        fifelse(grepl("heat|round 1|qual", round, ignore.case=TRUE), "heat", "final"))]
  s <- st[event_id == EV, .(athlete_id, R)]
  m <- merge(x, s, by = "athlete_id")
  if (!nrow(m)) return(NULL)
  m[, d := perf - R]
  data.table(event_id = EV,
             n_final = m[rc == "final", .N],
             bias_final = m[rc == "final", mean(d)],
             sd_final   = m[rc == "final", stats::sd(d)],
             n_heat  = m[rc != "final", .N],
             bias_heat  = m[rc != "final", mean(d)])
}), fill = TRUE)

cat("\nform rating vs actual 2026 performance, oriented log scale\n")
cat("(positive bias = athlete runs BETTER than the rating implies)\n\n")
print(res[, .(event_id, n_final,
              bias_final = round(bias_final, 5),
              pct_final  = round(100*bias_final, 3),
              sd_final   = round(sd_final, 4),
              n_heat, pct_heat = round(100*bias_heat, 3))])

cat(sprintf("\npooled finals bias: %.4f%% of a mark over %d finals\n",
            100*res[, weighted.mean(bias_final, n_final)], res[, sum(n_final)]))
cat(sprintf("pooled heat  bias: %.4f%% over %d non-finals\n",
            100*res[, weighted.mean(bias_heat, n_heat)], res[, sum(n_heat)]))
cat(sprintf("finals-minus-heats gap: %.4f%% — this is the size of the offset a\n",
            100*(res[, weighted.mean(bias_final, n_final)] - res[, weighted.mean(bias_heat, n_heat)])))
cat("displayed mark would need, and it is a LEVEL shift, so ordering is untouched.\n")
