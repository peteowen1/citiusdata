# Cold-start, on real rows, BEFORE any rule is written (2026-09-19).
#
# The 08-27 audit measured `seen == FALSE` pairs at 57.9% concordance against
# 74-79% for every other evidence band, and genuinely double-cold pairs stuck
# at 50.0% by construction. This prints what the forecast table actually holds
# for a cold athlete: the mark error by evidence band, and for a handful of
# cold rows at top-tier meets, what the SAME athlete had in OTHER events before
# that date -- because the only information a cold prior can borrow is
# athlete-level, and this shows how often it exists and what it would say.
#
# error = adj_perf - forecast_perf on the log scale (+ = the athlete did BETTER
# than forecast); |error| in % of the mark, lower is better.
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("CITIUS_FM_TAG", "final")
d <- as.data.table(arrow::read_parquet(file.path(OUT, sprintf("forecast_marks_%s.parquet", TAG))))
d <- d[is.finite(error)]
d[, band := cut(n_eff, c(-Inf, 0, 0.5, 1, 2, 5, 10, Inf), labels = c("0", "(0,0.5]", "(0.5,1]", "(1,2]", "(2,5]", "(5,10]", ">10"))]
d[, abs_pct := 100 * abs(error)]
cat(sprintf("%s scored rows; seen==FALSE %s (%.1f%%)\n\n", format(nrow(d), big.mark = ","), format(sum(!d$seen), big.mark = ","), 100 * mean(!d$seen)))
cat("=== mark error by evidence band (median |error| in % of mark, lower is better; bias = mean error, + = ran better than forecast) ===\n")
print(d[, .(n = .N, med_abs_pct = round(median(abs_pct), 2), bias_pct = round(100 * mean(error), 2), seen_share = round(mean(seen), 2)), by = band][order(band)])
cat("\nby family, cold (seen == FALSE) vs the rest:\n")
print(dcast(d[, .(med_abs_pct = round(median(abs_pct), 2)), by = .(family, cold = fifelse(seen, "seen", "cold"))], family ~ cold, value.var = "med_abs_pct")[])

# What does a cold athlete carry elsewhere? For each cold row, the athlete's
# rows in OTHER events strictly before the date, and their percentile there.
d[, perf_pct := frank(adj_perf) / .N, by = event_id]   # 1 = best in this table's event
cold <- d[seen == FALSE]
other <- d[, .(athlete_id, o_event = event_id, o_date = date, o_pct = perf_pct, o_n_eff = n_eff)]
setkey(other, athlete_id)
prior <- other[cold[, .(athlete_id, race_key, event_id, date)], on = "athlete_id", allow.cartesian = TRUE][o_date < date & o_event != event_id]
has <- prior[, .(n_other_events = uniqueN(o_event), n_other_rows = .N, best_other_pct = max(o_pct)), by = .(race_key, athlete_id)]
cold <- merge(cold, has, by = c("race_key", "athlete_id"), all.x = TRUE)
cold[is.na(n_other_rows), `:=`(n_other_events = 0L, n_other_rows = 0L)]
cat(sprintf("\ncold rows with ANY earlier row in another event: %s of %s (%.1f%%)\n",
            format(sum(cold$n_other_rows > 0), big.mark = ","), format(nrow(cold), big.mark = ","), 100 * mean(cold$n_other_rows > 0)))
cat("median |error| for cold rows WITH vs WITHOUT other-event history (lower is better):\n")
print(cold[, .(n = .N, med_abs_pct = round(median(abs_pct), 2), bias_pct = round(100 * mean(error), 2)), by = .(has_other = n_other_rows > 0)])
cat("\n...and does the other-event percentile predict the cold error? (bias by best_other_pct quartile; + = ran better than forecast)\n")
print(cold[n_other_rows > 0][, .(n = .N, bias_pct = round(100 * mean(error), 2), med_abs_pct = round(median(abs_pct), 2)), by = .(q = cut(best_other_pct, c(0, .5, .75, .9, 1), include.lowest = TRUE))][order(q)])

cat("\n=== eight real cold rows at top meets (walk these with Pete before writing a rule) ===\n")
ex <- cold[grepl("Olympic|World Championships|Diamond", comp_name)][order(-abs_pct)]
ex <- rbind(head(ex, 4), head(ex[abs_pct < 1], 4))
print(ex[, .(date, comp_name = substr(comp_name, 1, 28), event_id, athlete_id, forecast_mark = round(forecast_mark, 2), mark, error_pct = round(100 * error, 2),
             n_other_events, best_other_pct = round(best_other_pct, 2))])
