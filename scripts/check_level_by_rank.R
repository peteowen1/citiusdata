# The level bias is not uniform: elite athletes are under-rated because their
# ratings are dragged by jogged heats and domestic races while their elite-final
# form is far faster. Measure the finals bias BY HOW GOOD THE ATHLETE IS, which
# is what a display offset would have to condition on.
#
# Fitted on 2020-2024 so it stays out of sample for both scoring windows.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
p <- h[seen == TRUE & rc == "final" & is.finite(perf) & is.finite(r_pre)]
p[, resid := perf - r_pre]
# Rank each athlete WITHIN their event by the rating they carried in, so "elite"
# means elite for that event rather than fast in absolute terms.
p[, r_pct := frank(r_pre) / .N, by = event_id]
p[, band := cut(r_pct, c(0, .5, .8, .9, .95, .99, 1),
                labels = c("bottom 50%","50-80%","80-90%","90-95%","95-99%","top 1%"))]
fit <- p[date < as.Date("2025-01-01")]
val <- p[year(date) == 2026]
cat("FINALS BIAS BY RATING BAND (positive = the athlete outran their rating)\n")
cat("fit = 2020-2024, check = 2026 (out of sample)\n\n")
a <- fit[, .(n_fit = .N, fit_pct = round(100*mean(resid), 3)), by = band]
b <- val[, .(n_26 = .N, val_pct = round(100*mean(resid), 3)), by = band]
m <- merge(a, b, by = "band", all = TRUE)[order(band)]
m[, secs_on_118 := round(118*(exp(fit_pct/100)-1), 2)]
print(m)
cat("\nIf the top band is consistently positive, the display offset should depend\n")
cat("on the athlete's standing, not just on the event.\n")
cat(sprintf("\ntop 1%% band: fit %+.2f%% -> %.2f s on a 1:58; 2026 check %+.2f%%\n",
            m[band=="top 1%"]$fit_pct, m[band=="top 1%"]$secs_on_118, m[band=="top 1%"]$val_pct))
