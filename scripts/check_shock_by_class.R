# Field quality does not predict the shock (R2 0.022). But a PACED race and a
# TACTICAL race are different things, and which one you are looking at is known
# before the gun. Diamond League runs behind a pacemaker for time; a
# championship final is won, not timed.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h[, competition_id := tstrsplit(race_key, "|", fixed = TRUE)[[1]]]
h <- merge(h, cat0[, .(competition_id, class)], by = "competition_id", all.x = TRUE)
p <- h[seen == TRUE & rc == "final" & is.finite(perf) & is.finite(r_pre) & !is.na(class)]
p[, resid := perf - r_pre]
# the race_key itself flags a paced Diamond Discipline event
p[, paced := grepl("Diamond Discipline", race_key, fixed = TRUE)]
race <- p[, .(shock = mean(resid), n = .N, class = class[1], paced = paced[1],
              date = date[1]), by = race_key][n >= 5]
cat("MEAN RACE SHOCK by class (fit 2020-2024 / check 2026), in % and seconds on 1:58\n\n")
f <- race[date < as.Date("2025-01-01")]; v <- race[year(date) == 2026]
a <- f[, .(n_fit = .N, fit = round(100*mean(shock), 3)), by = class]
b <- v[, .(n_26 = .N, chk = round(100*mean(shock), 3)), by = class]
m <- merge(a, b, by = "class", all = TRUE)[order(-fit)]
m[, secs := round(118*(exp(fit/100)-1), 2)]
print(m[n_fit >= 50])
cat("\nPACED (Diamond Discipline) vs the rest:\n")
pa <- f[, .(races = .N, shock = round(100*mean(shock), 3),
            secs = round(118*(exp(mean(shock))-1), 2)), by = paced]
print(pa)
cat("\n2026 check:\n")
print(v[, .(races = .N, shock = round(100*mean(shock), 3)), by = paced])
tt <- stats::t.test(f[paced == TRUE]$shock, f[paced == FALSE]$shock)
cat(sprintf("\npaced vs not: gap %.3f pp, t = %.1f, p = %s\n",
            100*diff(rev(tt$estimate)), unname(tt$statistic), signif(tt$p.value, 2)))
