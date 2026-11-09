# Confident rating for the ratings page (Pete 2026-11-10, option 1): rank by
#   confident = E - k * sd,
# E = version E best estimate (a*R + (1-a)*L), sd = how uncertain E is, FITTED from how
# wrong E actually was. sd grows with time away and with thin recent evidence, so an
# absent star drops down the ranking because we are less sure, not because we pretend
# she got slower; one race restores her. The forecasts keep E.
#
# Fit: residual = (mean R1 perf over the next 12 months) - (E + level shift), on R1
# racers at the 4 2024 cuts; model log(residual^2) ~ log1p(gap/30) + log(wR) by lm, then
# rescale so mean(residual^2 / sd^2) = 1 on 2024. Check: 2025 cuts, predicted sd vs the
# observed RMS residual by decile of predicted sd (calibration). Only k is a choice.
# Reads data/flags/tune_E_cache + the corpus. Output: data/flags/confident_rating_*.csv.
# Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A"); MR <- 0.5
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
h[, r1 := toupper(trimws(race_code)) %in% R1]

fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE); stopifnot(length(fs) == 8)
out <- list()
for (cu in sort(unique(r$cut))) {
  cu <- as.Date(cu, origin = "1970-01-01"); d <- r[cut == cu]
  nx <- h[r1 == TRUE & date >= cu & date < cu + 365, .(t_mean = mean(perf), t_n = .N), by = .(athlete_id, event_id)]
  out[[length(out) + 1]] <- nx[d, on = c("athlete_id", "event_id"), nomatch = NULL]
}
s <- rbindlist(out, fill = TRUE)
s[, a := wR_2 / (wR_2 + MR * kR_2)][, E := a * R_2 + (1 - a) * long_365]
s <- s[is.finite(E) & is.finite(wR_2) & wR_2 > 0]
s[, gap := pmax(as.numeric(cut - last_date), 0)]
s[, fit_set := year(cut) == 2024]
fit <- s[fit_set == TRUE]; c0 <- median(fit$t_mean - fit$E)
s[, res := t_mean - (E + c0)]
fit <- s[fit_set == TRUE]
m <- lm(log(res^2 + 1e-10) ~ log1p(gap / 30) + log(wR_2), data = fit)
pr <- predict(m, newdata = as.data.frame(s)); s[, sd_raw := sqrt(exp(pr))]   # outside [ ]: the cache has a column named `s`
scale <- sqrt(mean(fit$res^2 / s[fit_set == TRUE, sd_raw]^2))
s[, sd := sd_raw * scale]
cat(sprintf("rows: fit 2024 %d, check 2025 %d\n", s[fit_set == TRUE, .N], s[fit_set == FALSE, .N]))
cat("model (log squared residual):\n"); print(round(coef(m), 4)); cat(sprintf("scale %.3f\n", scale))
fwrite(data.table(term = c(names(coef(m)), "scale"), value = c(unname(coef(m)), scale)),
       file.path(OUT, "flags", "confident_rating_sd_model.csv"))
V <- s[fit_set == FALSE]
V[, dec := cut(sd, unique(quantile(sd, 0:10 / 10)), include.lowest = TRUE, labels = FALSE)]
cal <- V[, .(n = .N, predicted_sd_pct = round(100 * sqrt(mean(sd^2)), 3), observed_rms_pct = round(100 * sqrt(mean(res^2)), 3),
             median_gap_days = median(gap), median_wR = round(median(wR_2), 3)), by = dec][order(dec)]
cat("\nCALIBRATION, 2025 cuts (held out): predicted sd vs observed RMS error, % of the mark, by decile of predicted sd.\n")
cat("Good = the two columns track each other.\n"); print(cal)
cat(sprintf("overall 2025: predicted %.3f%%, observed %.3f%%\n", 100 * sqrt(mean(V$sd^2)), 100 * sqrt(mean(V$res^2))))
fwrite(cal, file.path(OUT, "flags", "confident_rating_calibration.csv"))

# TODAY's page
ex <- fread(file.path(OUT, "flags", "thin_record_E_examples.csv"), colClasses = list(character = "athlete_id"))
AS_OF <- max(h$date)
ex[, gap := pmax(as.numeric(AS_OF - as.Date(last_date)), 0)]
ex <- ex[is.finite(wR) & wR > 0 & is.finite(a_E)]
ex[, wR_2 := wR]
pr <- predict(m, newdata = as.data.frame(ex)); ex[, sd := sqrt(exp(pr)) * scale]
fmt <- function(p) { x <- exp(-p); sprintf("%d:%05.2f", as.integer(x %/% 60), x %% 60) }
for (k in c(0, 1, 2)) ex[, (paste0("rk", k)) := frank(-(a_E - k * sd), ties.method = "min"), by = event_id]
cat(sprintf("\nTODAY (as of %s): top-10 slots (of 60) held by athletes unraced in the event for 12 months:\n", format(AS_OF)))
for (k in 0:2) cat(sprintf("  k = %d: %d\n", k, ex[get(paste0("rk", k)) <= 10 & races_12m == 0, .N]))
show <- function(ev, k) {
  rk <- paste0("rk", k); x <- ex[event_id == ev & get(rk) <= 10][order(get(rk))]
  cat(sprintf("\n%s, k = %d (rank | rank at k=0 | athlete | best estimate | sd %% | confident | races 12m | last race)\n", ev, k))
  x[, cat(sprintf("%3d | %4d | %-24s | %s | %.2f | %s | %2d | %s\n", get(rk), rk0, substr(athlete_name, 1, 24), fmt(a_E),
                  100 * sd, fmt(a_E - k * sd), races_12m, last_date), sep = ""), by = seq_len(nrow(x))]
  invisible()
}
for (ev in c("AT-1500Metres-W", "AT-1500Metres-M")) for (k in 1:2) show(ev, k)
for (nm in c("Gudaf Tsegay", "Diribe Welteji", "Faith Kipyegon", "Jessica Hull", "Beatrice Chebet"))
  print(ex[athlete_name == nm & event_id == "AT-1500Metres-W", .(athlete_name, best = fmt(a_E), sd_pct = round(100 * sd, 2),
                                                                rank_k0 = rk0, rank_k1 = rk1, rank_k2 = rk2)])
fwrite(ex[, .(athlete_id, athlete_name, event_id, a_E, sd, rk0, rk1, rk2, races_12m, last_date)],
       file.path(OUT, "flags", "confident_rating_today.csv"))
cat("total time", format(Sys.time() - t0), "\n")
