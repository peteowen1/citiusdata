# Fit the time-since-last-result offsets used by version C of the thin-record
# change (citius::apply_gap_offsets, Pete chose C 2026-11-09).
# Input: the backtest rows written by diagnostics/backtest_thin_record_prior.R.
# Fitted on the 2024 cuts ONLY (the 2025 cuts were the held-out verdict), against
# the own-record ability (ability_B), because C applies the offsets after B.
# Output: data/gap_offsets_middle_distance.csv. Scope: the 6 events measured
# (800/1500/5000 m, both sexes); nothing outside them was tested.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
r <- fread(file.path(OUT, "flags", "backtest_thin_record_prior.csv"))
cat("backtest rows", nrow(r), " cuts", paste(sort(unique(as.character(r$cut))), collapse = " "), "\n")
fit <- r[year(as.Date(cut)) == 2024]
stopifnot(nrow(fit) > 10000, uniqueN(fit$cut) == 4)
off <- fit_gap_offsets(fit[, .(perf, ability = ability_B, gap_days)], min_n = 500L)
off[, `:=`(fitted_on = "2024 cuts of backtest_thin_record_prior.csv", fitted_at = format(Sys.Date()),
           events = "AT-800Metres-M|AT-800Metres-W|AT-1500Metres-M|AT-1500Metres-W|AT-5000Metres-M|AT-5000Metres-W")]
print(off[, .(gap, n, offset_pct = round(100 * offset, 3))])
fwrite(off, file.path(OUT, "gap_offsets_middle_distance.csv"))
cat("wrote", file.path(OUT, "gap_offsets_middle_distance.csv"), "\n")
