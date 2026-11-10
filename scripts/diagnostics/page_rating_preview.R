# Ratings-page preview (Pete 2026-11-10): continuous, no cut-offs.
#   page = E - k * sqrt(sd_fit^2 + sd_avail^2)
#   E        = version E best estimate (forecasts use this, unchanged)
#   sd_fit   = fitted uncertainty of E from evidence and time away (confident_rating.R,
#              calibrated on 2025: predicted 2.64% vs observed 2.71%)
#   sd_avail = sigma_between * (1 - 0.5^(days since last race in ANY event / AVAIL_HL))
#              -- availability as shrinkage: 0 just after a race, half the event's
#              between-athlete spread after AVAIL_HL days, the full spread in the limit.
# Choices (not measurable, see docs/reference/backtests.md rules 2-3): k, AVAIL_HL.
# Shows all six events' top 10 for k = 1, 1.5, 2. Changes nothing published.
# Arrow via PowerShell.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
AVAIL_HL <- as.numeric(Sys.getenv("AVAIL_HL", "365")); KS <- c(1, 1.5, 2)
EV <- DEPLOYED$thin_blend$events
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[, athlete_id := as.character(athlete_id)]
la <- co[!is.na(date), .(last_any = max(date)), by = .(id0 = sub("^0+", "", athlete_id))]
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date)
sb <- .deployed_ability_raw(h, AS_OF, deployed_calibration(OUT), deployed_event_params())[
  , .(sigma_between = median(sigma_between, na.rm = TRUE)), by = event_id]
mod <- fread(file.path(OUT, "flags", "confident_rating_sd_model.csv")); b <- setNames(mod$value, mod$term)
ex <- fread(file.path(OUT, "flags", "thin_record_E_examples.csv"), colClasses = list(character = "athlete_id"))
ex <- ex[is.finite(a_E) & is.finite(wR) & wR > 0]
ex[, id0 := sub("^0+", "", athlete_id)]; ex <- la[ex, on = "id0"]; ex <- sb[ex, on = "event_id"]
ex[, gap := pmax(as.numeric(AS_OF - as.Date(last_date)), 0)]
ex[, gap_any := pmax(as.numeric(AS_OF - last_any), 0)]
ex[, sd_fit := sqrt(exp(b[["(Intercept)"]] + b[["log1p(gap/30)"]] * log1p(gap / 30) + b[["log(wR_2)"]] * log(wR))) * b[["scale"]]]
ex[, sd_avail := sigma_between * (1 - 0.5^(gap_any / AVAIL_HL))]
ex[, sd_page := sqrt(sd_fit^2 + sd_avail^2)]
fmt <- function(p) { x <- exp(-p); ifelse(x >= 600, sprintf("%d:%05.2f", as.integer(x %/% 60), x %% 60), sprintf("%d:%05.2f", as.integer(x %/% 60), x %% 60)) }
ex[, rk0 := frank(-a_E, ties.method = "min"), by = event_id]
for (k in KS) ex[, (paste0("rk", k)) := frank(-(a_E - k * sd_page), ties.method = "min"), by = event_id]
cat(sprintf("as of %s; availability half-life %g days\n", format(AS_OF), AVAIL_HL))
print(sb[, .(event_id, sigma_between_pct = round(100 * sigma_between, 2))])
cat("\nTop-10 slots (of 60) held by athletes with NO race in ANY event for 12 months:\n")
cat(sprintf("  best estimate only: %d\n", ex[rk0 <= 10 & gap_any > 365, .N]))
for (k in KS) cat(sprintf("  k = %g: %d\n", k, ex[get(paste0("rk", k)) <= 10 & gap_any > 365, .N]))
for (nm in c("Gudaf Tsegay", "Beatrice Chebet", "Diribe Welteji", "Faith Kipyegon", "Jessica Hull", "George Mills", "Soufiane EL Bakkali"))
  print(ex[athlete_name == nm, .(athlete_name, event = sub("AT-|Metres", "", event_id), best = fmt(a_E), days_since_any = gap_any,
                               sd_fit = round(100 * sd_fit, 2), sd_avail = round(100 * sd_avail, 2),
                               rank_best = rk0, rank_k1 = rk1, rank_k1.5 = rk1.5, rank_k2 = rk2)])
for (ev in EV) {
  x <- ex[event_id == ev & rk1.5 <= 10][order(rk1.5)]
  cat(sprintf("\n%s, k = 1.5 (rank | rank by best estimate | athlete | best | days since any race)\n", ev))
  x[, cat(sprintf("%3d | %4d | %-26s | %s | %d\n", rk1.5, rk0, substr(athlete_name, 1, 26), fmt(a_E), gap_any), sep = ""), by = seq_len(nrow(x))]
}
fwrite(ex[, .(athlete_id, athlete_name, event_id, a_E, sd_fit, sd_avail, sd_page, gap_any, rk0, rk1, rk1.5, rk2)],
       file.path(OUT, "flags", "page_rating_preview.csv"))
