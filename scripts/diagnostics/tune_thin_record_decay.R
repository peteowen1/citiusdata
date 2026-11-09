# Availability decay on top of version E (Pete 2026-11-10): after a grace period away,
# slide the rating towards a FLOOR = the event's R1 median, halving the gap every
# `half` days. Grid: grace {180,270,365,540} x half {90,180,365,730} x clock {event, any}.
#
#   E      = a*R + (1-a)*L   (tuned: recent half-life x2, kappa_R x0.5, own record 365 d)
#   gap    = days from the athlete's last race (in the event, or in ANY event) to the cut
#   b      = 0 if gap <= grace, else 1 - 0.5^((gap - grace) / half)
#   rating = (1 - b)*E + b*F,   F = median R1 perf in the event over the 730 days before the cut
#
# WHAT CAN AND CANNOT BE MEASURED. Backtests only score athletes who race again, so the
# decay's BENEFIT (demoting athletes who never come back) is invisible. Its COST is
# measurable: athletes who DO come back after a long gap get decayed first. Table 1 =
# that cost on R1 racers (2025 cuts held out; every config level-corrected on 2024).
# Table 2 = what each config does to TODAY's ratings page. Pete picks the trade-off.
# Reads data/flags/tune_E_cache (8 cuts) and the corpus. Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A")
GRACE <- c(180, 270, 365, 540); HALF <- c(90, 180, 365, 730)
HR <- 2; MR <- 0.5; HL <- 365

co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
co[, athlete_id := as.character(athlete_id)]
# last race in ANY event, per athlete, from the whole corpus (dates only: small)
anyd <- co[!is.na(date), .(athlete_id, date)]
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
h[, r1 := toupper(trimws(race_code)) %in% R1]
cat("middle-distance rows", nrow(h), "; R1 rows", h[r1 == TRUE, .N], "\n")

fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE)
stopifnot(length(fs) == 8, all(c("R_2", "wR_2", "kR_2", "long_365") %in% names(r)))
out <- list()
for (cu in sort(unique(r$cut))) {
  cu <- as.Date(cu, origin = "1970-01-01"); d <- r[cut == cu]
  fl <- h[r1 == TRUE & date < cu & date >= cu - 730, .(floor = median(perf), n_floor = .N), by = event_id]
  d <- fl[d, on = "event_id"]
  la <- anyd[date < cu, .(last_any = max(date)), by = athlete_id]
  d <- la[d, on = "athlete_id"]
  nx <- h[r1 == TRUE & date >= cu & date < cu + 365, .(t_mean = mean(perf), t_n = .N), by = .(athlete_id, event_id)]
  d <- nx[d, on = c("athlete_id", "event_id")]
  out[[length(out) + 1]] <- d[!is.na(t_n)]          # scored = raced R1 in the next 12 months
}
s <- rbindlist(out, fill = TRUE)
s <- s[is.finite(R_2) & is.finite(long_365) & is.finite(floor)]
s[, a := wR_2 / (wR_2 + MR * kR_2)][, E := a * R_2 + (1 - a) * long_365]
s <- s[is.finite(E)]
s[, `:=`(gap_event = as.numeric(cut - last_date), gap_any = as.numeric(cut - last_any))]
s[, fit_set := year(cut) == 2024]
cat(sprintf("scored R1 racers: fit 2024 %d, verdict 2025 %d; away 180d+ at the cut (event clock): %d / %d\n",
            s[fit_set == TRUE, .N], s[fit_set == FALSE, .N], s[fit_set == TRUE & gap_event > 180, .N], s[fit_set == FALSE & gap_event > 180, .N]))

decay <- function(E, F, gap, grace, half) { b <- ifelse(gap > grace, 1 - 0.5^((gap - grace) / half), 0); (1 - b) * E + b * F }
mae <- function(p, y) 100 * mean(abs(y - p))
lvl <- function(x, p) median(x$t_mean - p)          # level shift fitted on 2024
fit <- s[fit_set == TRUE]; V <- s[fit_set == FALSE]
c0E <- lvl(fit, fit$E); V[, pE := E + c0E]
grid <- CJ(clock = c("event", "any"), grace = GRACE, half = HALF)
boot <- function(x, p1, p2) { g <- x[, .(d = sum(abs(t_mean - get(p1)) - abs(t_mean - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:1000, function(k) { q <- g[sample(.N, .N, TRUE)]; sum(q$d) / sum(q$n) }, 0); 100 * quantile(b, c(.025, .975)) }
res <- grid[, {
  gcol <- paste0("gap_", clock)
  pf <- decay(fit$E, fit$floor, fit[[gcol]], grace, half); c0 <- lvl(fit, pf)
  V[, pD := decay(E, floor, get(gcol), grace, half) + c0]
  ret <- V[get(gcol) > 180]
  ci <- boot(ret, "pD", "pE")
  .(n_ret = nrow(ret), mae_ret_E = mae(ret$pE, ret$t_mean), mae_ret_D = mae(ret$pD, ret$t_mean),
    cost_lo = ci[1], cost_hi = ci[2], mae_all_E = mae(V$pE, V$t_mean), mae_all_D = mae(V$pD, V$t_mean),
    decayed_pct = 100 * mean(V[[gcol]] > grace))
}, by = .(clock, grace, half)]
cat("\nTABLE 1 -- COST on R1 racers, 2025 cuts. MAE % vs their mean R1 mark next 12 months (lower is better).\n")
cat("ret = athletes away 180+ days at the cut who raced R1 again; cost CI = MAE(decay) - MAE(E), > 0 means the decay hurts them.\n")
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

# TABLE 2 -- today's page
ex <- fread(file.path(OUT, "flags", "thin_record_E_examples.csv"), colClasses = list(character = "athlete_id"))
AS_OF <- max(h$date)
flt <- h[r1 == TRUE & date < AS_OF & date >= AS_OF - 730, .(floor = median(perf)), by = event_id]
la <- anyd[, .(last_any = max(date)), by = athlete_id]
ex <- flt[ex, on = "event_id"]; ex <- la[ex, on = "athlete_id"]
ex[, `:=`(gap_event = as.numeric(AS_OF - as.Date(last_date)), gap_any = as.numeric(AS_OF - last_any))]
fmt <- function(p) { x <- exp(-p); sprintf("%d:%05.2f", as.integer(x %/% 60), x %% 60) }
cat("\nFloors today (R1 median, last 730 days):\n"); print(flt[, .(event_id, floor = fmt(floor))])
pg <- grid[, {
  gcol <- paste0("gap_", clock)
  ex[, rD := decay(a_E, floor, get(gcol), grace, half)]
  ex[, rk := frank(-rD, ties.method = "min"), by = event_id]
  top <- ex[rk <= 10]
  .(top10_absent12m = top[races_12m == 0, .N],
    tsegay = fmt(ex[athlete_name == "Gudaf Tsegay" & event_id == "AT-1500Metres-W", rD]),
    tsegay_rank = ex[athlete_name == "Gudaf Tsegay" & event_id == "AT-1500Metres-W", rk],
    welteji = fmt(ex[athlete_name == "Diribe Welteji" & event_id == "AT-1500Metres-W", rD]),
    welteji_rank = ex[athlete_name == "Diribe Welteji" & event_id == "AT-1500Metres-W", rk],
    kipyegon_rank = ex[athlete_name == "Faith Kipyegon" & event_id == "AT-1500Metres-W", rk])
}, by = .(clock, grace, half)]
cat(sprintf("\nTABLE 2 -- TODAY's page (as of %s), six events. Version E with no decay: %d of 60 top-10 slots unraced in 12 months.\n",
            format(AS_OF), { ex[, rk0 := frank(-a_E, ties.method = "min"), by = event_id]; ex[rk0 <= 10 & races_12m == 0, .N] }))
print(pg)
fwrite(res, file.path(OUT, "flags", "tune_thin_record_decay_cost.csv"))
fwrite(pg, file.path(OUT, "flags", "tune_thin_record_decay_page.csv"))
cat("total time", format(Sys.time() - t0), "\n")
