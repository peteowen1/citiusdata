# WHICH per-athlete sigma estimator actually predicts who is consistent?
#
# THE PROBLEM (split_spread_shared_vs_individual.R, 2026-09-06, two hold-out
# windows). The deployed sigma varies a lot between athletes (cv 0.33-0.40
# within an event) but Spearman(sigma, that athlete's own hold-out scatter) is
# 0.05. It is spread that is noise: a right-sized-ish term handed to the wrong
# people. Placings are driven by spread, so this is a medal-probability defect,
# not just a marks one -- and it is why Noah Lyles was given a 5% chance of
# beating the world record.
#
# THE TEST. Fit each candidate on history BEFORE `FROM`, score it on what the
# athlete then did. The target is the athlete's race-demeaned scatter in the
# hold-out (a shared race shock is removed, because that is condition_sd's job,
# not sigma's). Two scores:
#
#   spearman    -- does the candidate rank athletes by consistency? (allocation)
#   logscore_k  -- Gaussian log density of hold-out residuals under N(0, k*cand),
#                  with ONE scale k fitted per event per candidate so every
#                  candidate is judged at its best level. Pure allocation skill;
#                  the level question is separate and reported as k itself.
#   logscore_1  -- the same at k = 1: allocation AND level together.
#
# Candidates fit on the training rows of the last 3 years before FROM:
#   dep       the deployed sigma from estimate_ability() with the calibration
#   dep_raw   its sigma_raw (weighted two-sided sd), dep_rob its sigma_rob
#             (weighted UPPER-side sd around the median -- the estimator in use)
#   sd_raw    unweighted two-sided sd of perf
#   sd_dm     unweighted sd of the race-demeaned residual (same construction
#             as the target, so it is the "right quantity" candidate)
#   upper_dm  upper-side sd of the race-demeaned residual (isolates whether the
#             one-sidedness is the problem)
#   ev_const  the event median of sd_dm -- what sigma_mode = "event" gives
#   eb_m      sd_dm shrunk toward ev_const with pseudo-n m in {2, 5, 10, 20, 40}
#
# Usage:
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/sigma_estimator_shootout.R'
#   (arrow: must run under PowerShell, not Git Bash)
# Env:
#   CITIUS_SHOOT_FROM   hold-out start (default 2024-01-01)
#   CITIUS_SHOOT_CAL    calibration (default the deployed one)
#   CITIUS_SHOOT_EVENTS comma list of event_ids (default: 24 T1-relevant events)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
OUT  <- here::here("citiusdata", "data")
FROM <- as.Date(Sys.getenv("CITIUS_SHOOT_FROM", "2024-01-01"))
CAL  <- Sys.getenv("CITIUS_SHOOT_CAL", "calibration_corpus_wac_coast_0904.rds")
EVS  <- Sys.getenv("CITIUS_SHOOT_EVENTS", paste(c(
  "AT-100Metres-M", "AT-100Metres-W", "AT-200Metres-M", "AT-200Metres-W",
  "AT-400Metres-M", "AT-400Metres-W", "AT-800Metres-M", "AT-800Metres-W",
  "AT-1500Metres-M", "AT-1500Metres-W", "AT-5000Metres-M", "AT-5000Metres-W",
  "AT-110MetresHurdles-M", "AT-100MetresHurdles-W", "AT-400MetresHurdles-M", "AT-400MetresHurdles-W",
  "AT-LongJump-M", "AT-LongJump-W", "AT-HighJump-M", "AT-HighJump-W",
  "AT-ShotPut-M", "AT-ShotPut-W", "AT-JavelinThrow-M", "AT-JavelinThrow-W"), collapse = ","))
EVS  <- trimws(strsplit(EVS, ",")[[1]])
TRAIN_YEARS <- 3
MIN_TRAIN <- 6L     # rows per athlete-event to fit a candidate
MIN_TEST  <- 4L     # rows per athlete-event to score it
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

cal <- readRDS(file.path(OUT, CAL))
cols <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "round", "tier",
          "meet_tier", "competition_id", "race_key", "wind", "momentum", "indoor",
          "venue_country")
store <- file.path(OUT, "athletics_corpus_store")
have <- intersect(cols, names(arrow::open_dataset(store)))
x <- as.data.table(read_results_store(store, events = EVS, from = FROM - 365 * 12,
                                      columns = have))
x <- flag_implausible(x)
x <- x[!is.na(perf) & !is.na(race_key) & !is.na(date)]
x[, athlete_id := as.character(athlete_id)]
x[, date := as.Date(date)]
say("%s rows, %d events, %s to %s", format(nrow(x), big.mark = ","), uniqueN(x$event_id),
    format(min(x$date)), format(max(x$date)))
stopifnot(nrow(x) > 1e5)

train <- x[date < FROM]
test  <- x[date >= FROM]

# --- deployed ability + sigma, as-of FROM, on training rows only -------------
ab <- as.data.table(estimate_ability(train, as_of = FROM, calibration = cal))
ab <- ab[, .(athlete_id = as.character(athlete_id), event_id, ability,
             dep = sigma, dep_raw = sigma_raw, dep_rob = sigma_rob, n_dep = n)]
say("%s athlete-events with a deployed estimate", format(nrow(ab), big.mark = ","))

# --- race-demeaned residuals, both sides -------------------------------------
# Residual against the athlete's own level. On the TRAIN side the level is the
# athlete's own 3-year mean (not the deployed ability, which is fit on the same
# rows and would make the residual circular with the deployed sigma). On the
# TEST side it is the deployed ability as of FROM -- a true forecast residual.
demean <- function(d) {
  d[, n_r := .N, by = race_key]
  d[, race_mean := if (.N >= 3L) mean(resid) else 0, by = race_key]
  d[, indiv := resid - race_mean]
  d[]
}
tr <- train[date >= FROM - 365 * TRAIN_YEARS]
tr[, lvl := mean(perf), by = .(athlete_id, event_id)]
tr[, resid := perf - lvl]
tr <- demean(tr)

te <- merge(test, ab[, .(athlete_id, event_id, ability)], by = c("athlete_id", "event_id"))
te[, resid := perf - ability]
te <- demean(te)
te <- te[n_r >= 3L]                       # a demeaned residual needs a field
say("test: %s rows in %s races", format(nrow(te), big.mark = ","), format(uniqueN(te$race_key), big.mark = ","))

upper_sd <- function(v) {
  med <- median(v); dv <- v - med; up <- dv > 0
  if (sum(up) < 2L) return(NA_real_)
  sqrt(mean(dv[up]^2))
}
cand <- tr[, .(n_tr = .N,
               sd_raw   = sd(perf),
               sd_dm    = sd(indiv),
               upper_dm = upper_sd(indiv)),
           by = .(athlete_id, event_id)][n_tr >= MIN_TRAIN]
cand <- merge(cand, ab[, .(athlete_id, event_id, dep, dep_raw, dep_rob)],
              by = c("athlete_id", "event_id"))
cand[, ev_const := median(sd_dm, na.rm = TRUE), by = event_id]
for (m in c(2, 5, 10, 20, 40)) {
  cand[, paste0("eb_", m) := sqrt((n_tr * sd_dm^2 + m * ev_const^2) / (n_tr + m))]
}
CANDS <- c("dep", "dep_raw", "dep_rob", "sd_raw", "sd_dm", "upper_dm", "ev_const",
           paste0("eb_", c(2, 5, 10, 20, 40)))

# --- hold-out target per athlete-event ---------------------------------------
tgt <- te[, .(n_te = .N, sd_test = sd(indiv)), by = .(athlete_id, event_id)][n_te >= MIN_TEST]
sc <- merge(cand, tgt, by = c("athlete_id", "event_id"))
say("%s athlete-events scoreable (>= %d train, >= %d test rows)",
    format(nrow(sc), big.mark = ","), MIN_TRAIN, MIN_TEST)
stopifnot(nrow(sc) > 500)

# --- score 1: allocation (Spearman) per event --------------------------------
sp <- rbindlist(lapply(CANDS, function(cn) {
  sc[is.finite(get(cn)) & get(cn) > 0,
     .(cand = cn, n = .N, rho = cor(sd_test, get(cn), method = "spearman")),
     by = event_id]
}))

# --- score 2: Gaussian log score on hold-out rows ----------------------------
# Join each row's candidate sigmas, fit k per event per candidate (k^2 = the mean
# standardised squared residual, the MLE scale), report mean log density with
# and without it.
rows <- merge(te[, .(athlete_id, event_id, indiv)],
              sc[, c("athlete_id", "event_id", CANDS), with = FALSE],
              by = c("athlete_id", "event_id"))
ls <- rbindlist(lapply(CANDS, function(cn) {
  r <- rows[is.finite(get(cn)) & get(cn) > 0]
  r[, s := get(cn)]
  r[, k := sqrt(mean((indiv / s)^2)), by = event_id]
  r[, .(cand = cn, n_rows = .N,
        logscore_1 = mean(dnorm(indiv, 0, s, log = TRUE)),
        logscore_k = mean(dnorm(indiv, 0, k * s, log = TRUE)),
        k_median   = median(k)),
    by = event_id]
}))

summ <- merge(
  sp[, .(spearman_med = median(rho), spearman_min = min(rho), events = .N), by = cand],
  ls[, .(logscore_1 = mean(logscore_1), logscore_k = mean(logscore_k),
         k_med = median(k_median), rows = sum(n_rows)), by = cand],
  by = "cand")
setorder(summ, -logscore_k)

cat("\n=== candidate sigma estimators, scored on the", format(FROM), "hold-out ===\n")
cat("spearman: rank agreement with each athlete's hold-out scatter (allocation)\n")
cat("logscore_k: mean Gaussian log density after one per-event scale k (allocation only)\n")
cat("logscore_1: the same at k=1 (allocation + level). k_med: the scale each needed.\n\n")
print(summ[, .(cand, events, spearman_med = round(spearman_med, 3),
               spearman_min = round(spearman_min, 3),
               logscore_k = round(logscore_k, 4), logscore_1 = round(logscore_1, 4),
               k_med = round(k_med, 3))])

cat("\n=== per event, best candidate by logscore_k vs deployed ===\n")
best <- ls[, .SD[which.max(logscore_k)], by = event_id][, .(event_id, best = cand, best_ls = logscore_k)]
depl <- ls[cand == "dep", .(event_id, dep_ls = logscore_k, dep_k = k_median)]
pe <- merge(merge(best, depl, by = "event_id"),
            sp[cand == "dep", .(event_id, dep_rho = rho)], by = "event_id")
pe <- merge(pe, sp[cand == "sd_dm", .(event_id, sd_dm_rho = rho)], by = "event_id")
print(pe[order(-(best_ls - dep_ls)), .(event_id, best, gain = round(best_ls - dep_ls, 4),
                                       dep_k = round(dep_k, 3), dep_rho = round(dep_rho, 3),
                                       sd_dm_rho = round(sd_dm_rho, 3))])

fwrite(summ, file.path(OUT, "sigma_shootout_summary.csv"))
fwrite(sp,   file.path(OUT, "sigma_shootout_spearman_by_event.csv"))
fwrite(ls,   file.path(OUT, "sigma_shootout_logscore_by_event.csv"))
fwrite(sc,   file.path(OUT, "sigma_shootout_athlete_events.csv"))
say("wrote sigma_shootout_*.csv")
