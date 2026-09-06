# IS THE PREDICTED SPREAD CALIBRATED? PIT coverage of the simulated distribution.
#
# The launch gate asks for variance that is better than baseline AND calibrated.
# No backtest arm stores a predicted spread (only median_mark), so this cannot
# be read from any existing arm. This script simulates the full deployed
# distribution for hold-out finals and asks where each actual mark landed in it.
#
#   PIT_i = P(simulated perf_std <= actual perf)   for athlete i in a race
#
# A calibrated spread puts 50% of PITs in [0.25, 0.75] and 90% in [0.05, 0.95],
# with a flat histogram. Too WIDE a spread piles PITs near 0.5 (over-coverage);
# too NARROW pushes them to the edges. Reported pooled, by family, and for the
# race favourite alone -- the Lyles case is a favourite whose spread is too wide.
#
# Two arms from the same abilities so the comparison is spread-only:
#   athlete   the deployed per-athlete sigma (sigma_parts estimator,weight)
#   event     sigma_mode = "event" with the calibration's sigma_within as target
#             (what _run_sigma_event_arm.ps1 tests for medals)
#
# Abilities are estimated ONCE as of FROM on rows strictly before it, and the
# hold-out is the six months after, so staleness is bounded at 6 months. Every
# race scored is a true forecast: nothing in it was seen by the fit.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/pit_coverage_check.R'
# Env: CITIUS_PIT_FROM (default 2024-01-01), CITIUS_PIT_MONTHS (6),
#      CITIUS_PIT_NSIM (3000), CITIUS_PIT_CAL, CITIUS_PIT_EVENTS
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
FROM  <- as.Date(Sys.getenv("CITIUS_PIT_FROM", "2024-01-01"))
TO    <- FROM + 30 * as.integer(Sys.getenv("CITIUS_PIT_MONTHS", "6"))
NSIM  <- as.integer(Sys.getenv("CITIUS_PIT_NSIM", "3000"))
CAL   <- Sys.getenv("CITIUS_PIT_CAL", DEPLOYED$calibration)
EVS   <- Sys.getenv("CITIUS_PIT_EVENTS", paste(c(
  "AT-100Metres-M", "AT-100Metres-W", "AT-200Metres-M", "AT-200Metres-W",
  "AT-400Metres-M", "AT-400Metres-W", "AT-800Metres-M", "AT-800Metres-W",
  "AT-1500Metres-M", "AT-1500Metres-W", "AT-5000Metres-M", "AT-5000Metres-W",
  "AT-110MetresHurdles-M", "AT-100MetresHurdles-W", "AT-400MetresHurdles-M", "AT-400MetresHurdles-W",
  "AT-LongJump-M", "AT-LongJump-W", "AT-HighJump-M", "AT-HighJump-W",
  "AT-ShotPut-M", "AT-ShotPut-W", "AT-JavelinThrow-M", "AT-JavelinThrow-W"), collapse = ","))
EVS   <- trimws(strsplit(EVS, ",")[[1]])
MIN_FIELD <- 5L
# Toggles added 2026-09-06 after the first run showed the centring, not the
# width, is the larger defect in T1 finals: size it by round and with the
# deployed debias off.
DEBIAS <- Sys.getenv("CITIUS_PIT_DEBIAS", "1") == "1"      # apply DEPLOYED$family_debias
# Alternate offsets file (same shape as DEPLOYED$family_debias$file), for
# testing a refit on a recent window against the deployed one.
DEBIAS_FILE <- Sys.getenv("CITIUS_PIT_DEBIAS_FILE", "")
# AGING. The first runs applied the field prior but NOT the aging projection,
# which the backtest (run_meet) and every predict script apply through
# deployed_field(). bias_by_context.R on the backtest's own predictions shows
# T1 finals +1.3..+1.8% OPTIMISTIC in sprint/jump/throw/hurdles while this
# harness without aging showed them centred; aging is the candidate.
AGING  <- Sys.getenv("CITIUS_PIT_AGING", "1") == "1"
# CONTEXT-CONDITIONAL condition_sd: pass the race's catalogue tier and round
# class to simulate_event(); only does anything when the calibration named by
# CITIUS_PIT_CAL carries a condition_sd_context table.
COND_CTX <- Sys.getenv("CITIUS_PIT_COND_CONTEXT", "0") == "1"
ROUND  <- Sys.getenv("CITIUS_PIT_ROUND", "final")            # final | heat | all
ARMS   <- trimws(strsplit(Sys.getenv("CITIUS_PIT_ARMS", "athlete,event"), ",")[[1]])
TAG    <- Sys.getenv("CITIUS_PIT_TAG", "")                   # suffix for the output files
# AS-OF REFIT. The first runs fitted ability once at FROM and scored six months
# of races against it. That reads in-season progression as model pessimism: the
# heats variant put 35% of PITs in the top two deciles, which the as-of backtest
# never shows. "monthly" refits ability at the start of each calendar month and
# scores that month's races against it, so staleness is bounded at ~30 days --
# the same as-of discipline backtest_athletics.R applies per meet, at 1/30 the cost.
REFIT  <- Sys.getenv("CITIUS_PIT_REFIT", "monthly")           # once | monthly
# sigma_parts for the "athlete" arm (default = deployed). "weight" alone gives
# the two-sided sigma_raw instead of the one-sided sigma_rob estimator.
# CITIUS_SIGMA_PSEUDO_N and CITIUS_SIGMA_SCALE are read inside the package.
ATH_PARTS <- trimws(strsplit(Sys.getenv("CITIUS_PIT_SIGMA_PARTS", "estimator,weight"), ",")[[1]])
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

cal <- readRDS(file.path(OUT, CAL))
aging_curve <- if (AGING) deployed_aging(OUT) else NULL
cols <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "round", "tier",
          "meet_tier", "competition_id", "race_key", "wind", "momentum", "indoor",
          "venue_country")
store <- file.path(OUT, "athletics_corpus_store")
have <- intersect(cols, names(arrow::open_dataset(store)))
x <- as.data.table(read_results_store(store, events = EVS, from = FROM - 365 * 12, to = TO,
                                      columns = have))
x <- flag_implausible(x)
x <- x[!is.na(perf) & !is.na(date)]
x[, athlete_id := as.character(athlete_id)]
x[, date := as.Date(date)]
say("%s rows, %d events, %s..%s", format(nrow(x), big.mark = ","), uniqueN(x$event_id),
    format(min(x$date)), format(max(x$date)))

train <- x[date < FROM]
test  <- x[date >= FROM & date <= TO & !is.na(race_key)]
# Finals at the top of the catalogue's tiering, where the medal forecast lives.
if ("meet_tier" %in% names(test)) {
  say("meet_tier in hold-out rows: %s", paste(names(table(test$meet_tier, useNA = "ifany")),
                                             table(test$meet_tier, useNA = "ifany"), sep = "=", collapse = " "))
  test <- test[meet_tier %in% c("T1_elite", "T1")]
}
is_final <- grepl("final", tolower(test$round)) & !grepl("semi|quarter", tolower(test$round))
is_heat  <- grepl("heat|round 1|qualif|prelim", tolower(test$round))
test <- switch(ROUND, final = test[is_final], heat = test[is_heat], all = test)
say("round filter %s | debias %s | aging %s | cond context %s | cal %s | arms %s", ROUND, DEBIAS, AGING, COND_CTX, CAL, paste(ARMS, collapse = ","))
test[, n_field := .N, by = race_key]
test <- test[n_field >= MIN_FIELD]
say("hold-out: %s finals with >= %d entrants (%s athlete-rows)",
    format(uniqueN(test$race_key), big.mark = ","), MIN_FIELD, format(nrow(test), big.mark = ","))
stopifnot(uniqueN(test$race_key) >= 50)

fit <- function(mode, as_of = FROM) {
  parts <- if (mode == "event") c("estimator", "weight", "target") else ATH_PARTS
  ab <- deployed_ability_with(x[date < as_of], mode, parts, as_of = as_of)
  ab[, athlete_id := as.character(athlete_id)]
  ab
}
# Same half-life-by-family stacking as deployed_ability(), with the sigma knobs
# exposed. Debias is applied too (it is deployed), though it cannot move a PIT
# by more than the level it corrects.
deployed_ability_with <- function(past, mode, parts, as_of = FROM) {
  reg_f <- as.data.table(citius_events()[, c("event_id", "family")])
  pf <- merge(as.data.table(past), reg_f, by = "event_id", all.x = TRUE)
  pf[is.na(family), family := ""]
  ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
    fam <- g$family[1]
    hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
    estimate_ability(g[, !"family"], as_of = as_of, half_life = hl, calibration = cal,
                     sigma_parts = parts, sigma_mode = mode)
  }), fill = TRUE)
  if (!DEBIAS) return(ab)
  if (nzchar(DEBIAS_FILE)) {
    o <- readRDS(file.path(OUT, DEBIAS_FILE))
    o$families <- DEPLOYED$family_debias$families; o$file <- DEBIAS_FILE
    return(deployed_debias(ab, o))
  }
  deployed_debias(ab)
}

reg <- as.data.table(citius_events())[, .(event_id, family, orientation)]
score_arm <- function(ab, label, races = unique(test$race_key)) {
  res <- vector("list", length(races)); k <- 0L
  for (rk in races) {
    r <- test[race_key == rk]
    ev <- r$event_id[1]
    ent <- ab[event_id == ev & athlete_id %in% r$athlete_id]
    if (nrow(ent) < MIN_FIELD) next
    ages <- if (AGING && "age" %in% names(r)) unique(r[!is.na(age), .(athlete_id, age_now = as.numeric(age))], by = "athlete_id") else NULL
    ent <- deployed_field(ent, aging = aging_curve, ages = ages)   # prior, then aging, as deployed
    ctx <- if (COND_CTX) list(meet_tier = r$meet_tier[1], round_class = .round_class(r$round[1])) else NULL
    sim <- simulate_event(ent, n_sims = NSIM, calibration = cal, seed = 1L, context = ctx)
    p <- if (!is.null(sim$perf_std)) sim$perf_std else sim$perf
    act <- r[match(colnames(p), athlete_id), perf]
    pit <- vapply(seq_len(ncol(p)), function(j) mean(p[, j] <= act[j], na.rm = TRUE), numeric(1))
    fav <- which.max(ent$ability[match(colnames(p), ent$athlete_id)])
    k <- k + 1L
    nn <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)[1]
    ix <- match(colnames(p), ent$athlete_id)
    med <- apply(p, 2L, function(v) stats::median(v[is.finite(v)]))
    res[[k]] <- data.table(arm = label, race_key = rk, event_id = ev,
                           athlete_id = colnames(p), pit = pit, is_fav = seq_along(pit) == fav,
                           # sd over FINITE draws: a simulated foul is -Inf, which
                           # would NA the whole column and every ratio built on it.
                           pred_sd = apply(p, 2L, function(v) stats::sd(v[is.finite(v)])), sigma = ent$sigma[ix],
                           # The terms the simulator adds, so observed residual variance
                           # can be set against each one (see the decomposition below).
                           ability_se = if ("ability_se" %in% names(ent)) ent$ability_se[ix] else NA_real_,
                           cond_sd    = nn(sim$settings$condition_sd),
                           form_sd    = nn(sim$settings$form_sd),
                           tail_df    = nn(sim$settings$df),
                           resid      = act - med)
  }
  rbindlist(res)
}

test[, month := as.Date(format(date, "%Y-%m-01"))]
cuts <- if (REFIT == "monthly") sort(unique(test$month)) else FROM
say("as-of refit: %s (%d fit date%s) | athlete sigma_parts %s | pseudo-n %s | scale %s", REFIT, length(cuts),
    if (length(cuts) == 1) "" else "s", paste(ATH_PARTS, collapse = ","),
    Sys.getenv("CITIUS_SIGMA_PSEUDO_N", "default"), Sys.getenv("CITIUS_SIGMA_SCALE", "1"))
pit <- rbindlist(lapply(ARMS, function(a) {
  rbindlist(lapply(cuts, function(cut) {
    races <- if (REFIT == "monthly") unique(test[month == cut]$race_key) else unique(test$race_key)
    say("arm %s | as of %s | %d races", a, format(cut), length(races))
    score_arm(fit(a, as_of = cut), a, races)
  }))
}))
pit <- merge(pit, reg, by = "event_id")
say("%s PITs scored per arm", format(nrow(pit[arm == "athlete"]), big.mark = ","))

cov <- function(d) d[, .(n = .N,
                         cover50 = mean(pit >= 0.25 & pit <= 0.75),
                         cover90 = mean(pit >= 0.05 & pit <= 0.95),
                         below05 = mean(pit < 0.05), above95 = mean(pit > 0.95),
                         pit_sd = sd(pit)), by = arm]
cat("\n=== pooled coverage (targets: cover50 0.50, cover90 0.90, pit_sd 0.289) ===\n")
cat("cover50 > 0.50 means the spread is too WIDE; < 0.50 too narrow.\n")
print(cov(pit)[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat("\n=== favourites only ===\n")
print(cov(pit[is_fav == TRUE])[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat("\n=== by family ===\n")
byf <- pit[, .(n = .N, cover50 = round(mean(pit >= 0.25 & pit <= 0.75), 3),
               cover90 = round(mean(pit >= 0.05 & pit <= 0.95), 3),
               below05 = round(mean(pit < 0.05), 3), above95 = round(mean(pit > 0.95), 3)),
           by = .(family, arm)]
print(dcast(byf, family ~ arm, value.var = c("cover50", "cover90", "n")))
cat("\n=== PIT histogram, deciles (flat = calibrated) ===\n")
h <- pit[, .(share = round(as.numeric(table(cut(pit, seq(0, 1, 0.1), include.lowest = TRUE))) / .N, 3)), by = arm]
h[, decile := rep(1:10, times = .N / 10)]
print(dcast(h, decile ~ arm, value.var = "share"))
cat("\nNote on direction: PIT is on perf (higher = better performance), so below05 =\n")
cat("actual much WORSE than predicted, above95 = actual much BETTER than predicted.\n")

# --- which term is oversized? -----------------------------------------------
# Observed residual variance (actual - simulated median), split into the part
# shared by a race (race-mean residual) and the rest, against the simulator's
# own terms: sigma^2 (scaled by the t-tail), ability_se^2, form_sd^2 and
# cond_sd^2. Ratios near 1 mean that term is the right size on this population.
pit[, race_mean_resid := mean(resid), by = .(arm, race_key)]
pit[, indiv_resid := resid - race_mean_resid]
tvar <- function(df) ifelse(is.finite(df) & df > 2, df / (df - 2), 1)
dec <- pit[, .(n = .N,
               mean_resid     = mean(resid),
               obs_total_var  = var(resid),
               obs_shared_var = var(unique(.SD, by = "race_key")$race_mean_resid),
               obs_indiv_var  = var(indiv_resid),
               sigma2_t       = mean(sigma^2 * tvar(tail_df)),
               se2            = mean(ability_se^2, na.rm = TRUE),
               form2          = mean(form_sd^2),
               cond2          = mean(cond_sd^2),
               pred_var       = mean(pred_sd^2)),
           by = .(arm, family)]
dec[, `:=`(bias_pct     = round(-100 * mean_resid, 3),
           ratio_total  = round(obs_total_var / pred_var, 3),
           ratio_shared = round(obs_shared_var / cond2, 3),
           ratio_indiv  = round(obs_indiv_var / (sigma2_t + se2 + form2), 3),
           share_cond   = round(cond2 / pred_var, 2),
           share_sigma  = round(sigma2_t / pred_var, 2),
           share_se     = round(se2 / pred_var, 2))]
cat("\n=== variance decomposition: observed / simulated, by family ===\n")
cat("bias_pct: predicted median minus actual, in % of mark; + = optimistic (same sign as bias_by_context.R).\n")
cat("ratio_total: obs resid var / simulated var. ratio_shared: race-mean resid var / cond_sd^2.\n")
cat("ratio_indiv: within-race resid var / (sigma^2 * t-inflation + ability_se^2 + form_sd^2).\n")
cat("share_*: fraction of the simulated variance each term contributes.\n\n")
print(dec[, .(arm, family, n, bias_pct, ratio_total, ratio_shared, ratio_indiv, share_cond, share_sigma, share_se)])
fwrite(dec, file.path(OUT, paste0("pit_variance_decomp", TAG, ".csv")))

fwrite(pit, file.path(OUT, paste0("pit_coverage_rows", TAG, ".csv")))
fwrite(byf, file.path(OUT, paste0("pit_coverage_by_family", TAG, ".csv")))
say("wrote pit_coverage_rows%s.csv, pit_coverage_by_family%s.csv (refit %s, round %s, debias %s)", TAG, TAG, REFIT, ROUND, DEBIAS)
