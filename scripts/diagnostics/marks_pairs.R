# MARKS PAIRS: build the small table a parameter sweep is arithmetic on.
#
# THE IDEA (Pete, 2026-09-07). Every step that turns history into a predicted
# mark is either an additive shift on the log scale or a weight, so a parameter
# change is column arithmetic. There is no need to refit the corpus per config,
# and no need for a month loop: each test row has its own as-of date, so the
# age of each of that athlete's prior marks is fixed when the table is built.
#
# THE TABLE. One row per (test athlete-race, one of that athlete's prior marks
# in the same event) -- about 600k rows rather than 5.9M x 20 months. Carried:
#   perf        the mark, already adjusted to a final-equivalent footing
#   age_days    as-of date minus the mark's date
#   w_static    the parameter-free half of result_weight (round x tier precision)
#   keep        survives the tactical trim (parameter-free: it ranks adjusted
#               perf within the athlete's own pre-cut set)
#
# Then ability = sum(w*perf)/sum(w) with w = w_static * 0.5^(age/half_life),
# shrunk toward the event's population mean. That last quantity is the only
# thing needing athletes outside the test set, so it is RECOVERED from the
# cached real run rather than recomputed: kappa = s*w/(1-s) gives
# sigma_between = sigma/sqrt(kappa), and prior_mu falls out of
# ability = (1-s)*raw + s*prior_mu once raw is known at the deployed config.
#
# WHY THE FIRST FAST SCORER FAILED ITS GATE (mean -0.87% of a mark): it omitted
# the tactical trim. Dropping the worst quarter of a tactical athlete's marks
# raises their ability, so leaving it out biases every such prediction low.
# That is exactly the sign and size observed. The trim is here.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_pairs.R'
# Env: CITIUS_LAB_CACHE (default marks_lab_cache)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
cal  <- readRDS(file.path(OUT, DEPLOYED$calibration))
test <- readRDS(file.path(CACHE, "test.rds"))
adj  <- readRDS(file.path(CACHE, "adj.rds"))
months <- sort(unique(test$month))
sig <- rbindlist(lapply(months, function(m)
  readRDS(file.path(CACHE, "sigma", paste0(format(m), ".rds")))[, month := m][]), fill = TRUE)
# THE TACTICAL FLAG IS A CALIBRATION OUTPUT, NOT THE REGISTRY'S (found
# 2026-09-07 while chasing a failed gate). estimate_ability() takes the
# registry flag and then OVERRIDES it wherever the calibration has a fitted
# tactical_index below -0.5 on a calibrated event: races that skew slow are
# evidence that times decouple from ability. Reading the registry here made the
# trim miss events the model trims -- Hammer Throw W among them, where it drops
# exactly 29 of one athlete's 116 marks. Replicate the override or the trim
# fires on a different set of events than the model's.
reg <- as.data.table(citius_events())[, .(event_id, tactical)]
ti <- as.data.table(cal$events)[, .(event_id, tactical_index,
                                    calibrated = if ("calibrated" %in% names(cal$events)) calibrated else NA)]
reg <- merge(reg, ti, by = "event_id", all.x = TRUE)
reg[calibrated %in% TRUE & is.finite(tactical_index), tactical := tactical_index < -0.5]
reg <- reg[, .(event_id, tactical)]
say("tactical after the calibration override: %d of %d events (registry said %d)",
    sum(reg$tactical, na.rm = TRUE), nrow(reg),
    sum(as.data.table(citius_events())$tactical, na.rm = TRUE))
say("test %s rows | adjusted history %s rows | %d months",
    format(nrow(test), big.mark = ","), format(nrow(adj), big.mark = ","), length(months))

# --- the pair table ----------------------------------------------------------
# One athlete-event-month is one prediction; join its whole prior history once.
keys <- unique(test[, .(athlete_id, event_id, month)])
keys <- merge(keys, sig[, .(athlete_id, event_id, month, sigma, ref_ability, ref_shrinkage, ref_w)],
              by = c("athlete_id", "event_id", "month"))
keys[, pid := .I]
say("predictions with a reference estimate: %s", format(nrow(keys), big.mark = ","))
t0 <- Sys.time()
pairs <- adj[keys[, .(athlete_id, event_id, month, pid)], on = .(athlete_id, event_id),
             allow.cartesian = TRUE][date < month]
pairs <- merge(pairs, reg, by = "event_id", all.x = TRUE)
pairs[, age_days := as.numeric(month - date)]
say("pair table: %s rows in %.0fs (%.1f prior marks per prediction)",
    format(nrow(pairs), big.mark = ","), as.numeric(difftime(Sys.time(), t0, units = "secs")),
    nrow(pairs) / nrow(keys))

# --- the tactical trim ------------------------------------------------------
# The trim fraction is itself a parameter worth sweeping (0.25 today, and the
# calibration now marks 55 of 122 events tactical including throws), so the
# table keeps every row and the RANK, and the trim is applied at score time.
pairs[, tactical := tactical %in% TRUE]
pairs[tactical == TRUE, grp_n := .N, by = pid]
pairs[tactical == TRUE & grp_n >= 4L, rk := frank(perf, ties.method = "first"), by = pid]
say("tactical rows: %s of %s (trim at 0.25 would drop %s)",
    format(sum(pairs$tactical), big.mark = ","), format(nrow(pairs), big.mark = ","),
    format(sum(pairs$tactical & !is.na(pairs$rk) & pairs$rk <= floor(pairs$grp_n * 0.25)), big.mark = ","))
pairs <- pairs[, .(pid, event_id, family, perf, perf_raw, age_days, w_static, tactical, grp_n, rk)]
trim_keep <- function(d, frac) if (frac <= 0) rep(TRUE, nrow(d)) else
  !(d$tactical & !is.na(d$rk) & d$rk <= floor(d$grp_n * frac))

# --- recover the population quantities from the cached real run --------------
hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) {
    hv <- unlist(hl_map)
    i <- match(fam, names(hv))
    v[!is.na(i)] <- hv[i[!is.na(i)]]
  }
  v
}
raw_at <- function(hl_global, hl_map, trim = 0.25) {
  pairs[, w := w_static * 0.5^(age_days / hl_of(family, hl_global, hl_map))]
  pairs[trim_keep(pairs, trim), .(ability_raw = sum(w * perf) / sum(w), w_total = sum(w)), by = pid]
}
dep <- raw_at(DEPLOYED$half_life, DEPLOYED$hl_family)
k <- merge(keys, dep, by = "pid")
# kappa from the reference's own shrinkage and weight, then the two population
# terms. Both are properties of the event's field, not of the half-life, so the
# sweep holds them fixed; the gate below measures what that costs.
k[, kappa := ref_shrinkage * ref_w / (1 - ref_shrinkage)]
k[, sigma_between := sigma / sqrt(kappa)]
k[, prior_mu := (ref_ability - (1 - ref_shrinkage) * ability_raw) / ref_shrinkage]
ok <- k[is.finite(kappa) & is.finite(prior_mu) & ref_shrinkage > 1e-6 & ref_shrinkage < 1]
say("population terms recovered for %s of %s predictions", format(nrow(ok), big.mark = ","),
    format(nrow(k), big.mark = ","))
# For predictions with negligible shrinkage prior_mu is unrecoverable and also
# irrelevant: ability is ability_raw. Keep them with prior_mu = ability_raw.
k[!is.finite(prior_mu) | ref_shrinkage <= 1e-6, `:=`(prior_mu = ability_raw, kappa = 0)]
k[!is.finite(sigma_between) | sigma_between <= 0, sigma_between := sigma]
saveRDS(pairs, file.path(CACHE, "pairs.rds"))
saveRDS(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu,
              ref_ability, ref_shrinkage, ref_w)], file.path(CACHE, "keys.rds"))

# --- gate: reproduce the reference at the deployed config --------------------
# `debias = FALSE` returns the estimator's own ability. The cached reference
# comes from estimate_ability(), and the family-pool debias is applied by
# deployed_ability(), a WRAPPER around it -- so the gate must compare
# like with like. Comparing a debiased prediction against an undebiased
# reference showed a -0.50% mean gap that was simply the debias itself.
sc <- function(hl_global = DEPLOYED$half_life, hl_map = DEPLOYED$hl_family, debias = TRUE, trim = 0.25) {
  r <- raw_at(hl_global, hl_map, trim)
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, shrinkage := (sigma^2 / sigma_between^2) / (w_total + sigma^2 / sigma_between^2)]
  m[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  if (debias) deployed_debias(m) else m
}
t0 <- Sys.time()
g <- sc(debias = FALSE)
say("scored the deployed config in %.2fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))
gg <- merge(g[, .(pid, ability)], k[, .(pid, ref_ability)], by = "pid")
err <- 100 * (gg$ability - gg$ref_ability)
say("GATE vs estimate_ability on %s predictions: mean %+.4f%%, sd %.4f%%, max |err| %.4f%%",
    format(nrow(gg), big.mark = ","), mean(err), sd(err), max(abs(err)))
cat(if (abs(mean(err)) < 0.02 && sd(err) < 0.1)
      "\nGATE PASSED - the pair table reproduces the model; sweeps from it are trustworthy.\n"
    else "\nGATE FAILED - do not read a sweep from this table yet.\n")
say("wrote pairs.rds and keys.rds")
