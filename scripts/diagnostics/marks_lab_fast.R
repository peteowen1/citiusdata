# MARKS LAB, FAST: sweep recency parameters in seconds, not hours.
#
# WHY THIS SHAPE. Profiled 2026-09-07 rather than guessed: `estimate_ability()`
# costs ~30s per million history rows even with `only =` (55.3s for 1.9M rows /
# 10 events; the context adjustment is 18s of that, the core estimation the
# rest). A monthly refit over the full 70-event history is ~3 min, so a
# 12-month window is ~35 min PER CONFIG. That is unusable for a sweep.
#
# The way out is that the two expensive parts do not depend on the parameter
# being swept:
#   * `.adjust_history_to_target()` puts every mark on a final-equivalent
#     footing using round, tier, coasting, wind, momentum, indoor, season and
#     championship. None of that reads `as_of` or `half_life`, so it is done
#     ONCE for all history.
#   * the non-recency half of `result_weight()` (round precision x tier
#     precision) is also parameter-free, so it is computed ONCE.
# What remains per config is a weighted group-by, which is seconds.
#
# WHAT IS HELD FIXED, and why that is honest. `sigma` (and through it `kappa`
# and `shrinkage`) does depend on the half-life, through the weighted spread.
# Recomputing the whole sigma pipeline here would be a reimplementation of the
# most-corrected code in the package. Instead sigma is taken once from a real
# `estimate_ability()` run at the deployed config and held fixed across the
# sweep, so shrinkage moves only through `w_total`. The VERIFY step measures
# what that costs: at the deployed half-life this scorer must reproduce
# `estimate_ability()`'s own ability to within a stated tolerance, or the
# sweep is not trustworthy and the script says so and stops.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_lab_fast.R'
# Env: CITIUS_FAST_FROM/_TO (window), CITIUS_FAST_SWEEP ("family:a,b,c" or
#      "global:a,b,c"), CITIUS_FAST_CAL, CITIUS_FAST_TOL (0.02 = 0.02% of a mark)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT  <- here::here("citiusdata", "data")
FROM <- as.Date(Sys.getenv("CITIUS_FAST_FROM", "2024-01-01"))
TO   <- as.Date(Sys.getenv("CITIUS_FAST_TO", "2026-09-01"))
CAL  <- Sys.getenv("CITIUS_FAST_CAL", DEPLOYED$calibration)
TOL  <- as.numeric(Sys.getenv("CITIUS_FAST_TOL", "0.02"))
say  <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
tk   <- function(l, e) { t0 <- Sys.time(); v <- force(e)
  say("%-38s %5.1fs", l, as.numeric(difftime(Sys.time(), t0, units = "secs"))); invisible(v) }
cal <- readRDS(file.path(OUT, CAL))

# --- test set: T1_elite finals ----------------------------------------------
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
test <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0 & !is.na(event_id),
                  .(race_key, athlete_id, event_id, date = as.Date(date), competition_id, round, mark)],
               by = c("race_key", "athlete_id"))
ct <- as.data.table(open_dataset(file.path(OUT, "competition_catalogue.parquet")) |>
                      dplyr::select(competition_id, meet_tier) |> dplyr::collect())
ct[, competition_id := as.character(competition_id)]
test <- merge(test, unique(ct[!is.na(meet_tier)], by = "competition_id"), by = "competition_id")
test <- test[meet_tier == "T1_elite" & date >= FROM & date < TO &
               grepl("final", tolower(round)) & !grepl("semi|quarter", tolower(round))]
reg <- as.data.table(citius_events())[, .(event_id, orientation, family)]
test <- merge(test, reg, by = "event_id")[, act := orientation * log(mark)][is.finite(act)]
test[, month := as.Date(format(date, "%Y-%m-01"))]
say("test set %s rows, %s races, %d events, %d months",
    format(nrow(test), big.mark = ","), format(uniqueN(test$race_key), big.mark = ","),
    uniqueN(test$event_id), uniqueN(test$month))

# --- history, adjusted ONCE --------------------------------------------------
store <- file.path(OUT, "athletics_corpus_store")
cols <- intersect(c("athlete_id","event_id","date","perf","age","round","tier","meet_tier",
                    "competition_id","race_key","wind","momentum","indoor","venue_country"),
                  names(open_dataset(store)))
h <- tk("read history", {
  x <- as.data.table(read_results_store(store, events = unique(test$event_id),
                                        from = FROM - DEPLOYED$history_days, to = TO, columns = cols))
  x <- flag_implausible(x)[is.finite(perf)]
  x[, athlete_id := as.character(athlete_id)]; x[, date := as.Date(date)]; x })
say("history %s rows", format(nrow(h), big.mark = ","))
adj <- tk("adjust history (once)", citius:::.adjust_history_to_target(copy(h), cal, isTRUE(DEPLOYED$adjust_race)))
# the parameter-free half of result_weight(): recency is 0.5^0 = 1 at half_life Inf
adj[, w_static := result_weight(date, tier = tier, round = round, as_of = TO,
                                half_life = Inf, calibration = cal,
                                tier_class = citius:::.tier_class_of(adj))]
adj <- adj[is.finite(perf) & is.finite(w_static) & w_static > 0,
           .(athlete_id, event_id, date, perf, w_static)]
adj <- merge(adj, reg[, .(event_id, family)], by = "event_id")
setkey(adj, event_id, athlete_id, date)

# --- sigma, once, from a real run at the deployed config ---------------------
sig <- tk("sigma from estimate_ability (once)", {
  rbindlist(lapply(sort(unique(test$month)), function(cut) {
    past <- h[date < cut]
    pf <- merge(past, reg[, .(event_id, family)], by = "event_id", all.x = TRUE)
    pf[is.na(family), family := ""]
    ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
      fam <- g$family[1]
      hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
      estimate_ability(g[, !"family"], as_of = cut, half_life = hl, calibration = cal,
                       adjust_race = isTRUE(DEPLOYED$adjust_race),
                       only = unique(test[month == cut]$athlete_id))
    }), fill = TRUE)
    ab[, .(month = cut, athlete_id = as.character(athlete_id), event_id, sigma,
           ref_ability = ability, ref_shrinkage = shrinkage, ref_w = w_total)]
  }), fill = TRUE)
})
say("sigma reference: %s athlete-event-months", format(nrow(sig), big.mark = ","))

# --- the fast scorer ---------------------------------------------------------
score <- function(hl_global = DEPLOYED$half_life, hl_map = DEPLOYED$hl_family) {
  out <- rbindlist(lapply(sort(unique(test$month)), function(cut) {
    d <- adj[date < cut]
    d[, hl := fifelse(family %chin% names(hl_map),
                      unlist(hl_map)[match(family, names(hl_map))], hl_global)]
    d[, w := w_static * 0.5^(as.numeric(cut - date) / hl)]
    ae <- d[, .(ability_raw = sum(w * perf) / sum(w), w_total = sum(w), n = .N),
            by = .(athlete_id, event_id)][n >= 1L & is.finite(ability_raw)]
    pri <- ae[, .(prior_mu = mean(ability_raw), sigma_between = sd(ability_raw)), by = event_id]
    ae <- merge(ae, pri, by = "event_id")
    s <- sig[month == cut]
    ae <- merge(ae, s[, .(athlete_id, event_id, sigma)], by = c("athlete_id", "event_id"))
    ae[!is.finite(sigma_between) | sigma_between <= 0, sigma_between := sigma]
    ae[, kappa := (sigma^2) / (sigma_between^2)]
    ae[, shrinkage := kappa / (w_total + kappa)]
    ae[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
    ae[, month := cut]
    ae[, .(month, athlete_id, event_id, ability, shrinkage, w_total)]
  }), fill = TRUE)
  # the debias is a per-event constant applied after estimation
  out <- deployed_debias(out)
  merge(test[, .(race_key, athlete_id, event_id, family, month, date, act)],
        out[, .(month, athlete_id, event_id, pred = ability)],
        by = c("athlete_id", "event_id", "month"))
}

# --- VERIFY: reproduce estimate_ability at the deployed config ---------------
v <- tk("score at deployed config", score())
vv <- merge(v, sig[, .(month, athlete_id, event_id, ref_ability)],
            by = c("month", "athlete_id", "event_id"))
err <- 100 * (vv$pred - vv$ref_ability)
say("VERIFY vs estimate_ability on %s rows: mean %+.4f%%, sd %.4f%%, max |err| %.4f%%",
    format(nrow(vv), big.mark = ","), mean(err), sd(err), max(abs(err)))
if (abs(mean(err)) > TOL || sd(err) > TOL * 5) {
  cat("\nFAIL: the fast scorer does not reproduce estimate_ability within tolerance.\n")
  cat("Do not read a sweep from this run. Likeliest cause: something in the\n")
  cat("adjustment or weighting chain that does depend on as_of or half_life.\n")
  quit(status = 1L)
}
say("within tolerance (%.2f%%) -- the sweep below is trustworthy", TOL)

# --- baseline + scoring ------------------------------------------------------
hh <- h[, .(athlete_id, event_id, date, perf)]
pk <- unique(test[, .(athlete_id, event_id, date)])
jj <- hh[pk, on = .(athlete_id, event_id), allow.cartesian = TRUE][date < i.date]
setorder(jj, athlete_id, event_id, i.date, -date)
jj[, rk := seq_len(.N), by = .(athlete_id, event_id, i.date)]
b5 <- jj[rk <= 5, .(base = mean(perf), n_prior = .N), by = .(athlete_id, event_id, date = i.date)]
b5 <- b5[n_prior >= 3L]
say("last-5 baseline on %s athlete-races", format(nrow(b5), big.mark = ","))

evaluate <- function(p, label) {
  d <- merge(p, b5, by = c("athlete_id", "event_id", "date"))
  d[, `:=`(ae_m = 100 * abs(pred - act), ae_b = 100 * abs(base - act))]
  ev <- d[, .(races = uniqueN(race_key), n = .N, m = mean(ae_m), b = mean(ae_b),
              se = sd(ae_m - ae_b) / sqrt(.N)), by = .(event_id, family)][races >= 10]
  ev[, `:=`(gap = 100 * (m - b) / b, t = (m - b) / se, beat = m < b)]
  cat(sprintf("%-42s beat %2d/%2d | pooled model %.3f base %.3f (%+.1f%%)\n",
              label, sum(ev$beat), nrow(ev), weighted.mean(ev$m, ev$n),
              weighted.mean(ev$b, ev$n),
              100 * (weighted.mean(ev$m, ev$n) - weighted.mean(ev$b, ev$n)) / weighted.mean(ev$b, ev$n)))
  ev[, config := label][]
}
res <- list(evaluate(v, "deployed (365, road 1095 walk 730 hurdles 180)"))

# --- the sweep ---------------------------------------------------------------
sw <- Sys.getenv("CITIUS_FAST_SWEEP", "global:60,90,120,180,270,365,540")
kind <- sub(":.*$", "", sw); vals <- as.numeric(strsplit(sub("^[^:]*:", "", sw), ",")[[1]])
cat("\n=== sweep:", sw, "===\n")
for (x in vals) {
  t0 <- Sys.time()
  p <- if (kind == "global") score(hl_global = x, hl_map = list()) else
    score(hl_map = utils::modifyList(DEPLOYED$hl_family, setNames(list(x), kind)))
  lbl <- sprintf("%s = %g", kind, x)
  r <- evaluate(p, lbl)
  r[, secs := round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)]
  res[[length(res) + 1]] <- r
}
all <- rbindlist(res, fill = TRUE)
fwrite(all, file.path(OUT, "marks_lab_fast.csv"))
cat("\nper-event, best config for each event:\n")
best <- all[, .SD[which.min(gap)], by = event_id][order(gap)]
print(best[, .(event_id, family, config, races, model = round(m, 3), last5 = round(b, 3),
               gap = round(gap, 1), beat)], nrows = 60)
say("wrote marks_lab_fast.csv")
