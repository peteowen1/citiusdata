# WHERE DOES THE 1pp COME FROM? One meet, athlete by athlete.
#
# On the same 1,637 athlete-races the PIT harness reads +0.08% optimism and the
# backtest +1.00%, correlation 0.945 -- a near-constant level offset, cause
# unknown after eliminating round pooling, tier dependence, the tier/round
# projections, momentum and refit staleness
# (docs/reviews/finals-level-bias-is-a-harness-artefact-2026-09-07.md).
#
# This rebuilds the harness's ability for ONE race and sets it beside the
# backtest's own stored numbers for the same athletes. The backtest's
# predictions carry `shrinkage` and `w_total`, and its `median_mark` is the
# median of the simulated marks, which for a symmetric zero-mean noise stack is
# `ability` itself -- so all three of the quantities that could differ are
# comparable without re-running the backtest.
#
# Three harness variants are built, to separate the candidates:
#   month   as-of the month start, 24-event history  (exactly what the PIT runs did)
#   raceday as-of the race date, 24-event history    (isolates refit staleness)
#   evonly  as-of the race date, THIS EVENT ONLY     (isolates the population
#           `estimate_ability()` sees, which sets prior_mu and shrinkage)
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/compare_harness_to_backtest.R'
# Env: CITIUS_CMP_ARM (backtest_ctrl_tierfix.rds), CITIUS_CMP_CAL
#      (calibration_corpus_wac_coast_0904.rds -- the one that arm ran),
#      CITIUS_CMP_RACE (default: the largest shared H1-2024 final)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
ARM <- Sys.getenv("CITIUS_CMP_ARM", "backtest_ctrl_tierfix.rds")
CAL <- Sys.getenv("CITIUS_CMP_CAL", "calibration_corpus_wac_coast_0904.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
cal <- readRDS(file.path(OUT, CAL))

b <- readRDS(file.path(OUT, ARM))
pred <- as.data.table(b$predictions)[, .(race_key = race_id, athlete_id = as.character(athlete_id),
                                         bt_mark = median_mark, bt_shrinkage = shrinkage, bt_w_total = w_total)]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
info <- unique(ch[!is.na(race_key), .(race_key, event_id, date, round, tier)], by = "race_key")
pred <- merge(pred, info, by = "race_key")
pred[, date := as.Date(date)]
# Only races the PIT harness itself scored, or the comparison is between two
# different populations rather than two pipelines. The first run picked a
# 206-athlete marathon, which is outside the harness's 24-event list.
PIT_ROWS <- Sys.getenv("CITIUS_CMP_PITROWS", "pit_coverage_rows_finals_aging1_debias0.csv")
cand <- pred[date >= as.Date("2024-01-01") & date < as.Date("2024-07-01") &
               grepl("final", tolower(round)) & !grepl("semi|quarter", tolower(round))]
if (file.exists(file.path(OUT, PIT_ROWS))) {
  pr <- fread(file.path(OUT, PIT_ROWS))
  cand <- cand[race_key %in% unique(pr$race_key)]
  say("candidate races restricted to the %s the PIT harness scored", PIT_ROWS)
}
# The gap is family-dependent (throw -1.74pp, jump -1.32, sprint -1.02,
# distance -0.27), so which race is picked matters: set CITIUS_CMP_EVENT to
# look at the family the gap is largest in.
EV_WANT <- Sys.getenv("CITIUS_CMP_EVENT", "")
if (nzchar(EV_WANT)) cand <- cand[event_id == EV_WANT]
stopifnot("no candidate races" = nrow(cand) > 0)
RK <- Sys.getenv("CITIUS_CMP_RACE", cand[, .N, by = race_key][order(-N)]$race_key[1])
r <- pred[race_key == RK]
EV <- r$event_id[1]; RACE_DATE <- r$date[1]
MONTH <- as.Date(format(RACE_DATE, "%Y-%m-01"))
ORI <- .citius_event_registry$orientation[match(EV, .citius_event_registry$event_id)]
say("race %s | %s | %s | %d athletes | month start %s (%d days stale)",
    RK, EV, format(RACE_DATE), nrow(r), format(MONTH), as.integer(RACE_DATE - MONTH))

EVS <- c("AT-100Metres-M", "AT-100Metres-W", "AT-200Metres-M", "AT-200Metres-W",
         "AT-400Metres-M", "AT-400Metres-W", "AT-800Metres-M", "AT-800Metres-W",
         "AT-1500Metres-M", "AT-1500Metres-W", "AT-5000Metres-M", "AT-5000Metres-W",
         "AT-110MetresHurdles-M", "AT-100MetresHurdles-W", "AT-400MetresHurdles-M", "AT-400MetresHurdles-W",
         "AT-LongJump-M", "AT-LongJump-W", "AT-HighJump-M", "AT-HighJump-W",
         "AT-ShotPut-M", "AT-ShotPut-W", "AT-JavelinThrow-M", "AT-JavelinThrow-W")
if (!EV %in% EVS) EVS <- c(EVS, EV)
store <- file.path(OUT, "athletics_corpus_store")
cols <- intersect(c("athlete_id", "event_id", "date", "perf", "age", "round", "tier", "meet_tier",
                    "competition_id", "race_key", "wind", "momentum", "indoor", "venue_country"),
                  names(open_dataset(store)))
x <- as.data.table(read_results_store(store, events = EVS, from = RACE_DATE - 365 * 12, to = RACE_DATE, columns = cols))
x <- flag_implausible(x)
x <- x[is.finite(perf)]
x[, athlete_id := as.character(athlete_id)]; x[, date := as.Date(date)]
say("history pull: %s rows across %d events", format(nrow(x), big.mark = ","), uniqueN(x$event_id))

fit <- function(as_of, events) {
  h <- x[date < as_of & event_id %in% events]
  reg_f <- as.data.table(citius_events()[, c("event_id", "family")])
  pf <- merge(h, reg_f, by = "event_id", all.x = TRUE)
  pf[is.na(family), family := ""]
  ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
    fam <- g$family[1]
    hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
    estimate_ability(g[, !"family"], as_of = as_of, half_life = hl, calibration = cal)
  }), fill = TRUE)
  ab[, athlete_id := as.character(athlete_id)]
  ab[event_id == EV & athlete_id %in% r$athlete_id]
}
variants <- list(month = fit(MONTH, EVS), raceday = fit(RACE_DATE, EVS), evonly = fit(RACE_DATE, EV))
for (nm in names(variants)) {
  v <- variants[[nm]]
  say("%-8s %d of %d entrants estimated", nm, nrow(v), nrow(r))
}

cmp <- copy(r)[, .(athlete_id, bt_mark, bt_shrinkage, bt_w_total)]
cmp[, bt_ability := ORI * log(bt_mark)]
for (nm in names(variants)) {
  v <- variants[[nm]][, .(athlete_id, ability, prior_mu, shrinkage, w_total)]
  # the field prior and aging, as deployed_field() applies them
  vf <- deployed_field(variants[[nm]])
  v[, ability_field := vf$ability[match(v$athlete_id, as.character(vf$athlete_id))]]
  # APPLES TO APPLES. The backtest stores a SIMULATED median, not `ability`:
  # simulate_event() scales the noise asymmetrically (.asymmetry_ratios) and
  # subtracts the resulting MEAN shift, which moves the MEDIAN by that same
  # constant. So the harness must be simulated too, or the comparison measures
  # the asymmetry term instead of the pipelines.
  sm <- medal_probs(simulate_event(vf, n_sims = 4000, calibration = cal, seed = 11L))
  v[, sim_ability := ORI * log(sm$median_mark[match(v$athlete_id, as.character(sm$athlete_id))])]
  setnames(v, c("ability", "prior_mu", "shrinkage", "w_total", "ability_field", "sim_ability"),
           paste0(nm, "_", c("ability", "prior_mu", "shrinkage", "w_total", "ability_field", "sim")))
  cmp <- merge(cmp, v, by = "athlete_id", all.x = TRUE)
}
cat("\n=== per athlete: the backtest's stored numbers against each harness variant ===\n")
print(cmp[, .(athlete_id,
              bt_mark = round(bt_mark, 2),
              bt_shr = round(bt_shrinkage, 4), mo_shr = round(month_shrinkage, 4), ev_shr = round(evonly_shrinkage, 4),
              bt_w = round(bt_w_total, 1), mo_w = round(month_w_total, 1), ev_w = round(evonly_w_total, 1))])

cat("\n=== level: mean difference in ability, as % of a mark (+ = harness predicts BETTER) ===\n")
for (nm in names(variants)) {
  a_raw <- cmp[[paste0(nm, "_ability")]]; a_fld <- cmp[[paste0(nm, "_ability_field")]]
  ok <- is.finite(a_raw) & is.finite(cmp$bt_ability)
  a_sim <- cmp[[paste0(nm, "_sim")]]
  say("%-8s raw ability %+.3f%%  | after prior+aging %+.3f%%  | SIMULATED (like for like) %+.3f%%  | shrinkage bt %.3f vs %.3f | w_total bt %.1f vs %.1f",
      nm, 100 * mean(a_raw[ok] - cmp$bt_ability[ok]),
      100 * mean(a_fld[ok] - cmp$bt_ability[ok], na.rm = TRUE),
      100 * mean(a_sim[ok] - cmp$bt_ability[ok], na.rm = TRUE),
      mean(cmp$bt_shrinkage[ok]), mean(cmp[[paste0(nm, "_shrinkage")]][ok], na.rm = TRUE),
      mean(cmp$bt_w_total[ok]), mean(cmp[[paste0(nm, "_w_total")]][ok], na.rm = TRUE))
}
cat("\nReading: if shrinkage and w_total match but ability does not, the gap is in\n")
cat("what is applied AFTER estimation (the field prior's field, aging). If they\n")
cat("differ, the gap is the history population or window each pipeline feeds in.\n")
fwrite(cmp, file.path(OUT, "compare_harness_to_backtest.csv"))
say("wrote compare_harness_to_backtest.csv")
