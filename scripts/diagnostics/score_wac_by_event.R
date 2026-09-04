# Per-event marks MAE and gold/medal logloss, control (deployed feed-tier) vs
# treatment (WAC meet_tier context adjustment), both restricted to T1_elite,
# both run on the current (2026-09-04) corpus and catalogue.
#
# Neither backtest_athletics.R's saved predictions/outcomes nor its console
# scorecard break down by event_id -- score_predictions() reports one number
# over the whole scored pool. event_id is not even a column on `pred`/`outc`;
# it has to be recovered by rejoining race_id (== race_key) to the finals
# table the same way run_meet() built it, or every event's numbers here would
# silently be wrong in the same way a mismatched join elsewhere in this repo
# has been before.
#
# Usage:  Rscript citiusdata/scripts/diagnostics/score_wac_by_event.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CTRL <- Sys.getenv("CITIUS_WAC_CTRL", "backtest_wac_ctrl_0904.rds")
TRT  <- Sys.getenv("CITIUS_WAC_TRT",  "backtest_wac_trt_0904.rds")
MIN_N <- as.integer(Sys.getenv("CITIUS_WAC_MIN_N", "20"))

say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

ctrl <- readRDS(file.path(OUT, CTRL))
trt  <- readRDS(file.path(OUT, TRT))

# --- provenance: these two arms must actually be comparable ------------------
# Same trap as score_arm.R's own vintage guard: two arms that silently read
# different history/catalogue vintages produce a delta that is the vintage
# difference, not the tier-basis difference this script exists to isolate.
stopifnot(
  "control/treatment scored different history files" =
    identical(ctrl$meta$history_md5, trt$meta$history_md5),
  "control/treatment tier_filter differs" =
    identical(ctrl$meta$tier_filter, trt$meta$tier_filter),
  "control tier_filter is not T1_elite" = identical(ctrl$meta$tier_filter, "T1_elite")
)
say(sprintf("provenance OK: both arms T1_elite, history_md5 %s", substr(ctrl$meta$history_md5, 1, 12)))

# --- event_id map, rebuilt exactly as run_meet() built `field`/`ev` ----------
champs <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
finals <- champs[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]
ev_map <- unique(finals[, .(race_id = race_key, event_id)], by = "race_id")
n_dup <- finals[, uniqueN(event_id), by = race_key][V1 > 1L, .N]
if (n_dup) say(sprintf("WARNING: %d race_key(s) map to >1 event_id; ev_map keeps the first seen", n_dup))

act <- finals[!is.na(mark), .(race_id = race_key, athlete_id = as.character(athlete_id), actual = mark)]
act <- unique(act, by = c("race_id", "athlete_id"))

reg <- as.data.table(citius_events())[, .(event_id, discipline, sex, family)]

score_one <- function(b, label) {
  pred <- as.data.table(b$predictions)
  outc <- as.data.table(b$outcomes)
  pred[, athlete_id := as.character(athlete_id)]
  outc[, athlete_id := as.character(athlete_id)]

  # Same "winner in field" and "not a merged race" restriction the headline
  # scorecard applies -- a race the winner-detection can't see, or one that is
  # several heats collapsed under one race_key, is not a fair test of either
  # arm and both arms already agree it should be excluded.
  cov <- outc[, .(wp = any(hit), any_merged = any(merged)), by = race_id]
  keep <- cov[wp == TRUE & any_merged == FALSE]$race_id
  pred <- pred[race_id %in% keep]
  outc <- outc[race_id %in% keep]

  d <- merge(pred[, .(race_id, athlete_id, p_gold, p_medal, median_mark)],
             outc[, .(race_id, athlete_id, hit, hit_medal)],
             by = c("race_id", "athlete_id"))
  d <- merge(d, ev_map, by = "race_id")
  d <- merge(d, act, by = c("race_id", "athlete_id"), all.x = TRUE)

  eps <- 1e-6
  d[, gold_ll  := -(hit       * log(pmax(p_gold,  eps)) + (1 - hit)       * log(pmax(1 - p_gold,  eps)))]
  d[, medal_ll := -(hit_medal * log(pmax(p_medal, eps)) + (1 - hit_medal) * log(pmax(1 - p_medal, eps)))]
  d[is.finite(actual) & is.finite(median_mark) & actual > 0,
    mark_ape := 100 * abs(median_mark - actual) / actual]

  d[, arm := label]
  d[]
}

sc  <- score_one(ctrl, "control_feed_tier")
sw  <- score_one(trt,  "treatment_wac_tier")
d <- rbind(sc, sw)
d <- merge(d, reg, by = "event_id", all.x = TRUE)

by_event <- d[, .(n = .N,
                  marks_mae = round(mean(mark_ape, na.rm = TRUE), 3),
                  n_marks   = sum(is.finite(mark_ape)),
                  gold_logloss  = round(mean(gold_ll), 4),
                  medal_logloss = round(mean(medal_ll), 4)),
             by = .(arm, event_id, discipline, sex, family)]

ctab <- by_event[arm == "control_feed_tier"]
ttab <- by_event[arm == "treatment_wac_tier"]
cmp <- merge(ctab[, .(event_id, discipline, sex, family, n_c = n,
                      mae_c = marks_mae, gll_c = gold_logloss, mll_c = medal_logloss)],
             ttab[, .(event_id, n_t = n,
                      mae_t = marks_mae, gll_t = gold_logloss, mll_t = medal_logloss)],
             by = "event_id")
cmp[, `:=`(mae_delta = round(mae_t - mae_c, 3),
           gll_delta = round(gll_t - gll_c, 4),
           mll_delta = round(mll_t - mll_c, 4))]
setorder(cmp, -mae_delta)

cli::cli_h1("Per-event: control (feed tier) vs treatment (WAC meet_tier), T1_elite")
cat(sprintf("events scored: %d | events with n >= %d: %d\n",
            nrow(cmp), MIN_N, sum(cmp$n_c >= MIN_N)))

cli::cli_h3(sprintf("all events (n >= %d), worst marks MAE delta first (+ = WAC tier worse)", MIN_N))
print(cmp[n_c >= MIN_N, .(event_id, discipline, sex, family, n_c,
                          mae_c, mae_t, mae_delta, gll_c, gll_t, gll_delta,
                          mll_c, mll_t, mll_delta)])

cli::cli_h3("aggregate (pooled over the same events, unweighted by n)")
agg <- d[, .(n = .N,
            marks_mae = round(mean(mark_ape, na.rm = TRUE), 3),
            gold_logloss = round(mean(gold_ll), 4),
            medal_logloss = round(mean(medal_ll), 4)), by = arm]
print(agg)

paired <- merge(sc[, .(race_id, athlete_id, mark_ape_c = mark_ape, gll_c = gold_ll, mll_c = medal_ll)],
                sw[, .(race_id, athlete_id, mark_ape_t = mark_ape, gll_t = gold_ll, mll_t = medal_ll)],
                by = c("race_id", "athlete_id"))
cli::cli_h3("paired significance, pooled T1_elite (t-test on the per-prediction delta)")
tt <- function(x, y, lab) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5) return(cat(sprintf("%s: too few paired rows (%d)\n", lab, sum(ok))))
  r <- stats::t.test(y[ok] - x[ok])
  cat(sprintf("%-14s n=%d  mean delta=%+.4f  p=%.3g  (%s)\n",
              lab, sum(ok), mean(y[ok] - x[ok]), r$p.value,
              if (mean(y[ok] - x[ok]) > 0) "WAC tier worse" else "WAC tier better"))
}
tt(paired$mark_ape_c, paired$mark_ape_t, "marks MAE")
tt(paired$gll_c, paired$gll_t, "gold logloss")
tt(paired$mll_c, paired$mll_t, "medal logloss")

fwrite(cmp, file.path(OUT, "wac_reverify_by_event_0904.csv"))
say("wrote wac_reverify_by_event_0904.csv")
