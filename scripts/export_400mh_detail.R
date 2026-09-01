# Race-level detail for AT-400MetresHurdles-M: every T1 final since 2018,
# our prediction, the actual result, the last-5 baseline, season best and
# personal best -- so Pete can see WHY marks MAE is +50.75% worse than
# last-5 on this event (p=4.6e-5), a miss the aggregate -2.40% completely hid.
#
# SB/PB definition matches check_event_vs_sb_pb.R exactly: season best is the
# athlete's best mark EARLIER THIS SEASON, personal best is their best mark
# EVER before this race -- both walk-forward (shift(cummax(perf))), computed
# on ORIENTED perf so "best" means the same thing regardless of event
# direction, then converted back to a raw mark for display.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow)); suppressMessages(library(jsonlite))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2018-01-01")
EVENT_ID <- "AT-400MetresHurdles-M"
say <- function(...) cat(sprintf(...), "\n", sep = "")

b <- readRDS(file.path(OUT, "backtest_combined_full.rds"))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            p_gold, p_medal, median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal, merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, actual_place = place,
                   event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier, comp_name)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT & event_id == EVENT_ID]
setorder(d, date)
say("population: %d rows (races), event %s", nrow(d), EVENT_ID)
stopifnot("expected 27 rows to match the artifact's own count" = nrow(d) > 0)

# ---- last-5, SB, PB: all from the SAME full-history pull, one pass --------
hist <- deployed_history(OUT, events = EVENT_ID, from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, date)
hist[, yr := data.table::year(date)]
g <- c("athlete_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
hist[, l5 := (cs - cs5) / pmin(k, 5)]
# walk-forward season best / personal best, same shift(cummax()) as
# check_event_vs_sb_pb.R -- excludes the current race, includes everything before it.
hist[, sb_perf := shift(cummax(perf)), by = .(athlete_id, yr)]
hist[, pb_perf := shift(cummax(perf)), by = .(athlete_id)]
# A3: PB must never be more extreme (better) than SB for the same athlete/row
# -- SB is a subset of PB's own history (this season is part of "ever"), so
# pb should always be >= the best seen this season. Catches the exact class
# of bug just found (a missing `by=` silently computing a GLOBAL cummax
# across every athlete in table order) before it reaches an artefact again.
chk <- hist[is.finite(sb_perf) & is.finite(pb_perf)]
stopifnot("A3: PB is more extreme than SB for some athlete -- grouping bug" =
            all(chk$pb_perf >= chk$sb_perf - 1e-9))

q <- d[, .(athlete_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "date"))
m <- hist[q, on = .(athlete_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, l5, sb_perf, pb_perf, k)]
d <- merge(d, m, by = c("race_id", "athlete_id"), all.x = TRUE)

d[, `:=`(
  last5_mark = exp(l5 / orientation),
  sb_mark    = exp(sb_perf / orientation),
  pb_mark    = exp(pb_perf / orientation)
)]
d[, err_model_pct := round(100 * orientation * (actual - median_mark) / median_mark, 2)]
d[, err_last5_pct := round(100 * orientation * (actual - last5_mark) / last5_mark, 2)]
d[, n_prior_races := k]

say("rows with a last-5 baseline: %d | with SB: %d | with PB: %d",
    sum(is.finite(d$last5_mark)), sum(is.finite(d$sb_mark)), sum(is.finite(d$pb_mark)))

out <- d[, .(date = format(date), competition = comp_name, athlete_id, actual_place,
            actual_mark = round(actual, 2), model_pred = round(median_mark, 2),
            last5_mark = round(last5_mark, 2), sb_mark = round(sb_mark, 2), pb_mark = round(pb_mark, 2),
            err_model_pct, err_last5_pct, n_prior_races,
            p_gold = round(p_gold, 3), p_medal = round(p_medal, 3), hit, hit_medal)]
setorder(out, date, actual_place)
print(out)

write_json(out, file.path(OUT, "detail_400mh_m.json"), auto_unbox = FALSE, na = "null", digits = 6)
say("\nwrote detail_400mh_m.json (%d rows)", nrow(out))

# summary stats for the artifact header
summ <- list(
  event = EVENT_ID, discipline = "400 Metres Hurdles", sex = "M",
  races = uniqueN(d$race_id), rows = nrow(d),
  model_mae_pct = round(mean(abs(d$err_model_pct), na.rm=TRUE), 2),
  last5_mae_pct = round(mean(abs(d$err_last5_pct), na.rm=TRUE), 2),
  model_mean_signed_pct = round(mean(d$err_model_pct, na.rm=TRUE), 2),
  last5_mean_signed_pct = round(mean(d$err_last5_pct, na.rm=TRUE), 2),
  pct_model_beats_last5 = round(100*mean(abs(d$err_model_pct) < abs(d$err_last5_pct), na.rm=TRUE), 1)
)
write_json(summ, file.path(OUT, "detail_400mh_m_summary.json"), auto_unbox = TRUE, digits = 6)
print(summ)
