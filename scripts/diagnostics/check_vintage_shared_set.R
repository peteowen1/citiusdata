# Did the model's marks accuracy REGRESS, or did the scored population get harder?
#
# On 2026-09-01 a fresh control arm scored T1 centred marks MAE at +7.36% worse
# than a last-5 baseline, against +3.18% measured on 2026-08-31. Same scorer,
# same population definition, SAME calibration (md5 d162143... on both arms).
# What changed was the corpus: 2026-08-31's session grew championship_results
# from 4,183,376 to 4,544,586 rows (corruption recovery, a 22-competition
# backfill, a T3 pilot of ~802 competitions), and races_scored went 994 -> 1,962.
#
# THE CONFOUND THIS SCRIPT EXISTS TO SETTLE. A near-doubled scored pool means
# the two numbers are not measured on the same races. If the newly-scoreable
# races are harder -- lower tier, thinner evidence, smaller fields -- mean MAE
# rises with no model change at all. That is the null and it has to be excluded
# before anyone concludes the model regressed.
#
# METHOD: restrict both arms to the (race_id, athlete_id) rows they BOTH scored
# and recompute there. The last-5 baseline is built ONCE from the current corpus
# and used for both arms, which is the point -- holding the baseline fixed
# isolates the only thing that differs, the model's own predictions.
#
# Baseline construction is lifted from check_marks_vs_naive_baselines.R (history
# rolled to `date - 1L`, so nothing sees the race it predicts).
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  the two arms must report the SAME calibration md5. If they differ, this
#       is not a vintage test and the result means nothing.
#   A2  the shared set must be non-empty and materially smaller than the new
#       arm. If it equals the new arm, the pool did not actually change and the
#       premise is wrong.
#   A3  on shared rows the two arms' predictions must DIFFER for at least some
#       athletes -- same calibration but different history. If they are
#       identical, the corpus change did not reach ability estimation and the
#       whole question is moot.
#   A4  last-5 must be near-unbiased (it is a central estimate), as it was when
#       measured on 2026-08-31.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")

OLD <- Sys.getenv("CITIUS_VINTAGE_OLD", "backtest_ctrl_now.rds")
NEW <- Sys.getenv("CITIUS_VINTAGE_NEW", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")

bo <- readRDS(file.path(OUT, OLD)); bn <- readRDS(file.path(OUT, NEW))

cat("\n==== ANCHOR A1: same calibration? ====\n")
say("OLD %s : calibration %s  md5 %s", OLD, bo$meta$calibration, bo$meta$calibration_md5)
say("NEW %s : calibration %s  md5 %s", NEW, bn$meta$calibration, bn$meta$calibration_md5)
same_cal <- identical(bo$meta$calibration_md5, bn$meta$calibration_md5)
say("A1 %s", if (same_cal) "PASS - same calibration, so this isolates the corpus"
    else "FAIL - different calibrations; this is NOT a clean vintage test")
say("OLD store_md5 %s | races_scored %s", bo$meta$store_md5, format(bo$meta$races_scored))
say("NEW store_md5 %s | races_scored %s", bn$meta$store_md5, format(bn$meta$races_scored))

grab <- function(b) merge(
  as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                   a_mark = median_mark, shrinkage, w_total)],
  as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
  by = c("race_id", "athlete_id"))
po <- grab(bo); pn <- grab(bn)

setnames(po, c("a_mark", "shrinkage", "w_total"), c("mark_old", "shr_old", "w_old"))
setnames(pn, c("a_mark", "shrinkage", "w_total"), c("mark_new", "shr_new", "w_new"))
sh <- merge(po, pn, by = c("race_id", "athlete_id"))

cat("\n==== ANCHOR A2: shared set ====\n")
say("OLD rows %s | NEW rows %s | SHARED %s",
    format(nrow(po), big.mark = ","), format(nrow(pn), big.mark = ","),
    format(nrow(sh), big.mark = ","))
say("shared races %s ; new-only races %s ; old-only races %s",
    format(uniqueN(sh$race_id), big.mark = ","),
    format(length(setdiff(pn$race_id, po$race_id)), big.mark = ","),
    format(length(setdiff(po$race_id, pn$race_id)), big.mark = ","))
say("A2 %s", if (nrow(sh) > 0 && nrow(sh) < nrow(pn)) "PASS" else "CHECK - shared set is not a strict subset")

cat("\n==== ANCHOR A3: do the arms actually differ on shared rows? ====\n")
sh[, dmark := 100 * (mark_new - mark_old) / mark_old]
say("identical predictions on %.1f%% of shared rows; mean |change| %.4f%% of a mark",
    100 * mean(abs(sh$dmark) < 1e-9, na.rm = TRUE), mean(abs(sh$dmark), na.rm = TRUE))
say("A3 %s", if (mean(abs(sh$dmark) < 1e-9, na.rm = TRUE) < 0.99) "PASS - the corpus change reached ability estimation" else "FAIL - predictions unchanged; question is moot")

# ---- actuals, event orientation, tier -------------------------------------
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
sh <- merge(sh, act, by = c("race_id", "athlete_id"))
sh <- merge(sh, as.data.table(citius_events())[, .(event_id, orientation)], by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
sh[, competition_id := as.character(competition_id)]
sh <- merge(sh, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- last-5 baseline, ONE build, used for both arms ------------------------
hist <- deployed_history(OUT, events = unique(sh$event_id),
                         from = min(sh$date) - 3650, to = max(sh$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)
g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- sh[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
sh <- merge(sh, unique(m[, .(race_id, athlete_id, l5)], by = c("race_id","athlete_id")),
            by = c("race_id", "athlete_id"))

sh[, `:=`(act_perf = orientation * log(actual),
          po_perf  = orientation * log(mark_old),
          pn_perf  = orientation * log(mark_new))]
sh <- sh[date >= HOLDOUT & !is.na(l5) & !is.na(act_perf) & !is.na(po_perf) & !is.na(pn_perf)]

report <- function(d, label) {
  if (!nrow(d)) { say("\n%s: no rows", label); return(invisible()) }
  eo <- 100 * (d$po_perf - d$act_perf)
  en <- 100 * (d$pn_perf - d$act_perf)
  eb <- 100 * (d$l5      - d$act_perf)
  ctr <- function(v) v - mean(v, na.rm = TRUE)
  rel <- function(x, y) 100 * (mean(abs(x)) - mean(abs(y))) / mean(abs(y))
  say("\n---- %s : %s rows, %s races ----", label,
      format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","))
  say("  %-26s %9s %9s %9s", "", "OLD arm", "NEW arm", "last-5")
  say("  %-26s %9.4f %9.4f %9.4f", "signed error (bias) %", mean(eo), mean(en), mean(eb))
  say("  %-26s %9.4f %9.4f %9.4f", "raw MAE", mean(abs(eo)), mean(abs(en)), mean(abs(eb)))
  say("  %-26s %9.4f %9.4f %9.4f", "centred MAE",
      mean(abs(ctr(eo))), mean(abs(ctr(en))), mean(abs(ctr(eb))))
  say("  vs last-5 :  raw     OLD %+.2f%%   NEW %+.2f%%", rel(eo, eb), rel(en, eb))
  say("  vs last-5 :  centred OLD %+.2f%%   NEW %+.2f%%",
      rel(ctr(eo), ctr(eb)), rel(ctr(en), ctr(eb)))
  p <- t.test(abs(ctr(en)), abs(ctr(eo)), paired = TRUE)
  say("  NEW vs OLD centred |err|, paired: diff %+.4f pp, p = %.3g",
      mean(abs(ctr(en))) - mean(abs(ctr(eo))), p$p.value)
}

cat("\n==== ANCHOR A4: last-5 bias (should be near zero) ====\n")
eb_all <- 100 * (sh$l5 - sh$act_perf)
say("last-5 signed error on shared set: %+.4f%%  -> A4 %s",
    mean(eb_all), if (abs(mean(eb_all)) < 1.5) "PASS" else "CHECK")

cat("\n================ SHARED SET (the decisive comparison) ================\n")
report(sh, "SHARED, all tiers")
report(sh[meet_tier == "T1_elite"], "SHARED, T1_elite")

# ---- composition: what are the NEW-only races like? ------------------------
cat("\n================ COMPOSITION of newly-scored races ================\n")
newonly <- pn[!po, on = c("race_id", "athlete_id")]
newonly <- merge(newonly, act, by = c("race_id", "athlete_id"))
newonly[, competition_id := as.character(competition_id)]
newonly <- merge(newonly, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
shr_cmp <- rbind(
  sh[,      .(set = "shared",   meet_tier, shrinkage = shr_new, w_total = w_new)],
  newonly[, .(set = "new-only", meet_tier, shrinkage = shr_new, w_total = w_new)])
say("rows: shared %s | new-only %s",
    format(nrow(sh), big.mark = ","), format(nrow(newonly), big.mark = ","))
cat("\nmeet_tier mix (% of rows):\n")
print(dcast(shr_cmp[!is.na(meet_tier), .N, by = .(set, meet_tier)],
            meet_tier ~ set, value.var = "N", fill = 0)[
              , lapply(.SD, function(v) if (is.numeric(v)) round(100*v/sum(v),1) else v)])
cat("\nevidence depth (w_total) and shrinkage by set:\n")
print(shr_cmp[, .(rows = .N,
                  med_w = round(median(w_total, na.rm = TRUE), 2),
                  med_shrink = round(median(shrinkage, na.rm = TRUE), 3)), by = set])
