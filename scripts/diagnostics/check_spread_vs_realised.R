# Is the simulated WITHIN-RACE spread too narrow for the events where medal
# Brier loses to last-5?
#
# WHY WITHIN-RACE, AND WHY condition_sd IS IRRELEVANT HERE. A race-condition
# shock shared by the whole field cannot change finishing positions -- it
# cancels out of every pairwise comparison (CLAUDE.md, verified in the test
# suite). Medal probabilities are a function of the ORDER, so `condition_sd`
# cannot move them at all. What sets the width of the simulated finishing order
# is `sigma_within` and athlete-specific sensitivity `s_i`. So the diagnostic
# is: realised within-race residual dispersion vs the sigma_within the
# simulator actually used.
#
# THE SIGNATURE WE ARE TESTING (check_brier_cuts_sweep.R, 2026-09-01): gold
# Brier is BETTER than last-5 in exactly the cells where medal Brier is WORSE
# (women's jumps/sprints). That is what "too narrow" looks like -- probability
# piles correctly onto the favourite while being mis-spread across ranks 2-3.
# If the ratio below is >1 in those cells and ~1 elsewhere, the spread is the
# mechanism. If it is flat across cells, it is not, and this is refuted.
#
# ANCHOR CHECKS, written before looking at output:
#   A1 residuals must be computed on the ORIENTED PERF scale, not raw marks --
#      a raw-mark sd is not comparable across events and sigma_within is a
#      perf-scale quantity. Asserted by construction + a units check below.
#   A2 merged (collapsed-heat) races excluded, same rule as every other script
#      here; a merged race's "within-race spread" spans several real races.
#   A3 every reported cut clears MIN_RACES, and the pooled ratio is printed
#      first so a cut can be read against it rather than against 1.0 in the
#      abstract.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_SPREAD_HOLDOUT", "2025-01-01"))
ARM <- Sys.getenv("CITIUS_SPREAD_ARM", "backtest_tierctrl.rds")
MIN_RACES <- 10L
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s | MIN_RACES %d", ARM, format(HOLDOUT), MIN_RACES)

b <- readRDS(file.path(OUT, ARM))
pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                         median_mark, p_medal, p_gold)]
outc <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal, merged)]
d <- merge(pred, outc, by = c("race_id", "athlete_id"))
say("raw merged rows: %s | collapsed-heat rows excluded: %s",
    format(nrow(d), big.mark = ","), format(sum(d$merged), big.mark = ","))
d <- d[merged == FALSE]                                                   # A2

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date,
                   competition_id)], by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[date >= HOLDOUT & meet_tier == "T1_elite"]

# A1: oriented perf scale, the same one sigma_within lives on.
d[, resid_perf := orientation * (log(actual) - log(median_mark))]
d <- d[is.finite(resid_perf)]
stopifnot("residuals are not on a plausible perf scale" =
            median(abs(d$resid_perf), na.rm = TRUE) < 0.5)

evs <- as.data.table(deployed_calibration(OUT)$events)[
  calibrated %in% TRUE, .(event_id, sigma_within, df_eff = if ("df_eff" %in% names(.SD)) df_eff else NA_real_)]
d <- merge(d, evs, by = "event_id")
say("T1 population after all joins: %s rows, %s races, %s events",
    format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","),
    uniqueN(d$event_id))
stopifnot(nrow(d) > 0)

# WITHIN-RACE dispersion: centre each race on its own mean residual, so a
# shared condition shock (which cannot reorder anyone) is removed rather than
# counted as spread. What is left is the dispersion that actually drives who
# medals.
d[, resid_ctr := resid_perf - mean(resid_perf), by = race_id]
race_sd <- d[, .(n = .N, realised_sd = sd(resid_perf), sigma = sigma_within[1],
                 event_id = event_id[1], family = family[1], sex = sex[1],
                 discipline = discipline[1]),
             by = race_id][n >= 4 & is.finite(realised_sd)]
# df correction: sd of residuals around a mean fitted from the same race
# understates the true dispersion at small field sizes.
race_sd[, realised_sd := realised_sd * sqrt(n / pmax(n - 1, 1))]
race_sd[, ratio := realised_sd / sigma]

pooled <- race_sd[, .(races = .N, realised = mean(realised_sd),
                      sigma = mean(sigma), ratio = mean(ratio))]
cat("\n==== POOLED (read every cut against this, not against 1.0) ====\n")
say("races %d | mean realised within-race sd %.4f | mean sigma_within %.4f | ratio %.3f",
    pooled$races, pooled$realised, pooled$sigma, pooled$ratio)
say("ratio > 1 means the simulator runs NARROWER than reality (too confident).")

cut_ratio <- function(by_col, label, min_races = MIN_RACES) {
  cat(sprintf("\n---- spread ratio by %s ----\n", label))
  r <- race_sd[, .(races = .N,
                   realised_sd = round(mean(realised_sd), 4),
                   sigma_within = round(mean(sigma), 4),
                   ratio = round(mean(ratio), 3),
                   # paired test of log ratio against 0 = "sim width is right"
                   p = tryCatch(signif(t.test(log(ratio))$p.value, 3),
                                error = function(e) NA_real_)),
               by = c(by_col)][races >= min_races]
  if (!nrow(r)) { cat("  no level clears MIN_RACES\n"); return(invisible(NULL)) }
  setorder(r, -ratio)
  print(r, row.names = FALSE)
  invisible(r)
}

race_sd[, fs := paste(family, sex, sep = "|")]
cut_ratio("sex", "sex")
cut_ratio("family", "family")
cut_ratio("fs", "family x sex")
cut_ratio("discipline", "discipline", min_races = 8L)

# The decisive comparison, stated explicitly so it cannot be read off the wrong
# row: the cells medal Brier loses on vs the cells it wins on.
LOSE <- c("jump|W", "sprint|W", "hurdles|W")
WIN  <- c("throw|M", "throw|W", "middle|M", "middle|W", "distance|M", "distance|W")
cat("\n==== DECISIVE: spread ratio where medal Brier LOSES vs WINS ====\n")
lo <- race_sd[fs %chin% LOSE]; wi <- race_sd[fs %chin% WIN]
if (nrow(lo) >= MIN_RACES && nrow(wi) >= MIN_RACES) {
  tt <- t.test(log(lo$ratio), log(wi$ratio))
  say("medal-Brier LOSING cells (%s): %d races, ratio %.3f", paste(LOSE, collapse=", "), nrow(lo), mean(lo$ratio))
  say("medal-Brier WINNING cells (%s): %d races, ratio %.3f", paste(WIN, collapse=", "), nrow(wi), mean(wi$ratio))
  say("difference in log ratio: p = %.4g", tt$p.value)
  say("")
  say("SUPPORTS the too-narrow theory only if the losing cells' ratio is")
  say("materially HIGHER. Similar ratios refute it, and the medal-Brier")
  say("deficit is then about the SHAPE of the spread or the ordering, not width.")
} else say("not enough races in one of the two groups to compare")

# ---- CONFOUND CHECK: is the low ratio just no-mark truncation? -------------
# An athlete who fouls out, no-heights or DNFs has no mark, so the actual-mark
# join drops them -- and those are precisely the bad-tail rows. Dropping them
# shrinks realised spread MECHANICALLY, with nothing wrong with sigma_within.
# The events with the lowest ratios (Discus .641, Shot Put .664, Pole Vault
# .666) are exactly the ones where failure produces no mark, so this has to be
# ruled out before the ratio can be read as a property of the calibration.
#
# `pred` holds every athlete the model made a prediction for; `d` holds the
# subset that ended up with a usable actual mark. The gap is the exclusion.
cat("\n==== CONFOUND: no-mark exclusion rate vs spread ratio ====\n")
pred_t1 <- merge(pred, outc[merged == FALSE, .(race_id, athlete_id)],
                 by = c("race_id", "athlete_id"))
pred_t1 <- merge(pred_t1, unique(d[, .(race_id, event_id, discipline, family, sex)]),
                 by = "race_id", allow.cartesian = TRUE)
pred_t1 <- unique(pred_t1, by = c("race_id", "athlete_id"))
kept <- unique(d[, .(race_id, athlete_id, kept = TRUE)], by = c("race_id", "athlete_id"))
pred_t1 <- merge(pred_t1, kept, by = c("race_id", "athlete_id"), all.x = TRUE)
pred_t1[is.na(kept), kept := FALSE]
excl <- pred_t1[, .(entrants = .N, excluded = sum(!kept),
                    excl_rate = round(mean(!kept), 3)), by = discipline]
disc_ratio <- race_sd[, .(races = .N, ratio = round(mean(ratio), 3)), by = discipline]
cmp <- merge(excl, disc_ratio, by = "discipline")[races >= 8L]
setorder(cmp, ratio)
print(cmp, row.names = FALSE)
say("NOTE: an all-zero column here does NOT clear the confound -- the backtest's")
say("own outcomes table is built from athletes who recorded a result, so no-mark")
say("athletes never reach this join. Measured at the SOURCE below instead.")

# Measured where the truncation would actually happen: championship_results
# holds the unfiltered entrant list, no-marks included (that is how foul_rate
# is measured at all -- see calibrate.R's note that the input must be
# unfiltered). Count, per race, how many entrants carry no mark.
cat("\n==== CONFOUND, measured at source: no-mark rate in championship_results ====\n")
src <- ch[race_key %chin% unique(d$race_id), .(race_key, athlete_id, mark, place)]
if (nrow(src)) {
  src_disc <- merge(unique(src[, .(race_id = race_key, athlete_id, mark)]),
                    unique(d[, .(race_id, discipline)]), by = "race_id")
  nm <- src_disc[, .(entrants = .N, no_mark = sum(is.na(mark)),
                     no_mark_rate = round(mean(is.na(mark)), 3)), by = discipline]
  cmp2 <- merge(nm, disc_ratio, by = "discipline")[races >= 8L]
  setorder(cmp2, ratio)
  print(cmp2, row.names = FALSE)
  if (nrow(cmp2) >= 5 && cmp2[, uniqueN(no_mark_rate)] > 1) {
    ct <- suppressWarnings(cor.test(cmp2$no_mark_rate, cmp2$ratio, method = "spearman"))
    say("\nSpearman(no-mark rate, spread ratio) = %.3f, p = %.4g",
        unname(ct$estimate), ct$p.value)
    say("Strongly NEGATIVE => the low ratios are bad-tail truncation, not sigma.")
    say("Flat/NULL => truncation does not explain them and sigma_within really")
    say("is too wide for T1 finals in those events.")
  } else {
    say("\nno-mark rate does not vary across disciplines here (or too few cuts):")
    say("this corpus cannot settle the confound -- do NOT treat the ratio as a")
    say("property of the calibration until it is measured on unfiltered data.")
  }
} else say("no source rows matched; confound UNRESOLVED")

saveRDS(race_sd, file.path(OUT, "spread_vs_realised.rds"))
say("\nwrote spread_vs_realised.rds (%s races)", format(nrow(race_sd), big.mark = ","))
