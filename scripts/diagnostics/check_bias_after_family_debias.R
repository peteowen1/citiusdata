# Does the family-pool debias fix already absorb the bias floor and throw's
# excess on top of it?
#
# check_attempt_structure_bias.R found, on the DEPLOYED calibration:
#   single-attempt track -1.50%, vertical jump -1.57%, horizontal jump -1.41%
#   -- three unrelated attempt structures on the same floor -- and throw at
#   -2.43%, roughly 0.9pp worse than the floor. Attempt structure is refuted as
#   the mechanism; the floor tracks power-vs-endurance instead, which is the
#   SAME split the family-pool debias fix was built against.
#
# So: is throw's bias still live after that fix, or did yesterday's work
# already close it?
#
# Population discipline: the two arms were run over different meet samples, so
# comparing their headline numbers directly would measure the population, not
# the fix. Both arms are restricted to the INTERSECTION of (race_id,
# athlete_id) and the shared-set size is asserted before anything is reported.
#   backtest_tierctrl.rds -- control arm for the tier sweep
#   backtest_tier05fd.rds -- project_tier shrink=0.5 + family-pool debias
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

load_arm <- function(fn) {
  b <- readRDS(file.path(D, fn))
  pred <- as.data.table(b$predictions); pred[, athlete_id := as.character(athlete_id)]
  outc <- as.data.table(b$outcomes);    outc[, athlete_id := as.character(athlete_id)]
  d <- merge(pred, outc, by = c("race_id", "athlete_id"))
  d <- d[merged == FALSE]
  list(d = d, meta = b$meta)
}

ctrl <- load_arm(Sys.getenv("CITIUS_FD_CTRL", "backtest_tierctrl.rds"))
fix  <- load_arm(Sys.getenv("CITIUS_FD_FIX",  "backtest_tier05fd.rds"))
say("ctrl arm: %s", ctrl$meta$calibration)
say("fix  arm: %s", fix$meta$calibration)

# The arms MUST differ. Byte-identical predictions would mean a flag never took
# effect, and every number below would be a vacuous pass.
mc <- merge(ctrl$d[, .(race_id, athlete_id, mm_ctrl = median_mark)],
            fix$d[, .(race_id, athlete_id, mm_fix = median_mark)],
            by = c("race_id", "athlete_id"))
say("shared (race, athlete) rows: %s", format(nrow(mc), big.mark = ","))
stopifnot("no shared rows -- the arms do not cover the same races" = nrow(mc) > 500)
moved <- mean(abs(mc$mm_ctrl - mc$mm_fix) > 1e-9)
say("rows where the prediction actually moved: %.1f%%", 100 * moved)
# Not >0.5: with the apply-date gate in place a debias arm differs from its
# tier-only control ONLY on post-holdout meets, which are a minority of the
# span. The era table below is the real check on which rows moved.
stopifnot("arms are identical -- a flag did not take effect" = moved > 0.001)

ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
ch[, `:=`(competition_id = as.character(competition_id), athlete_id = as.character(athlete_id))]
act <- unique(ch[!is.na(race_key) & !is.na(mark) & !is.na(place),
                 .(race_id = race_key, athlete_id, actual = mark, actual_place = place,
                   event_id, date, competition_id)])
ev <- as.data.table(citius_events())[, .(event_id, discipline, sex, orientation, family)]
cat_tbl <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]

prep <- function(d) {
  d <- merge(d, act, by = c("race_id", "athlete_id"))
  d <- merge(d, ev, by = "event_id")
  d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
  d[, bias_pct := orientation * (actual - median_mark) / median_mark * 100]
  d[, structure := fifelse(family == "throw", "best_of_6_throw",
                    fifelse(discipline %chin% c("Long Jump", "Triple Jump"), "best_of_6_horiz_jump",
                     fifelse(discipline %chin% c("High Jump", "Pole Vault"), "progressive_vertical",
                      fifelse(family %chin% c("sprint", "hurdles"), "single_attempt_track",
                       fifelse(family %chin% c("middle", "distance"), "endurance_control", "other")))))]
  d[]
}

dc <- prep(ctrl$d); df <- prep(fix$d)

# An athlete can appear twice under one race_key carrying DIFFERENT marks --
# the merged-race artefact this repo already excludes elsewhere, leaking back in
# through the actual-mark join rather than through `merged`. We cannot tell
# which mark is the real one, so both copies go: picking one would invent a
# result. Counted out loud, because a silent dedupe here would quietly change
# the population the verdict is read off.
dupe_report <- function(d, nm) {
  k <- d[, .N, by = .(race_id, athlete_id)][N > 1]
  say("%s: %s of %s (race, athlete) pairs are duplicated -- dropping both copies",
      nm, format(nrow(k), big.mark = ","),
      format(uniqueN(d, by = c("race_id", "athlete_id")), big.mark = ","))
  if (!nrow(k)) return(d)
  d[!k, on = .(race_id, athlete_id)]
}
dc <- dupe_report(dc, "ctrl"); df <- dupe_report(df, "fix")
stopifnot("ctrl still has duplicate (race, athlete) pairs" =
            !anyDuplicated(dc, by = c("race_id", "athlete_id")))
stopifnot("fix still has duplicate (race, athlete) pairs" =
            !anyDuplicated(df, by = c("race_id", "athlete_id")))
# Restrict BOTH to the shared set, after the actual-mark merge has had its say.
key <- merge(dc[, .(race_id, athlete_id)], df[, .(race_id, athlete_id)],
             by = c("race_id", "athlete_id"))
dc <- merge(dc, key, by = c("race_id", "athlete_id"))
df <- merge(df, key, by = c("race_id", "athlete_id"))
stopifnot("shared-set restriction left the arms different sizes" = nrow(dc) == nrow(df))
say("shared analysis population: %s predictions, %s races",
    format(nrow(dc), big.mark = ","), format(uniqueN(dc$race_id), big.mark = ","))

clustered <- function(x, race) {
  by_race <- data.table(x = x, race = race)[, .(m = mean(x), n = .N), by = race]
  n_r <- nrow(by_race)
  if (n_r < 2) return(list(est = mean(x), se = NA_real_))
  w <- by_race$n / sum(by_race$n)
  est <- sum(w * by_race$m)
  list(est = est, se = sqrt(sum(w^2 * (by_race$m - est)^2) * n_r / max(n_r - 1, 1)))
}

# Paired: the same rows in both arms, so the difference is differenced per row
# and clustered on race. This is the number that says whether the fix moved it.
compare <- function(sc, sf, label) {
  if (!nrow(sc)) return(NULL)
  a <- clustered(sc$bias_pct, sc$race_id)
  b <- clustered(sf$bias_pct, sf$race_id)
  m <- merge(sc[, .(race_id, athlete_id, bc = bias_pct)],
             sf[, .(race_id, athlete_id, bf = bias_pct)], by = c("race_id", "athlete_id"))
  dl <- clustered(m$bf - m$bc, m$race_id)
  data.table(cut = label, n = nrow(sc), races = uniqueN(sc$race_id),
             ctrl = a$est, fix = b$est, delta = dl$est, delta_se = dl$se,
             t = dl$est / dl$se,
             closer_to_zero = abs(b$est) < abs(a$est))
}

cuts <- setdiff(unique(dc$structure), "other")
cat("\n================ T1_elite: bias before vs after the fix ================\n")
out1 <- rbindlist(lapply(cuts, function(k)
  compare(dc[meet_tier == "T1_elite" & structure == k],
          df[meet_tier == "T1_elite" & structure == k], k)), fill = TRUE)
if (nrow(out1)) print(out1[order(ctrl)])

cat("\n================ all tiers pooled: bias before vs after ================\n")
out2 <- rbindlist(lapply(cuts, function(k)
  compare(dc[structure == k], df[structure == k], k)), fill = TRUE)
if (nrow(out2)) print(out2[order(ctrl)])

# --- three-arm check: is the overshoot a DOUBLE correction? ----------------
# Every cut above overshot past zero in the same direction, including the
# endurance control that was already near-unbiased. That is the signature of
# two level corrections stacking, not of one debias landing where it was fit.
# tier05 (project_tier only) sits between ctrl and tier05fd; if it already
# removes most of the bias, the family offsets are re-removing what the tier
# correction had taken out.
tri <- tryCatch(load_arm("backtest_tier05.rds"), error = function(e) NULL)
if (!is.null(tri)) {
  dt5 <- dupe_report(prep(tri$d), "tier05")
  k3 <- Reduce(function(a, b) merge(a, b, by = c("race_id", "athlete_id")),
               list(dc[, .(race_id, athlete_id)], df[, .(race_id, athlete_id)],
                    dt5[, .(race_id, athlete_id)]))
  a_ctrl <- merge(dc,  k3, by = c("race_id", "athlete_id"))
  a_fix  <- merge(df,  k3, by = c("race_id", "athlete_id"))
  a_t5   <- merge(dt5, k3, by = c("race_id", "athlete_id"))
  stopifnot("three-arm shared set is not aligned" =
              nrow(a_ctrl) == nrow(a_fix) && nrow(a_fix) == nrow(a_t5))
  cat("\n================ THREE ARMS, T1_elite (shared rows) ================\n")
  say("rows: %s | races: %s", format(nrow(a_ctrl), big.mark = ","),
      format(uniqueN(a_ctrl$race_id), big.mark = ","))
  tri_out <- rbindlist(lapply(cuts, function(k) {
    s1 <- a_ctrl[meet_tier == "T1_elite" & structure == k]
    s2 <- a_t5[meet_tier   == "T1_elite" & structure == k]
    s3 <- a_fix[meet_tier  == "T1_elite" & structure == k]
    if (!nrow(s1)) return(NULL)
    data.table(cut = k, n = nrow(s1),
               ctrl = clustered(s1$bias_pct, s1$race_id)$est,
               tier05 = clustered(s2$bias_pct, s2$race_id)$est,
               tier05fd = clustered(s3$bias_pct, s3$race_id)$est)
  }), fill = TRUE)
  if (nrow(tri_out)) {
    tri_out[, `:=`(tier_step = tier05 - ctrl, debias_step = tier05fd - tier05)]
    print(tri_out[order(ctrl)])
    say("")
    say("If tier_step and debias_step are both large and same-signed, the two")
    say("corrections are removing the same location bias twice.")
  }
}

# --- the population split that decides whether any of the above is readable --
# fit_family_pool_offsets.R fits on [2023-01-01, FIT_HOLDOUT) and its header
# says the table is applied by "every meet the backtest arm scores ON OR AFTER
# that date". The arm spans far more than that. So the pooled numbers above mix
# rows that SHOULD be corrected with rows that should not, and an overshoot
# read off the pool could be an artefact of pooling rather than a property of
# the fix. Split on the holdout date before believing anything.
#   pre-holdout rows that MOVED  => the apply-date guard is not holding
#   pre-holdout rows unchanged   => read the fix's effect off post-holdout only
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_FAMILY_POOL_FIT_HOLDOUT", "2025-01-01"))
say("\nfit holdout used for the split: %s", format(FIT_HOLDOUT))
mm <- merge(dc[, .(race_id, athlete_id, date, meet_tier, structure, b_ctrl = bias_pct)],
            df[, .(race_id, athlete_id, b_fix = bias_pct)],
            by = c("race_id", "athlete_id"))
mm[, era := fifelse(date < FIT_HOLDOUT, "pre_holdout (should be UNCHANGED)",
                                        "post_holdout (should be corrected)")]
mm[, moved := abs(b_fix - b_ctrl) > 1e-9]
cat("\n================ did the apply-date guard hold? ================\n")
print(mm[, .(rows = .N, races = uniqueN(race_id),
             pct_rows_moved = round(100 * mean(moved), 1),
             mean_shift_pp = round(mean(b_fix - b_ctrl), 3)), by = era][order(era)])

cat("\n================ bias by structure, SPLIT on the holdout date ================\n")
split_out <- mm[structure != "other",
                .(n = .N, races = uniqueN(race_id),
                  ctrl = round(mean(b_ctrl), 3), fix = round(mean(b_fix), 3),
                  delta = round(mean(b_fix - b_ctrl), 3)),
                by = .(era, structure)]
setorder(split_out, era, ctrl)
print(split_out)

cat("\n================ post-holdout only, T1_elite (the honest read) ================\n")
post_t1 <- mm[date >= FIT_HOLDOUT & meet_tier == "T1_elite" & structure != "other"]
if (nrow(post_t1)) {
  po <- rbindlist(lapply(split(post_t1, post_t1$structure), function(s) {
    a <- clustered(s$b_ctrl, s$race_id); b <- clustered(s$b_fix, s$race_id)
    dl <- clustered(s$b_fix - s$b_ctrl, s$race_id)
    data.table(cut = s$structure[1], n = nrow(s), races = uniqueN(s$race_id),
               ctrl = a$est, fix = b$est, delta = dl$est, t = dl$est / dl$se,
               closer_to_zero = abs(b$est) < abs(a$est))
  }), fill = TRUE)
  print(po[order(ctrl)])
} else say("no post-holdout T1_elite rows")

cat("\n================ VERDICT ================\n")
src <- if (nrow(out1) >= 3) out1 else out2
if (nrow(src)) {
  thr <- src[cut == "best_of_6_throw"]
  floor_cuts <- src[cut %chin% c("single_attempt_track", "progressive_vertical", "best_of_6_horiz_jump")]
  if (nrow(thr) && nrow(floor_cuts)) {
    say("throw:         %+.2f%% -> %+.2f%%  (delta %+.2f pp, t=%.1f)",
        thr$ctrl, thr$fix, thr$delta, thr$t)
    say("floor (mean of 3 power cuts): %+.2f%% -> %+.2f%%",
        mean(floor_cuts$ctrl), mean(floor_cuts$fix))
    say("throw's EXCESS over the floor: %+.2f pp -> %+.2f pp",
        thr$ctrl - mean(floor_cuts$ctrl), thr$fix - mean(floor_cuts$fix))
    say("")
    say("If the excess is gone, throw is not a live finding and the campaign's")
    say("#1 priority is stale. If the floor moved but the excess did not, throw")
    say("still needs its own mechanism.")
  }
}
