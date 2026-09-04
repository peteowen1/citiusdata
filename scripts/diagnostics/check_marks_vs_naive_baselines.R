# Does the model's predicted mark beat naive SB / PB / last-5 guesses?
#
# Mirrors score_arm.R's leakage-safe construction exactly: history rolled to
# `date - 1L`, so every baseline sees only results STRICTLY BEFORE the race
# being predicted. perf = orientation * log(mark), higher is better, so PB/SB
# are cummax not cummin.
#
# TWO CHANGES, 2026-09-01 -- both fix ways this script overstated what it knew.
#
# (1) INFERENCE IS NOW RACE-CLUSTERED. This script used a plain paired t.test
#     over athlete-race rows. Rows within a race are not independent: a race is
#     one draw of shared conditions across 8-12 athletes, so the effective
#     sample is nearer the RACE count than the row count. Every model-vs-
#     baseline p-value this project has published came through the naive test.
#     See _cluster.R for the estimator (CR2 + Bell-McCaffrey dof) and for the
#     measured 8.6x case that prompted the change.
#
# (2) NO MORE COMPLETE-CASE DROPPING. The old A5 check achieved "every predictor
#     on the identical row set" by DELETING every row where any predictor was
#     undefined -- which silently restricted the whole comparison to athletes
#     with a full history, i.e. the population where naive baselines are
#     strongest. Each baseline now has an explicit fallback chain so every row
#     is scorable, the rung used is recorded per row, and results are reported
#     cut by rung. A model edge that lives only in weak-rung rows is a much
#     smaller claim than beating a well-informed last-5, and that distinction is
#     now visible rather than assumed away.
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  PB must be biased OPTIMISTIC (mean signed error > 0). A personal best
#       is a maximum; if it comes out unbiased the leakage guard has failed.
#   A2  SB must be biased optimistic too, but by LESS than PB.
#   A3  last-5 mean must be near-unbiased -- it is a central estimate.
#   A4  raw SB must be NA for an athlete's first race of a season. If nothing is
#       NA, the within-season restriction is not being applied. (Checked on the
#       PRE-fallback column; post-fallback SB is never NA by construction, so
#       this check must never be moved onto the filled column.)
#   A5  every predictor scored on the IDENTICAL row set -- now satisfied by
#       DEFINING every predictor everywhere rather than by dropping rows.
#   A6  every clustered SE must be >= its naive counterpart. A clustered SE that
#       comes out smaller means the clustering is misspecified, not that the
#       estimate is sharp.
#   A7  fallback rungs must be ordered by informativeness: rows falling back to
#       the event prior must have WORSE baseline MAE than rows with a full
#       last-5. If they do not, the chain is wired wrong.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_cluster.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2023-01-01")

# Which arm to score. Defaults to the arm this script was written against, so
# behaviour is unchanged for an existing caller; set CITIUS_MARKS_ARM to point
# it at another arm's output (added 2026-09-01 for the project_tier A/B).
ARM <- Sys.getenv("CITIUS_MARKS_ARM", "backtest_ctrl_now.rds")
cli::cli_alert_info("Scoring marks baselines against {.file {ARM}}")
b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation)], by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, class, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- history, exactly as score_arm.R builds it -----------------------------
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)

g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
hist[, pbrun := cummax(perf), by = g]                      # running personal best
hist[, yr := year(date)]
hist[, sbrun := cummax(perf), by = .(athlete_id, event_id, yr)]  # running season's best

# EVERY query below is asked at `date - 1L`. That one shift is the whole leakage
# guard, so it is built once here and reused, rather than re-typed per join --
# a re-typed shift is the easiest thing in this script to get wrong.
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id, yr = year(date))]
stopifnot(identical(q$date, d$date - 1L))                  # the shift, asserted

# last-5, PB, last-1: roll on (athlete, event, date). NA <=> no prior result in
# this event at all, which is what the fallback chain below is for.
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5, pbrun, lastperf = perf)]
m[, l5 := (cs - cs5) / pmin(k, 5)]

# SB: roll WITHIN the same calendar year, so no prior result that season -> NA
setkeyv(hist, c("athlete_id", "event_id", "yr", "date"))
s <- hist[q, on = .(athlete_id, event_id, yr, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, sbrun)]

# PREVIOUS season's best. Rolling on `yr` against the query year MINUS ONE finds
# the most recent season strictly before this one, so the whole of that season
# is before the race and the shift is not needed a second time.
sbst <- hist[, .(sb_year = max(perf)), by = .(athlete_id, event_id, yr)]
setkeyv(sbst, c("athlete_id", "event_id", "yr"))
qprev <- d[, .(athlete_id, event_id, yr = year(date) - 1L, race_id)]
pv <- sbst[qprev, on = .(athlete_id, event_id, yr), roll = TRUE, mult = "last",
           .(race_id, athlete_id, prev_sb = sb_year)]

# EVENT PRIOR -- the last rung. Expanding mean of the event over all history
# strictly before the race. Deliberately weak (it includes club runners); its
# job is to make a row scorable, not to be a good guess.
he <- hist[, .(event_id, date, perf)]
setorder(he, event_id, date)
he[, eprior := cumsum(perf) / seq_len(.N), by = event_id]
setkeyv(he, c("event_id", "date"))
ep <- he[d[, .(event_id, date = date - 1L, race_id, athlete_id)],
         on = .(event_id, date), roll = TRUE, mult = "last",
         .(race_id, athlete_id, eprior)]

kj <- c("race_id", "athlete_id")
d <- merge(d, m[, .(race_id, athlete_id, l5, pb = pbrun, last1 = lastperf, k)], by = kj)
d <- merge(d, s[, .(race_id, athlete_id, sb = sbrun)], by = kj)
d <- merge(d, unique(pv, by = kj), by = kj, all.x = TRUE)
d <- merge(d, unique(ep, by = kj), by = kj, all.x = TRUE)
d <- d[date >= HOLDOUT]

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d[, k := fifelse(is.na(k), 0L, k)]

# ---- leakage assertion: brute-force spot check ------------------------------
# The rolling joins are where a leak would hide, so a sample of rows is
# recomputed from scratch and compared. Structural checks on the query tables
# cannot catch a join that silently picked up a same-day result.
set.seed(1)
chk <- d[!is.na(last1)][sample(.N, min(200L, .N))]
setkeyv(hist, c("athlete_id", "event_id", "date"))
leak <- vapply(seq_len(nrow(chk)), function(i) {
  h <- hist[.(chk$athlete_id[i], chk$event_id[i]), nomatch = NULL]
  h <- h[date < chk$date[i]]                    # STRICTLY before the race
  if (!nrow(h)) return(NA_real_)
  max(abs(c(h[which.max(date)]$perf - chk$last1[i], max(h$perf) - chk$pb[i])))
}, numeric(1))
cat(sprintf("\nLEAKAGE SPOT CHECK  %d rows recomputed by brute force | max |delta| %.2e | %s\n",
            sum(!is.na(leak)), max(leak, na.rm = TRUE),
            if (max(leak, na.rm = TRUE) < 1e-9) "PASS" else "**FAIL - baselines see the race**"))
stopifnot(max(leak, na.rm = TRUE) < 1e-9)

# ---- fallback chains --------------------------------------------------------
# Each baseline degrades through explicit rungs instead of dropping the row.
# The rung is recorded so the comparison can be cut by it: a model edge that
# exists only where the baseline fell back to the event prior is a claim about
# thin histories, not about beating last-5.
d[, sb_raw := sb]                                  # pre-fallback, for anchor A4
d[, sb_rung := fcase(!is.na(sb),      "season",
                     !is.na(prev_sb), "prev_season",
                     !is.na(pb),      "career_pb",
                     default = "event_prior")]
d[, sb := fcoalesce(sb, prev_sb, pb, eprior)]

d[, l5_rung := fcase(!is.na(l5) & k >= 5L, "l5_full",
                     !is.na(l5),           "l5_partial",
                     !is.na(last1),        "last1",
                     !is.na(sb),           "sb",
                     default = "event_prior")]
d[, l5 := fcoalesce(l5, last1, sb, eprior)]

d[, pb_rung    := fifelse(!is.na(pb),    "career", "event_prior")]
d[, last1_rung := fifelse(!is.na(last1), "last",   "event_prior")]
d[, `:=`(pb = fcoalesce(pb, eprior), last1 = fcoalesce(last1, eprior))]

full <- d[!is.na(a_perf) & !is.na(act_perf) & !is.na(l5) & !is.na(sb) & !is.na(pb) & !is.na(last1)]

cat("\n==== ANCHOR CHECKS ====\n")
cat(sprintf("A4  raw SB undefined (first race of season): %s of %s rows (%.1f%%)  %s\n",
            format(sum(is.na(d$sb_raw)), big.mark = ","), format(nrow(d), big.mark = ","),
            100 * mean(is.na(d$sb_raw)),
            if (any(is.na(d$sb_raw))) "PASS" else "**FAIL - within-season restriction not applied**"))
cat(sprintf("A5  rows scorable by every predictor: %s of %s (%.1f%%) -- was complete-case, now fallback\n",
            format(nrow(full), big.mark = ","), format(nrow(d), big.mark = ","),
            100 * nrow(full) / nrow(d)))
if (nrow(full) < nrow(d))
  cat(sprintf("    %s rows still unscorable (no model prediction or no actual)\n",
              format(nrow(d) - nrow(full), big.mark = ",")))

cat("\n---- fallback rung usage (share of scorable rows) ----\n")
for (v in c("l5_rung", "sb_rung", "pb_rung", "last1_rung")) {
  tb <- full[, .N, by = v][order(-N)]
  cat(sprintf("  %-11s %s\n", sub("_rung", "", v),
              paste(sprintf("%s %s (%.1f%%)", tb[[v]], format(tb$N, big.mark = ","),
                            100 * tb$N / nrow(full)), collapse = " | ")))
}
# Some rungs are structurally unreachable and that is worth saying out loud, so
# a zero count is never mistaken for "the chain was never exercised".
cat("  NOTE: sb 'career_pb' and l5 'last1'/'sb' rungs are unreachable by",
    "construction --\n        a career PB exists iff some prior season does, and l5/last1/pb",
    "all derive from\n        the same join, so they are NA together.\n")

err <- function(p) 100 * (p - full$act_perf)     # % of a mark, model convention
E <- list(model = err(full$a_perf), last5 = err(full$l5), sb = err(full$sb),
          pb = err(full$pb), last1 = err(full$last1))

cat("\nA1-A3  mean signed error (%, +ve = predicted better than actual), race-clustered CI:\n")
for (nm in names(E)) {
  r <- cluster_stat(mean(E[[nm]]), E[[nm]] - mean(E[[nm]]), full$race_id)
  cat(sprintf("    %-6s %+7.3f  [%+6.3f, %+6.3f]  SE %.1fx naive\n",
              nm, r$est, r$lo, r$hi, r$infl))
}
cat(sprintf("    A1 %s | A2 %s | A3 %s\n",
            if (mean(E$pb) > 0) "PASS" else "**FAIL**",
            if (mean(E$sb) > 0 && mean(E$sb) < mean(E$pb)) "PASS" else "**FAIL**",
            if (abs(mean(E$last5)) < abs(mean(E$pb))) "PASS" else "**FAIL**"))

# ---- reporting --------------------------------------------------------------
# An empty or too-small population now prints LOUDLY. The old version returned
# invisibly below 25 races, which meant the "PRIMARY: majors" block printed
# NOTHING for its entire existence while majors were being excluded upstream --
# indistinguishable from a block that was never reached. A decision population
# must never be able to vanish silently.
MIN_RACES <- 25L
rep_pop <- function(dd, label) {
  nr <- uniqueN(dd$race_id)
  if (nr == 0L) {
    cat(sprintf("\n%s  |  ** EMPTY -- selector matched no rows. NOT SCORED. **\n", label))
    return(invisible(NULL))
  }
  idx <- which(full$race_id %in% dd$race_id)
  if (nr < MIN_RACES || !length(idx)) {
    cat(sprintf("\n%s  |  ** ONLY %d races (<%d) -- TOO FEW TO TEST. NOT SCORED. **\n",
                label, nr, MIN_RACES))
    return(invisible(NULL))
  }
  e  <- lapply(E, function(v) v[idx])
  ec <- lapply(e, function(v) v - mean(v, na.rm = TRUE))
  cl <- full$race_id[idx]
  cat(sprintf("\n%s  |  %d races, %s predictions (%.1f rows/race)\n", label,
              uniqueN(cl), format(length(idx), big.mark = ","), length(idx) / uniqueN(cl)))
  cat(sprintf("  %-8s %9s %9s %9s %9s\n", "", "MAE", "RMSE", "MAE ctr", "RMSE ctr"))
  for (nm in names(e))
    cat(sprintf("  %-8s %9.4f %9.4f %9.4f %9.4f\n", nm,
                mean(abs(e[[nm]])), sqrt(mean(e[[nm]]^2)),
                mean(abs(ec[[nm]])), sqrt(mean(ec[[nm]]^2))))
  # Model vs each naive predictor, RAW and CENTRED, race-clustered. Raw is
  # reported too because centring removes exactly the level error a per-event
  # offset is meant to fix, so a centred-only table cannot see that work.
  for (nm in c("last5", "last1", "sb", "pb")) {
    rr <- cluster_rel_mae(e$model,  e[[nm]],  cl)
    rc <- cluster_rel_mae(ec$model, ec[[nm]], cl)
    stopifnot(rr$infl >= 0.999, rc$infl >= 0.999)          # A6
    cat(sprintf("    vs %-5s raw MAE %s\n", nm, fmt_cl(rr)))
    cat(sprintf("    vs %-5s ctr MAE %s\n", nm, fmt_cl(rc)))
  }
}

cli::cli_h1("Predicted mark vs naive baselines (deployed model, holdout {HOLDOUT})")
rep_pop(full[class %in% c("olympics", "world_champs", "commonwealth")], "PRIMARY: majors")
rep_pop(full[meet_tier == "T1_elite"], "DECISIONS: T1 elite")
rep_pop(full[meet_tier == "T2_strong"], "T2 strong")
rep_pop(full, "CONTEXT: all scored finals")

# ---- is the edge only in weak-baseline rows? --------------------------------
cli::cli_h1("Model vs last-5, cut by the rung last-5 fell back to")
cat("A7: baseline MAE must WORSEN down the rungs, or the chain is wired wrong.\n")
for (rg in c("l5_full", "l5_partial", "event_prior")) {
  dd <- full[l5_rung == rg]
  if (!nrow(dd)) { cat(sprintf("  %-12s ** no rows **\n", rg)); next }
  i <- which(full$l5_rung == rg)
  if (uniqueN(full$race_id[i]) < MIN_RACES) {
    cat(sprintf("  %-12s ** only %d races -- TOO FEW TO TEST (%s rows) **\n",
                rg, uniqueN(full$race_id[i]), format(length(i), big.mark = ",")))
    next
  }
  rr <- cluster_rel_mae(E$model[i], E$last5[i], full$race_id[i])
  cat(sprintf("  %-12s last5 MAE %7.4f | model MAE %7.4f | %s\n",
              rg, mean(abs(E$last5[i])), mean(abs(E$model[i])), fmt_cl(rr)))
}
