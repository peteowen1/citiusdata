# The scorer the framework mandates. One arm in, six metrics out, three
# populations, always against the five-race baseline.
#
# Replaces the ad-hoc scoring that let a week of tuning optimise Brier over
# 3,621 mixed races -- of which 172 were majors -- against a uniform-within-race
# prior that is trivial to beat. See docs/plans/OPTIMISATION-FRAMEWORK.md.
#
#   the pick, gold    gold Brier      gold logloss
#   the pick, medal   medal Brier     medal logloss
#   the mark          MAE             RMSE        (raw and centred)
#
# Within each pair the first is robust and the second is tail-sensitive, and the
# GAP between them is the diagnostic: flat Brier with moving logloss means the
# change touched confidence, not ordering.
#
# Usage:
#   CITIUS_SCORE_ARM=backtest_crob.rds Rscript scripts/score_arm.R
#   CITIUS_SCORE_ARM=backtest_casym.rds CITIUS_SCORE_VS=backtest_crob.rds ...
#
# With CITIUS_SCORE_VS the comparison is arm-vs-arm; without it, arm-vs-baseline.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")

ARM <- Sys.getenv("CITIUS_SCORE_ARM", "backtest_crob.rds")
VS  <- Sys.getenv("CITIUS_SCORE_VS", "")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_SCORE_HOLDOUT", "2023-01-01"))

arm_meta <- new.env(parent = emptyenv())

load_arm <- function(f) {
  b <- readRDS(file.path(OUT, f))
  assign(f, b$meta, envir = arm_meta)
  merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                         p_gold, p_medal, median_mark)],
        as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal)],
        by = c("race_id", "athlete_id"))
}
d <- load_arm(ARM)
setnames(d, c("p_gold", "p_medal", "median_mark"), c("a_gold", "a_medal", "a_mark"))

# AN ARM TOO SMALL TO MEAN ANYTHING MUST NOT PRODUCE A SCORECARD.
#
# backtest_athletics.R caps meets per run at CITIUS_BT_MEETS, which DEFAULTS TO
# 25. Forget to set it and the arm finishes early, writes a perfectly well-formed
# artefact, and scores clean -- on a sample that cannot separate anything. There
# is no error anywhere in that chain, which is what makes it dangerous: the
# output looks exactly like a real result. Caught in review on 2026-07-31 before
# a three-arm queue ran that way.
#
# Same principle as withholding meet strength below five scored events: when the
# sample is too thin, decline rather than report.
MIN_RACES <- as.integer(Sys.getenv("CITIUS_SCORE_MIN_RACES", "200"))
n_races <- uniqueN(d$race_id)
if (n_races < MIN_RACES) {
  cli::cli_abort(c(
    "x" = "{.file {ARM}} scored only {n_races} race{?s}; refusing to report.",
    "i" = "Almost always CITIUS_BT_MEETS unset (it defaults to 25). Re-run the
           arm with it set, or lower {.envvar CITIUS_SCORE_MIN_RACES} if this
           small a sample is genuinely what you want."))
}

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation, family)], by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d <- merge(d, cat_tbl[, .(competition_id, class, strength, meet_tier)],
           by = "competition_id", all.x = TRUE)

if (nzchar(VS)) {
  o <- load_arm(VS)

  # AN ARM COMPARISON IS ONLY AN ARM COMPARISON IF BOTH ARMS SAW THE SAME DATA.
  #
  # The history store is rebuilt whenever a harvest lands, so two arms run days
  # apart differ in their input corpus as well as in the variable under test,
  # and the scorer cannot tell those apart. On 2026-07-31 this produced six
  # supposedly-independent arms all showing an identical -1.7% marks gain over
  # the reference -- which was the gap between two history vintages, not any
  # arm's effect. Every one of them was simultaneously WORSE on gold Brier,
  # which is the tell: real single-variable effects do not move in lockstep.
  #
  # backtest_athletics.R has always recorded history_md5. Nothing read it.
  # Fingerprint whatever the run ACTUALLY read. When the parquet store exists
  # the .rds is never opened, so comparing history_md5 alone can pass two arms
  # that read different stores. Arms written before history_source existed fall
  # back to the .rds hash, which is what they really used.
  vintage <- function(f) {
    m <- get(f, envir = arm_meta)
    if (is.null(m$history_md5)) return(NULL)
    if (identical(m$history_source, "store")) c(m$history_md5, m$store_md5) else m$history_md5
  }
  # A T1-only arm and an all-tier arm score different meet pools, so their
  # numbers are not comparable even on the T1 block -- the pool selection is
  # evenly spaced across time, so narrowing it changes WHICH T1 meets are in.
  tf <- function(f) {
    m <- get(f, envir = arm_meta)
    v <- m$tier_filter
    v <- if (is.null(v) || is.na(v)) "all" else v
    # Elite-history screening mode restricts the history, so it belongs in the
    # same comparability key as the meet pool: a screening arm and a full arm
    # are not measuring the same model.
    paste0(v, if (isTRUE(m$elite_history)) " +elite-history" else "")
  }
  if (!identical(tf(ARM), tf(VS))) {
    cli::cli_abort(c(
      "x" = "{.file {ARM}} and {.file {VS}} were scored on different meet pools.",
      "*" = "{ARM}: tier filter {tf(ARM)}",
      "*" = "{VS}: tier filter {tf(VS)}",
      "i" = "Re-run one with the other's {.envvar CITIUS_BT_TIER}."))
  }
  h_arm <- vintage(ARM); h_vs <- vintage(VS)
  if (is.null(h_arm) || is.null(h_vs)) {
    cli::cli_abort(c("x" = "{.file {if (is.null(h_arm)) ARM else VS}} has no {.field history_md5}.",
                     "i" = "Pre-dates provenance tracking; re-run it before comparing."))
  }
  if (!identical(h_arm, h_vs)) {
    cli::cli_abort(c(
      "x" = "{.file {ARM}} and {.file {VS}} were built on different history vintages.",
      "*" = "{ARM}: {paste(substr(h_arm, 1, 8), collapse = '/')}",
      "*" = "{VS}: {paste(substr(h_vs, 1, 8), collapse = '/')}",
      "i" = "Any difference confounds the arm variable with the data. Re-run one
             on the other's corpus, or score both against the baseline instead."))
  }
  setnames(o, c("p_gold", "p_medal", "median_mark"), c("b_gold", "b_medal", "b_mark"))
  d <- merge(d, o[, .(race_id, athlete_id, b_gold, b_medal, b_mark)],
             by = c("race_id", "athlete_id"))
  BLAB <- VS
} else {
  # The baseline: last five oriented performances, one global sigma fitted on
  # the pre-holdout period, through our own simulator. No shrinkage, no context,
  # no aging, no per-athlete uncertainty.
  hist <- deployed_history(OUT, events = unique(d$event_id),
                           from = min(d$date) - 3650, to = max(d$date))
  hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
  setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
  hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
  q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
  setkeyv(hist, c("athlete_id", "event_id", "date"))
  m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
            .(race_id, athlete_id, k, cs, cs5)]
  m[, l5 := (cs - cs5) / pmin(k, 5)]
  d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
  # Two baseline flavours. `global` fits ONE sigma for the whole sport, which
  # is the crudest honest baseline. `event` gives it the measured per-event
  # sigma_within instead -- still trivially simple, no shrinkage and no context,
  # but no longer artificially over-confident. The second matters because the
  # logloss gap between model and baseline IS an over-confidence gap, so a
  # baseline handicapped on uncertainty would flatter us.
  BASE <- Sys.getenv("CITIUS_SCORE_BASE", "event")
  # Restricted to CALIBRATED events. An uncalibrated event still carries a
  # `sigma_within`, but it is the registry's `cv_prior` placeholder rather than
  # anything measured -- so taking the column wholesale hands the baseline a
  # guessed spread for those events AND contaminates the fallback median with
  # guesses. The baseline is what the arm is judged against, so a wrong sigma
  # there moves the verdict in a direction nothing reports.
  ev_all <- as.data.table(deployed_calibration(OUT)$events)
  evs <- ev_all[calibrated %in% TRUE, .(event_id, sigma_within)]
  if (!nrow(evs)) {
    cli::cli_abort(c(
      "x" = "No calibrated events in the deployed calibration.",
      "i" = "The per-event baseline sigma would be entirely placeholder values."))
  }
  cli::cli_alert_info(
    "Baseline sigma from {nrow(evs)} calibrated event{?s} of {nrow(ev_all)}; \\
     uncalibrated events take the calibrated median.")
  d <- merge(d, evs, by = "event_id", all.x = TRUE)
  d[!is.finite(sigma_within), sigma_within := median(evs$sigma_within, na.rm = TRUE)]
  sim <- function(dd, sg, n) rbindlist(lapply(split(dd, dd$race_id), function(r) {
    sig <- if (BASE == "event") r$sigma_within else sg
    ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                     ability = r$l5, sigma = sig)
    mp <- medal_probs(simulate_event(ab, n_sims = n, condition_sd = 0, seed = 11L))
    data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
               b_gold = mp$p_gold, b_medal = mp$p_medal)
  }))
  # Only the `global` baseline has a parameter to fit. Running the grid under
  # `event` wasted a minute and, with an early holdout, crashed on an empty
  # training set -- a fit for a value nothing then used.
  SG <- NA_real_
  if (BASE != "event") {
    tr <- d[date < HOLDOUT]
    if (!nrow(tr)) cli::cli_abort(c(
      "No races before {HOLDOUT} to fit the global baseline sigma on.",
      i = "Use {.code CITIUS_SCORE_BASE=event}, which fits nothing, or move the holdout."))
    fit <- rbindlist(lapply(seq(0.012, 0.024, by = 0.004), function(s) {
      x <- merge(tr, sim(tr, s, 1200L), by = c("race_id", "athlete_id"))
      data.table(sigma = s, brier = mean((x$b_gold - x$hit)^2))}))
    SG <- fit$sigma[which.min(fit$brier)]
  }
  cli::cli_alert_info(if (BASE == "event")
    "Baseline sigma: measured per-event sigma_within." else
    "Baseline sigma: one global value fitted on pre-{HOLDOUT} races: {SG}")
  d <- merge(d, sim(d, SG, 4000L), by = c("race_id", "athlete_id"))
  d[, b_mark := exp(l5 / orientation)]
  BLAB <- paste0("last-5 baseline (", BASE, " sigma)")
}

d <- d[date >= HOLDOUT]

# Re-assert the floor on what is ACTUALLY SCORED, not on what was predicted.
#
# The check at the top runs before the merge to real outcomes, before the
# catalogue join and before this holdout filter, so it counts races the arm made
# a prediction for -- not races that survive to contribute a number. An arm can
# clear 200 predicted races there and report on 30 here, which is precisely the
# thin-sample-reported-as-real failure the first guard exists to prevent, just
# displaced past the point where the sample is decided.
n_scored <- uniqueN(d$race_id)
if (n_scored < MIN_RACES) {
  cli::cli_abort(c(
    "x" = "{.file {ARM}} has {n_scored} race{?s} left after the holdout filter
           and the merge to actual results; refusing to report.",
    "i" = "{n_races} race{?s} were predicted, so the loss is in the outcome
           merge or {.envvar CITIUS_BT_HOLDOUT}, not in the backtest itself.",
    "i" = "Lower {.envvar CITIUS_SCORE_MIN_RACES} if this small a sample is
           genuinely what you want."))
}

d[, `:=`(act_perf = orientation * log(actual),
         a_perf = orientation * log(a_mark), b_perf = orientation * log(b_mark))]
EPS <- 1e-4
llf <- function(p, y) { p <- pmin(pmax(p, EPS), 1 - EPS); -(y * log(p) + (1 - y) * log(1 - p)) }

pop <- function(dd, label) {
  nr <- uniqueN(dd$race_id); if (nr < 25) return(invisible(NULL))
  pair <- function(am, bm, nm) {
    t <- t.test(bm, am, paired = TRUE)
    sprintf("  %-16s %9.5f  %9.5f   %+7.2f%%  %s  p=%.3g", nm, mean(am), mean(bm),
            100 * (mean(am) - mean(bm)) / mean(bm),
            ifelse(mean(am) < mean(bm), "ARM ", "base"), t$p.value)
  }
  byrace <- function(f) dd[, .(a = f(.SD, "a"), b = f(.SD, "b")), by = race_id]
  gB <- byrace(function(s, p) mean((s[[paste0(p, "_gold")]] - s$hit)^2))
  gL <- byrace(function(s, p) mean(llf(s[[paste0(p, "_gold")]], s$hit)))
  mB <- byrace(function(s, p) mean((s[[paste0(p, "_medal")]] - s$hit_medal)^2))
  mL <- byrace(function(s, p) mean(llf(s[[paste0(p, "_medal")]], s$hit_medal)))
  ea <- 100 * (dd$a_perf - dd$act_perf); eb <- 100 * (dd$b_perf - dd$act_perf)
  eac <- ea - mean(ea, na.rm = TRUE); ebc <- eb - mean(eb, na.rm = TRUE)
  # `post` turns the mean of the per-prediction quantity into the reported
  # statistic: identity for MAE, sqrt for RMSE. The paired test still runs on
  # the untransformed quantities, which is where the pairing lives.
  mk <- function(x, y, nm, f, post = identity) {
    t <- t.test(f(y), f(x), paired = TRUE)
    ax <- post(mean(f(x), na.rm = TRUE)); ay <- post(mean(f(y), na.rm = TRUE))
    sprintf("  %-16s %9.4f  %9.4f   %+7.2f%%  %s  p=%.3g", nm, ax, ay,
            100 * (ax - ay) / ay, ifelse(ax < ay, "ARM ", "base"), t$p.value)
  }
  cat(sprintf("\n%s  |  %d races, %s predictions\n", label, nr, format(nrow(dd), big.mark = ",")))
  cat(sprintf("  %-16s %9s  %9s   %8s\n", "", "arm", "base", "rel"))
  cat(pair(gB$a, gB$b, "gold Brier"), "\n")
  cat(pair(gL$a, gL$b, "gold logloss"), "\n")
  cat(pair(mB$a, mB$b, "medal Brier"), "\n")
  cat(pair(mL$a, mL$b, "medal logloss"), "\n")
  cat(mk(ea, eb, "marks MAE", abs), "\n")
  cat(mk(ea, eb, "marks RMSE", function(v) v^2, sqrt), "\n")
  cat(mk(eac, ebc, "marks MAE ctr", abs), "\n")
  cat(mk(eac, ebc, "marks RMSE ctr", function(v) v^2, sqrt), "\n")
  fa <- dd[, .(w = athlete_id[which.max(a_gold)] == athlete_id[hit == TRUE][1]), by = race_id]
  fb <- dd[, .(w = athlete_id[which.max(b_gold)] == athlete_id[hit == TRUE][1]), by = race_id]
  cat(sprintf("  %-16s %8.1f%%  %8.1f%%\n", "favourite wins",
              100 * mean(fa$w, na.rm = TRUE), 100 * mean(fb$w, na.rm = TRUE)))
}
cli::cli_h1("{ARM} vs {BLAB}   (holdout from {HOLDOUT})")
# Populations by TIER, not by class. T1 is the tier the catalogue assigns, so it
# picks up the strong meets we could not classify by name as well as the ones we
# could -- which is what "elite racing" actually means. Majors stay separate
# because they are the goal, but 86 test races cannot resolve a 1% effect, so
# decisions are made on T1 and confirmed on majors.
MAJ <- c("olympics", "world_champs", "commonwealth")
pop(d[class %in% MAJ], "PRIMARY: majors (the goal)")
pop(d[meet_tier == "T1_elite"], "DECISIONS: T1 elite")
pop(d[meet_tier == "T2_strong"], "T2 strong")
pop(d, "CONTEXT: all scored finals")
