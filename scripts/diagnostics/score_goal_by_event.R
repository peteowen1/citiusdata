# THE GOAL SCORER (Pete, 2026-09-05): does EVERY event beat the last-5
# baseline on BOTH marks MAE and medal logloss, on the WAC T1 test set
# since 2020?
#
# WHY A NEW SCRIPT. score_arm.R answers this pooled, over three populations.
# Pooled is the wrong resolution for this goal: today's WAC promotion was a
# pooled win that still had ~19 of 67 events going BACKWARDS, so "the arm wins"
# and "every event wins" are different claims and only the second one is the
# goal. score_by_event.R is per-event but scores concordance between two arms,
# not either metric here, and never touches a baseline.
#
# THE BASELINE IS COPIED FROM score_arm.R, DELIBERATELY, NOT RE-DERIVED.
# Its last-5 construction (roll to the last 5 prior oriented performances, feed
# them through OUR simulator at the measured per-event sigma_within, no
# shrinkage/context/aging/per-athlete uncertainty) is the framework's mandated
# baseline. Re-deriving it here is how the two silently drift apart and a goal
# gets declared met against a baseline nobody else uses. If score_arm.R's
# baseline changes, change this with it.
#
# Usage:
#   Rscript citiusdata/scripts/diagnostics/score_goal_by_event.R
#   CITIUS_GOAL_ARM=backtest_wac_trt_0904.rds CITIUS_GOAL_FROM=2020-01-01 ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")

ARM      <- Sys.getenv("CITIUS_GOAL_ARM", "backtest_wac_trt_0904.rds")
FROM     <- as.Date(Sys.getenv("CITIUS_GOAL_FROM", "2020-01-01"))
TIER     <- Sys.getenv("CITIUS_GOAL_TIER", "T1_elite")
# An event with a handful of races cannot pass or fail this goal, it can only
# produce a number. Reported separately rather than counted either way -- the
# same "decline when the sample is too thin" rule score_arm.R applies to whole
# arms and the catalogue applies to meet strength.
MIN_RACES <- .env_int("CITIUS_GOAL_MIN_RACES", "10")
NSIM      <- .env_int("CITIUS_GOAL_NSIM", "4000")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

b <- readRDS(file.path(OUT, ARM))
say(sprintf("arm %s | tier_filter %s | %s races scored", ARM,
            (function(x) if (is.null(x) || is.na(x)) "none" else x)(b$meta$tier_filter),
            format(b$meta$races_scored, big.mark = ",")))
# MARKS_ONLY arms carry NA probabilities by construction, so half this goal
# would silently evaluate to nothing. Fail loudly -- UNLESS the caller has said
# explicitly that it wants the marks half alone.
#
# CITIUS_GOAL_MARKS_ONLY=1 is legitimate for exactly one class of arm: levers
# proven incapable of touching a probability. project_tier and the family-pool
# debias are that class -- p_medal is bit-for-bit identical whether they are on
# or off -- so the medal-logloss column from the full-sim arm still applies and
# re-simulating it would burn hours to reproduce identical numbers. It is NOT a
# general shortcut: use it on an arm that can move probabilities and the medal
# half of this goal silently becomes a stale copy of a different model's.
MARKS_ONLY_MODE <- .env_int("CITIUS_GOAL_MARKS_ONLY", "0") == 1
if (all(is.na(as.data.table(b$predictions)$p_medal))) {
  if (!MARKS_ONLY_MODE) cli::cli_abort(c(
    "x" = "{.file {ARM}} has no medal probabilities (a MARKS_ONLY arm).",
    "i" = "The medal-logloss half of this goal cannot be scored from it.",
    "i" = "Set {.envvar CITIUS_GOAL_MARKS_ONLY=1} only if this arm's levers
           provably cannot move a probability."))
  cli::cli_alert_warning(
    "MARKS-ONLY MODE: scoring the marks half only. The medal-logloss half of
     the goal is NOT evaluated here.")
}

d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_gold = p_gold, a_medal = p_medal, a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal)],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, as.data.table(citius_events())[, .(event_id, orientation, family, discipline, sex)],
           by = "event_id")

cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
d[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

n_before <- nrow(d)
d <- d[meet_tier == TIER & date >= FROM]
say(sprintf("test set: %s -> %s predictions (%s, from %s), %s races, %d events",
            format(n_before, big.mark = ","), format(nrow(d), big.mark = ","),
            TIER, FROM, format(uniqueN(d$race_id), big.mark = ","), uniqueN(d$event_id)))
if (!nrow(d)) cli::cli_abort("Test set is empty; check the tier label and date window.")

# --- the last-5 baseline, lifted from score_arm.R ---------------------------
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
setDT(hist)
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
n_pre <- nrow(d)
d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
say(sprintf("last-5 baseline available for %s of %s predictions (%.1f%%)",
            format(nrow(d), big.mark = ","), format(n_pre, big.mark = ","),
            100 * nrow(d) / n_pre))

# FREE THE HISTORY BEFORE SIMULATING. `hist` is every result for all 70 events
# over ten years -- millions of rows -- and it is needed only to build the `l5`
# column just merged above. Holding it through the simulation loop is what made
# the first run thrash: on a box with 0.6 GB free, Windows paged the process
# almost entirely to disk and it fell to ~1% CPU, alive but not progressing.
# Nothing below this line reads hist/m/q.
rm(hist, m, q); invisible(gc())
say(sprintf("history released; %.2f GB in use", sum(gc()[, 2]) / 1024))

ev_all <- as.data.table(deployed_calibration(OUT)$events)
evs <- ev_all[calibrated %in% TRUE, .(event_id, sigma_within)]
if (!nrow(evs)) cli::cli_abort("No calibrated events; the baseline sigma would be all placeholder.")
d <- merge(d, evs, by = "event_id", all.x = TRUE)
d[!is.finite(sigma_within), sigma_within := median(evs$sigma_within, na.rm = TRUE)]
say(sprintf("baseline sigma: measured per-event sigma_within (%d calibrated of %d)",
            nrow(evs), nrow(ev_all)))

# CHECKPOINTED, because this box kills long jobs under memory pressure.
#
# The baseline simulation is the only expensive step here (NSIM draws per race,
# thousands of races) and it has no natural resume point. Four runs were killed
# mid-flight on 2026-09-04; each one that lacked a cache lost everything it had
# computed. Written in chunks so a kill costs at most one chunk.
#
# The cache is keyed on the RACE SET and NSIM, not just a filename: a different
# test set or a different sim count is a different baseline, and silently
# reading one back as the other is the same failure shape as
# backtest_athletics.R's arm-fingerprint trap.
CKPT <- file.path(OUT, "goal_baseline_ckpt.rds")
race_ids <- sort(unique(d$race_id))
# The baseline SIMULATION exists only to give the baseline medal probabilities.
# The baseline MARK is closed-form (exp(l5 / orientation)) and needs none of
# it, so marks-only mode skips the expensive half entirely.
if (MARKS_ONLY_MODE) {
  say("marks-only mode: skipping the baseline simulation (needed only for medal probs)")
  bs <- data.table(race_id = character(0), athlete_id = character(0),
                   b_gold = numeric(0), b_medal = numeric(0))
  race_ids <- character(0)
}
key <- list(n_races = length(race_ids), nsim = NSIM, from = as.character(FROM),
            tier = TIER, arm = ARM,
            digest = sum(utf8ToInt(paste(race_ids, collapse = ""))))
bs <- NULL
if (file.exists(CKPT)) {
  prev <- tryCatch(readRDS(CKPT), error = function(e) NULL)
  if (!is.null(prev) && identical(prev$key, key)) {
    bs <- prev$bs
    say(sprintf("resuming: %s of %s races already simulated",
                format(uniqueN(bs$race_id), big.mark = ","),
                format(length(race_ids), big.mark = ",")))
  } else if (!is.null(prev)) {
    say("checkpoint exists but is for a DIFFERENT test set/NSIM; ignoring it")
  }
}
todo_ids <- setdiff(race_ids, if (is.null(bs)) character(0) else unique(bs$race_id))
if (length(todo_ids)) {
  say(sprintf("simulating the baseline field for %s race%s ...",
              format(length(todo_ids), big.mark = ","),
              if (length(todo_ids) == 1) "" else "s"))
  CHUNK <- 250L
  chunks <- split(todo_ids, ceiling(seq_along(todo_ids) / CHUNK))
  for (ci in seq_along(chunks)) {
    part <- rbindlist(lapply(split(d[race_id %chin% chunks[[ci]]], by = "race_id"), function(r) {
      ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                       ability = r$l5, sigma = r$sigma_within)
      mp <- medal_probs(simulate_event(ab, n_sims = NSIM, condition_sd = 0, seed = 11L))
      data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
                 b_gold = mp$p_gold, b_medal = mp$p_medal)
    }))
    bs <- if (is.null(bs)) part else rbindlist(list(bs, part))
    # Write-then-rename: an interrupt leaves either the old whole checkpoint or
    # the new whole one, never a truncated file that readRDS dies on.
    tmp <- paste0(CKPT, ".tmp")
    saveRDS(list(key = key, bs = bs), tmp)
    if (!isTRUE(file.rename(tmp, CKPT))) unlink(tmp)
    say(sprintf("  chunk %d/%d done (%s of %s races cached)", ci, length(chunks),
                format(uniqueN(bs$race_id), big.mark = ","),
                format(length(race_ids), big.mark = ",")))
  }
}
if (!MARKS_ONLY_MODE) {
  stopifnot("baseline simulation produced nothing" = !is.null(bs) && nrow(bs) > 0)
  d <- merge(d, bs, by = c("race_id", "athlete_id"))
}
d[, b_mark := exp(l5 / orientation)]

# --- the two goal metrics ---------------------------------------------------
eps <- 1e-6
ll <- function(p, y) -(y * log(pmax(p, eps)) + (1 - y) * log(pmax(1 - p, eps)))
if (MARKS_ONLY_MODE) {
  # NA, not 0 and not a copy of anything: an unscored metric must not be able
  # to look like a passing one further down.
  d[, `:=`(a_ll = NA_real_, b_ll = NA_real_)]
} else {
  d[, `:=`(a_ll = ll(a_medal, hit_medal), b_ll = ll(b_medal, hit_medal))]
}
d[is.finite(actual) & actual > 0, `:=`(
  a_ape = 100 * abs(a_mark - actual) / actual,
  b_ape = 100 * abs(b_mark - actual) / actual)]

per_ev <- d[, .(
  races      = uniqueN(race_id),
  n          = .N,
  mae_model  = round(mean(a_ape, na.rm = TRUE), 3),
  mae_base   = round(mean(b_ape, na.rm = TRUE), 3),
  mll_model  = round(mean(a_ll), 4),
  mll_base   = round(mean(b_ll), 4)),
  by = .(event_id, discipline, sex, family)]
per_ev[, `:=`(mae_delta = round(mae_model - mae_base, 3),
              mll_delta = round(mll_model - mll_base, 4))]
# Negative delta = model better = that half of the goal met for that event.
per_ev[, `:=`(mae_ok = mae_delta < 0, mll_ok = mll_delta < 0)]
# In marks-only mode goal_met is the MARKS half alone and is labelled as such
# in the output. An NA mll_ok would silently make `mae_ok & mll_ok` NA and drop
# every event out of both the pass and fail counts -- the vacuous-pass shape
# this repo keeps hitting.
per_ev[, goal_met := if (MARKS_ONLY_MODE) mae_ok else (mae_ok & mll_ok)]
setorder(per_ev, -mae_delta)

thin <- per_ev[races < MIN_RACES]
scoreable <- per_ev[races >= MIN_RACES]

cli::cli_h1("GOAL: every event beats last-5 on BOTH marks MAE and medal logloss")
cat(sprintf("test set: %s, %s+ | %s races | %s predictions\n",
            TIER, format(FROM, "%Y"), format(uniqueN(d$race_id), big.mark = ","),
            format(nrow(d), big.mark = ",")))
cat(sprintf("events: %d scoreable (>= %d races), %d too thin to judge\n\n",
            nrow(scoreable), MIN_RACES, nrow(thin)))

if (MARKS_ONLY_MODE) {
  cat(sprintf("MARKS HALF ONLY on %d of %d scoreable events (%.0f%%)\n",
              sum(scoreable$mae_ok), nrow(scoreable), 100 * mean(scoreable$mae_ok)))
  cat("  medal logloss: NOT SCORED in this mode -- the goal is not evaluated here.\n\n")
} else {
  cat(sprintf("GOAL MET on %d of %d scoreable events (%.0f%%)\n",
              sum(scoreable$goal_met), nrow(scoreable),
              100 * mean(scoreable$goal_met)))
  cat(sprintf("  marks MAE beaten:     %d of %d\n", sum(scoreable$mae_ok), nrow(scoreable)))
  cat(sprintf("  medal logloss beaten: %d of %d\n", sum(scoreable$mll_ok), nrow(scoreable)))
  cat(sprintf("  BOTH (the goal):      %d of %d\n\n", sum(scoreable$goal_met), nrow(scoreable)))
}

cli::cli_h3("events FAILING the goal, worst marks gap first")
print(scoreable[goal_met == FALSE, .(discipline, sex, family, races,
                                     mae_model, mae_base, mae_delta,
                                     mll_model, mll_base, mll_delta)])

cli::cli_h3("events MEETING the goal")
print(scoreable[goal_met == TRUE, .(discipline, sex, family, races,
                                    mae_delta, mll_delta)])

if (nrow(thin)) {
  cli::cli_h3(sprintf("too thin to judge (< %d races) -- counted neither way", MIN_RACES))
  print(thin[, .(discipline, sex, races, mae_delta, mll_delta)])
}

cli::cli_h3("pooled, for context only (the goal is per-event)")
cat(sprintf("marks MAE      model %.3f  base %.3f  %+.2f%%  p=%.3g\n",
            mean(d$a_ape, na.rm = TRUE), mean(d$b_ape, na.rm = TRUE),
            100 * (mean(d$a_ape, na.rm = TRUE) / mean(d$b_ape, na.rm = TRUE) - 1),
            stats::t.test(d$a_ape - d$b_ape)$p.value))
if (MARKS_ONLY_MODE) {
  cat("medal logloss  NOT SCORED in marks-only mode\n")
} else {
  cat(sprintf("medal logloss  model %.4f  base %.4f  %+.2f%%  p=%.3g\n",
              mean(d$a_ll), mean(d$b_ll),
              100 * (mean(d$a_ll) / mean(d$b_ll) - 1),
              stats::t.test(d$a_ll - d$b_ll)$p.value))
}

fwrite(per_ev, file.path(OUT, "goal_by_event.csv"))
say("wrote goal_by_event.csv")
