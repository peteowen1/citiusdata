# Is the thing that shipped the thing that was measured?
#
# The marks lab licensed the recency blend on a baseline defined as: the mean
# of an athlete's last five RAW stored marks before the target date, minimum
# three. The package now computes `recent_mean` inside estimate_ability(). If
# those two definitions have drifted -- a decay creeping in, the tactical trim
# applying, the context adjustment applying, a different window -- then the
# deployed term is not the one with 30-of-35 evidence behind it and the
# evidence does not transfer.
#
# This reads the store directly, recomputes the baseline the lab's way, and
# compares. It also asserts the two things the blend's safety argument rests
# on: that `ability` is bit-identical with the blend on and off, and that
# `median_mark` is not.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/check_marks_blend_wiring.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
FAIL <- 0L
check <- function(ok, msg) {
  cat(sprintf("%s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", msg))
  if (!isTRUE(ok)) FAIL <<- FAIL + 1L
}

EVENTS <- c("AT-100Metres-M", "AT-100Metres-W", "AT-LongJump-W")
AS_OF  <- as.Date("2025-07-01")
STORE <- file.path(here::here("citiusdata", "data"),
                   Sys.getenv("CITIUS_BT_STORE", "athletics_corpus_store"))
# Same column set the lab's prep asks for, intersected the same way, so the
# comparison cannot differ because one side saw a column the other did not.
cols <- intersect(c("athlete_id", "event_id", "date", "perf", "age", "round",
                    "tier", "meet_tier", "competition_id", "race_key", "wind",
                    "momentum", "indoor", "venue_country"),
                  names(arrow::open_dataset(STORE)))
h <- as.data.table(read_results_store(
  STORE, events = EVENTS, from = AS_OF - DEPLOYED$history_days, to = AS_OF,
  columns = cols))
h <- flag_implausible(h)[is.finite(perf)]
h[, athlete_id := as.character(athlete_id)][, date := as.Date(date)]
say("history: %s rows, %d athletes, %s to %s", format(nrow(h), big.mark = ","),
    uniqueN(h$athlete_id), format(min(h$date)), format(max(h$date)))
check(nrow(h) > 1000, sprintf("store returned a usable history (%s rows)",
                              format(nrow(h), big.mark = ",")))

# The lab's definition, recomputed here from the same rows.
setorder(h, athlete_id, event_id, -date)
h[, .rk := seq_len(.N), by = .(athlete_id, event_id)]
lab <- h[.rk <= 5L, .(lab_recent = mean(perf), n5 = .N), by = .(athlete_id, event_id)][n5 >= 3L]

ab <- estimate_ability(h[, !".rk"], as_of = AS_OF, half_life = DEPLOYED$half_life,
                       calibration = NULL, adjust_context = TRUE,
                       adjust_race = isTRUE(DEPLOYED$adjust_race))
check("recent_mean" %in% names(ab), "estimate_ability() emits recent_mean")

m <- merge(ab[, .(athlete_id, event_id, ability, recent_mean)], lab,
           by = c("athlete_id", "event_id"), all.x = TRUE)
both <- m[is.finite(recent_mean) & is.finite(lab_recent)]
say("%d athlete-events with a baseline on both sides", nrow(both))
check(nrow(both) > 200, "enough overlap to be a real comparison")
worst <- if (nrow(both)) max(abs(both$recent_mean - both$lab_recent)) else NA_real_
check(is.finite(worst) && worst < 1e-12,
      sprintf("recent_mean equals the lab baseline exactly (max |diff| %.3g)", worst))

# Coverage, not presence: a column can be present, correctly typed and empty.
cov <- 100 * mean(is.finite(ab$recent_mean))
say("recent_mean populated on %.1f%% of %s rated athlete-events",
    cov, format(nrow(ab), big.mark = ","))
check(cov > 40, sprintf("recent_mean is actually populated (%.1f%%)", cov))
# Anyone the lab has but the package does not, or vice versa, is a definition gap.
only_lab <- nrow(m[is.na(recent_mean) & is.finite(lab_recent)])
check(only_lab == 0,
      sprintf("no athlete has a lab baseline but no recent_mean (%d do)", only_lab))

# The safety argument, end to end on real abilities.
fld <- ab[event_id == "AT-100Metres-M"][order(-ability)][1:12]
fld <- fld[is.finite(ability) & is.finite(sigma)]
if (nrow(fld) >= 4) {
  b_on <- withr::with_envvar(c(CITIUS_MARKS_BLEND = "0.6"), {
    set.seed(1); medal_probs(simulate_event(fld, n_sims = 6000L))[order(athlete_id)] })
  b_off <- withr::with_envvar(c(CITIUS_MARKS_BLEND = "0"), {
    set.seed(1); medal_probs(simulate_event(fld, n_sims = 6000L))[order(athlete_id)] })
  check(identical(b_on$p_gold, b_off$p_gold),   "p_gold identical with the blend on and off")
  check(identical(b_on$p_medal, b_off$p_medal), "p_medal identical with the blend on and off")
  check(!isTRUE(all.equal(b_on$median_mark, b_off$median_mark)),
        "median_mark DOES move (otherwise the blend is inert)")
  cat("\ntop 6 men's 100m, blend off vs on:\n")
  print(data.table(athlete = b_off$athlete_id, p_gold = round(b_off$p_gold, 3),
                   mark_off = round(b_off$median_mark, 3),
                   mark_on = round(b_on$median_mark, 3))[order(-p_gold)][1:6])
} else {
  check(FALSE, "not enough rated men's 100m athletes to simulate")
}

cat(sprintf("\n%s: %d check(s) failed\n", if (FAIL == 0L) "ALL PASS" else "FAILURES", FAIL))
if (FAIL > 0L) quit(status = 1L)
