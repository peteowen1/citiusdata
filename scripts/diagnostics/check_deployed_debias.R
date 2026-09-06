# GUARD for the family-pool marks debias in _deployed.R (promoted 2026-09-06).
#
# Three things must hold, and each has failed silently somewhere in this repo
# before, so each is asserted rather than assumed:
#
#   1. GATE. Events in DEPLOYED$family_debias$families are shifted by exactly
#      the fitted offset; events outside it are byte-identical to the raw
#      estimate. (The meet_tier flag was a no-op for a week -- a flag that is
#      on must be shown to do something.)
#   2. CONSTANT PER EVENT. Every entrant in an event moves by the same amount,
#      which is what makes placings and p_gold/p_medal invariant. Checked
#      directly on medal_probs() with a fixed seed: probabilities identical,
#      median_mark moved.
#   3. AUDIT COLUMN. `debias_offset` is present, 0 where not applied, and the
#      offsets file named by DEPLOYED exists and resolves for every gated event
#      in the registry (no gated event falls all the way to mu0 unnoticed).
#
# Synthetic history, no data dependency, runs in seconds:
#   Rscript citiusdata/scripts/diagnostics/check_deployed_debias.R
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
fail <- 0L
check <- function(ok, msg) {
  ok <- isTRUE(ok)
  say("%s %s", if (ok) "PASS" else "FAIL", msg)
  if (!ok) fail <<- fail + 1L
  invisible(ok)
}

reg <- as.data.table(citius_events())
gated <- DEPLOYED$family_debias$families
say("gate: %s", paste(gated, collapse = ", "))
off <- deployed_debias_offsets()
check(!is.null(off), sprintf("offsets file %s loads", DEPLOYED$family_debias$file))

# One event from each of four gated and three ungated families, both sexes
# where the registry has them.
pick <- reg[family %in% c(gated, "road", "middle", "distance"),
            .SD[1L], by = .(family, sex)][, .(event_id, family, sex, orientation)]
say("events under test: %d (%d gated)", nrow(pick), sum(pick$family %in% gated))

# --- 3. every gated registry event resolves to something better than mu0 -----
gated_ev <- reg[family %in% gated]$event_id
resolved <- !is.na(off$ev_map[gated_ev]) |
  !is.na(off$fs_map[paste(reg[family %in% gated]$family, reg[family %in% gated]$sex, sep = "|")])
check(all(resolved),
      sprintf("all %d gated registry events resolve via ev_map or fs_map (%d fall to mu0)",
              length(gated_ev), sum(!resolved)))

# --- synthetic history: 6 athletes x 8 results per event ---------------------
set.seed(7)
as_of <- as.Date("2026-09-01")
hist <- rbindlist(lapply(seq_len(nrow(pick)), function(i) {
  ev <- pick$event_id[i]
  base <- if (pick$orientation[i] < 0) 4.6 else 2.0     # log-mark scale
  rbindlist(lapply(1:6, function(a) data.table(
    athlete_id = sprintf("%s_a%d", ev, a), event_id = ev,
    date = as_of - sample(30:700, 8),
    perf = pick$orientation[i] * (base + a * 0.01 + rnorm(8, 0, 0.01)),
    age = 25 + a)))
}))

raw <- .deployed_ability_raw(hist, as_of = as_of, calibration = NULL)
deb <- deployed_debias(copy(raw), off)
check("debias_offset" %in% names(deb), "debias_offset column present")

m <- merge(raw[, .(athlete_id, event_id, a0 = ability)],
           deb[, .(athlete_id, event_id, a1 = ability, debias_offset)],
           by = c("athlete_id", "event_id"))
m[reg, on = "event_id", family := i.family]
m[, delta := a1 - a0]

# --- 1. gate ------------------------------------------------------------------
ung <- m[!family %in% gated]
check(nrow(ung) > 0 && all(ung$delta == 0) && all(ung$debias_offset == 0),
      sprintf("%d ungated rows unchanged (max |delta| %.3g)", nrow(ung), max(abs(ung$delta))))
g <- m[family %in% gated]
check(nrow(g) > 0 && all(g$debias_offset != 0),
      sprintf("%d gated rows carry a nonzero offset", nrow(g)))
check(all(abs(g$delta + g$debias_offset / 100) < 1e-12),
      "gated shift equals -offset/100 exactly")

# --- 2. constant per event => probabilities invariant --------------------------
per_ev <- g[, .(spread = max(delta) - min(delta)), by = event_id]
# Float noise only: (a - k) - a differs from -k in the last ulp per athlete.
check(all(per_ev$spread < 1e-12), sprintf("shift constant within each of %d gated events (max spread %.2g)",
                                          nrow(per_ev), max(per_ev$spread)))

ev1 <- g$event_id[1]
e_raw <- raw[event_id == ev1]; e_deb <- deb[event_id == ev1]
s0 <- medal_probs(simulate_event(e_raw, n_sims = 2000, seed = 11))
s1 <- medal_probs(simulate_event(e_deb, n_sims = 2000, seed = 11))
s <- merge(s0, s1, by = "athlete_id", suffixes = c("0", "1"))
check(all(s$p_gold0 == s$p_gold1) && all(s$p_medal0 == s$p_medal1) &&
        all(s$median_rank0 == s$median_rank1),
      sprintf("%s: p_gold, p_medal, median_rank identical with and without debias", ev1))
check(all(s$median_mark0 != s$median_mark1),
      sprintf("%s: median_mark moved for every athlete (mean %.3f%%)", ev1,
              100 * mean(s$median_mark1 / s$median_mark0 - 1)))

say("\n%s", if (fail == 0L) "ALL CHECKS PASSED" else sprintf("%d CHECK(S) FAILED", fail))
if (fail > 0L) quit(status = 1L)
