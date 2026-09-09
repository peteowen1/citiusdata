# One athlete, one race: every number, before and after.
#
# Shows the full adjustment chain applied to each historical mark (raw mark ->
# perf -> round offset -> tier offset -> coasting excess -> adjusted perf),
# its decay weight, and its contribution to the weighted mean -- then the
# predicted mark BEFORE the athlete's last race and AFTER it, so the effect of
# a single new result is visible as a number rather than described.
#
# Usage:
#   CITIUS_WALK_NAME=LYLES CITIUS_WALK_EVENT=AT-100Metres-M \
#     Rscript citiusdata/scripts/diagnostics/walk_before_after_race.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
NAME  <- Sys.getenv("CITIUS_WALK_NAME", "LYLES")
EVENT <- Sys.getenv("CITIUS_WALK_EVENT", "AT-100Metres-M")
ADJR  <- nzchar(Sys.getenv("CITIUS_WALK_ADJUST_RACE", ""))

reg <- as.data.table(citius_events())[event_id == EVENT]
ORI <- reg$orientation[1]; FAM <- reg$family[1]
HL  <- if (FAM %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[FAM]] else DEPLOYED$half_life
cal <- deployed_calibration(OUT)

full <- setDT(deployed_history(OUT, events = EVENT,
                               from = as.Date("2014-01-01"), to = Sys.Date()))
full[, athlete_id := as.character(athlete_id)]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
cand <- unique(ch[event_id == EVENT & grepl(NAME, athlete_name, ignore.case = TRUE),
                  .(athlete_id, athlete_name)])
n_by <- full[athlete_id %chin% cand$athlete_id, .N, by = athlete_id][order(-N)]
AID <- n_by$athlete_id[1]
WHO <- cand[athlete_id == AID]$athlete_name[1]

h <- full[athlete_id == AID & is.finite(mark)][order(date)]
last <- h[.N]
cat(strrep("=", 78), "\n")
cat(sprintf("%s (%s) | %s | adjust_race = %s\n", WHO, AID, EVENT, ADJR))
cat(sprintf("HIS LAST RACE ON RECORD: %s, mark %.2f, round '%s', tier '%s'\n",
            last$date, last$mark, last$round,
            if ("meet_tier" %in% names(last)) last$meet_tier else last$tier))
cat(strrep("=", 78), "\n\n")

# --- the adjustment chain, per historical mark -------------------------------
r_off <- setNames(cal$round$offset, cal$round$round_class)
t_off <- setNames(cal$tier$offset,  cal$tier$tier_class)
show <- copy(h)
show[, perf_raw := ORI * log(mark)]
show[, round_class := .round_class(round)]
show[, tier_class := .tier_class_of(show)]
show[, r_adj := fifelse(is.na(r_off[round_class]), 0, r_off[round_class])]
show[, t_adj := fifelse(is.na(t_off[tier_class]),  0, t_off[tier_class])]
show[, perf_adj := perf_raw - r_adj - t_adj]
# What each adjustment is worth AS A MARK, which is the only readable unit.
show[, mark_if_final_top := perf_to_mark(perf_adj, ORI)]

cat("STEP A. EVERY MARK, AND WHAT THE MODEL TURNS IT INTO\n")
cat("  (adjusted = moved to 'what this would have been in a top-tier FINAL')\n\n")
print(tail(show[, .(date, mark, round_class, tier_class,
                    r_adj = round(r_adj, 5), t_adj = round(t_adj, 5),
                    adjusted_mark = round(mark_if_final_top, 3))], 12))

cat("\n  NOTE what is NOT in that list: wind, altitude, track, weather.\n")
cat("  Those live in calibration$race (the shared per-race effect) and it is\n")
cat(sprintf("  only applied when adjust_race is ON. It is currently %s.\n",
            if (ADJR) "ON" else "OFF (the deployed default)"))

# --- before / after ----------------------------------------------------------
est <- function(as_of, label) {
  hh <- full[date < as_of]
  ab <- setDT(estimate_ability(hh, as_of = as_of, half_life = HL, calibration = cal,
                               adjust_context = TRUE, adjust_race = ADJR,
                               only = AID))
  a <- ab[athlete_id == AID & event_id == EVENT]
  if (!nrow(a)) { cat(sprintf("\n%s: no estimate\n", label)); return(NULL) }
  cat(sprintf("\n%s (as of %s)\n", label, as_of))
  cat(sprintf("   results used      : %d   total decayed weight: %.3f\n", a$n, a$w_total))
  cat(sprintf("   ability_raw       : %.6f  -> mark %.3f\n", a$ability_raw,
              perf_to_mark(a$ability_raw, ORI)))
  cat(sprintf("   shrinkage         : %.2f%% toward prior %.6f (mark %.3f)\n",
              100*a$shrinkage, a$prior_mu, perf_to_mark(a$prior_mu, ORI)))
  cat(sprintf("   ability           : %.6f  -> PREDICTED MARK %.3f\n", a$ability,
              perf_to_mark(a$ability, ORI)))
  cat(sprintf("   sigma %.5f | ability_se %.5f\n", a$sigma, a$ability_se))
  a
}

cat("\n", strrep("-", 78), "\n", sep = "")
cat("STEP B. BEFORE vs AFTER HIS LAST RACE\n")
cat(strrep("-", 78), "\n")
before <- est(last$date, "BEFORE the race")
cat(sprintf("\n   >>> HE THEN RAN %.2f on %s (round '%s') <<<\n", last$mark, last$date, last$round))
after <- est(last$date + 1L, "AFTER the race")

if (!is.null(before) && !is.null(after)) {
  mb <- perf_to_mark(before$ability, ORI); ma <- perf_to_mark(after$ability, ORI)
  cat("\n", strrep("-", 78), "\n", sep = "")
  cat(sprintf("EFFECT OF THAT ONE RACE: predicted mark %.3f -> %.3f  (%+.3f s)\n",
              mb, ma, ma - mb))
  cat(sprintf("  weight added: %.3f (a race on the day carries 0.5^0 = 1.000)\n",
              after$w_total - before$w_total))
  cat(sprintf("  sigma %.5f -> %.5f | ability_se %.5f -> %.5f\n",
              before$sigma, after$sigma, before$ability_se, after$ability_se))
  cat(sprintf("\n  his actual mark was %.2f; the model had predicted %.3f, so it was\n",
              last$mark, mb))
  cat(sprintf("  %s by %.3f s, and moved %.3f s toward it.\n",
              if ((last$mark - mb) * ORI > 0) "beaten" else "missed",
              abs(last$mark - mb), abs(ma - mb)))
}
