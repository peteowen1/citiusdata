# Every step, with numbers, of how one athlete's next mark is predicted.
#
# Written because "how does the model get its number" should be answerable by
# reading real values out of the deployed pipeline, not by describing it. Uses
# the DEPLOYED config throughout (_deployed.R), so what it prints is what the
# shipping path would do for that athlete on that date.
#
# Usage:
#   Rscript citiusdata/scripts/diagnostics/walk_athlete_prediction.R
#   CITIUS_WALK_NAME="Noah LYLES" CITIUS_WALK_EVENT=AT-100Metres-M \
#     CITIUS_WALK_ASOF=2026-09-10 Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
NAME  <- Sys.getenv("CITIUS_WALK_NAME", "LYLES")
EVENT <- Sys.getenv("CITIUS_WALK_EVENT", "AT-100Metres-M")
ASOF  <- as.Date(Sys.getenv("CITIUS_WALK_ASOF", as.character(Sys.Date())))

reg <- as.data.table(citius_events())[event_id == EVENT]
ORI <- reg$orientation[1]; FAM <- reg$family[1]
HL  <- if (FAM %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[FAM]] else DEPLOYED$half_life

cat(strrep("=", 74), "\n")
cat(sprintf("PREDICTING %s | %s | as of %s\n", NAME, EVENT, ASOF))
cat(sprintf("deployed stamp: %s\n", DEPLOYED$stamp))
cat(strrep("=", 74), "\n\n")

cal <- deployed_calibration(OUT)
hist <- setDT(deployed_history(OUT, events = EVENT,
                               from = ASOF - DEPLOYED$history_days, to = ASOF - 1L))
hist[, athlete_id := as.character(athlete_id)]

# The parquet store carries no athlete_name (it is narrowed to modelled
# columns), so the name is resolved against championship_results and only the
# id is used downstream.
AID <- Sys.getenv("CITIUS_WALK_ID", "")
if (nzchar(AID)) {
  cat(sprintf("using athlete_id %s (given directly)\n\n", AID))
} else {
  ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
  ch[, athlete_id := as.character(athlete_id)]
  cand <- unique(ch[event_id == EVENT & grepl(NAME, athlete_name, ignore.case = TRUE),
                    .(athlete_id, athlete_name)])
  cand <- cand[athlete_id %chin% unique(hist$athlete_id)]
  if (!nrow(cand)) stop("no athlete matching '", NAME, "' with ", EVENT, " history")
  # Prefer whoever actually has the most history in this event -- a loose name
  # match can pull in a namesake with three results.
  n_by <- hist[athlete_id %chin% cand$athlete_id, .N, by = athlete_id]
  cand <- merge(cand, n_by, by = "athlete_id")[order(-N)]
  print(cand)
  AID <- cand$athlete_id[1]
  cat(sprintf("\nusing athlete_id %s (%s), %d results in this event\n\n",
              AID, cand$athlete_name[1], cand$N[1]))
}

h <- hist[athlete_id == AID][order(-date)]
cat(sprintf("STEP 1. HISTORY the model may use: %d results in the last %d days\n",
            nrow(h), DEPLOYED$history_days))
cat(sprintf("        (half-life for family '%s' = %d days)\n\n", FAM, as.integer(HL)))

h[, age_days := as.numeric(ASOF - date)]
h[, decay_w := 0.5 ^ (age_days / HL)]
show <- head(h[, .(date, mark, tier, meet_tier = if ("meet_tier" %in% names(h)) meet_tier else NA,
                   round, age_days, decay_w = round(decay_w, 4))], 12)
print(show)
cat(sprintf("\n  sum of decay weights over ALL %d results = %.3f\n", nrow(h), sum(h$decay_w)))
cat("  (this is what drives shrinkage: evidence WEIGHT, not result count)\n\n")

cat("STEP 2. CONTEXT ADJUSTMENT -- each past mark is moved to a common context\n")
cat("        (a top-tier final), so a heat or a weak meet is not compared raw.\n")
tier_tbl <- as.data.table(cal$tier); round_tbl <- as.data.table(cal$round)
cat("\n  fitted tier offsets (%, subtracted from history in that class):\n")
print(tier_tbl)
cat("\n  fitted round offsets:\n"); print(head(round_tbl, 6))

cat("\nSTEP 3. THE ESTIMATE ITSELF (deployed_ability -> estimate_ability)\n")
ab_all <- deployed_ability(hist, as_of = ASOF, calibration = cal)
setDT(ab_all)
ab <- ab_all[athlete_id == AID & event_id == EVENT]
if (!nrow(ab)) stop("no ability row produced for that athlete")
cat("\n  columns the estimator returns:\n  ", paste(names(ab), collapse = ", "), "\n\n")
num <- names(ab)[vapply(ab, is.numeric, logical(1))]
print(t(round(ab[, ..num], 5)))

cat("\nSTEP 4. WHAT THOSE NUMBERS MEAN AS A MARK\n")
cat(sprintf("  ability (log-perf, oriented)      = %.6f\n", ab$ability))
cat(sprintf("  => median predicted mark          = %.3f\n", perf_to_mark(ab$ability, ORI)))
if ("ability_raw" %in% names(ab))
  cat(sprintf("  ability BEFORE shrinkage          = %.6f  (mark %.3f)\n",
              ab$ability_raw, perf_to_mark(ab$ability_raw, ORI)))
if ("shrinkage" %in% names(ab))
  cat(sprintf("  shrinkage toward the event prior  = %.1f%%\n", 100 * ab$shrinkage))
if ("prior_mu" %in% names(ab))
  cat(sprintf("  the prior it shrinks toward       = %.6f  (mark %.3f)\n",
              ab$prior_mu, perf_to_mark(ab$prior_mu, ORI)))

cat("\nSTEP 5. THE TWO SOURCES OF SPREAD -- these are different quantities\n")
sg <- ab$sigma; se <- if ("ability_se" %in% names(ab)) ab$ability_se else NA_real_
cat(sprintf("  sigma       = %.5f  how much HE varies around his own true level\n", sg))
cat(sprintf("  ability_se  = %.5f  how little we KNOW that level (= sigma/sqrt(w+kappa))\n", se))
# tail_df has changed shape across calibrations (named vector in some, table in
# others), so read it defensively rather than assuming columns that may not be
# there -- guessing here would print a confident wrong number.
tdf <- tryCatch({
  td <- cal$tail_df
  if (is.null(td)) NA_real_
  else if (is.numeric(td) && !is.null(names(td)) && EVENT %in% names(td)) unname(td[[EVENT]])
  else if (is.numeric(td)) stats::median(td, na.rm = TRUE)
  else {
    dt <- as.data.table(td)
    ec <- intersect(c("event_id", "event"), names(dt))
    dc <- intersect(c("df", "tail_df", "nu"), names(dt))
    if (length(ec) && length(dc)) {
      v <- dt[get(ec[1]) == EVENT][[dc[1]]]
      if (length(v)) v[1] else stats::median(dt[[dc[1]]], na.rm = TRUE)
    } else NA_real_
  }
}, error = function(e) NA_real_)
cat(sprintf("  tail_df     = %s  fat-tailed draw, not Gaussian\n",
            if (is.finite(tdf)) sprintf("%.2f", tdf) else "n/a"))

cat("\n  as MARKS, one standard deviation either side of the median:\n")
cat(sprintf("    from sigma alone      : %.3f  ..  %.3f\n",
            perf_to_mark(ab$ability + sg, ORI), perf_to_mark(ab$ability - sg, ORI)))
if (is.finite(se)) {
  tot <- sqrt(sg^2 + se^2)
  cat(sprintf("    sigma + ability_se    : %.3f  ..  %.3f   (total sd %.5f)\n",
              perf_to_mark(ab$ability + tot, ORI), perf_to_mark(ab$ability - tot, ORI), tot))
}

cat("\nSTEP 6. WHAT THE SIMULATOR ACTUALLY DRAWS (10,000 races)\n")
sim_in <- ab[, .(athlete_id, event_id, ability, sigma,
                 ability_se = if ("ability_se" %in% names(ab)) ability_se else 0)]
# simulate_event() models a RACE, so it requires a field. A second entrant is
# added only to satisfy that: an athlete's own MARK distribution does not
# depend on who he is racing (only his PLACING does), so reading his rows back
# out gives the same distribution the real card would show him.
sim_in <- rbind(sim_in, copy(sim_in)[, athlete_id := "__fieldmate__"])
s <- simulate_event(sim_in, n_sims = 10000L, calibration = cal, seed = 11L)
# citius_sim is a list holding a `perf` MATRIX (n_sims x athletes), not a
# table -- read his column rather than coercing the object.
stopifnot(inherits(s, "citius_sim"), AID %in% colnames(s$perf))
m <- perf_to_mark(s$perf[, AID], ORI)
qs <- stats::quantile(m, c(.05, .25, .5, .75, .95), na.rm = TRUE)
cat(sprintf("  p05 %.3f | p25 %.3f | MEDIAN %.3f | p75 %.3f | p95 %.3f\n",
            qs[1], qs[2], qs[3], qs[4], qs[5]))
# Filter to finite explicitly rather than trusting na.rm: the foul model puts
# non-finite values in some draws, and `sd(..., na.rm = TRUE)` still returned
# NaN here. Report how many draws were dropped -- a spread computed on an
# unstated subset is the kind of number that gets quoted later as if it covered
# everything.
mf <- m[is.finite(m)]
cat(sprintf("  sd of simulated marks: %.4f s (on %s of %s finite draws; %s were fouls/no-marks)\n",
            stats::sd(mf), format(length(mf), big.mark = ","),
            format(length(m), big.mark = ","), format(length(m) - length(mf), big.mark = ",")))
cat(sprintf("  90%% of simulated runs land in %.2f - %.2f\n", qs[1], qs[5]))
cat(sprintf("  simulator settings: condition_sd %s, tail df %s, foul_prob %s\n",
            format(s$settings$condition_sd), format(s$settings$df),
            format(s$settings$foul_prob)))
cat("\nNOTE: a race-day condition shock is shared by the field, so it moves the\n")
cat("MARK but cancels from finishing positions except through per-athlete\n")
cat("sensitivity. Predicting a mark and predicting a win are different jobs.\n")
