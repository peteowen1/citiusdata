# Rebuild athletics calibration and backtest from the full competition harvest.
#
# History now comes from competition results rather than per-athlete fetches.
# That is a better source in every respect: whole fields (so shared race effects
# are identifiable), no-marks retained (so foul rates are measurable), and
# coverage of 85k athletes rather than 643 — coverage being what capped the
# earlier backtest at 78%.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))

OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache")
N_SIMS <- 10000L
MAX_BACKTEST_COMPS <- .env_int("CITIUS_BACKTEST_COMPS", "200")

# UNION WITH WHAT IS ALREADY THERE - never replace it.
#
# This assembles championship_results.rds from the cache directory alone, and
# five OTHER harvesters write the same file: harvest_gap.R,
# harvest_gap_20260818.R, harvest_missing_majors.R, harvest_referenced.R and
# harvest_athletics_meets.R. Rebuilding from the cache therefore DELETED
# everything they had contributed. Measured 2026-08-21: 4,797 competitions
# gained and 2,324 LOST - 763,531 rows - with the total going DOWN by 21,147 on
# a run that had just harvested 4,817 new competitions overnight. It exits 0,
# and the only symptom is a file that got smaller.
#
# Only 4 of the lost were T2 and none were T1, and the engine inner-joins on
# T1/T2, so the model barely noticed. But the corpus also feeds percentiles,
# meet strength and the tier analysis, and all of those read T3.
#
# Cache rows WIN where a competition is in both - they are the fuller,
# eventName-carrying version this re-harvest exists to get - and prior rows are
# carried forward for any competition the cache does not hold.
.prior_f <- file.path(OUT, "championship_results.rds")
.prior <- if (file.exists(.prior_f)) as.data.table(readRDS(.prior_f)) else NULL
champs <- rbindlist(lapply(list.files(CACHE, full.names = TRUE), readRDS),
                    use.names = TRUE, fill = TRUE)
champs <- champs[!is.na(date)]
if (!is.null(.prior) && nrow(.prior)) {
  .cache_ids <- unique(as.character(champs$competition_id))
  .keep <- .prior[!as.character(competition_id) %chin% .cache_ids]
  .n_before <- nrow(champs)
  if (nrow(.keep)) {
    champs <- rbindlist(list(champs, .keep), use.names = TRUE, fill = TRUE)
    cat(sprintf("carried forward %s competition(s) / %s rows the cache does not hold\n",
                format(uniqueN(.keep$competition_id), big.mark = ","),
                format(nrow(.keep), big.mark = ",")))
  }
  # A UNION CANNOT SHRINK, and cannot lose a competition. The failure this
  # replaces was a silent shrink that returned exit 0.
  stopifnot(
    "the union lost rows - it must only ever add" = nrow(champs) >= .n_before,
    "the union dropped competitions the prior file had" =
      length(setdiff(unique(as.character(.prior$competition_id)),
                     unique(as.character(champs$competition_id)))) == 0)
}

# CACHE ROWS WIN ON DATA, NOT ON THE MEET'S NAME.
#
# "Cache wins" is right for results - they are the fuller, eventName-carrying
# version. It is wrong for comp_name, which is a property of the COMPETITION and
# not of the row, and which the competition endpoint frequently omits entirely.
# Applied naively on 2026-08-21 it wiped the name off every major: Paris 2024,
# Rio, London, Sydney and the World Championships all came through with
# named_rows = 0, `class` is derived from comp_name by regex, and so all of them
# became `unclassified` and dropped out of T1_elite. Catalogue naming fell from
# 83.0% to 67.1% in one run, and the anchor that exists to catch exactly this
# passed vacuously - `cat_tbl[class == "olympics"]` was EMPTY, and all() of
# nothing is TRUE.
#
# So the name is coalesced across every source for the competition: whoever has
# one, wins. A name cannot conflict in a way that matters here - it is the same
# meet - and having any name is strictly better than having none.
# THE NAME MAP MUST INCLUDE THE PRIOR FILE, not just the rows we kept.
# Coalescing only within `champs` cannot help when the cached rows carry no name
# at all and the prior rows holding it were the ones "cache wins" discarded -
# which is exactly the state the majors were in. Build the map from BOTH.
.src <- if (!is.null(.prior) && nrow(.prior))
          rbindlist(list(champs[, .(competition_id, comp_name)],
                         .prior[, .(competition_id, comp_name)]),
                    use.names = TRUE, fill = TRUE) else
          champs[, .(competition_id, comp_name)]
.nm <- .src[!is.na(comp_name) & nzchar(comp_name),
            .(.fill_name = comp_name[1]), by = competition_id]
if (nrow(.nm)) {
  .missing_before <- champs[is.na(comp_name) | !nzchar(comp_name), .N]
  champs <- merge(champs, .nm, by = "competition_id", all.x = TRUE)
  champs[(is.na(comp_name) | !nzchar(comp_name)) &
         !is.na(.fill_name), comp_name := .fill_name]
  champs[, .fill_name := NULL]
  .missing_after <- champs[is.na(comp_name) | !nzchar(comp_name), .N]
  cat(sprintf("meet names: %s rows unnamed, %s filled from a sibling row of the same competition\n",
              format(.missing_before, big.mark = ","),
              format(.missing_before - .missing_after, big.mark = ",")))
  # NAMING MUST NOT GO BACKWARDS. This is the check the vacuous anchor failed to
  # be: it compares against the file this run is replacing.
  if (!is.null(.prior) && nrow(.prior)) {
    .pn <- uniqueN(.prior[!is.na(comp_name) & nzchar(comp_name)]$competition_id)
    .cn <- uniqueN(champs[!is.na(comp_name) & nzchar(comp_name)]$competition_id)
    cat(sprintf("named competitions: %s -> %s\n", format(.pn, big.mark = ","),
                format(.cn, big.mark = ",")))
    stopifnot("fewer competitions are named than before - a source overwrote names it should have kept" =
                .cn >= .pn)
  }
}

saveRDS(champs, file.path(OUT, "championship_results.rds"))
cli::cli_alert_success(
  "{format(nrow(champs), big.mark=',')} results | {uniqueN(champs$competition_id)} meets | {format(uniqueN(champs$athlete_id), big.mark=',')} athletes"
)

# --- calibration -------------------------------------------------------------
clean <- flag_implausible(champs)
calibration <- calibrate(clean, min_races = 30L)
half_life <- fit_half_life(clean[!is.na(perf) & !is.na(event_id)])
aging <- fit_aging_curve(clean[!is.na(perf) & !is.na(event_id)])

saveRDS(calibration, file.path(OUT, "calibration.rds"))
saveRDS(half_life, file.path(OUT, "half_life.rds"))
saveRDS(aging, file.path(OUT, "aging.rds"))

cli::cli_h2("Calibration")
cat("tail_df:", calibration$tail_df, "| events calibrated:",
    sum(calibration$events$calibrated, na.rm = TRUE), "\n")
cat("\nhalf-lives:\n"); print(half_life)
cat("\nround:\n"); print(calibration$round)
cat("\ntier:\n"); print(calibration$tier)
cat("\naging peaks:\n"); print(aging$peaks)

# --- backtest ----------------------------------------------------------------
# Ability is re-estimated per competition from strictly prior results, so the
# meet being predicted cannot inform its own forecast. That is the expensive
# part, hence the cap: competitions are sampled across tiers and years rather
# than taking the most recent, so the sample is not all one era.
finals <- clean[!is.na(event_id) & !is.na(place) & !is.na(perf) &
                  grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]

pool <- unique(finals[, .(competition_id, comp_start, comp_tier)])
pool <- pool[!is.na(comp_start) & comp_start >= as.Date("2015-01-01")]
setorder(pool, comp_start)
if (nrow(pool) > MAX_BACKTEST_COMPS) {
  pool <- pool[round(seq(1, .N, length.out = MAX_BACKTEST_COMPS))]
}
cli::cli_alert_info("Backtesting {nrow(pool)} meet{?s}.")

all_pred <- list(); all_out <- list()
for (i in seq_len(nrow(pool))) {
  cid <- pool$competition_id[i]
  cut_date <- pool$comp_start[i]
  block <- finals[competition_id == cid]

  past <- clean[date < cut_date & !is.na(perf) & !is.na(event_id)]
  if (nrow(past) < 5000L) next
  ability <- estimate_ability(past, as_of = cut_date, half_life = half_life,
                              calibration = calibration)

  for (ev in unique(block$event_id)) {
    field <- unique(block[event_id == ev], by = "athlete_id")
    entrants <- ability[event_id == ev &
                          athlete_id %in% as.character(field$athlete_id)]
    if (nrow(entrants) < 4L) next

    sim <- simulate_event(entrants, n_sims = N_SIMS,
                          calibration = calibration, seed = 11L)
    mp <- medal_probs(sim)
    key <- paste(cid, ev)
    mp[, race_id := key]
    all_pred[[length(all_pred) + 1L]] <- mp
    all_out[[length(all_out) + 1L]] <- data.table(
      race_id = key, athlete_id = mp$athlete_id,
      hit = mp$athlete_id %in% as.character(field[place == 1L]$athlete_id),
      hit_medal = mp$athlete_id %in% as.character(field[place <= 3L]$athlete_id))
  }
  if (i %% 25 == 0) cli::cli_alert("  backtest {i}/{nrow(pool)}")
}

pred <- rbindlist(all_pred, fill = TRUE)
outc <- rbindlist(all_out, fill = TRUE)
cov <- outc[, .(wp = any(hit)), by = race_id]
keep <- cov[wp == TRUE]$race_id
cli::cli_alert_info(
  "{nrow(cov)} race{?s}; winner in field for {length(keep)} ({round(100*length(keep)/nrow(cov))}%)."
)

gold <- score_predictions(pred[race_id %in% keep], outc[race_id %in% keep], "p_gold")
medal <- score_predictions(pred[race_id %in% keep],
                           outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")

cli::cli_h2("Backtest (winner-in-field)")
cat(sprintf("gold  brier %.4f vs %.4f  skill %+.3f  (%d races)\n",
            gold$overall$brier, gold$overall$brier_baseline,
            gold$overall$brier_skill, gold$overall$n_races))
cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
            medal$overall$brier, medal$overall$brier_baseline, medal$overall$brier_skill))
cat("\nreliability:\n"); print(gold$reliability[n >= 20])
br <- gold$by_race
cat(sprintf("\nraces beating baseline: %d of %d (%.0f%%)\n",
            sum(br$skill > 0), nrow(br), 100 * mean(br$skill > 0)))

# NOT `backtest.rds`. That name is backtest_athletics.R's default CITIUS_BT_OUT
# and is what diagnose_backtest.R reads by default, so this script -- which caps
# at 200 competitions and fits its own calibration -- was overwriting the current
# arm's artefact with a differently-configured one. Same collision the
# backtest_championships.R rename closed on 2026-08-14; this was the second
# writer, found by the review of that fix.
saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc),
        file.path(OUT, "backtest_rebuild.rds"))
cli::cli_alert_success(
  "Wrote {.file backtest_rebuild.rds}. {.file backtest.rds} belongs to backtest_athletics.R.")
