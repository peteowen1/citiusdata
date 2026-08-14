# Score a meet's published card against results as they land.
#
#   CITIUS_MEET=birmingham2026 Rscript scripts/score_meet.R
#
# WHY THIS EXISTS. `run_meet.ps1` builds and publishes a card but has no scoring
# step, so the European Championships ran for two days with nothing measuring
# them (found 2026-08-12). Cadence decided in ticket 12 is "scored the next
# morning" -- that needs a script, not a habit.
#
# Meet-generic on purpose. The competition id, the cutoff and the prediction file
# all come from the calendar, so adding a meet is adding a calendar row. Glasgow
# has its own scorer (`score_glasgow2026.R`) carrying Games-specific medal-table
# logic; this one is the per-meet card scorer and does not replace it.
#
# Safe to run repeatedly through a meet: it re-harvests, scores whatever is
# complete, and says plainly what is not. Nothing is cached.
#
# Two things are scored, and they answer different questions:
#   Finals - medal probabilities against outcomes (Brier, skill vs
#            uniform-within-race). The real test.
#   All races - whether pre-meet ability ORDERS each field correctly. Heats are
#            coasted once an athlete is safe, so this is a floor on ranking
#            skill, not a forecast. Grouped by `race_key`, never by event:
#            `place` is within race, so cross-race pairs are meaningless.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT   <- here::here("citiusdata", "data")
BLOG  <- here::here("citiusdata", "blog")
CAL   <- file.path(OUT, "athletics_calendar.csv")
MEET  <- Sys.getenv("CITIUS_MEET", "")
if (!nzchar(MEET)) cli::cli_abort("Set {.envvar CITIUS_MEET} to a calendar meet_id.")

cal <- fread(CAL)
row <- cal[meet_id == MEET]
if (!nrow(row)) {
  cli::cli_abort(c("{.val {MEET}} is not in the calendar.",
                   i = "Known: {.val {cal$meet_id}}"))
}
COMP <- suppressWarnings(as.integer(row$wa_competition_id[1]))
if (is.na(COMP)) {
  cli::cli_abort(c("{.val {MEET}} has no {.field wa_competition_id} on the calendar.",
                   i = "A meet cannot be scored until its competition id is known."))
}
CUTOFF <- as.Date(row$prediction_cutoff[1])
cli::cli_h1("{row$name[1]} - {row$short_name[1]}")
cli::cli_alert_info("competition {COMP}, cutoff {format(CUTOFF)}, state {.val {row$state[1]}}")

# --- results ------------------------------------------------------------------
results <- tryCatch(setDT(athletics_harvest_competitions(COMP)),
                    error = function(e) NULL)
if (is.null(results) || !nrow(results)) {
  cli::cli_alert_warning("No results in the feed for competition {COMP} yet.")
  quit(save = "no")
}
saveRDS(results, file.path(OUT, paste0(MEET, "_results.rds")))
results[, athlete_id := as.character(athlete_id)]
cli::cli_alert_info(
  "{nrow(results)} result{?s}, {uniqueN(results$event_id)} event{?s}, {uniqueN(results$race_key)} race{?s}, {format(min(results$date, na.rm = TRUE))} to {format(max(results$date, na.rm = TRUE))}."
)

# A result dated on or before the cutoff would mean the forecast could see the
# meet it forecasts. Cheap to check, and silent if it ever goes wrong.
leaked <- results[!is.na(date) & date <= CUTOFF]
if (nrow(leaked)) {
  cli::cli_abort(c("{nrow(leaked)} result{?s} dated on or before the prediction cutoff.",
                   x = "The card may have been built with knowledge of this meet."))
}

# --- predictions --------------------------------------------------------------
pred_file <- file.path(BLOG, paste0(MEET, "-predictions.parquet"))
if (!file.exists(pred_file)) {
  cli::cli_abort(c("No prediction file at {.file {pred_file}}.",
                   i = "Run {.code pwsh scripts/run_meet.ps1 {MEET}} first."))
}
pred <- setDT(arrow::read_parquet(pred_file))
pred[, athlete_id := as.character(athlete_id)]
cli::cli_alert_info("Scoring against {.file {basename(pred_file)}} ({nrow(pred)} row{?s}, {uniqueN(pred$event_id)} event{?s}, config {.val {pred$config[1]}}).")

# Predictions and results can key athletes in DISJOINT namespaces: an entry-list
# card carries a name-derived key, the results feed carries World Athletics
# numeric ids. Joining the wrong pair matched 0 of 110 Glasgow finalists and every
# downstream check read that absence as a MODELLING result. `athlete_key()` sorts
# name tokens, so it reproduces the name-keyed id exactly; it is a lossless remap.
results[, pred_key := athlete_id]
if (!any(grepl("^[0-9]+$", pred$athlete_id))) {
  results[, pred_key := athlete_key(athlete_name)]
}

finals <- results[!is.na(place) & place > 0L & !is.na(event_id) &
                    grepl("final", round, ignore.case = TRUE) &
                    !grepl("semi", round, ignore.case = TRUE)]
finals[, athlete_id := as.character(pred_key)]

cli::cli_h2("Finals")
ev <- intersect(unique(finals$event_id), unique(pred$event_id))
if (!length(ev)) {
  cli::cli_alert_info("No predicted final is complete yet.")
} else {
  fe <- finals[event_id %in% ev]
  hit_rate <- mean(fe$athlete_id %in% pred$athlete_id)
  cli::cli_alert_info("Finalist->prediction id match: {round(100 * hit_rate)}% ({sum(fe$athlete_id %in% pred$athlete_id)} of {nrow(fe)}).")
  # Fail loudly rather than report a model that "missed every winner": a
  # scoreboard measuring a broken join is indistinguishable from a broken model.
  if (hit_rate < 0.5) {
    cli::cli_abort(c("Only {round(100 * hit_rate)}% of finalists match a prediction by id.",
                     x = "Scores on this join would measure the join, not the model."))
  }

  p <- pred[event_id %in% ev, .(race_id = event_id, athlete_id, p_gold, p_medal)]
  o <- finals[event_id %in% ev, .(race_id = event_id, athlete_id,
                                  hit = place == 1L, hit_medal = place <= 3L)]
  # Only score races whose actual winner was in our field; otherwise this
  # measures entry-list coverage rather than the model.
  #
  # Tested against the PREDICTIONS. `o` comes from the results, so `any(hit)` is
  # true for every completed final and the old form here excluded nothing --
  # inherited verbatim from score_glasgow2026.R along with the rest of this
  # block. An uncovered winner leaves only hit = 0 rows in the race, which lowers
  # the Brier, so the gap flattered the model rather than being held out of it.
  # It also made `median rank of the actual winner` NA for the whole meet, since
  # the winner has no rank in a list they are not in.
  keep <- unique(o[hit == TRUE, .(race_id, athlete_id)][
    p[, .(race_id, athlete_id)], on = .(race_id, athlete_id), nomatch = NULL]$race_id)
  skipped <- setdiff(ev, keep)
  if (length(skipped)) {
    cli::cli_alert_warning("{length(skipped)} final{?s} skipped - winner absent from our field: {.val {skipped}}")
  }
  if (length(keep)) {
    g <- score_predictions(p[race_id %in% keep], o[race_id %in% keep], "p_gold")
    m <- score_predictions(p[race_id %in% keep],
                           o[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")
    cat(sprintf("\ngold  brier %.4f vs %.4f  skill %+.3f  (%d race%s)\n",
                g$overall$brier, g$overall$brier_baseline, g$overall$brier_skill,
                g$overall$n_races, if (g$overall$n_races == 1) "" else "s"))
    cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
                m$overall$brier, m$overall$brier_baseline, m$overall$brier_skill))

    top <- p[race_id %in% keep][order(race_id, -p_gold)][, .SD[1], by = race_id]
    top <- merge(top, o[hit == TRUE, .(race_id, winner = athlete_id)], by = "race_id")
    top[, called := athlete_id == winner]
    cat(sprintf("\nfavourite won %d of %d final%s (%.0f%%)\n", sum(top$called), nrow(top),
                if (nrow(top) == 1) "" else "s", 100 * mean(top$called)))
    # "Favourite won" is a harsh binary in a deep field; the winner's rank shows
    # whether a miss was a near-miss.
    rk <- p[race_id %in% keep][order(race_id, -p_gold)][
      , .(rank = which(athlete_id == o[race_id == .BY$race_id & hit == TRUE]$athlete_id[1]),
          n = .N), by = race_id]
    cat("winner's rank in our list: ",
        paste(sprintf("%s/%d", rk$rank, rk$n), collapse = ", "), "\n", sep = "")
    cat(sprintf("median rank of the actual winner: %.0f\n", median(rk$rank)))

    # Probability sitting on athletes who never appeared is a FIELD error, not a
    # model error, and inflates apparent miscalibration -- so report it rather
    # than absorb it. "Appeared" must NOT be filtered on place > 0: an athlete
    # who fouls out still contested the final.
    contested <- unique(results[grepl("final", round, ignore.case = TRUE) &
                                  !grepl("semi", round, ignore.case = TRUE) &
                                  event_id %in% keep,
                                .(race_id = event_id, athlete_id = pred_key)])
    ghost <- p[race_id %in% keep][!contested, on = .(race_id, athlete_id)]
    at_meet <- unique(results$pred_key)
    ghost[, eliminated := athlete_id %in% at_meet]
    cat(sprintf("\nprobability on athletes who never contested the final:\n  eliminated earlier : %.2f gold\n  never at the meet  : %.2f gold  <- true withdrawals\n",
                sum(ghost[eliminated == TRUE]$p_gold),
                sum(ghost[eliminated == FALSE]$p_gold)))
  }
}

# --- ranking check ------------------------------------------------------------
# Does pre-meet ability order each field? Uses the same history cut the card was
# built from, and excludes this competition by ID as well as by date -- a leak
# would make this score the model against races it had already seen, while
# looking better rather than worse.
cli::cli_h2("Ranking check (all completed races)")
# USES THE DEPLOYED CONFIG, not its own literals (fixed 2026-08-13).
#
# This block previously read `calibration.rds` -- a 3MB file last written
# 2026-07-28 -- and hardcoded `half_life = 365`, while the deployed model was a
# 20MB calibration on a different corpus with per-FAMILY half-lives
# (road = 1095, walk = 730). So the ranking check scored a model that was never
# shipped, and reported the number as if it described the card. The 75.5%
# recorded for Birmingham was measured that way.
#
# `_deployed.R` exists precisely to stop this: its header records a 2026-07-31
# audit that found five scripts each re-expressing the config as literals. This
# script was written 2026-08-12, two weeks later, and did it again. Calling
# `deployed_ability()` rather than `estimate_ability()` directly is what makes it
# stay fixed -- the per-family half-lives come with it instead of being another
# literal to forget.
# NOTE this also changes WHICH STORE the history comes from: `athletics_store`
# (championship results) -> `athletics_corpus_store` (the corpus), because
# DEPLOYED$history_store names the corpus and the deployed calibration is fitted
# on it. Both stores exist with 87 partitions each. Scoring the deployed model
# against a history it was not fitted on is the same mismatch, one level over.
source(here::here("citiusdata", "scripts", "_deployed.R"))
STORE <- file.path(OUT, DEPLOYED$history_store)
if (!dir.exists(STORE)) {
  cli::cli_alert_warning("Store missing; skipping the ranking check.")
} else {
  cal <- deployed_calibration(OUT)
  cli::cli_alert_info("Ranking check on the DEPLOYED model: {DEPLOYED$stamp}")
  past <- read_results_store(STORE, events = unique(results$event_id),
                             from = CUTOFF - 4380, to = CUTOFF)[
                               !is.na(event_id) & !is.na(perf)]
  past <- past[is.na(competition_id) | competition_id != COMP]
  stopifnot("history must not contain the competition being scored" =
              !any(past$competition_id == COMP, na.rm = TRUE))
  ab <- deployed_ability(past, as_of = CUTOFF, calibration = cal)
  scored <- merge(results[!is.na(place) & place > 0L],
                  ab[, .(athlete_id, event_id, ability)],
                  by = c("athlete_id", "event_id"))
  cli::cli_alert_info("{nrow(scored)} of {sum(!is.na(results$place) & results$place > 0L)} placed result{?s} have a prior ability estimate.")
  pw <- scored[, {
    if (.N < 2L) .(conc = 0L, tot = 0L) else {
      g <- expand.grid(i = seq_len(.N), j = seq_len(.N)); g <- g[g$i < g$j, ]
      .(conc = sum((ability[g$i] > ability[g$j]) == (place[g$i] < place[g$j])),
        tot = nrow(g))
    }
  }, by = .(race_key, event_id, round)]
  if (sum(pw$tot)) {
    cat(sprintf("\npooled: %d of %d pairs correct (%.1f%%) across %d race%s\n",
                sum(pw$conc), sum(pw$tot), 100 * sum(pw$conc) / sum(pw$tot),
                nrow(pw[tot > 0]), if (nrow(pw[tot > 0]) == 1) "" else "s"))
    cat("(50% is a coin flip; heats are coasted, so this is a floor)\n")
    print(pw[, .(races = .N, pairs = sum(tot),
                 acc = round(100 * sum(conc) / sum(tot), 1)), by = round][order(-pairs)])
  }
}
