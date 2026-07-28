# Score Glasgow 2026 predictions against results as they land.
#
# Safe to run repeatedly through the Games: it re-harvests, scores whatever is
# complete, and says plainly what is not yet available. Nothing is cached, so a
# later run supersedes an earlier one.
#
# Two things are scored, and they answer different questions:
#
#   Finals  - medal probabilities against outcomes (Brier, skill vs a
#             uniform-within-race baseline). This is the real test.
#   Heats   - whether pre-Games ability ORDERS each field correctly. Heats are
#             qualifying runs where athletes coast once safe, so this is a floor
#             on ranking skill, not a forecast of finals.
#
# The heat check must group by `race_key`, never by event: `place` is within
# heat, so comparing across heats of the same event is meaningless. That is what
# the raceNumber fix in athletics_competition_results() made possible.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
GLASGOW <- 7187518L
HALF_LIFE <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "730"))

results <- tryCatch(setDT(athletics_harvest_competitions(GLASGOW)), error = function(e) NULL)
if (is.null(results) || !nrow(results)) {
  cli::cli_alert_warning("No Glasgow results in the feed yet.")
  quit(save = "no")
}
saveRDS(results, file.path(OUT, "glasgow2026_results.rds"))
cli::cli_alert_info(
  "{nrow(results)} result{?s}, {uniqueN(results$event_id)} event{?s}, {uniqueN(results$race_key)} race{?s}."
)
print(results[, .(results = .N, races = uniqueN(race_key)), by = round][order(-results)])

# --- finals: medal probabilities vs outcomes ---------------------------------
pred_files <- list.files(OUT, pattern = "glasgow2026.*predictions.*parquet$", full.names = TRUE)
if (!length(pred_files)) {
  cli::cli_alert_warning("No prediction file found; run predict_glasgow_entries.R first.")
  quit(save = "no")
}
# Most recent by mtime, NOT by filename. Sorting names descending picks
# "glasgow2026_predictions_" over "glasgow2026_entrylist_predictions_" because
# "p" > "e", regardless of timestamp - which silently scored against a stale,
# smaller prediction set.
latest <- pred_files[which.max(file.info(pred_files)$mtime)]
pred <- setDT(arrow::read_parquet(latest))
pred[, athlete_id := as.character(athlete_id)]
cli::cli_alert_info("Scoring against {.file {basename(latest)}} ({nrow(pred)} row{?s}, {uniqueN(pred$event_id)} event{?s}).")

# The two sides key athletes in DISJOINT namespaces. Entry-list predictions carry
# a name-derived id (`athlete_key()` of the entry name, e.g. "HOBBSZOE"); the
# results feed carries World Athletics numeric ids ("14499691"). Joining them on
# `athlete_id` matched 0 of 110 finalists, and every downstream check then read
# that absence as a MODELLING result: the winner was reported "not in our list"
# and 45% of gold probability as sitting on non-starters. Both were false --
# Eseme and Hobbs were predicted, at 1.6% and 1.3%.
#
# `athlete_key()` sorts name tokens, so it is invariant to given-first vs
# surname-first and reproduces the prediction id exactly. This is a lossless
# remap, not fuzzy matching.
# The remap is confined to the finals-scoring block. `results$athlete_id` must
# stay numeric: the ranking check further down joins it against ability estimates
# from the World Athletics harvest, which is the numeric namespace.
results[, athlete_id := as.character(athlete_id)]
results[, pred_key := athlete_key(athlete_name)]
pred_is_name_keyed <- !any(grepl("^[0-9]+$", pred$athlete_id))
if (!pred_is_name_keyed) results[, pred_key := athlete_id]

finals <- results[!is.na(place) & place > 0L &
                    grepl("final", round, ignore.case = TRUE) &
                    !grepl("semi", round, ignore.case = TRUE)]
# From here on `athlete_id` within `finals` is whatever the predictions use.
finals[, athlete_id := as.character(pred_key)]

# Fail loudly on a namespace mismatch. The previous silent failure produced a
# confident wrong answer rather than an error, which is strictly worse: a
# scoreboard that says the model missed every winner is indistinguishable from a
# model that did.
if (nrow(finals)) {
  shared_ev <- intersect(unique(finals$event_id), unique(pred$event_id))
  fin_ev <- finals[event_id %in% shared_ev]
  hit_rate <- if (nrow(fin_ev)) mean(fin_ev$athlete_id %in% pred$athlete_id) else NA_real_
  cli::cli_alert_info(
    "Finalist->prediction id match: {round(100 * hit_rate)}% ({sum(fin_ev$athlete_id %in% pred$athlete_id)} of {nrow(fin_ev)})."
  )
  if (!is.na(hit_rate) && hit_rate < 0.5) {
    cli::cli_abort(c(
      "Only {round(100 * hit_rate)}% of finalists match a prediction by id.",
      x = "Scores computed on this join would measure the join, not the model.",
      i = "Check that predictions and results use the same athlete id namespace."
    ))
  }
}

cli::cli_h2("Finals")
if (!nrow(finals)) {
  cli::cli_alert_info("No finals complete yet.")
} else {
  ev <- intersect(unique(finals$event_id), unique(pred$event_id))
  cli::cli_alert_info("{length(ev)} final{?s} complete and predicted.")
  if (length(ev)) {
    p <- pred[event_id %in% ev, .(race_id = event_id, athlete_id, p_gold, p_medal)]
    o <- finals[event_id %in% ev, .(race_id = event_id, athlete_id,
                                    hit = place == 1L, hit_medal = place <= 3L)]
    # Only score races whose actual winner we predicted at all; otherwise this
    # measures entry-list coverage rather than the model.
    keep <- o[, .(ok = any(hit)), by = race_id][ok == TRUE]$race_id
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

      cat("\n=== called winners ===\n")
      top <- p[race_id %in% keep][order(race_id, -p_gold)][, .SD[1], by = race_id]
      top <- merge(top, o[hit == TRUE, .(race_id, winner = athlete_id)], by = "race_id")
      top[, called := athlete_id == winner]
      cat(sprintf("favourite won %d of %d final%s (%.0f%%)\n", sum(top$called), nrow(top),
                  if (nrow(top) == 1) "" else "s", 100 * mean(top$called)))
      # Where did the winner rank in our list? "Favourite won" is a harsh binary
      # in a 12-strong field; rank shows whether a miss was a near-miss.
      rk <- p[race_id %in% keep][order(race_id, -p_gold)][, .(rank = which(athlete_id ==
              o[race_id == .BY$race_id & hit == TRUE]$athlete_id[1]), n = .N), by = race_id]
      cat("winner's rank in our list:",
          paste(sprintf("%s of %d", rk$rank, rk$n), collapse = ", "), "\n")

      # Predictions come from entry lists, and athletes withdraw. Probability
      # mass sitting on non-starters is a FIELD error, not a model error, and
      # inflates apparent miscalibration -- so report it rather than absorb it.
      cat("\n=== probability mass on athletes who never appeared ===\n")
      # "Appeared" must NOT be filtered on place > 0. An athlete who fouls out
      # of the high jump or fails to finish still contested the final -- they
      # are a no-mark, which the simulator already models via foul_rate, not a
      # non-starter. Filtering on place counted Shankar and Samarawee (0.10 of
      # gold probability between them) as absent from a final they competed in.
      contested <- unique(results[grepl("final", round, ignore.case = TRUE) &
                                    !grepl("semi", round, ignore.case = TRUE) &
                                    event_id %in% keep,
                                  .(race_id = event_id, athlete_id = pred_key)])
      ghost <- p[race_id %in% keep][!contested, on = .(race_id, athlete_id)]
      # Split what remains: an athlete seen elsewhere at these Games was
      # eliminated earlier (a round-progression question, which simulate_rounds
      # handles), not a withdrawal.
      at_games <- unique(results$pred_key)
      ghost[, eliminated := athlete_id %in% at_games]
      cat(sprintf("  eliminated earlier : %.2f gold prob (%d athlete%s)\n",
                  sum(ghost[eliminated == TRUE]$p_gold), nrow(ghost[eliminated == TRUE]),
                  if (nrow(ghost[eliminated == TRUE]) == 1) "" else "s"))
      cat(sprintf("  never at the Games : %.2f gold prob (%d athlete%s) <- true withdrawals\n",
                  sum(ghost[eliminated == FALSE]$p_gold), nrow(ghost[eliminated == FALSE]),
                  if (nrow(ghost[eliminated == FALSE]) == 1) "" else "s"))
      if (nrow(ghost)) {
        gs <- ghost[, .(lost_gold = sum(p_gold), n = .N), by = race_id][order(-lost_gold)]
        print(gs)
        cat(sprintf("total: %.2f of %d gold probability (%.0f%%) on non-starters\n",
                    sum(gs$lost_gold), length(keep), 100 * sum(gs$lost_gold) / length(keep)))
        cat("Re-predict against actual start lists once published.\n")
      } else cat("none - every predicted athlete started\n")
    }
  }
}

# --- heats: does prior ability order the field? ------------------------------
cli::cli_h2("Ranking check (all completed races)")
CUT <- min(results$date, na.rm = TRUE)
# Read only the events and window needed, from the partitioned store.
# Measured at 8.6M rows: 46.1s to load an .rds and filter it against 0.09s
# here, because partition pruning never opens the other event files.
# flag_implausible() is already applied at store-build time -- it is a
# GLOBAL operation and cannot be redone on a slice.
STORE <- file.path(OUT, "athletics_store")
USE_STORE <- dir.exists(STORE)
champs <- if (USE_STORE) NULL else readRDS(file.path(OUT, "championship_results.rds"))
cal <- readRDS(file.path(OUT, "calibration.rds"))
clean <- if (USE_STORE) NULL else flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]
# Excluded by ID as well as by date. The date cut here is correct (CUT is the
# Games' own first day), but citiusdata#1 showed how easily a date cut goes
# wrong, and a leak would make this script score the model against races it had
# already seen — while looking better, not worse.
past <- if (USE_STORE) {
  read_results_store(STORE, events = unique(results$event_id),
                     from = CUT - 4380, to = CUT - 1L)[
                       !is.na(event_id) & !is.na(perf)]
} else {
  clean[date < CUT & date >= CUT - 4380 & event_id %in% unique(results$event_id)]
}
past <- past[is.na(competition_id) | competition_id != GLASGOW]
stopifnot("history must not contain the competition being scored" =
            !any(past$competition_id == GLASGOW, na.rm = TRUE))
ab <- estimate_ability(past, as_of = CUT, half_life = HALF_LIFE, calibration = cal)

scored <- results[!is.na(place) & place > 0L]
scored[, athlete_id := as.character(athlete_id)]
scored <- merge(scored, ab[, .(athlete_id, event_id, ability)],
                by = c("athlete_id", "event_id"))
cli::cli_alert_info(
  "{nrow(scored)} of {sum(!is.na(results$place) & results$place > 0L)} placed result{?s} have a prior ability estimate."
)

# Grouped by race_key: `place` is within race, so cross-race pairs are invalid.
pw <- scored[, {
  if (.N < 2L) .(conc = 0L, tot = 0L) else {
    g <- expand.grid(i = seq_len(.N), j = seq_len(.N))
    g <- g[g$i < g$j, ]
    .(conc = sum((ability[g$i] > ability[g$j]) == (place[g$i] < place[g$j])), tot = nrow(g))
  }
}, by = .(race_key, event_id, round)]

if (sum(pw$tot)) {
  cat(sprintf("\npooled: %d of %d pair%s correct (%.1f%%) across %d race%s\n",
              sum(pw$conc), sum(pw$tot), if (sum(pw$tot) == 1) "" else "s",
              100 * sum(pw$conc) / sum(pw$tot), nrow(pw[tot > 0]),
              if (nrow(pw[tot > 0]) == 1) "" else "s"))
  cat("(50% is a coin flip; heats are coasted, so this is a floor)\n\n")
  by_ev <- pw[, .(races = .N, pairs = sum(tot),
                  acc = round(100 * sum(conc) / sum(tot), 1)), by = .(event_id, round)]
  print(by_ev[order(-pairs)], nrows = 40)
}
