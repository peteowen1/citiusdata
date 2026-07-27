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
# the raceNumber fix in competition_results() made possible.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
GLASGOW <- 7187518L
HALF_LIFE <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "730"))

results <- tryCatch(setDT(harvest_competitions(GLASGOW)), error = function(e) NULL)
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

finals <- results[!is.na(place) & place > 0L &
                    grepl("final", round, ignore.case = TRUE) &
                    !grepl("semi", round, ignore.case = TRUE)]
finals[, athlete_id := as.character(athlete_id)]

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
    }
  }
}

# --- heats: does prior ability order the field? ------------------------------
cli::cli_h2("Ranking check (all completed races)")
CUT <- min(results$date, na.rm = TRUE)
champs <- readRDS(file.path(OUT, "championship_results.rds"))
cal <- readRDS(file.path(OUT, "calibration.rds"))
clean <- flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]
past <- clean[date < CUT & date >= CUT - 4380 & event_id %in% unique(results$event_id)]
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
