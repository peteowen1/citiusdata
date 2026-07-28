# Glasgow 2026 swimming: predict every final, then score it (citiusdata#3).
#
# The meet is finished, so this is a full retrospective forward-test: ability is
# estimated only from World Aquatics history dated before the Games, and scored
# against what actually happened.
#
# Why this did not exist until now: the two feeds order names oppositely --
# World Aquatics writes "SJOESTROEM Sarah", the Games results system writes
# "Hannah STERRY" -- so a straight name key matched 1 of 388 swimmers to their
# own history. `athlete_key()` sorts the name tokens and lifts that to 46%.
#
# Coverage is honest rather than complete: 63% of finalists and 68% of
# medallists carry World Aquatics history. A Commonwealth entry list includes
# many swimmers from small federations with none, and no amount of matching
# invents a history that does not exist. Finals whose WINNER is unrated are
# reported but excluded from scoring, because there they measure coverage rather
# than the model.
#
# There is no leak to guard against here the way there is for athletics: Glasgow
# swimming reaches us through the CRS scrape and is not in swimming_history at
# all. Asserted below rather than assumed.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
N_SIMS <- 20000L
HALF_LIFE <- as.numeric(Sys.getenv("CITIUS_SWIM_HALF_LIFE", "180"))

g <- setDT(parse_crs_export(file.path(OUT, "glasgow2026_swimming.json")))
g <- g[!is.na(event_id)]
sw <- setDT(readRDS(file.path(OUT, "swimming_history.rds")))
cal <- readRDS(file.path(OUT, "calibration_swimming.rds"))
CUT <- min(g$date, na.rm = TRUE)
cli::cli_alert_info("Glasgow swimming: {nrow(g)} swim{?s}, {uniqueN(g$event_id)} event{?s}, cut {format(CUT)}.")

stopifnot("Glasgow must not be inside the swimming history" =
            !any(sw$date >= CUT, na.rm = TRUE) ||
            !any(sw$competition_id %in% unique(g$competition_id), na.rm = TRUE))

# --- link Glasgow swimmers to their history ---------------------------------
lk <- unique(sw[!is.na(athlete_name), .(key = athlete_key(athlete_name),
                                        hist_id = as.character(athlete_id))])
lk <- lk[!is.na(key), .(hist_id = hist_id[1]), by = key]
g[, key := athlete_key(athlete_name)]
g <- merge(g, lk, by = "key", all.x = TRUE)
cli::cli_alert_info(
  "Linked {uniqueN(g[!is.na(hist_id)]$athlete_name)} of {uniqueN(g$athlete_name)} swimmer{?s} to World Aquatics history."
)

hist <- flag_implausible(sw)[!is.na(event_id) & !is.na(perf) & date < CUT]
ability <- estimate_ability(hist, as_of = CUT, half_life = HALF_LIFE, calibration = cal)

# --- predict each final ------------------------------------------------------
fin <- g[grepl("final", round, ignore.case = TRUE) & !grepl("semi", round, ignore.case = TRUE)]
preds <- list(); outs <- list()
for (ev in sort(unique(fin$event_id))) {
  field <- unique(fin[event_id == ev], by = "athlete_name")
  ent <- ability[event_id == ev & athlete_id %in% field$hist_id]
  if (nrow(ent) < 3L) next
  sim <- simulate_event(ent, n_sims = N_SIMS, calibration = cal, seed = 20260728L)
  mp <- medal_probs(sim)
  pos <- position_probs(sim, max_position = 8L, wide = TRUE)
  mp <- merge(mp, pos, by = "athlete_id", all.x = TRUE)
  mp[, `:=`(event_id = ev, race_id = ev)]
  nm <- unique(field[!is.na(hist_id), .(athlete_id = hist_id, athlete_name)])
  mp <- merge(mp, nm, by = "athlete_id", all.x = TRUE)
  preds[[length(preds) + 1L]] <- mp
  outs[[length(outs) + 1L]] <- data.table(
    race_id = ev, athlete_id = mp$athlete_id,
    hit = mp$athlete_id %in% field[place == 1L]$hist_id,
    hit_medal = mp$athlete_id %in% field[place <= 3L]$hist_id)
}
if (!length(preds)) {
  cli::cli_alert_danger("No final had enough rated entrants."); quit(save = "no")
}
pred <- rbindlist(preds, fill = TRUE); outc <- rbindlist(outs, fill = TRUE)
pred[, generated_at := Sys.time()]
arrow::write_parquet(pred, file.path(OUT, "glasgow2026_swimming_predictions.parquet"))

# --- score, on races where the winner was rated -----------------------------
cov <- outc[, .(winner_rated = any(hit)), by = race_id]
keep <- cov[winner_rated == TRUE]$race_id
cli::cli_alert_info(
  "{nrow(cov)} final{?s} predicted; winner rated in {length(keep)} ({round(100*length(keep)/nrow(cov))}%)."
)
if (length(keep)) {
  gold <- score_predictions(pred[race_id %in% keep, .(race_id, athlete_id, p_gold)],
                            outc[race_id %in% keep, .(race_id, athlete_id, hit)], "p_gold")
  medal <- score_predictions(pred[race_id %in% keep, .(race_id, athlete_id, p_medal)],
                             outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                             "p_medal")
  cli::cli_h2("Glasgow 2026 swimming (winner-rated finals)")
  cat(sprintf("gold  brier %.4f vs %.4f  skill %+.3f  (%d final%s)\n",
              gold$overall$brier, gold$overall$brier_baseline, gold$overall$brier_skill,
              gold$overall$n_races, if (gold$overall$n_races == 1) "" else "s"))
  cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
              medal$overall$brier, medal$overall$brier_baseline, medal$overall$brier_skill))
  top <- pred[race_id %in% keep][order(race_id, -p_gold)][, .SD[1], by = race_id]
  won <- merge(top, outc[hit == TRUE, .(race_id, w = athlete_id)], by = "race_id")
  cat(sprintf("favourite won %d of %d (%.0f%%)\n", sum(won$athlete_id == won$w),
              nrow(won), 100 * mean(won$athlete_id == won$w)))
}

cli::cli_h2("Per final")
setorder(pred, event_id, -p_gold)
for (ev in unique(pred$event_id)) {
  x <- pred[event_id == ev]
  fieldsz <- nrow(unique(fin[event_id == ev], by = "athlete_name"))
  cat(sprintf("\n%s  (%d rated of %d)%s\n", ev, nrow(x), fieldsz,
              if (!ev %in% keep) "  [winner unrated - not scored]" else ""))
  print(head(x[, .(athlete = substr(athlete_name, 1, 22), gold = round(p_gold, 3),
                   medal = round(p_medal, 3), pos_4 = round(pos_4, 3))], 4))
}
