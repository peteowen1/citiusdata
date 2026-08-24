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
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))

OUT <- here::here("citiusdata", "data")
N_SIMS <- 20000L
HALF_LIFE <- .env_num("CITIUS_SWIM_HALF_LIFE", "180")

g <- glasgow_swimming(OUT)
g <- g[!is.na(event_id)]
sw <- setDT(readRDS(file.path(OUT, "swimming_history.rds")))
cal <- readRDS(file.path(OUT, "calibration_swimming.rds"))
CUT <- min(g$date, na.rm = TRUE)
cli::cli_alert_info("Glasgow swimming: {nrow(g)} swim{?s}, {uniqueN(g$event_id)} event{?s}, cut {format(CUT)}.")

stopifnot("Glasgow must not be inside the swimming history" =
            !any(sw$date >= CUT, na.rm = TRUE) ||
            !any(sw$competition_id %in% unique(g$competition_id), na.rm = TRUE))

# --- link Glasgow swimmers to their history ---------------------------------
# Primary: the cross-source crosswalk (person_id), which resolves identity
# across World Aquatics/CRS/SwimEngland/SwimCloud instead of guessing from name
# order alone. Falls back to the athlete_key() name-token match only where the
# crosswalk has no link -- costs nothing, no regression risk (citius#3).
xw <- setDT(arrow::read_parquet(file.path(OUT, "athlete_crosswalk_swimming.parquet")))
xw_g  <- unique(xw[source == "crs_glasgow2026" & !is.na(person_id),
                    .(athlete_name, person_id)])[, .SD[1L], by = athlete_name]
xw_wa <- unique(xw[source == "worldaquatics" & !is.na(person_id),
                    .(person_id, wa_id = athlete_id)])[, .SD[1L], by = person_id]
xw_link <- merge(xw_g, xw_wa, by = "person_id")[, .(athlete_name, wa_id)]

lk <- unique(sw[!is.na(athlete_name), .(key = athlete_key(athlete_name),
                                        hist_id = as.character(athlete_id))])
lk <- lk[!is.na(key), .(hist_id = hist_id[1]), by = key]
g[, key := athlete_key(athlete_name)]
g <- merge(g, lk, by = "key", all.x = TRUE)
g <- merge(g, xw_link, by = "athlete_name", all.x = TRUE)
g[, hist_id := fifelse(!is.na(wa_id), wa_id, hist_id)][, wa_id := NULL]
cli::cli_alert_info(
  "Linked {uniqueN(g[!is.na(hist_id)]$athlete_name)} of {uniqueN(g$athlete_name)} swimmer{?s} to World Aquatics history."
)

# In-meet rounds are prior form for the final, and they are what makes the field
# complete. Only 72% of finalists carry World Aquatics history -- a Commonwealth
# entry list includes many swimmers whose first international meet this is -- but
# 100% of them swam a heat in their own event. Heats and semis precede finals,
# so using them is legitimate rather than leakage; it is the same thing
# predict_glasgow_live.R does for athletics.
#
# Swimmers unknown to World Aquatics enter with a single heat swim, so
# ability_se is large and shrinkage heavy. That is the correct treatment: they
# are rated, but weakly, and the simulator knows it.
pre <- g[!grepl("final", round, ignore.case = TRUE) | grepl("semi", round, ignore.case = TRUE)]
pre_hist <- pre[!is.na(perf) & !is.na(event_id),
                .(athlete_id = fifelse(is.na(hist_id), paste0("CRS:", key), hist_id),
                  event_id, perf, date, round, tier = "top",
                  sport = "Swimming", competition_id = -1L, race_key = race_key)]
hist <- flag_implausible(sw)[!is.na(event_id) & !is.na(perf) & date < CUT]
hist <- rbind(hist, pre_hist, fill = TRUE)
cli::cli_alert_info(
  "Ability from {format(nrow(hist), big.mark = ',')} swim{?s}, including {nrow(pre_hist)} in-meet round{?s}."
)
# as_of is the FINALS date, not the meet start, so the in-meet swims are not
# discounted as though they were a week old.
AS_OF <- max(g$date, na.rm = TRUE)
ability <- estimate_ability(hist, as_of = AS_OF, half_life = HALF_LIFE, calibration = cal)

# --- predict each final ------------------------------------------------------
fin <- g[grepl("final", round, ignore.case = TRUE) & !grepl("semi", round, ignore.case = TRUE)]
preds <- list(); outs <- list()
for (ev in sort(unique(fin$event_id))) {
  field <- unique(fin[event_id == ev], by = "athlete_name")
  # Match on the World Aquatics id where we have one, otherwise on the CRS
  # surrogate built from the name key.
  field[, use_id := fifelse(is.na(hist_id), paste0("CRS:", key), hist_id)]
  ent <- ability[event_id == ev & athlete_id %in% field$use_id]
  if (nrow(ent) < 3L) next
  sim <- simulate_event(ent, n_sims = N_SIMS, calibration = cal, seed = 20260728L)
  mp <- medal_probs(sim)
  pos <- position_probs(sim, max_position = 8L, wide = TRUE)
  mp <- merge(mp, pos, by = "athlete_id", all.x = TRUE)
  mp[, `:=`(event_id = ev, race_id = ev)]
  nm <- unique(field[, .(athlete_id = use_id, athlete_name)])
  mp <- merge(mp, nm, by = "athlete_id", all.x = TRUE)
  preds[[length(preds) + 1L]] <- mp
  outs[[length(outs) + 1L]] <- data.table(
    race_id = ev, athlete_id = mp$athlete_id,
    hit = mp$athlete_id %in% field[place == 1L]$use_id,
    hit_medal = mp$athlete_id %in% field[place <= 3L]$use_id)
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
