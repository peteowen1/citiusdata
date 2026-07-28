# Athletics backtest over the full competition harvest.
#
# Ability is re-estimated per meet from performances dated strictly before it
# began, so a meet can never inform its own forecast. That per-meet refit is the
# expensive part — 300k rows each time — so results are cached per meet and the
# script is resumable. Run repeatedly until it reports nothing remaining.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
BT_CACHE <- file.path(OUT, "backtest_cache")
dir.create(BT_CACHE, recursive = TRUE, showWarnings = FALSE)

N_SIMS <- 10000L
MAX_PER_RUN <- as.integer(Sys.getenv("CITIUS_BT_MEETS", "25"))
# History depth per refit. At a 730-day half-life, 8 years back carries weight
# 2^-4 = 6%; 12 years carries 1.5%. Beyond that the compute is pure waste.
HISTORY_DAYS <- as.integer(Sys.getenv("CITIUS_HISTORY_DAYS", "4380"))

champs      <- readRDS(file.path(OUT, "championship_results.rds"))
calibration <- readRDS(file.path(OUT, "calibration.rds"))
# Half-life tuned on ranking skill rather than next-result MAE. fit_half_life()
# optimises point prediction, which favours recency (180 days) and leaves
# w_total below 1 for most athletes - so ability_se dominates and favourites are
# under-rated. 730 days costs no measurable skill and halves the calibration gap.
half_life   <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "730"))

clean <- flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]

# Narrow to the columns actually read, ONCE, before any per-meet filtering.
#
# The per-meet refit brackets this table 825 times. A bracket filter copies
# every column of every passing row, so carrying 33 columns when 8 are read
# means ~4x the allocation per iteration -- and R's gc() does not see the
# resulting growth, only the OS does. Two runs of this backtest were killed with
# no error output, which is what an out-of-memory kill looks like from inside.
# See the data.table RSS notes in C:/dev/.claude/rules.
#
# estimate_ability() reads athlete_id, event_id, date, perf, age, round and
# tier; the finals block additionally needs competition_id, comp_start, place
# and race_key. Anything else (marks, wind, venue, the new feed fields) is
# harvest metadata that no model touches.
keep_cols <- c("athlete_id", "event_id", "date", "perf", "age", "round", "tier",
               "competition_id", "comp_start", "place", "race_key")
clean <- clean[, intersect(keep_cols, names(clean)), with = FALSE]
cli::cli_alert_info(
  "Narrowed to {ncol(clean)} column{?s} ({format(object.size(clean), units = 'MB')})."
)
finals <- clean[!is.na(place) &
                  grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]

# Sample meets evenly across time rather than taking the most recent, so the
# backtest is not all one era.
pool <- unique(finals[, .(competition_id, comp_start)])[!is.na(comp_start) &
                                                          comp_start >= as.Date("2016-01-01")]
setorder(pool, comp_start)
# All meets with finals, not a sample. The old 250 cap dated from when each
# refit took 17s; restricting history to the meet's own events made it 2.5s, so
# the full set is ~35 minutes rather than four hours. At 250 meets the backtest
# used only 13% of the 13,108 available finals.
TARGET <- as.integer(Sys.getenv("CITIUS_BT_TARGET", "900"))
if (nrow(pool) > TARGET) pool <- pool[round(seq(1, .N, length.out = TARGET))]

todo <- pool[!file.exists(file.path(BT_CACHE, paste0(competition_id, ".rds")))]
cli::cli_alert_info("{nrow(todo)} of {nrow(pool)} meet{?s} remaining.")

n <- min(nrow(todo), MAX_PER_RUN)
for (i in seq_len(n)) {
  cid <- todo$competition_id[i]
  cut_date <- todo$comp_start[i]
  block <- finals[competition_id == cid]

  # Two restrictions make the per-meet refit ~10x cheaper without changing a
  # single prediction:
  #
  #  1. Only estimate ability for the events this meet actually contests.
  #     Previously every meet refitted all 46 events to use maybe 8 of them.
  #  2. Only use history within HISTORY_YEARS of the cut. At a 730-day
  #     half-life a mark from 2010 carries weight 2^-8, which cannot move an
  #     estimate but is fully paid for in compute.
  #
  # Both are exact given the decay, not approximations that trade accuracy.
  meet_events <- unique(block$event_id)
  past <- clean[date < cut_date & date >= cut_date - HISTORY_DAYS &
                  event_id %in% meet_events]
  if (nrow(past) < 2000L) { saveRDS(list(), file.path(BT_CACHE, paste0(cid, ".rds"))); next }
  ability <- estimate_ability(past, as_of = cut_date, half_life = half_life,
                              calibration = calibration)

  out <- list()
  # Score one RACE, not one competition+event. Club and gala meets run an event
  # in many sections, each labelled "Final" -- Sparkassen Gala 2026 ran the
  # women's 200m as 18 separate finals. Keying on competition+event merged them
  # into a single scored race with 18 winners, inflating the field and awarding
  # many golds. That alone put 16.9% of scored races on more than one winner;
  # keyed by race_key it is 0.4%, which is the sport's genuine tie rate.
  #
  # The damage was not confined to those races: the merged ones looked like huge
  # fields, which is why calibration appeared to degrade with field size.
  for (rk in unique(block$race_key)) {
    field <- unique(block[race_key == rk], by = "athlete_id")
    ev <- field$event_id[1]
    entrants <- ability[event_id == ev &
                          athlete_id %in% as.character(field$athlete_id)]
    if (nrow(entrants) < 4L) next
    sim <- simulate_event(entrants, n_sims = N_SIMS,
                          calibration = calibration, seed = 11L)
    mp <- medal_probs(sim)
    key <- rk
    mp[, race_id := key]
    out[[length(out) + 1L]] <- list(
      pred = mp,
      outc = data.table(
        race_id = key, athlete_id = mp$athlete_id,
        hit = mp$athlete_id %in% as.character(field[place == 1L]$athlete_id),
        hit_medal = mp$athlete_id %in% as.character(field[place <= 3L]$athlete_id)))
  }
  saveRDS(out, file.path(BT_CACHE, paste0(cid, ".rds")))
  cli::cli_alert("  {i}/{n}: {cid} -> {length(out)} race{?s}")
}

# --- assemble and score ------------------------------------------------------
blobs <- unlist(lapply(list.files(BT_CACHE, full.names = TRUE), readRDS), recursive = FALSE)
blobs <- Filter(function(b) is.list(b) && !is.null(b$pred), blobs)
if (!length(blobs)) { cli::cli_alert_warning("Nothing scored yet."); quit(save = "no") }

pred <- rbindlist(lapply(blobs, `[[`, "pred"), fill = TRUE)
outc <- rbindlist(lapply(blobs, `[[`, "outc"), fill = TRUE)

cov <- outc[, .(wp = any(hit)), by = race_id]
keep <- cov[wp == TRUE]$race_id
cli::cli_alert_info(
  "{nrow(cov)} race{?s} scored; winner in field for {length(keep)} ({round(100*length(keep)/nrow(cov))}%)."
)

gold <- score_predictions(pred[race_id %in% keep], outc[race_id %in% keep], "p_gold")
medal <- score_predictions(pred[race_id %in% keep],
                           outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")

cli::cli_h2("Athletics backtest (winner-in-field)")
cat(sprintf("gold  brier %.4f vs %.4f  skill %+.3f  (%d races)\n",
            gold$overall$brier, gold$overall$brier_baseline,
            gold$overall$brier_skill, gold$overall$n_races))
cat(sprintf("medal brier %.4f vs %.4f  skill %+.3f\n",
            medal$overall$brier, medal$overall$brier_baseline, medal$overall$brier_skill))
cat("\nreliability:\n"); print(gold$reliability[n >= 20])
br <- gold$by_race
cat(sprintf("\nraces beating baseline: %d of %d (%.0f%%)\n",
            sum(br$skill > 0), nrow(br), 100 * mean(br$skill > 0)))

saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc),
        file.path(OUT, "backtest.rds"))
