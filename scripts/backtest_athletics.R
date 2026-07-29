# Athletics backtest over the full competition harvest.
#
# Ability is re-estimated per meet from performances dated strictly before it
# began, so a meet can never inform its own forecast. That per-meet refit is the
# expensive part — 300k rows each time — so results are cached per meet and the
# script is resumable. Run repeatedly until it reports nothing remaining.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
BT_CACHE <- file.path(OUT, Sys.getenv("CITIUS_BT_CACHE", "backtest_cache"))
dir.create(BT_CACHE, recursive = TRUE, showWarnings = FALSE)

N_SIMS <- 10000L
MAX_PER_RUN <- as.integer(Sys.getenv("CITIUS_BT_MEETS", "25"))
# History depth per refit. TWELVE YEARS, and do not shorten it on the argument
# that old marks carry negligible weight.
#
# That argument was tried on 2026-07-29 and is wrong. An individual mark seven
# years old carries 2^-7 = 0.8% at a 365-day half-life, which is indeed nothing
# for the weighted MEAN. But `w_total` is a SUM, and it drives shrinkage: an
# athlete with 50 marks in years 7-12 contributes ~0.4 to a w_total that averages
# 1.59 across the backtest. Cutting the window to seven years changed 4,397 of
# 4,809 predictions, moving p_gold by up to 0.246 and predicted marks by up to
# 5.3%.
#
# Negligible for the mean, decisive for the sum. Verified, not assumed.
HISTORY_DAYS <- as.integer(Sys.getenv("CITIUS_HISTORY_DAYS", "4380"))

# OUTCOMES and HISTORY are separate inputs so two ability sources can be compared
# on an IDENTICAL scored set. Pointing one file at both roles silently swaps the
# test set along with the model: the swimming A/B did exactly that and compared
# 43 competitions against 5,868, which measures coverage, not skill. Outcomes must
# always come from the competition harvest -- it is the only route that carries
# whole fields with finishing places.
OUTCOMES <- Sys.getenv("CITIUS_BT_OUTCOMES", "championship_results.rds")
HISTORY  <- Sys.getenv("CITIUS_BT_HISTORY", OUTCOMES)
champs      <- readRDS(file.path(OUT, OUTCOMES))
hist_raw    <- if (identical(HISTORY, OUTCOMES)) champs else readRDS(file.path(OUT, HISTORY))
calibration <- readRDS(file.path(OUT, Sys.getenv("CITIUS_BT_CALIBRATION", "calibration.rds")))
cli::cli_alert_info("Outcomes from {.file {OUTCOMES}}; ability history from {.file {HISTORY}}.")
# 365 days, selected by A/B on out-of-sample RANKING skill over 5,872 backtest
# races -- not by fit_half_life(), which optimises next-result MAE and returns 90
# for sprints. Measured 2026-07-29 across six arms on an identical scored set:
# gold skill 0.183 (90d), 0.224 (180), 0.234 (270), 0.237 (365), 0.236 (540),
# 0.234 (730). Paired t-test vs 365: beats 730 (t=5.78), 540 (t=4.23), 180
# (t=3.44) and 90 (t=10.52); tied with 270. It also cuts the top-band
# over-confidence from -0.106 to -0.073.
half_life   <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "365"))
# 0 = shrink toward the unconditional event mean (previous behaviour);
# 1 = shrink fully toward the field being predicted. See condition_prior().
#
# 0.5 adopted 2026-07-29. Paired over 5,867 races against weight 0:
#   marks  MAE 2.642% -> 2.533%, bias -0.480% -> -0.202%
#   medal  Brier diff +0.00089, t = +8.33  (wins)
#   gold   Brier diff -0.00013, t = -1.93, p = 0.053  (no significant cost)
# Weight 1.0 predicts marks best of all (MAE 2.497%, bias +0.081%) but
# SIGNIFICANTLY damages gold (t = -6.34) and drops AUC 0.8461 -> 0.8392: shrinking
# everyone toward the field mean compresses the field and blurs the favourite's
# edge. 0.5 takes most of the mark gain without paying that.
PRIOR_WEIGHT <- as.numeric(Sys.getenv("CITIUS_PRIOR_WEIGHT", "0.5"))

# --- DEV HARNESS: restrict to a named set of athletes ------------------------
# The full run takes ~2.8 hours on the corpus, which is too slow to iterate on.
# Restricting to championship-calibre athletes cuts it to roughly a fifth, and it
# is also the population this package exists for -- there is little point tuning
# on club runners who will never contest a Games.
#
# THIS IS A DEV HARNESS, NOT A REPORTING ONE. Two reasons its absolute numbers
# must never be quoted:
#
#  1. Restricting the history changes `prior_mu` and `sigma_between`, which are
#     population statistics. So this is a DIFFERENT MODEL, not a subsample of the
#     full one, and its MAE is not comparable to the headline.
#  2. Athletes are selected by having reached a top-tier final at some point,
#     which for an early cut date uses information from after it. Harmless when
#     every arm gets the identical set and only their ORDERING is read; not
#     harmless if the number itself is reported.
#
# Validated by reproducing the arm ordering of known full runs -- see
# scripts/validate_dev_harness.R.
ATHLETES <- Sys.getenv("CITIUS_BT_ATHLETES", "")
dev_ids <- if (nzchar(ATHLETES)) {
  as.character(readRDS(file.path(OUT, ATHLETES)))
} else NULL
if (!is.null(dev_ids)) {
  cli::cli_alert_warning(
    "DEV HARNESS: history restricted to {format(length(dev_ids), big.mark = ',')} athletes. Absolute metrics are NOT comparable to a full run."
  )
}

clean <- flag_implausible(hist_raw)[!is.na(event_id) & !is.na(perf)]
if (!is.null(dev_ids)) clean <- clean[as.character(athlete_id) %in% dev_ids]
outcome_rows <- if (identical(HISTORY, OUTCOMES)) {
  clean
} else {
  flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]
}

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
# `wind` is read by estimate_ability() when the calibration carries a wind
# coefficient. Narrowing it away would silently disable the adjustment and the
# A/B would report a dead heat.
keep_cols <- c("athlete_id", "event_id", "date", "perf", "age", "round", "tier",
               "competition_id", "comp_start", "place", "race_key", "wind")
clean <- clean[, intersect(keep_cols, names(clean)), with = FALSE]
outcome_rows <- outcome_rows[, intersect(keep_cols, names(outcome_rows)), with = FALSE]

# Prefer the partitioned parquet store when it exists: the per-meet read drops
# from 46.1s to 0.39s at 8.6M rows. The .rds path is kept so the script still
# runs before build_stores.R has been run.
STORE <- file.path(OUT, Sys.getenv("CITIUS_BT_STORE", "athletics_store"))
# The store is built from ONE history file. Reading it while HISTORY points
# somewhere else would silently ignore the arm under test and run the baseline
# twice -- the A/B would come back a dead heat and look like a null result.
USE_STORE <- dir.exists(STORE) && (identical(HISTORY, OUTCOMES) ||
                                   nzchar(Sys.getenv("CITIUS_BT_STORE")))
cli::cli_alert_info(if (USE_STORE) "Reading history from the parquet store."
                    else "No parquet store; filtering the in-memory corpus.")
# Ask the store only for columns it holds. `comp_start` and `place` are OUTCOME
# fields used to build the meet pool; the history side never reads them, and the
# corpus store does not carry comp_start at all. Requesting them aborted the run.
STORE_COLS <- if (USE_STORE)
  intersect(keep_cols, names(arrow::open_dataset(STORE))) else keep_cols
cli::cli_alert_info(
  "Narrowed to {ncol(clean)} column{?s} ({format(object.size(clean), units = 'MB')})."
)
finals <- outcome_rows[!is.na(place) &
                  grepl("final", round, ignore.case = TRUE) &
                  !grepl("semi", round, ignore.case = TRUE)]
if (!is.null(dev_ids)) {
  finals <- finals[as.character(athlete_id) %in% dev_ids]
  finals[, .n_dev := .N, by = race_key]
  finals <- finals[.n_dev >= 4L][, .n_dev := NULL]
}

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

# Per-phase timing, so an optimisation is aimed rather than guessed. Written to
# the log every meet and summarised at the end.
TIMING <- new.env(parent = emptyenv())
TIMING$read <- 0; TIMING$ability <- 0; TIMING$sim <- 0; TIMING$rows <- 0
tick <- function(slot, expr) {
  t0 <- Sys.time()
  out <- force(expr)
  assign(slot, get(slot, TIMING) + as.numeric(difftime(Sys.time(), t0, units = "secs")), TIMING)
  out
}

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
  # Read only this meet's events and date window from the partitioned store.
  # Measured on 8.6M rows: 46.1s to load an .rds and filter it, against 0.39s
  # here, because partition pruning never opens the other 80-odd event files.
  # Falls back to the in-memory corpus when no store exists.
  past <- tick("read", if (USE_STORE) {
    read_results_store(STORE, events = meet_events,
                       from = cut_date - HISTORY_DAYS, to = cut_date - 1L,
                       columns = STORE_COLS)
  } else {
    clean[date < cut_date & date >= cut_date - HISTORY_DAYS &
            event_id %in% meet_events]
  })
  if (!is.null(dev_ids)) past <- past[as.character(athlete_id) %in% dev_ids]
  TIMING$rows <- TIMING$rows + nrow(past)
  if (nrow(past) < 2000L) { saveRDS(list(), file.path(BT_CACHE, paste0(cid, ".rds"))); next }
  ability <- tick("ability", estimate_ability(past, as_of = cut_date,
                                             half_life = half_life,
                                             calibration = calibration))

  # Key ONCE per meet, not once per race. The loop below previously bracket-filtered
  # `ability` for every race -- O(races x nrow(ability)) -- which is cheap on the
  # 308k harvest and expensive on the 4.99M corpus, where a single event carries
  # 13,506 rated athletes. A keyed join is a binary search instead of a full scan.
  #
  # `.ord` preserves the original row order. `ability[cond]` returns rows in
  # ability's order; a keyed join returns them in key order. That matters because
  # simulate_event() draws with a fixed seed, so a different row order would
  # silently change every simulation -- not wrongly, but not comparably either,
  # and every A/B in the log would shift for no reason.
  ability[, .ord := .I]
  data.table::setkey(ability, event_id, athlete_id)
  # Split once rather than bracketing `block` inside the loop for the same reason.
  by_race <- split(block, block$race_key)

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
  for (rk in names(by_race)) {
    field <- unique(by_race[[rk]], by = "athlete_id")
    ev <- field$event_id[1]
    entrants <- ability[.(ev, as.character(field$athlete_id)), nomatch = NULL]
    if (nrow(entrants) < 4L) next
    data.table::setorder(entrants, .ord)
    # Optional: shrink toward the FIELD rather than the whole event. Empirical
    # Bayes otherwise pulls a thinly-evidenced entrant toward the unconditional
    # event mean, which includes a long tail of athletes who never contest a
    # final -- measured at a median +1.36% below the finalist population, and the
    # predicted-mark bias runs to -2.18% for athletes shrunk over 60%.
    if (PRIOR_WEIGHT > 0) {
      entrants <- condition_prior(entrants, field = entrants$athlete_id,
                                  weight = PRIOR_WEIGHT)
    }
    sim <- tick("sim", simulate_event(entrants, n_sims = N_SIMS,
                                      calibration = calibration, seed = 11L))
    mp <- medal_probs(sim)
    key <- rk
    mp[, race_id := key]
    # Carry the evidence weight alongside the probability. Hypotheses about
    # shrinkage stayed untestable for weeks because the quantity they were about
    # was never written down next to the prediction it supposedly explained.
    if ("shrinkage" %in% names(entrants))
      mp[entrants, on = "athlete_id", shrinkage := i.shrinkage]
    if ("w_total" %in% names(entrants))
      mp[entrants, on = "athlete_id", w_total := i.w_total]
    out[[length(out) + 1L]] <- list(
      pred = mp,
      outc = data.table(
        race_id = key, athlete_id = mp$athlete_id,
        hit = mp$athlete_id %in% as.character(field[place == 1L]$athlete_id),
        hit_medal = mp$athlete_id %in% as.character(field[place <= 3L]$athlete_id)))
  }
  saveRDS(out, file.path(BT_CACHE, paste0(cid, ".rds")))
  cli::cli_alert("  {i}/{n}: {cid} -> {length(out)} race{?s}  [read {round(TIMING$read)}s ability {round(TIMING$ability)}s sim {round(TIMING$sim)}s]")
}

cli::cli_h3("Timing")
tot <- TIMING$read + TIMING$ability + TIMING$sim
cli::cli_alert_info(
  "read {round(TIMING$read)}s ({round(100*TIMING$read/tot)}%) | ability {round(TIMING$ability)}s ({round(100*TIMING$ability/tot)}%) | simulate {round(TIMING$sim)}s ({round(100*TIMING$sim/tot)}%) | {format(TIMING$rows, big.mark=',')} history rows read"
)

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
        file.path(OUT, Sys.getenv("CITIUS_BT_OUT", "backtest.rds")))
