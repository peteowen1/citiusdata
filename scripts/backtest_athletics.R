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
# DEFAULT HISTORY IS THE CORPUS as of 2026-07-29. Paired against the harvest on
# 28,737 common marks with the aging file, cohort and outcomes all held fixed:
# marks MAE 2.216% -> 2.007% (t = +23.8), gold Brier +0.0113 (t = +18.5), medal
# +0.0273 (t = +24.2). That is 10-50x any parameter change adopted the same day,
# and it holds while LACKING several of them. Mean w_total 1.59 -> 7.47.
HISTORY  <- Sys.getenv("CITIUS_BT_HISTORY", "athletics_corpus.rds")
champs      <- readRDS(file.path(OUT, OUTCOMES))
hist_raw    <- if (identical(HISTORY, OUTCOMES)) champs else readRDS(file.path(OUT, HISTORY))
# Resolved ONCE. This line and the meta block below used to call Sys.getenv()
# separately with DIFFERENT defaults -- "calibration_corpus.rds" here and
# "calibration.rds" there -- so an arm run without the variable set loaded one
# calibration and recorded the name and md5 of another. cp0's meta claims
# "calibration.rds", a file it never opened.
CALIBRATION <- Sys.getenv("CITIUS_BT_CALIBRATION", "calibration_corpus.rds")
calibration <- readRDS(file.path(OUT, CALIBRATION))
# The aging curve. Found missing from this script on 2026-07-29: project_ability()
# is applied in predict_glasgow2026.R but was never called here, so the backtest
# was measuring a DIFFERENT pipeline from the one that ships -- and every
# parameter tuned against it was tuned without ageing. Set CITIUS_BT_AGING empty
# to reproduce the old, ageing-free behaviour.
# Momentum: an exponentially decayed count of recent race days. Set
# CITIUS_BT_MOMENTUM to a fitted per-family effect table to enable it. The
# history must carry a `momentum` column (see build_mom.R) and the entrants'
# momentum AT THE CUT DATE is computed below from that same history -- so it uses
# only what was knowable before the meet.
MOM_FILE <- Sys.getenv("CITIUS_BT_MOMENTUM", "")
mom_eff <- if (nzchar(MOM_FILE) && file.exists(file.path(OUT, MOM_FILE)))
  as.data.table(readRDS(file.path(OUT, MOM_FILE))) else NULL
PEAK_GAMMA <- as.numeric(Sys.getenv("CITIUS_BT_PEAK_GAMMA", "0"))
ROBUST_LOCATION <- as.logical(Sys.getenv("CITIUS_BT_ROBUST_LOCATION", "FALSE"))
DECOUPLE_PEAK <- as.logical(Sys.getenv("CITIUS_BT_DECOUPLE_PEAK", "FALSE"))
if (!is.null(mom_eff)) {
  calibration$momentum <- mom_eff
  cli::cli_alert_info("Momentum enabled from {.file {MOM_FILE}} ({nrow(mom_eff)} famil{?y/ies}).")
}

AGING_FILE <- Sys.getenv("CITIUS_BT_AGING", "aging.rds")
aging <- if (nzchar(AGING_FILE) && file.exists(file.path(OUT, AGING_FILE)))
  readRDS(file.path(OUT, AGING_FILE)) else NULL
cli::cli_alert_info(if (is.null(aging)) "No aging curve: ability is NOT age-projected."
                    else paste("Age projection from", AGING_FILE))
cli::cli_alert_info("Outcomes from {.file {OUTCOMES}}; ability history from {.file {HISTORY}}.")
# 365 days, selected by A/B on out-of-sample RANKING skill over 5,872 backtest
# races -- not by fit_half_life(), which optimises next-result MAE and returns 90
# for sprints. Measured 2026-07-29 across six arms on an identical scored set:
# gold skill 0.183 (90d), 0.224 (180), 0.234 (270), 0.237 (365), 0.236 (540),
# 0.234 (730). Paired t-test vs 365: beats 730 (t=5.78), 540 (t=4.23), 180
# (t=3.44) and 90 (t=10.52); tied with 270. It also cuts the top-band
# over-confidence from -0.106 to -0.073.
half_life   <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "365"))
# Optionally vary the half-life BY FAMILY. fit_half_life() finds a 6x spread --
# road 1095 days against 180 for sprint, throw, middle, distance and hurdles --
# and a single global value is applied to all of them. The families with the most
# headroom above their own noise floor (distance 1.54x, road 1.60x) are precisely
# the ones whose fitted half-life is furthest from 365.
# Adopted 2026-07-29: road 1095, walk 730, everything else the global 365.
#
# Measured, not guessed. A profile that also lengthened distance and middle to
# 545 gained nothing there (+0.004 and -0.002 MAE), so only the two families with
# real evidence are varied. Paired over 28,737 marks the change is t = +8.24,
# p = 1.8e-16 on marks and NOT significant on gold (p = 0.78).
#
# The mechanism is race FREQUENCY, not physiology. A marathoner races twice a
# year, so under a 365-day half-life their previous marathon carries 0.5 and the
# one before 0.25 -- a two-race athlete has almost no evidence left. The right
# knob is probably observation frequency rather than family, which would
# generalise to any sparsely-raced athlete; this is the cheap version.
HL_BY_FAMILY <- Sys.getenv("CITIUS_HALF_LIFE_FAMILY", "road=1095,walk=730")
hl_map <- if (nzchar(HL_BY_FAMILY)) {
  kv <- strsplit(strsplit(HL_BY_FAMILY, ",")[[1]], "=")
  stats::setNames(as.numeric(vapply(kv, `[`, character(1), 2)),
                  vapply(kv, `[`, character(1), 1))
} else NULL
if (!is.null(hl_map)) cli::cli_alert_info(
  "Per-family half-life: {paste(names(hl_map), round(hl_map), sep = '=', collapse = ', ')}")
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
# "event" gives every athlete their event's measured spread instead of their own.
# A test, not a preference: per-athlete sigma REORDERS the field at the
# simulation stage -- in the men's 100m, rank correlation with recent form falls
# from 0.736 at the ability stage to 0.573 at p_gold -- because the win
# probability rewards being unpredictable. This asks whether that reordering
# carries information or destroys it.
SIGMA_MODE <- Sys.getenv("CITIUS_BT_SIGMA_MODE", "athlete")

# Round and tier adjustment, on unless explicitly switched off. There was no way
# to run without it, so the layer had never been measured against its own
# absence -- and it is the current suspect for the 400m and throws, where our
# ability estimate correlates with the truth WORSE than a plain last-five mean
# (0.595 vs 0.648 and 0.694 vs 0.726). Set CITIUS_BT_CONTEXT=off for that arm.
ADJUST_CONTEXT <- !identical(tolower(Sys.getenv("CITIUS_BT_CONTEXT", "on")), "off")
# Use the catalogue's meet_tier for the context adjustment instead of the feed's
# per-result `tier`, which varies within a single meet and labels the Diamond
# League "low". Off by default so it is measured as its own arm.
USE_MEET_TIER <- nzchar(Sys.getenv("CITIUS_BT_MEET_TIER", ""))

# Restrict the SCORED MEETS to given tiers. Decisions are made on T1 (see
# OPTIMISATION-FRAMEWORK.md), but the arm was scoring every tier: of 367 meets,
# 105 are T1 and 262 are T2/T3 -- so 72% of a 90-minute run went to populations
# the framework calls "context only, never the headline".
#
# CITIUS_BT_TIER=T1_elite keeps everything the T1 decision needs: 43 scored
# meets after the holdout, plus 62 before it, which is what score_arm.R fits the
# baseline sigma on. ~25 minutes instead of ~90.
#
# The T2 and "all finals" blocks of the scorecard go empty when this is set, so
# it is a deliberate narrowing rather than a default.
# FAST ITERATION MODE: restrict the HISTORY to athletes who have contested a T1
# final, plus the meet's own entrants.
#
# Not free, and not adopted -- an opt-in screening mode. Measured on a real meet
# (7214023, 16 events, 191 entrants): history rows 2,121,195 -> 453,978 and
# ability estimation 36.2s -> 7.5s, a 4.8x cut, while the median change in final
# ability was 0.014% of a mark against the ~0.8% effects being chased. The max
# was 3.68%, concentrated in athletes whose record is mostly non-elite meets --
# the thinly evidenced ones whose shrinkage leans hardest on the population just
# removed.
#
# It is nearly free because `condition_prior()` runs afterwards and re-conditions
# ability onto the field's own prior. That step is exact and linear
# (ability_new = ability_old + shrinkage * (prior_new - prior_old)), so the
# estimation prior CANCELS. Only sigma_between and the robust-sigma scale k
# survive the restriction, and both move very little.
#
# Use it to screen hypotheses at ~10x with the tier filter, then confirm the
# winner on full history before adopting anything. The median says a screening
# result will nearly always survive; the max says confirmation is not optional.
ELITE_HISTORY <- nzchar(Sys.getenv("CITIUS_BT_ELITE_HISTORY", ""))
TIER_FILTER <- Sys.getenv("CITIUS_BT_TIER", "")
if (USE_MEET_TIER || nzchar(TIER_FILTER) || ELITE_HISTORY) {
  ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
  # The catalogue round-trips competition_id through parquet as character while
  # the harvest holds an integer. A silent type mismatch here would abort the
  # join, or worse, match nothing and leave every meet_tier NA -- which looks
  # exactly like "the fix did nothing".
  ctl[, competition_id := as.character(competition_id)]
  ctl <- ctl[, .(competition_id, meet_tier)]
  if (USE_MEET_TIER) cli::cli_alert_info("Context adjustment uses meet_tier from the catalogue.")
}
if (SIGMA_MODE != "athlete") cli::cli_alert_info("Sigma mode: {SIGMA_MODE}")

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
# TWO SEPARATE KNOBS, deliberately. Conflating them is easy and wrong.
#
#   CITIUS_BT_COHORT   restricts the SCORED RACES to fields containing at least
#                      four of these athletes. This is a MODELLING CHOICE about
#                      what the model is for -- championship races, not club
#                      meets -- and it leaves the model itself untouched, so its
#                      metrics remain directly comparable across arms and to any
#                      other run using the same cohort. It is also most of the
#                      speedup, because the meet pool shrinks and the expensive
#                      per-meet ability refit is skipped along with the meet.
#
#   CITIUS_BT_ATHLETES additionally restricts the HISTORY. That changes prior_mu
#                      and sigma_between, which are population statistics, so it
#                      produces a DIFFERENT MODEL rather than a subsample. Use it
#                      only when speed matters more than comparability, and never
#                      quote the absolute numbers it produces.
COHORT <- Sys.getenv("CITIUS_BT_COHORT", "elite_cohort.rds")
cohort_ids <- if (nzchar(COHORT) && file.exists(file.path(OUT, COHORT))) {
  as.character(readRDS(file.path(OUT, COHORT)))
} else NULL
ATHLETES <- Sys.getenv("CITIUS_BT_ATHLETES", "")
dev_ids <- if (nzchar(ATHLETES)) {
  as.character(readRDS(file.path(OUT, ATHLETES)))
} else NULL
if (!is.null(cohort_ids)) {
  cli::cli_alert_info(
    "Scoring races with 4+ of {format(length(cohort_ids), big.mark = ',')} cohort athletes. History is UNRESTRICTED."
  )
}
if (!is.null(dev_ids)) {
  cli::cli_alert_warning(
    "DEV HARNESS: history ALSO restricted. Absolute metrics are NOT comparable to a full run."
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
               "competition_id", "comp_start", "place", "race_key", "wind",
               "momentum")
clean <- clean[, intersect(keep_cols, names(clean)), with = FALSE]
outcome_rows <- outcome_rows[, intersect(keep_cols, names(outcome_rows)), with = FALSE]

# Prefer the partitioned parquet store when it exists: the per-meet read drops
# from 46.1s to 0.39s at 8.6M rows. The .rds path is kept so the script still
# runs before build_stores.R has been run.
STORE <- file.path(OUT, Sys.getenv("CITIUS_BT_STORE", "athletics_corpus_store"))
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
sel <- if (!is.null(dev_ids)) dev_ids else cohort_ids
if (!is.null(sel)) {
  # Keep the WHOLE field of a qualifying race, not just its cohort members --
  # a medal probability is over everyone who lined up, and dropping the rest
  # would silently redefine the race.
  finals[, .n_sel := sum(as.character(athlete_id) %in% sel), by = race_key]
  finals <- finals[.n_sel >= 4L][, .n_sel := NULL]
}

elite_ids <- NULL
if (ELITE_HISTORY) {
  ec <- outcome_rows[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
                       !grepl("semi", round, ignore.case = TRUE),
                     .(competition_id = as.character(competition_id),
                       athlete_id = as.character(athlete_id))]
  ec <- merge(ec, ctl, by = "competition_id", all.x = TRUE)
  elite_ids <- unique(ec[meet_tier == "T1_elite"]$athlete_id)
  cli::cli_alert_info("Elite history mode: {length(elite_ids)} athlete{?s} have a T1 final.")
  # Zero would silently mean "history is just the entrants", which is a
  # different and much worse model, not a faster one.
  if (!length(elite_ids)) cli::cli_abort(
    "Elite history mode found no T1 finalists; check the catalogue join.")
}

if (nzchar(TIER_FILTER)) {
  want <- trimws(strsplit(TIER_FILTER, ",")[[1]])
  keep <- ctl[meet_tier %in% want]$competition_id
  before <- uniqueN(finals$competition_id)
  finals <- finals[as.character(competition_id) %in% keep]
  after <- uniqueN(finals$competition_id)
  cli::cli_alert_info("Tier filter {.val {want}}: {after} of {before} meets kept.")
  # An empty result here would run zero meets and write an artefact that scores
  # as a clean null. Fail loudly instead -- the usual cause is the parquet/harvest
  # competition_id type mismatch above silently matching nothing.
  if (!after) cli::cli_abort(c(
    "x" = "Tier filter {.val {want}} matched no meets.",
    "i" = "Check the catalogue has those meet_tier values and that
           competition_id types agree."))
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
  if (!is.null(elite_ids)) {
    # Always union the entrants: an entrant without a prior T1 final still needs
    # their own history, or they would be estimated from nothing.
    past <- past[as.character(athlete_id) %in%
                   union(elite_ids, as.character(block$athlete_id))]
  }
  if (USE_MEET_TIER && "competition_id" %in% names(past)) {
    past[, .cid := as.character(competition_id)]
    past <- merge(past, ctl, by.x = ".cid", by.y = "competition_id",
                  all.x = TRUE, sort = FALSE)
    past[, .cid := NULL]
    if (i == 1L) cli::cli_alert_info(
      "meet_tier attached to {round(100*mean(!is.na(past$meet_tier)))}% of history rows.")
  }
  TIMING$rows <- TIMING$rows + nrow(past)
  if (nrow(past) < 2000L) { saveRDS(list(), file.path(BT_CACHE, paste0(cid, ".rds"))); next }
  ability <- if (is.null(hl_map)) {
    tick("ability", estimate_ability(past, as_of = cut_date,
                                     half_life = half_life,
                                     calibration = calibration,
                                     adjust_context = ADJUST_CONTEXT,
                                     sigma_mode = SIGMA_MODE,
                                     only = unique(as.character(block$athlete_id)),
                                     peak_gamma = PEAK_GAMMA,
                                     robust_location = ROBUST_LOCATION,
                                     decouple_peak = DECOUPLE_PEAK))
  } else {
    # Refit per family. estimate_ability takes a single half-life, so split the
    # history by family and stack -- each event only ever belongs to one family,
    # so no athlete-event is estimated twice.
    reg_f <- as.data.table(citius_events()[, c("event_id", "family")])
    pf <- merge(past, reg_f, by = "event_id", all.x = TRUE)
    tick("ability", data.table::rbindlist(lapply(split(pf, pf$family), function(g) {
      hl <- if (!is.na(g$family[1]) && g$family[1] %in% names(hl_map))
        hl_map[[g$family[1]]] else half_life
      estimate_ability(g[, !"family"], as_of = cut_date, half_life = hl,
                       calibration = calibration, adjust_context = ADJUST_CONTEXT,
                       sigma_mode = SIGMA_MODE, peak_gamma = PEAK_GAMMA,
                       robust_location = ROBUST_LOCATION,
                       decouple_peak = DECOUPLE_PEAK)
    }), fill = TRUE))
  }

  # estimate_ability() strips the championship offset from championship history,
  # leaving ability on a NON-championship top-tier-final footing. Every meet
  # scored here is a championship, so it has to go back on -- without this half
  # the correction runs one way and makes predictions worse, not better.
  if (!is.null(calibration$championship) && nrow(calibration$championship)) {
    ability <- project_championship(ability, calibration)
  }

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
    # Age-project onto the day of the race. `age_now` comes from the meet's own
    # rows, which carry each athlete's age on the day; `age_ref` is the weighted
    # mean age behind the estimate. project_ability() scales the shift by
    # (1 - shrinkage), so a heavily-shrunk athlete is not aged as if the event
    # mean were their own career.
    # The athlete's momentum ON THE DAY, from history strictly before the cut.
    if (!is.null(mom_eff) && "momentum" %in% names(past)) {
      last_m <- past[!is.na(momentum), .(momentum = momentum[which.max(date)],
                                         last = max(date)),
                     by = .(athlete_id = as.character(athlete_id))]
      # Decay from that athlete's last race to the day of this one.
      last_m[, momentum_now := momentum * 0.5^(as.numeric(cut_date - last) / 120)]
      entrants <- apply_momentum(entrants, last_m[, .(athlete_id, momentum_now)],
                                 calibration)
    }
    if (!is.null(aging) && "age" %in% names(field)) {
      entrants[field[, .(athlete_id = as.character(athlete_id), age_now = age)],
               on = "athlete_id", age_now := i.age_now]
      ok <- entrants[!is.na(age_now) & !is.na(age_ref)]
      if (nrow(ok)) {
        proj <- suppressWarnings(project_ability(ok, aging))
        entrants[proj, on = "athlete_id", ability := i.ability]
      }
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

# Stamp the run with what it actually scored. A comment warning that arms either
# side of the cohort change are incomparable is worth nothing in a week; a field
# the scoreboard can READ is worth something, because it can refuse to put two
# incomparable arms in the same table.
saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc,
             meta = list(
               cohort = if (is.null(cohort_ids)) "all" else COHORT,
               cohort_n = if (is.null(cohort_ids)) NA_integer_ else length(cohort_ids),
               history_restricted = !is.null(dev_ids),
               history = HISTORY, outcomes_file = OUTCOMES,
               calibration = CALIBRATION,
               # HASHES, not just filenames. `aging.rds` meant three different
               # curves on 2026-07-29, and an A/B that recorded only the name
               # attributed an aging change to a calibration change. A stamp that
               # cannot distinguish two versions of the same path is not a stamp.
               calibration_md5 = tryCatch(tools::md5sum(file.path(OUT, CALIBRATION))[[1]],
                 error = function(e) NA_character_),
               aging_file = AGING_FILE,
               aging_md5 = tryCatch(tools::md5sum(file.path(OUT, AGING_FILE))[[1]],
                                    error = function(e) NA_character_),
               history_md5 = tryCatch(tools::md5sum(file.path(OUT, HISTORY))[[1]],
                                      error = function(e) NA_character_),
               # WHERE THE HISTORY ACTUALLY CAME FROM. When the parquet store
               # exists it is read instead of the .rds, so history_md5 was
               # stamping a file the run never opened -- and the store can be
               # rebuilt from a different corpus than the .rds sitting next to
               # it. score_arm.R compares these hashes to decide whether two
               # arms are comparable at all, so a hash describing the wrong
               # source is worse than no hash: it makes a mismatch look like a
               # match. Fingerprinted by name+size rather than content because
               # hashing a multi-GB partitioned dataset per arm is not worth it.
               history_source = if (USE_STORE) "store" else "rds",
               store_md5 = if (!USE_STORE) NA_character_ else tryCatch({
                 f <- sort(list.files(STORE, recursive = TRUE, full.names = TRUE))
                 tf <- tempfile(); on.exit(unlink(tf), add = TRUE)
                 writeLines(paste0(basename(f), ":", file.size(f)), tf)
                 unname(tools::md5sum(tf))
               }, error = function(e) NA_character_),
               half_life = half_life, prior_weight = PRIOR_WEIGHT,
               sigma_mode = SIGMA_MODE, adjust_context = ADJUST_CONTEXT,
               # Recorded because an arm scored on T1 only is not comparable to
               # one scored on every tier, and nothing else in the meta would say so.
               tier_filter = if (nzchar(TIER_FILTER)) TIER_FILTER else NA_character_,
               # Screening mode changes the history, so an arm run this way is
               # not comparable to one run on full history.
               elite_history = ELITE_HISTORY,
               history_days = HISTORY_DAYS, n_sims = N_SIMS,
               races_scored = length(keep), run_at = Sys.time())),
        file.path(OUT, Sys.getenv("CITIUS_BT_OUT", "backtest.rds")))

# ---------------------------------------------------------------------------
# DO NOT EDIT THIS FILE WHILE AN ARM IS RUNNING.
#
# Rscript evaluates top-level expressions AS IT READS THEM rather than parsing
# the whole file up front, so editing it mid-run desynchronises the parse of
# everything below the edit point. On 2026-07-31 the `csigma` arm completed all
# 380 meets and then died on the final saveRDS() with "unexpected symbol",
# quoting a mash-up of an argument and the comment above it -- an edit inserted
# ~150 lines earlier had shifted the offsets underneath it.
#
# The per-meet cache makes this recoverable (relaunching re-reads the cache and
# only redoes aggregation), but the run still looks like a crash in the model.
# Copy the script to a new name if an arm needs a change while another is live.
# ---------------------------------------------------------------------------
