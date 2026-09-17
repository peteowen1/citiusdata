# Persist a forecast for every race we model, keyed so it joins to anything.
#
# WHY THIS EXISTS. Every card run and every backtest arm computes a full
# simulated field and then keeps three numbers from it -- p_gold, p_medal,
# p_top8 -- discarding the rest with the simulation object. Asking "what did we
# forecast for this race" later therefore meant recomputing from scratch, and
# asking "what were his odds of finishing 4th" was unanswerable at any price
# because the rank distribution was never written down.
#
# GRAIN AND KEY. One row per (config, competition_id, event_id, round,
# athlete_id).
#
# NOT keyed on `race_key`, and the reason is the whole design. The feed's
# race_key looks like `7212925|10229609|Final|1` -- competition, then
# **wa_event_id**, then round, then race number. Two problems for a forecast:
# it embeds WA's event id rather than our canonical `event_id`, and more
# fundamentally **the feed has not minted it yet**, because the race has not
# happened. A forecast written before the race cannot be keyed on an
# identifier the race creates. Discovered 2026-09-14 by testing the join: a
# synthesised `<comp>|<event_id>|Final|1` matched zero feed rows.
#
# `race_key` is therefore carried as a NULLABLE column, populated by a
# post-hoc resolve once results land, and is a convenience for joining to
# results -- never the key.
#   * `config` because a forecast is meaningless without the model vintage that
#     produced it. Same discipline as deployed_ability_snapshot() and the
#     backtest's arm fingerprint: a re-score under a new model must not
#     silently overwrite an old model's predictions.
#   * NO surrogate athlete_race_id. It would have to be regenerated on every
#     rebuild and buys nothing the natural key does not already give, while
#     costing the join back to the corpus.
#
# Note (race_key, athlete_id) is NOT unique on the RESULTS side: 21,455 corpus
# rows (0.475%) duplicate it, concentrated in the jumps, where a series or
# qualifying mark sits alongside the final result. That is a results-side
# resolution problem and does not affect this table, which has exactly one row
# per athlete the model simulated.
#
# WIDE, TOP-8. pos_1 .. pos_8 rather than a long row-per-position form.
# Long scales with field size squared per race: the largest race in the corpus
# is an 816-starter marathon, 816 rows wide against 665,856 long. Eight is the
# domain's boundary, not an arbitrary cut -- athletics scores to eighth place --
# and nothing is lost, since p(worse than 8th) is 1 - sum(pos_1..8).
#
# Usage:  Rscript citiusdata/scripts/build_forecast_store.R [meet_id ...]
#         (no args = every meet on the calendar with a prediction cutoff in the
#          current season)

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
D <- file.path(VERSE, "citiusdata", "data")

N_SIMS <- as.integer(Sys.getenv("CITIUS_FC_SIMS", "20000"))
SEED   <- as.integer(Sys.getenv("CITIUS_FC_SEED", "20260914"))
SEASON <- as.integer(Sys.getenv("CITIUS_FC_SEASON", format(Sys.Date(), "%Y")))
OUT    <- file.path(D, "forecasts")
K_POS  <- 8L

args <- commandArgs(trailingOnly = TRUE)
cal <- fread(file.path(D, "athletics_calendar.csv"))
meets <- if (length(args)) args else
  cal[as.integer(format(as.Date(date_start), "%Y")) == SEASON]$meet_id
if (!length(meets)) cli::cli_abort("No meets selected (season {SEASON}).")
cli::cli_alert_info("Forecast store: {length(meets)} meet{?s} -- {.val {meets}}")

# The config stamp names the directory, so two vintages coexist rather than one
# overwriting the other. Slashes and spaces are not path-safe.
STAMP <- gsub("[^A-Za-z0-9._-]+", "_", DEPLOYED$stamp)
dir.create(file.path(OUT, STAMP), recursive = TRUE, showWarnings = FALSE)

calibration <- deployed_calibration(D)
aging <- deployed_aging(D)

# THE CONFIG STAMP DOES NOT NAME THE DATA, and the directory is named after the
# config stamp. So re-running a meet under an unchanged config but a CHANGED
# corpus silently overwrites the earlier forecast with a different number and
# leaves nothing to tell them apart. Found 2026-09-17: zurich2026 was rebuilt
# into the 2026-09-09 directory after the corpus grew 4.77M -> 5.04M rows that
# morning.
#
# Recorded as a COLUMN rather than by restructuring the path: changing the
# directory scheme would orphan the forecasts already written under the old
# one, and the question this needs to answer ("which data produced this row?")
# is a property of the row. Same cheap fingerprint _score_weights.R uses --
# mtime and size, enough to catch a rebuild without hashing millions of rows.
HISTORY_VINTAGE <- local({
  p <- file.path(D, DEPLOYED$history_store)
  fs <- list.files(p, recursive = TRUE, full.names = TRUE)
  if (!length(fs)) return(NA_character_)
  i <- file.info(fs)
  sprintf("%s_%.0f", format(max(i$mtime), "%Y%m%d%H%M%S"), sum(i$size))
})
cli::cli_alert_info("history vintage: {.val {HISTORY_VINTAGE}}")

# Parameter is `mid`, not `meet_id`: a param named after the column it filters
# on shadows that column inside `[...]` and the filter silently self-joins or
# fails to resolve. Documented gotcha in C:\dev\.claude\rules.
one_meet <- function(mid) {
  row <- cal[meet_id == mid]
  if (!nrow(row)) { cli::cli_alert_warning("{mid}: not on the calendar, skipped."); return(NULL) }
  CUT <- as.Date(row$prediction_cutoff[1])
  COMP <- suppressWarnings(as.integer(row$wa_competition_id[1]))
  if (is.na(COMP)) { cli::cli_alert_warning("{mid}: no wa_competition_id, skipped."); return(NULL) }

  ids_f <- file.path(D, paste0(mid, "_athlete_ids.csv"))
  if (!file.exists(ids_f)) { cli::cli_alert_warning("{mid}: no athlete_ids.csv, skipped."); return(NULL) }
  # TWO RESOLVER SCHEMAS, because the meets are resolved by different scripts.
  # The DL-shaped meets carry `athlete_id` directly; Birmingham (feed entry
  # list) carries `ids`, PIPE-DELIMITED because one athlete can match several
  # WA ids, with `n_ids` counting them. predict_birmingham2026.R:29-35 takes
  # the first as canonical and filters n_ids > 0; this follows that convention
  # rather than inventing a second one. Assuming `athlete_id` dropped
  # Birmingham silently -- it has an ids file, it just spells the column
  # differently (found 2026-09-14).
  ids <- fread(ids_f, colClasses = if ("ids" %in% names(fread(ids_f, nrows = 0L)))
                 list(character = "ids") else NULL)
  if ("athlete_id" %in% names(ids)) {
    ids[, athlete_id := as.character(athlete_id)]
  } else if ("ids" %in% names(ids)) {
    if ("n_ids" %in% names(ids)) ids <- ids[n_ids > 0]
    ids[, athlete_id := data.table::tstrsplit(ids, "\\|")[[1]]]
  } else {
    cli::cli_alert_warning("{mid}: ids file has neither {.field athlete_id} nor {.field ids}, skipped.")
    return(NULL)
  }
  ids <- ids[!is.na(athlete_id) & nzchar(athlete_id)]
  if (!nrow(ids)) { cli::cli_alert_warning("{mid}: no resolved entrants, skipped."); return(NULL) }
  events <- unique(ids$event_id)

  past <- deployed_history(D, events = events, from = CUT - DEPLOYED$history_days, to = CUT)
  past <- past[!is.na(event_id) & !is.na(perf)]
  past <- past[is.na(competition_id) | competition_id != COMP]
  past <- past[!(grepl("Combined", round, ignore.case = TRUE) &
                   !event_id %in% c("AT-Decathlon-M", "AT-Heptathlon-W"))]
  if (!nrow(past)) { cli::cli_alert_warning("{mid}: empty history, skipped."); return(NULL) }

  ability <- deployed_ability(past, as_of = CUT, calibration = calibration)
  ability[, athlete_id := as.character(athlete_id)]
  ability <- temper_unevidenced(drop_impossible_sigma(ability))

  ages <- past[!is.na(age), .(age_last = max(age), age_asof = max(date)),
               by = .(athlete_id = as.character(athlete_id), event_id)]
  ages[, age_now := age_last + as.numeric(CUT - age_asof) / 365.25]

  # The race_key for a forecast is the race we are predicting, which has not
  # happened yet -- so it is composed the same way the feed composes one for a
  # final, from the competition and event. A meet with rounds would need the
  # round and race number too; every meet this script currently serves is a
  # straight final.
  out <- rbindlist(lapply(events, function(ev) {
    f <- ids[event_id == ev]
    ab <- ability[event_id == ev & athlete_id %in% f$athlete_id]
    if (nrow(ab) < 3L) return(NULL)
    ab <- deployed_field(ab, aging = aging,
                         ages = ages[event_id == ev, .(athlete_id, age_now)])
    proj <- tryCatch(project_field(ab, event = ev, as_of = CUT, size = nrow(f)),
                     error = function(e) NULL)
    if (is.null(proj) || !nrow(proj)) return(NULL)
    sim <- tryCatch(simulate_event(proj, n_sims = N_SIMS, calibration = calibration,
                                   seed = SEED, context = deployed_race_context("final")),
                    error = function(e) NULL)
    if (is.null(sim)) return(NULL)

    mp <- as.data.table(medal_probs(sim, top_n = K_POS))
    # citius::position_probs() ALREADY did all of this -- capped at 8 by
    # default, pooling the remainder, with a wide option emitting pos_1..pos_8.
    # A duplicate was written on 2026-09-14 without grepping for the name
    # first; it silently shadowed this one and broke test-positions.R, caught
    # only by a later full-suite run. Use the original.
    pp <- as.data.table(position_probs(sim, max_position = K_POS, wide = TRUE))
    r <- merge(mp, pp, by = "athlete_id")
    keep <- intersect(c("athlete_id", "ability", "sigma", "ability_se", "w_total", "n"),
                      names(ab))
    r <- merge(r, ab[, ..keep], by = "athlete_id", all.x = TRUE)
    # race_key deliberately NA here -- see the header. It is resolved against
    # the feed after the race runs, not invented now.
    r[, `:=`(event_id = ev, round = "Final", race_key = NA_character_)]
    r[]
  }), fill = TRUE)
  if (!nrow(out)) return(NULL)

  out[, `:=`(meet_id = mid, competition_id = COMP, cutoff = CUT,
             config = DEPLOYED$stamp, n_sims = N_SIMS, seed = SEED,
             history_vintage = HISTORY_VINTAGE,
             generated_at = Sys.time())]
  setnames(out, paste0("p_top", K_POS), "p_top8", skip_absent = TRUE)
  setcolorder(out, c("config", "competition_id", "event_id", "round",
                     "athlete_id", "meet_id", "race_key", "cutoff"))
  out[]
}

all_fc <- rbindlist(lapply(meets, function(m)
  tryCatch(one_meet(m), error = function(e) {
    cli::cli_alert_danger("{m}: {conditionMessage(e)}"); NULL })), fill = TRUE)

if (!nrow(all_fc)) cli::cli_abort("No forecasts produced.")

# THE KEY MUST HOLD. A duplicate here means two athletes were simulated twice
# into the same race, which would silently double-count them downstream.
PK <- c("config", "competition_id", "event_id", "round", "athlete_id")
dupes <- nrow(all_fc) - uniqueN(all_fc, by = PK)
if (dupes) cli::cli_abort("{dupes} duplicate {.field {PK}} row{?s} -- the key does not hold.")

# Positions are a distribution: each athlete's top-8 mass cannot exceed 1.
pcols <- paste0("pos_", seq_len(K_POS))
mass <- rowSums(as.matrix(all_fc[, ..pcols]))
if (any(mass > 1 + 1e-9)) cli::cli_abort("position probabilities sum above 1 for {sum(mass > 1 + 1e-9)} row{?s}.")
stopifnot("pos_1 must equal p_gold" = isTRUE(all.equal(all_fc$pos_1, all_fc$p_gold)))

# ONE FILE PER MEET, not one file for the run.
#
# A single `forecasts.parquet` per config means rebuilding one meet DELETES
# every other meet in that vintage -- running birmingham2026 alone wiped the
# five meets built minutes earlier (2026-09-14). Partitioning by meet makes a
# rebuild idempotent and scoped: re-running a meet replaces only its own file,
# which is the same reason backtest_cache_* is keyed per competition.
for (m in unique(all_fc$meet_id)) {
  write_parquet(all_fc[meet_id == m], file.path(OUT, STAMP, paste0(m, ".parquet")))
}
f_out <- file.path(OUT, STAMP)
# Counts races by the KEY, not by race_key -- which is NA at forecast time by
# design and would report 1 for any number of races.
n_races <- uniqueN(all_fc, by = c("competition_id", "event_id", "round"))
cli::cli_alert_success(
  "{format(nrow(all_fc), big.mark = ',')} forecast row{?s} | {n_races} race{?s} | {uniqueN(all_fc$meet_id)} meet{?s} -> {.path {f_out}}")

# ---- the races dimension, derived from the corpus ---------------------------
# One row per race. Does not exist anywhere else: race_key is a column on the
# result tables, never a table of its own.
corp <- as.data.table(read_parquet(file.path(D, "athletics_corpus.parquet"),
  col_select = c("race_key", "competition_id", "event_id", "date", "round", "race_code")))
races <- corp[!is.na(race_key), .(competition_id = first(competition_id),
                                   event_id = first(event_id), date = first(date),
                                   round = first(round), race_code = first(race_code),
                                   n_starters = .N), by = race_key]
f_races <- file.path(OUT, "races.parquet")
write_parquet(races, f_races)
cli::cli_alert_success("{format(nrow(races), big.mark = ',')} races -> {.path {f_races}}")
cli::cli_alert_info("Meets dimension is competition_catalogue.parquet; join on competition_id.")
