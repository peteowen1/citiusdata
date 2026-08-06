# Birmingham 2026 European Athletics Championships -- pre-meet round-by-round
# predictions.
#
# Unlike predict_glasgow_pretournament.R, which simulated a single race per
# event with simulate_event(), this runs simulate_rounds() over the published
# round structure. That is the difference between "who wins the final" and "who
# reaches it" -- the questions the card is actually built to answer.
#
# Nothing here names a model input. Everything comes from DEPLOYED. See
# _deployed.R, which exists because five shipping scripts once each named their
# own and drifted apart silently.

VERSE <- "C:/dev/citiusverse"
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(jsonlite)
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
D <- file.path(VERSE, "citiusdata", "data")

# The championships run 10-16 Aug. Predictions are locked from data strictly
# BEFORE the first day of competition; anything at or after it is an outcome,
# never evidence. The date cut alone is not enough -- see the ID exclusion below.
MEET_START <- as.Date("2026-08-10")
CUT        <- MEET_START - 1L
BIRMINGHAM <- 7192415L
N_SIMS     <- 20000L
SEED       <- 20260810L

# --- inputs -------------------------------------------------------------------
ids <- fread(file.path(D, "birmingham2026_athlete_ids.csv"),
             colClasses = list(character = "ids"))
ids <- ids[n_ids > 0]
# Multi-id entrants take the union of their history (one athlete split across
# ids); the first is used as the canonical key.
ids[, athlete_id := tstrsplit(ids, "\\|")[[1]]]
ids[, all_ids := strsplit(ids, "\\|")]

st <- fread(file.path(D, "birmingham2026_round_structure.csv"))
events <- intersect(unique(ids$event_id), unique(st$event_id))
cli::cli_alert_info("{nrow(ids)} resolved entr{?y/ies} across {length(events)} event{?s}.")

calibration <- deployed_calibration(D)
aging       <- deployed_aging(D)

# --- history ------------------------------------------------------------------
# Exclude the competition being forecast BY ID, not by date. A date cut alone is
# only ever safe by accident: if the feed is re-harvested mid-meet, finals enter
# the ability estimates and the model starts scoring itself against races it has
# already seen, silently.
past <- deployed_history(D, events = events,
                         from = CUT - DEPLOYED$history_days, to = CUT)
past <- past[!is.na(event_id) & !is.na(perf)]
n_before <- nrow(past)
past <- past[is.na(competition_id) | competition_id != BIRMINGHAM]
if (n_before != nrow(past)) {
  cli::cli_alert_warning("Removed {n_before - nrow(past)} row{?s} from the meet being forecast.")
}

# --- combined-event contamination --------------------------------------------
# A decathlon 100m and a standalone 100m share an event_id. NEXT-STEPS logs this
# ("nothing excludes them, so they inflate the standalone event's measured
# spread"); measured over exactly this history window it is 230,458 rows, 5.44%,
# and combined marks sit 1.6 SDs worse on average. Including them inflates the
# measured spread by a median 10.6% and up to 27.3% in the men's discus.
#
# That spread is what the simulator converts into win probability, so this is
# not cosmetic. `round` carries "Combined", which is the only thing that
# distinguishes them.
#
# The combined events THEMSELVES keep their rows: AT-Decathlon-M and
# AT-Heptathlon-W are scored on points under their own event_id, and a
# decathlete forecast in the decathlon is not affected by this at all.
#
# Cost, measured before applying: 13 of 5,051 entrant athlete-events lose all
# their history (0.26%) and fall back to the field prior, which is the designed
# behaviour for an athlete with no usable evidence.
COMBINED_OWN <- c("AT-Decathlon-M", "AT-Heptathlon-W")
if (!"round" %in% names(past)) {
  cli::cli_abort("History has no `round` column; combined-event rows cannot be identified.")
}
is_comb <- grepl("Combined", past$round, ignore.case = TRUE) &
           !past$event_id %in% COMBINED_OWN
n_combined_excluded <- sum(is_comb)
cli::cli_alert_warning(
  "Excluding {format(n_combined_excluded, big.mark = ',')} combined-event row{?s} ({round(100*mean(is_comb), 2)}%) filed under standalone event ids.")
past <- past[!is_comb]
# The count is stamped onto the artefact below. The store itself is still
# contaminated -- that is a corpus-level defect, not something this script can
# fix -- so a check that reads the store will always find these rows. What must
# be auditable is that THIS FORECAST excluded them, which only the artefact can
# record.
stopifnot(
  "history must not contain the competition being predicted" =
    !any(past$competition_id == BIRMINGHAM, na.rm = TRUE),
  "history must not reach the day of competition" = max(past$date, na.rm = TRUE) < MEET_START)
cli::cli_alert_info(
  "History: {format(nrow(past), big.mark = ',')} row{?s}, {min(past$date)} to {max(past$date)}.")

ability <- deployed_ability(past, as_of = CUT, calibration = calibration)

# --- publication guards -------------------------------------------------------
# Both are on the Glasgow shipping path (predict_glasgow_pretournament.R:136-141)
# and neither is optional. Omitting them was a real gap in the first draft of
# this script: without drop_impossible_sigma() a merged crosswalk identity can
# reach the card as a live threat, which is how a 10.97 sprinter became second
# favourite before ce4881e.
#
# Note what they do NOT cover. temper_unevidenced() fires only below
# w_total = 0.05 -- no usable evidence at all. An athlete with a handful of
# genuine recent races is untouched by it, and the known thin-evidence
# over-crediting (NEXT-STEPS item 7, detector 0.808) still applies to them.
entrant_ids <- unique(unlist(ids$all_ids))

ability <- drop_impossible_sigma(ability)
dropped <- attr(ability, "dropped")
# Report the drops scoped to ENTRANTS. `ability` covers every athlete with
# history in these events -- thousands of people who are not at Birmingham -- so
# a raw drop count says nothing about the card and reads as though the field had
# been cleaned when it may not have been touched at all.
if (!is.null(dropped) && nrow(dropped)) {
  in_field <- dropped[athlete_id %in% entrant_ids]
  cli::cli_alert_info(
    "drop_impossible_sigma(): {nrow(dropped)} athlete-event{?s} dropped across the whole history population, of which {nrow(in_field)} {?is/are} in the Birmingham field.")
  if (nrow(in_field)) {
    cli::cli_alert_warning("Merged identities removed FROM THE FIELD:")
    print(in_field)
  }
} else {
  cli::cli_alert_info("drop_impossible_sigma(): nothing dropped anywhere.")
}

se_before <- ability[athlete_id %in% entrant_ids, sum(ability_se, na.rm = TRUE)]
ability <- temper_unevidenced(ability)
se_after <- ability[athlete_id %in% entrant_ids, sum(ability_se, na.rm = TRUE)]
cli::cli_alert_info(
  "temper_unevidenced(): entrant ability_se reduced by {round(100*(1 - se_after/se_before), 2)}% (entrants only, not the whole population).")

ages <- past[!is.na(age), .(age_last = max(age), age_asof = max(date)),
             by = .(athlete_id = as.character(athlete_id), event_id)]
ages[, age_now := age_last + as.numeric(CUT - age_asof) / 365.25]

# --- simulate -----------------------------------------------------------------
mk_structure <- function(ev) {
  s <- st[event_id == ev][order(round_index)]
  lapply(seq_len(nrow(s)), function(i) {
    r <- list(races = as.integer(s$races[i]))
    if (!is.na(s$advance[i]))        r$advance        <- as.integer(s$advance[i])
    if (!is.na(s$fastest_losers[i])) r$fastest_losers <- as.integer(s$fastest_losers[i])
    r
  })
}

res <- list(); skipped <- list()
for (ev in events) {
  field_ids <- unique(unlist(ids[event_id == ev]$all_ids))
  ent <- ability[event_id == ev & athlete_id %in% field_ids]
  if (nrow(ent) < 3L) {
    skipped[[length(skipped) + 1L]] <- data.table(event_id = ev, n = nrow(ent))
    next
  }
  ent <- deployed_field(ent, aging = aging, ages = ages[event_id == ev, .(athlete_id, age_now)])
  structure_ev <- mk_structure(ev)
  sim <- simulate_rounds(ent, structure = structure_ev, n_sims = N_SIMS,
                         calibration = calibration, seed = SEED)
  sim <- as.data.table(sim)
  sim[, `:=`(event_id = ev, n_rounds = length(structure_ev),
             field_modelled = nrow(ent))]
  # Carry the ability estimate the simulation ran on. Two reasons: it is what
  # the site's own rankings view needs, and it is the only thing the sanity
  # script can anchor p_gold against -- simulate_rounds() returns probabilities
  # and no predicted mark, so without this there is nothing to check the
  # ordering against except itself.
  keep_ab <- intersect(c("athlete_id", "ability", "sigma", "ability_se", "w_total"),
                       names(ent))
  sim <- merge(sim, ent[, ..keep_ab], by = "athlete_id", all.x = TRUE)
  res[[length(res) + 1L]] <- sim
}
pred <- rbindlist(res, fill = TRUE)

if (length(skipped)) {
  sk <- rbindlist(skipped)
  cli::cli_alert_warning("{nrow(sk)} event{?s} skipped for fewer than 3 rated entrants:")
  print(sk)
}

# --- label and stamp ----------------------------------------------------------
info <- unique(ids[, .(athlete_id, athlete, nation, event_id)])
pred <- merge(pred, info, by = c("athlete_id", "event_id"), all.x = TRUE)
pred <- merge(pred, as.data.table(citius_events())[, .(event_id, discipline, sex)],
              by = "event_id", all.x = TRUE)
pred[, `:=`(generated_at = Sys.time(), cutoff = CUT, meet = "birmingham2026",
            competition_id = BIRMINGHAM, field_type = "official_entry_list",
            half_life = DEPLOYED$half_life, config = DEPLOYED$stamp,
            counts_source = "derived",
            combined_rows_excluded = n_combined_excluded)]

stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
f <- file.path(D, paste0("birmingham2026_pretournament_", stamp, ".parquet"))
arrow::write_parquet(pred, f)
saveRDS(pred, file.path(D, "birmingham2026_pretournament.rds"))

cli::cli_alert_success(
  "{format(nrow(pred), big.mark = ',')} row{?s} across {uniqueN(pred$event_id)} event{?s} -> {basename(f)}")
cli::cli_alert_info("Config: {DEPLOYED$stamp} | cutoff {CUT} | {N_SIMS} sims.")

# Probability columns must sum to the number of places they describe, per event.
# A sum that is not 1 for gold is a broken simulation, not a rounding artefact.
chk <- pred[, .(gold = round(sum(p_gold, na.rm = TRUE), 3),
                medal = round(sum(p_medal, na.rm = TRUE), 3),
                final = round(sum(p_final, na.rm = TRUE), 1),
                n = .N), by = .(event_id, n_rounds)]
cli::cli_h2("Per-event probability sums")
print(chk[order(gold)], nrows = 50)
bad <- chk[abs(gold - 1) > 0.01 | abs(medal - 3) > 0.05]
if (nrow(bad)) {
  cli::cli_alert_danger("{nrow(bad)} event{?s} with implausible probability sums:")
  print(bad)
} else {
  cli::cli_alert_success("Every event sums to 1 gold and 3 medals.")
}
