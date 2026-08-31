# Diamond-League-shaped meet forecast (Brussels Final, and reusable for
# Budapest Ultimate Championship): straight finals, no rounds.
#
# WHY THIS SCRIPT EXISTS. Neither has ever had a working forecast pipeline --
# run_meet.ps1's own header says outright that Diamond League meets "are not
# a parameterisation of [the Birmingham chain]... they get their own step
# list when that card is built." That step list was never built until now
# (2026-08-31), with Brussels 4 days out and no official WA entry list
# published yet. The field here comes from resolve_diamond_league_athletes.R,
# itself built on a THIRD-PARTY qualifier list (etusuora.com), not an
# official entry list -- see the field_type/caveat stamp on the output, and
# do not let this be mistaken for the same provenance as Birmingham's
# feed-sourced card.
#
# Simpler shape than Birmingham on purpose: one final per event, no rounds,
# so this follows predict_glasgow_pretournament.R's project_field() +
# simulate_event() + medal_probs() pattern, not predict_birmingham2026.R's
# simulate_rounds() chain.
#
# Usage:  Rscript scripts/predict_diamond_league_final.R <meet_id>
#   e.g.  Rscript scripts/predict_diamond_league_final.R brussels2026

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
D <- file.path(VERSE, "citiusdata", "data")
N_SIMS <- 20000L
SEED <- 20260831L

args <- commandArgs(trailingOnly = TRUE)
MEET <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_DL_MEET", "brussels2026")

cal <- fread(file.path(D, "athletics_calendar.csv"))
row <- cal[meet_id == MEET]
if (!nrow(row)) cli::cli_abort("{.val {MEET}} is not in athletics_calendar.csv.")
MEET_START <- as.Date(row$date_start[1])
CUT <- as.Date(row$prediction_cutoff[1])
if (!(CUT < MEET_START)) cli::cli_abort("prediction_cutoff does not precede date_start.")
# The calendar's wa_competition_id is blank for brussels2026/budapest2026
# (never wired up, since these meets never had a working pipeline before) --
# COMPETITION_ID here is the id this session's own athletics_calendar()
# lookup found live (Brussels: "Allianz Memorial van Damme", 7214029).
# Hardcoded per meet rather than trusted from the stale calendar column.
COMPETITION_ID <- switch(MEET,
  brussels2026 = 7214029L,
  cli::cli_abort("No known competition_id for {.val {MEET}} -- look it up via athletics_calendar(query=...) before running."))

ids_all <- fread(file.path(D, paste0(MEET, "_athlete_ids.csv")))
# fread() auto-detects athlete_id as numeric since every value looks
# numeric -- the resolve script's own internal representation is character
# (championship_results.rds's convention), so this silently reintroduces
# the exact integer/character mismatch fixed above unless forced back.
ids_all[, athlete_id := as.character(athlete_id)]
# ids_all keeps the UNRESOLVED entrants too. They are real qualified finalists
# and the accounting below has to explain them; only `ids` (the resolvable
# subset) feeds the simulation.
ids <- ids_all[!is.na(athlete_id)]
events <- unique(ids$event_id)
cli::cli_alert_info("{nrow(ids)} resolved entr{?y/ies} of {nrow(ids_all)} across {length(events)} event{?s}.")

calibration <- deployed_calibration(D)
aging <- deployed_aging(D)

past <- deployed_history(D, events = events, from = CUT - DEPLOYED$history_days, to = CUT)
past <- past[!is.na(event_id) & !is.na(perf)]
n_before <- nrow(past)
past <- past[is.na(competition_id) | competition_id != COMPETITION_ID]
if (n_before != nrow(past)) {
  cli::cli_alert_warning("Removed {n_before - nrow(past)} row{?s} from the meet being forecast.")
}

# Same combined-event contamination guard as predict_birmingham2026.R (NEXT-STEPS
# item: a decathlon 100m shares an event_id with the standalone 100m).
COMBINED_OWN <- c("AT-Decathlon-M", "AT-Heptathlon-W")
is_comb <- grepl("Combined", past$round, ignore.case = TRUE) & !past$event_id %in% COMBINED_OWN
n_combined_excluded <- sum(is_comb)
past <- past[!is_comb]

stopifnot(
  "history must not contain the competition being predicted" =
    !any(past$competition_id == COMPETITION_ID, na.rm = TRUE),
  "history is empty, so the date guard below would pass without checking" =
    nrow(past) > 0,
  "history must not reach the day of competition" =
    max(past$date, na.rm = TRUE) < MEET_START)
cli::cli_alert_info("History: {format(nrow(past), big.mark = ',')} row{?s}, {min(past$date)} to {max(past$date)}.")

ability <- deployed_ability(past, as_of = CUT, calibration = calibration)
# The store's athlete_id is integer; the resolve script's ids/ages are
# character (it builds them from championship_results.rds's aid <-
# as.character(athlete_id) convention, same as resolve_birmingham_athletes.R).
# Coerce once here rather than let a downstream merge silently join zero rows
# or, as it did on the first run, throw on a type mismatch.
ability[, athlete_id := as.character(athlete_id)]
# Snapshot BEFORE the publication guards run. The accounting block below needs
# to tell "this athlete never had an ability row at all" (no history in the
# event -- estimate_ability() only emits rows for (athlete, event) pairs
# actually present in the history) apart from "had one and a guard removed it".
# Those are different facts about a missing finalist and the card should not
# conflate them.
ab_raw_keys <- unique(ability[, .(event_id, athlete_id)])
ability <- drop_impossible_sigma(ability)
dropped <- attr(ability, "dropped")
entrant_ids <- unique(ids$athlete_id)
if (!is.null(dropped) && nrow(dropped)) {
  in_field <- dropped[athlete_id %in% entrant_ids]
  cli::cli_alert_info("drop_impossible_sigma(): {nrow(dropped)} athlete-event{?s} dropped overall, {nrow(in_field)} in the field.")
  if (nrow(in_field)) print(in_field)
}
ability <- temper_unevidenced(ability)
ab_final_keys <- unique(ability[, .(event_id, athlete_id)])

ages <- past[!is.na(age), .(age_last = max(age), age_asof = max(date)), by = .(athlete_id = as.character(athlete_id), event_id)]
ages[, age_now := age_last + as.numeric(CUT - age_asof) / 365.25]

sim_event <- function(field_ids, ev) {
  f <- ids[event_id == ev & athlete_id %in% field_ids]
  ab_ev <- ability[event_id == ev & athlete_id %in% f$athlete_id]
  if (nrow(ab_ev) < 3L) return(data.table(event_id = ev, skipped = TRUE, n = nrow(ab_ev)))
  ab_ev <- deployed_field(ab_ev, aging = aging, ages = ages[event_id == ev, .(athlete_id, age_now)])
  proj <- tryCatch(project_field(ab_ev, event = ev, as_of = CUT, size = nrow(f)),
                   error = function(e) NULL)
  if (is.null(proj) || !nrow(proj)) return(data.table(event_id = ev, skipped = TRUE, n = nrow(ab_ev)))
  sim <- tryCatch(simulate_event(proj, n_sims = N_SIMS, calibration = calibration, seed = SEED),
                  error = function(e) NULL)
  if (is.null(sim)) return(data.table(event_id = ev, skipped = TRUE, n = nrow(proj)))
  s <- tryCatch(medal_probs(sim), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(data.table(event_id = ev, skipped = TRUE, n = nrow(proj)))
  s[, event_id := ev]
  keep_ab <- intersect(c("athlete_id", "ability", "sigma", "ability_se", "w_total"), names(ab_ev))
  s <- merge(s, ab_ev[, ..keep_ab], by = "athlete_id", all.x = TRUE)
  s[, skipped := FALSE][, n := nrow(proj)]
  s[]
}

res <- rbindlist(lapply(events, function(ev) sim_event(ids[event_id == ev]$athlete_id, ev)), fill = TRUE)
skipped <- unique(res[skipped == TRUE, .(event_id, n)])
if (nrow(skipped)) {
  cli::cli_alert_warning("{nrow(skipped)} event{?s} skipped for fewer than 3 rated entrants:")
  print(skipped)
}
pred <- res[skipped == FALSE]

dup <- pred[, .N, by = .(event_id, athlete_id)][N > 1]
if (nrow(dup)) { print(dup); cli::cli_abort("{nrow(dup)} athlete{?s} appear{?s/} more than once in an event.") }

# --- entrant accounting -------------------------------------------------------
# WHY THIS EXISTS. estimate_ability() only emits a row for an (athlete, event)
# pair that actually appears in the history, so a qualified finalist with NO
# history in the event they are entered for never enters `ability`, never
# enters sim_event()'s field, and vanishes from the card with nothing erroring.
# On a public forecast card that is silently omitting a real finalist. The
# resolve script warns about it, but a console warning nobody is obliged to
# read is not a gate -- the same "reported-but-ungated is not enough" point
# resolve_birmingham_athletes.R makes about its own birthdate coverage.
# Found in review 2026-08-31; Brussels had one such entrant (Lazaro Martinez,
# Triple Jump).
#
# DECISION: an unmodelled entrant is EXCLUDED from the simulated field, not
# included with a placeholder ability. Inventing an ability for someone with no
# evidence is a known-wrong default -- this project has measured what that costs
# (DECISIONS.md 2026-08-23: debutants seeded at the population mean ran 1.553 sd
# below it) -- and it would break the card's own invariant that per-event
# probabilities sum to one gold and three medals over the field ACTUALLY
# simulated.
#
# What must not happen is the exclusion being invisible. So every entrant is
# accounted for with a REASON, written to <meet>_unmodelled_entrants.csv, and
# the per-event counts are stamped onto the card itself, so a page can say
# "6 of the 7 qualified athletes are forecast here" rather than quietly show six.
modelled <- unique(pred[, .(event_id, athlete_id)])[, in_card := TRUE]
acct <- merge(ids_all[, .(event_id, athlete_id, athlete, country, event)],
              modelled, by = c("event_id", "athlete_id"), all.x = TRUE)
if (nrow(acct) != nrow(ids_all)) {
  cli::cli_abort("Accounting fanned out ({nrow(acct)} rows from {nrow(ids_all)} entries) -- duplicate (event_id, athlete_id) in the id file.")
}
acct[is.na(in_card), in_card := FALSE]
acct[, k := paste(event_id, athlete_id)]
key_raw   <- paste(ab_raw_keys$event_id, ab_raw_keys$athlete_id)
key_final <- paste(ab_final_keys$event_id, ab_final_keys$athlete_id)
# Reasons in PIPELINE ORDER -- the earliest cause wins, so an athlete with no
# history in an event that was also skipped reads as "no history", the fact
# that actually explains them.
acct[, reason := NA_character_]
acct[in_card == TRUE, reason := "modelled"]
acct[is.na(reason) & is.na(athlete_id), reason := "unresolved_name"]
acct[is.na(reason) & !(k %in% key_raw), reason := "no_history_in_event"]
acct[is.na(reason) & !(k %in% key_final), reason := "dropped_by_publication_guard"]
acct[is.na(reason) & event_id %in% skipped$event_id, reason := "event_skipped_fewer_than_3_rated"]
acct[is.na(reason), reason := "unexplained"]

unmodelled <- acct[in_card == FALSE, .(event_id, event, athlete, country, athlete_id, reason)]
setorder(unmodelled, event_id, athlete)
unm_f <- file.path(D, paste0(MEET, "_unmodelled_entrants.csv"))
fwrite(unmodelled, unm_f)

cli::cli_h2("Entrant accounting")
print(acct[, .(entries = .N), by = reason][order(-entries)])
if (nrow(unmodelled)) {
  cli::cli_alert_warning("{nrow(unmodelled)} of {nrow(ids_all)} qualified entr{?y/ies} are NOT on the card -> {basename(unm_f)}")
  print(unmodelled)
} else {
  cli::cli_alert_success("Every one of the {nrow(ids_all)} qualified entries is on the card.")
}
# "unexplained" means the pipeline dropped a resolved, rated entrant from a
# scored event for a reason this script cannot name. That is a bug, not a data
# gap, and it is exactly the silent-omission class this block exists to stop.
if (acct[reason == "unexplained", .N]) {
  print(acct[reason == "unexplained", .(event_id, athlete, country)])
  cli::cli_abort("{acct[reason == 'unexplained', .N]} entrant{?s} unaccounted for -- see above.")
}

pred <- merge(pred, ids_all[, .(field_entrants = .N), by = event_id], by = "event_id", all.x = TRUE)
pred <- merge(pred, acct[in_card == FALSE, .(field_unmodelled = .N), by = event_id],
              by = "event_id", all.x = TRUE)
pred[is.na(field_unmodelled), field_unmodelled := 0L]

info <- unique(ids[, .(athlete_id, athlete, country)])
pred <- merge(pred, info, by = "athlete_id", all.x = TRUE)
setnames(pred, "country", "nation")
pred <- merge(pred, as.data.table(citius_events())[, .(event_id, discipline, sex)], by = "event_id", all.x = TRUE)
pred[, `:=`(
  generated_at = Sys.time(), cutoff = CUT, meet = MEET, competition_id = COMPETITION_ID,
  # THE HONEST STAMP. Birmingham/Glasgow both say "official_entry_list" here --
  # this field genuinely is not that, and the site's own caveat convention
  # (caveats live in the data, not the qmd) needs this to render differently.
  field_type = "third_party_qualifier_list_unofficial",
  field_source = "etusuora.com post-Zurich Diamond League qualifier compilation, 2026-08-31",
  half_life = DEPLOYED$half_life, config = DEPLOYED$stamp,
  counts_source = "derived", combined_rows_excluded = n_combined_excluded,
  n_rounds = 1L, field_modelled = n)]

stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
f <- file.path(D, paste0(MEET, "_pretournament_", stamp, ".parquet"))
arrow::write_parquet(pred, f)
saveRDS(pred, file.path(D, paste0(MEET, "_pretournament.rds")))

cli::cli_alert_success("{format(nrow(pred), big.mark = ',')} row{?s} across {uniqueN(pred$event_id)} event{?s} -> {basename(f)}")
cli::cli_alert_info("Config: {DEPLOYED$stamp} | cutoff {CUT} | {N_SIMS} sims.")

chk <- pred[, .(gold = round(sum(p_gold, na.rm = TRUE), 3), medal = round(sum(p_medal, na.rm = TRUE), 3), n = .N), by = event_id]
bad <- chk[abs(gold - 1) > 0.01 | abs(medal - 3) > 0.05]
if (nrow(bad)) {
  cli::cli_alert_danger("{nrow(bad)} event{?s} with implausible probability sums:")
  print(bad)
} else {
  cli::cli_alert_success("Every event sums to 1 gold and 3 medals.")
}
