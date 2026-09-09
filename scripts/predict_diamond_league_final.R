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
# READ FROM THE CALENDAR, not a switch(). The first version of this hardcoded
# 7214029 for Brussels because the calendar's wa_competition_id column was
# blank for both DL-shaped meets -- but a hardcoded id in a script parameterised
# by meet is a contradiction, and it made Budapest abort on a meet the pipeline
# was explicitly built to also serve. Both ids were filled in on 2026-08-31
# (Brussels 7214029 "Allianz Memorial van Damme", Budapest 7212925), each
# verified against athletics_calendar(). The calendar is the single source of
# truth for meet metadata everywhere else in this pipeline; this is no longer
# an exception.
COMPETITION_ID <- suppressWarnings(as.integer(row$wa_competition_id[1]))
if (is.na(COMPETITION_ID)) {
  cli::cli_abort(c(
    "{.val {MEET}} has no wa_competition_id on the calendar.",
    i = "Look it up with {.code athletics_calendar(query = ...)} and fill the column in -- do not hardcode it here."))
}

# PROVENANCE IS PER-MEET AND MUST BE HONEST PER-MEET. Brussels and Budapest
# have genuinely different field sources and the card has to say which is
# which: Brussels' field is a third-party compilation (World Athletics
# publishes nothing machine-readable for a DL final -- no entry list, and the
# championship-qualification endpoint 500s for DL competition ids), while
# Budapest's comes from World Athletics' own qualification standings. Stamping
# them identically would either overclaim Brussels or underclaim Budapest.
#
# Budapest is OFFICIAL BUT PROVISIONAL, which is a third thing again: the
# world-rankings window closed 2026-09-01 and the Brussels DL Final (Sep 4-5)
# awards auto-qualifying slots that displace some current bottom-ranked
# qualifiers. Saying "official" without "provisional" would be its own kind of
# wrong.
FIELD <- switch(MEET,
  brussels2026 = list(
    type = "third_party_qualifier_list_unofficial",
    source = "etusuora.com post-Zurich Diamond League qualifier compilation, 2026-08-31"),
  budapest2026 = list(
    type = "official_qualification_standings_provisional",
    source = "World Athletics championship-qualification standings (competition 7212925), fetched 2026-08-31. Official but PROVISIONAL: the world-rankings window closed 2026-09-01 and Diamond League Final winners (Sep 4-5) take auto-qualifying slots that will displace some current qualifiers."),
  cli::cli_abort(c(
    "No field provenance recorded for {.val {MEET}}.",
    i = "Add an entry to FIELD saying where this meet's entry list came from -- a card must never publish without one.")))

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

# FABLE-redteam-2026-09-07 F3: the calendar's prediction_cutoff is a PLAN (the
# day before the meet), not the data boundary. Run on 2026-08-31 from a corpus
# ending 2026-08-27, the cards were stamped cutoff 2026-09-10 / 2026-09-03 and
# the site printed "locked from data before <cutoff>", claiming 1-10 September
# results were used. They were not. The stamped cutoff is now the smaller of the
# requested cutoff and the last date actually in the history; the requested
# date and the history boundary are stamped alongside it so the page can say
# both. CUT itself (the as_of for ability decay and ages) is deliberately left
# as the meet-relative date -- that is a modelling choice, not a provenance one.
CUT_REQUESTED <- CUT
HISTORY_MAX_DATE <- as.Date(max(past$date, na.rm = TRUE))
CUT_STAMPED <- min(CUT_REQUESTED, HISTORY_MAX_DATE)
if (CUT_STAMPED < CUT_REQUESTED) {
  cli::cli_alert_warning(
    "Requested cutoff {CUT_REQUESTED} is after the last history date {HISTORY_MAX_DATE}; stamping cutoff = {CUT_STAMPED}.")
}
stopifnot("stamped cutoff must not be later than the run date" = CUT_STAMPED <= Sys.Date())

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

# WHY EACH tryCatch NAMES ITS OWN STAGE. A bare `error = function(e) NULL`
# collapsed project_field()/simulate_event()/medal_probs() throwing into the
# SAME `skipped = TRUE` row as a genuine too-few-entrants skip, and that wrong
# attribution then propagated into the permanent audit trail below
# (`event_skipped_fewer_than_3_rated`) -- a real crash reported, on a public
# artefact, as a data characteristic of the field. Caught in review, 2026-09-09.
#
# MEDAL_DRAWS, one per event, accumulated here for the nation table built after
# the main loop. Same JOINT-draws requirement predict_birmingham2026.R's own
# comment explains: summing marginal p_gold/p_medal per nation overstates the
# spread for any nation with two-plus entrants in one event, because their
# medal chances are strongly negatively dependent (they are competing for the
# same three medals). simulate_event() already returns the per-simulation rank
# matrix (`sim$rank`) -- no citius change needed, unlike simulate_rounds()'s
# opt-in medal_draws= flag, which exists only because ITS return value doesn't
# carry the raw matrix.
MEDAL_DRAWS <- list()
sim_event <- function(field_ids, ev) {
  f <- ids[event_id == ev & athlete_id %in% field_ids]
  ab_ev <- ability[event_id == ev & athlete_id %in% f$athlete_id]
  if (nrow(ab_ev) < 3L)
    return(data.table(event_id = ev, skipped = TRUE, n = nrow(ab_ev), reason = "too_few_rated_entrants"))
  ab_ev <- deployed_field(ab_ev, aging = aging, ages = ages[event_id == ev, .(athlete_id, age_now)])
  proj <- tryCatch(project_field(ab_ev, event = ev, as_of = CUT, size = nrow(f)),
                   error = function(e) conditionMessage(e))
  if (is.character(proj) || !nrow(proj))
    return(data.table(event_id = ev, skipped = TRUE, n = nrow(ab_ev),
                       reason = if (is.character(proj)) paste("project_field_error:", proj) else "project_field_empty"))
  sim <- tryCatch(simulate_event(proj, n_sims = N_SIMS, calibration = calibration, seed = SEED,
                                 context = deployed_race_context("final")),
                  error = function(e) conditionMessage(e))
  if (is.character(sim))
    return(data.table(event_id = ev, skipped = TRUE, n = nrow(proj), reason = paste("simulate_event_error:", sim)))
  s <- tryCatch(medal_probs(sim), error = function(e) conditionMessage(e))
  if (is.character(s) || !nrow(s))
    return(data.table(event_id = ev, skipped = TRUE, n = nrow(proj),
                       reason = if (is.character(s)) paste("medal_probs_error:", s) else "medal_probs_empty"))
  # `<<-`, not `<-`: same reasoning backtest_athletics.R's own comment gives for
  # local_shock_applied -- this sits inside sim_event()'s body, one scope
  # deeper than the top-level MEDAL_DRAWS list it must escape to.
  r <- sim$rank
  idx <- which(r <= 3L, arr.ind = TRUE)
  if (nrow(idx)) MEDAL_DRAWS[[ev]] <<- data.table::data.table(
    sim = idx[, 1L], athlete_id = colnames(r)[idx[, 2L]], place = r[idx], event_id = ev)
  s[, event_id := ev]
  keep_ab <- intersect(c("athlete_id", "ability", "sigma", "ability_se", "w_total"), names(ab_ev))
  s <- merge(s, ab_ev[, ..keep_ab], by = "athlete_id", all.x = TRUE)
  s[, skipped := FALSE][, n := nrow(proj)][, reason := NA_character_]
  s[]
}

res <- rbindlist(lapply(events, function(ev) sim_event(ids[event_id == ev]$athlete_id, ev)), fill = TRUE)
skipped <- unique(res[skipped == TRUE, .(event_id, n, reason)])
if (nrow(skipped)) {
  cli::cli_alert_warning("{nrow(skipped)} event{?s} skipped:")
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
ev_reason <- setNames(skipped$reason, skipped$event_id)
acct[is.na(reason) & event_id %in% names(ev_reason),
     reason := paste0("event_skipped: ", unname(ev_reason[event_id]))]
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
  # FABLE-redteam-2026-09-07 F3: cutoff is the data boundary, never a future plan.
  generated_at = Sys.time(), cutoff = CUT_STAMPED, cutoff_requested = CUT_REQUESTED,
  history_max_date = HISTORY_MAX_DATE,
  meet = MEET, competition_id = COMPETITION_ID,
  # THE HONEST STAMP, per meet -- see FIELD above for why these differ between
  # Brussels and Budapest. Birmingham/Glasgow both say "official_entry_list";
  # neither DL-shaped meet can honestly claim that, and they cannot claim the
  # same thing as each other either. The site's caveat convention (caveats live
  # in the data, not the qmd) is what renders this to a reader.
  field_type = FIELD$type,
  field_source = FIELD$source,
  half_life = DEPLOYED$half_life, config = DEPLOYED$stamp,
  counts_source = "derived", combined_rows_excluded = n_combined_excluded,
  n_rounds = 1L, field_modelled = n)]

stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
f <- file.path(D, paste0(MEET, "_pretournament_", stamp, ".parquet"))
arrow::write_parquet(pred, f)
saveRDS(pred, file.path(D, paste0(MEET, "_pretournament.rds")))

# --- nation projection ---------------------------------------------------
# Same shape and same reasoning as predict_birmingham2026.R's own nation table:
# built from the JOINT per-simulation podium (MEDAL_DRAWS, accumulated in
# sim_event() above), not by summing p_gold/p_medal marginals -- a nation with
# two-plus entrants in one event has strongly negatively dependent medal
# chances (they compete for the same three medals), so an independent sum
# overstates the spread. Requested by inthegame-blog#680: nations.qmd reads
# "{meet_id}-nations.parquet" and had none for a DL-shaped meet.
if (length(MEDAL_DRAWS)) {
  dr <- rbindlist(MEDAL_DRAWS, fill = TRUE)
  nat <- unique(ids[, .(athlete_id, nation = country)])
  dr <- merge(dr, nat, by = "athlete_id", all.x = TRUE)
  if (dr[is.na(nation), .N]) {
    cli::cli_abort("{dr[is.na(nation), .N]} medal draw{?s} could not be attributed to a nation.")
  }
  per_sim <- dr[, .(medals = .N, golds = sum(place == 1L)), by = .(sim, nation)]
  grid <- CJ(sim = seq_len(N_SIMS), nation = unique(per_sim$nation))
  per_sim <- merge(grid, per_sim, by = c("sim", "nation"), all.x = TRUE)
  per_sim[is.na(medals), `:=`(medals = 0L, golds = 0L)]

  proj <- per_sim[, .(
    exp_medals = mean(medals), exp_golds = mean(golds),
    p10 = stats::quantile(medals, 0.10, names = FALSE),
    p50 = stats::quantile(medals, 0.50, names = FALSE),
    p90 = stats::quantile(medals, 0.90, names = FALSE),
    p_any_gold  = mean(golds > 0),
    p_any_medal = mean(medals > 0)), by = nation]
  setorder(proj, -exp_medals)
  proj[, `:=`(meet = MEET, generated_at = Sys.time(), scope = "model",
              events_scored = uniqueN(pred$event_id), n_sims = N_SIMS)]

  n_ev_with_draws <- uniqueN(dr$event_id)
  stopifnot(
    "expected golds must sum to the number of events with medal draws" =
      abs(sum(proj$exp_golds) - n_ev_with_draws) < 0.01,
    "expected medals must sum to three per event with medal draws" =
      abs(sum(proj$exp_medals) - 3 * n_ev_with_draws) < 0.05)
  arrow::write_parquet(proj, file.path(D, paste0(MEET, "_nations.parquet")))
  cli::cli_alert_success(
    "Nation projection: {nrow(proj)} nation{?s} across {n_ev_with_draws} event{?s}; expected golds sum to {round(sum(proj$exp_golds), 2)}, medals to {round(sum(proj$exp_medals), 2)}.")
} else {
  cli::cli_alert_warning("No medal draws captured -- {MEET}_nations.parquet NOT written.")
}

cli::cli_alert_success("{format(nrow(pred), big.mark = ',')} row{?s} across {uniqueN(pred$event_id)} event{?s} -> {basename(f)}")
cli::cli_alert_info("Config: {DEPLOYED$stamp} | cutoff {CUT_STAMPED} (requested {CUT_REQUESTED}, history to {HISTORY_MAX_DATE}) | {N_SIMS} sims.")

chk <- pred[, .(gold = round(sum(p_gold, na.rm = TRUE), 3), medal = round(sum(p_medal, na.rm = TRUE), 3), n = .N), by = event_id]
bad <- chk[abs(gold - 1) > 0.01 | abs(medal - 3) > 0.05]
if (nrow(bad)) {
  cli::cli_alert_danger("{nrow(bad)} event{?s} with implausible probability sums:")
  print(bad)
} else {
  cli::cli_alert_success("Every event sums to 1 gold and 3 medals.")
}
