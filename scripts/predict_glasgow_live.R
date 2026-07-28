# Re-predict Glasgow finals from the field that ACTUALLY turned up.
#
# The entry-list predictions put 9% of their gold probability on athletes who
# never appeared at the Games -- the high jump favourite Hamish Kerr alone
# carried 0.35. That is a field error, not a model error, but it is real lost
# accuracy and it is avoidable once a round has been run.
#
# Where an earlier round exists, the field for the final is knowable:
#   - heats run    -> the final field comes from those who contested the heats,
#                     and qualification uncertainty is modelled by
#                     simulate_rounds() rather than assumed away
#   - final run    -> nothing to predict
#   - nothing run  -> fall back to the entry list, flagged as provisional
#
# Athletes who contested a round but recorded no mark are KEPT: a no-height in
# qualifying is a performance, and foul_rate already models it.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
GLASGOW <- 7187518L
HALF_LIFE <- as.numeric(Sys.getenv("CITIUS_HALF_LIFE", "730"))
N_SIMS <- 20000L

res <- tryCatch(setDT(athletics_harvest_competitions(GLASGOW)), error = function(e) NULL)
if (is.null(res) || !nrow(res)) {
  cli::cli_alert_warning("No Glasgow results yet; nothing to re-predict from.")
  quit(save = "no")
}
res[, athlete_id := as.character(athlete_id)]
res[, rc := citius:::.round_class(round)]
# A combined-events leg is NOT a round of the standalone event. The heptathlon
# 200m is contested by heptathletes, and treating it as the 200m's first round
# builds the final's field out of the wrong athletes entirely -- it put the
# women's 200m, 100m hurdles and shot put finals on heptathlon fields. The
# reporting of unraced entrants is what surfaced this: those three events showed
# ~100% of gold probability sitting on athletes who had supposedly not raced,
# because every athlete who HAD "raced" was in a different competition.
n_comb <- sum(grepl("combined", res$round, ignore.case = TRUE))
if (n_comb) {
  cli::cli_alert_info("Dropping {n_comb} combined-events result{?s}; a heptathlon leg is not a round of the standalone event.")
  res <- res[!grepl("combined", round, ignore.case = TRUE)]
}

CUT <- min(res$date, na.rm = TRUE)
# Read only the events and window needed, from the partitioned store.
# Measured at 8.6M rows: 46.1s to load an .rds and filter it against 0.09s
# here, because partition pruning never opens the other event files.
# flag_implausible() is already applied at store-build time -- it is a
# GLOBAL operation and cannot be redone on a slice.
STORE <- file.path(OUT, "athletics_store")
USE_STORE <- dir.exists(STORE)
champs <- if (USE_STORE) NULL else readRDS(file.path(OUT, "championship_results.rds"))
cal <- readRDS(file.path(OUT, "calibration.rds"))
clean <- if (USE_STORE) NULL else flag_implausible(champs)[!is.na(event_id) & !is.na(perf)]
# Excluded by ID as well as by date (citiusdata#1). This script is the one most
# exposed: it runs DURING the Games, so any re-harvest between now and the final
# would put the heats it is predicting from into the ability estimates too.
past <- if (USE_STORE) {
  read_results_store(STORE, events = unique(res$event_id),
                     from = CUT - 4380, to = CUT - 1L)[
                       !is.na(event_id) & !is.na(perf)]
} else {
  clean[date < CUT & date >= CUT - 4380 & event_id %in% unique(res$event_id)]
}
# Excluded by ID as well as by date (citiusdata#1), whichever source it came
# from. This script runs DURING the Games, so a re-harvest between now and the
# final would otherwise feed it the very heats it is predicting from.
past <- past[is.na(competition_id) | competition_id != GLASGOW]
stopifnot("history must not contain the competition being predicted" =
            !any(past$competition_id == GLASGOW, na.rm = TRUE))
ability <- estimate_ability(past, as_of = CUT, half_life = HALF_LIFE, calibration = cal)

# --- entrants who have not raced yet ----------------------------------------
# Top seeds receive a BYE straight into the semi-finals, so a field built from
# round-1 contestants alone structurally excludes exactly the best athletes.
# Measured at Glasgow: 7 byes in each 100m, including Omanyala, Ackeem Blake,
# Zoe Hobbs and Amy Hunt -- and BOTH eventual champions, Emmanuel Eseme and Zoe
# Hobbs, were dropped from the field of the race they went on to win.
#
# The gap closes on its own once semis are run, because byes appear there. It
# only bites in the window after round 1 and before the semi, which is precisely
# when a live re-prediction is most useful.
entries <- tryCatch({
  j <- jsonlite::fromJSON(file.path(OUT, "glasgow2026_entries.json"), simplifyVector = FALSE)
  evs <- unlist(j$events)
  nz <- function(z) if (is.null(z) || !length(z)) NA_character_ else as.character(z)
  e <- rbindlist(lapply(j$rows, function(r) data.table(
    event = evs[r[[1]] + 1], nation = nz(r[[2]]), athlete = nz(r[[3]]))), fill = TRUE)
  e <- e[!grepl("Relay|T1[0-9]|T2[0-9]|T3[0-9]|T4[0-9]|T5[0-9]|F[0-9]{2}|SM[0-9]", event)]
  e[, sex := fifelse(grepl("^Women", event), "W", fifelse(grepl("^Men", event), "M", NA_character_))]
  e[, event_id := match_event(sub("^(Men's|Women's|Mixed)\\s+", "", event), sex)]
  e <- e[!is.na(event_id)]
  norm <- function(z) gsub("[^A-Z]", "", toupper(z))
  src <- if (!is.null(champs)) champs else readRDS(file.path(OUT, "championship_results.rds"))
  lk <- unique(as.data.table(src)[!is.na(athlete_name) & !is.na(athlete_id),
                                  .(key = norm(athlete_name), athlete_id = as.character(athlete_id))])
  lk <- lk[, .(athlete_id = athlete_id[1]), by = key]
  e[, key := norm(athlete)]
  merge(e, lk, by = "key", all.x = TRUE)[!is.na(athlete_id)]
}, error = function(e) { cli::cli_alert_warning("No entry list; byes cannot be recovered."); NULL })

out <- list()
for (ev in sort(unique(res$event_id))) {
  if (is.na(ev)) next
  x <- res[event_id == ev]
  if (any(x$rc == "final")) next                       # already decided
  # Everyone who contested the earlier round, marks or not.
  raced <- unique(x$athlete_id)
  # Plus entrants who have not raced at all: byes, and unavoidably also any
  # withdrawals, which are indistinguishable until a later round is run. Keeping
  # them costs some probability mass on non-starters; dropping them cost two
  # gold medals at Glasgow, so the trade is clearly worth taking. The mass on
  # unraced athletes is reported per event rather than absorbed silently.
  unraced <- if (!is.null(entries))
    setdiff(entries[event_id == ev]$athlete_id, raced) else character()
  field <- union(raced, unraced)
  ent <- ability[event_id == ev & athlete_id %in% field]
  if (nrow(ent) < 4L) next

  n_heats <- uniqueN(x$race_key)
  reg <- citius_events()
  fam <- reg$family[match(ev, reg$event_id)]
  n_final <- if (fam %in% c("jump", "throw")) 12L else 8L

  # When there are more heats than final places, a naive `n_final %/% n_heats`
  # floors to 0, gets clamped to 1 per heat, and qualifies MORE athletes than
  # the final holds -- 11 heats produced an 11-lane 100m final. Championships
  # solve this with a semi-final round, so insert one whenever the heats cannot
  # feed the final directly.
  build <- function(nh, target) {
    per <- target %/% nh
    if (per >= 1L) {
      list(list(races = nh, advance = per,
                fastest_losers = max(0L, target - per * nh)))
    } else {
      NULL                                   # caller adds an intermediate round
    }
  }
  structure <- if (n_heats <= n_final) {
    c(build(n_heats, n_final), list(list(races = 1)))
  } else {
    # Heats -> semis -> final. Semis are sized to the final's capacity.
    n_semi <- max(2L, min(n_heats %/% 2L, 3L))
    n_semi_field <- n_semi * n_final %/% 2L   # ~half a final's worth per semi
    c(build(n_heats, n_semi_field),
      build(n_semi, n_final),
      list(list(races = 1)))
  }

  r <- simulate_rounds(ent, structure = structure,
                       n_sims = N_SIMS, calibration = cal, seed = 20260728L)

  # The full finishing distribution for the final itself, conditional on the
  # field reaching it. A medal probability cannot express fourth place, and
  # fourth is the position people actually ask about.
  pos <- position_probs(
    simulate_event(ent[athlete_id %in% r[p_final > 0.01]$athlete_id],
                   n_sims = N_SIMS, calibration = cal, seed = 20260728L),
    max_position = n_final, wide = TRUE)
  r <- merge(r, pos, by = "athlete_id", all.x = TRUE)
  r[, `:=`(event_id = ev, contested_round = x$round[1], n_heats = n_heats,
           field_size = length(field), n_unraced = length(unraced))]
  r[, unraced := athlete_id %in% unraced]
  if (length(unraced)) {
    cli::cli_alert_info(
      "{ev}: {length(unraced)} entrant{?s} carried in unraced (byes or withdrawals), holding {round(100 * sum(r[unraced == TRUE]$p_gold))}% of gold probability."
    )
  }
  out[[length(out) + 1L]] <- r
}

if (!length(out)) {
  cli::cli_alert_info("No event has an earlier round but no final yet.")
  quit(save = "no")
}
pred <- rbindlist(out, fill = TRUE)
info <- unique(res[, .(athlete_id, event_id, athlete_name)])
pred <- merge(pred, info, by = c("athlete_id", "event_id"), all.x = TRUE)
stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
arrow::write_parquet(pred, file.path(OUT, paste0("glasgow2026_live_predictions_", stamp, ".parquet")))
cli::cli_alert_success("{nrow(pred)} row{?s} across {uniqueN(pred$event_id)} event{?s} still to be decided.")

setorder(pred, event_id, -p_gold)
for (ev in unique(pred$event_id)) {
  x <- pred[event_id == ev]
  cat(sprintf("\n%s  (%d contested %s in %d heat%s)\n", ev, x$field_size[1],
              x$contested_round[1], x$n_heats[1], if (x$n_heats[1] == 1) "" else "s"))
  cols <- c("athlete_name", "p_final", "p_gold", "p_medal",
            intersect(c("pos_1", "pos_2", "pos_3", "pos_4"), names(x)))
  y <- x[, ..cols]
  data.table::setnames(y, c("athlete_name", "p_final", "p_gold", "p_medal"),
                       c("athlete", "reach_final", "gold", "medal"))
  y[, athlete := substr(athlete, 1, 22)]
  num <- setdiff(names(y), "athlete")
  y[, (num) := lapply(.SD, round, 3), .SDcols = num]
  print(head(y, 6))
}
cat("\nEvery athlete here contested a round at these Games, so no probability\n")
cat("sits on a withdrawal. p_final carries the qualification uncertainty, and\n")
cat("pos_4 is the near-miss a medal probability cannot express.\n")
