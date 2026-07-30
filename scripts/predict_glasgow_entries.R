# Glasgow 2026 predictions from the OFFICIAL entry list.
#
# Supersedes predict_glasgow2026.R, which used projected fields because no start
# list was published. We now have confirmed entries (athlete, nation, DOB, PB,
# SB) scraped from the Games results system, so these are predictions over the
# real field rather than a guess at who would turn up.
#
# Entry names are matched to World Athletics ids via our own harvest rather than
# the search API: 85k harvested athletes give an offline lookup, and a name that
# does not resolve is dropped rather than guessed at.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(jsonlite)

OUT <- here::here("citiusdata", "data")
GAMES_DATE <- as.Date("2026-07-30")
# The competition being forecast, excluded from its own history by ID. Glasgow
# runs 27 Jul - 1 Aug, so GAMES_DATE alone does not cover it (citiusdata#1).
GLASGOW_2026 <- 7187518L
# 365 days, selected by A/B on out-of-sample RANKING skill over 5,872 backtest
# races -- not by fit_half_life(), which optimises next-result MAE and returns 90
# for sprints. Measured 2026-07-29 across six arms on an identical scored set:
# gold skill 0.183 (90d), 0.224 (180), 0.234 (270), 0.237 (365), 0.236 (540),
# 0.234 (730). Paired t-test vs 365: beats 730 (t=5.78), 540 (t=4.23), 180
# (t=3.44) and 90 (t=10.52); tied with 270. It also cuts the top-band
# over-confidence from -0.106 to -0.073.
# Model inputs come from DEPLOYED, never from literals here. See _deployed.R.
source(here::here("citiusdata", "scripts", "_deployed.R"))
N_SIMS <- 20000L

# The entry list is resolved to athlete ids by NAME, so the competition harvest
# is still read for its name -> id lookup. It is no longer the model's history.
champs      <- readRDS(file.path(OUT, "championship_results.rds"))
calibration <- deployed_calibration(OUT)
aging       <- deployed_aging(OUT)

# --- entry list --------------------------------------------------------------
j <- fromJSON(file.path(OUT, "glasgow2026_entries.json"), simplifyVector = FALSE)
evs <- unlist(j$events)
nz <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)
entries <- rbindlist(lapply(j$rows, function(r) data.table(
  event = evs[r[[1]] + 1], nation = nz(r[[2]]), athlete = nz(r[[3]]),
  dob = nz(r[[4]]), pb = nz(r[[5]]), sb = nz(r[[6]]))), fill = TRUE)

entries <- entries[!grepl("Relay|T1[0-9]|T2[0-9]|T3[0-9]|T4[0-9]|T5[0-9]|F[0-9]{2}|SM[0-9]", event)]
entries[, sex := fifelse(grepl("^Women", event), "W", fifelse(grepl("^Men", event), "M", NA_character_))]
entries[, discipline := sub("^(Men's|Women's|Mixed)\\s+", "", event)]
entries[, event_id := match_event(discipline, sex)]
entries <- entries[!is.na(event_id)]
cli::cli_alert_info("{nrow(entries)} entr{?y/ies} across {uniqueN(entries$event_id)} event{?s}.")

# --- resolve names to ids ----------------------------------------------------
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
lookup <- unique(champs[!is.na(athlete_name) & !is.na(athlete_id),
                        .(key = norm(athlete_name), athlete_id = as.character(athlete_id))])
lookup <- lookup[, .(athlete_id = athlete_id[1]), by = key]   # first id wins

entries[, key := norm(athlete)]
entries <- merge(entries, lookup, by = "key", all.x = TRUE)
cli::cli_alert_info("Resolved {sum(!is.na(entries$athlete_id))} of {nrow(entries)} entries to an athlete id.")

# --- ability -----------------------------------------------------------------
# Exclude the competition being predicted BY ID, not by date (citiusdata#1).
#
# The date cut alone was wrong and only safe by accident: GAMES_DATE is
# 2026-07-30 while Glasgow runs 27 Jul - 1 Aug, so `date < GAMES_DATE` admits
# days 1-3 of the very meet being forecast. It has never leaked because the
# harvest happens to stop at 2026-07-26 -- the moment anyone re-harvests
# mid-Games, finals enter the ability estimates and score_glasgow2026.R starts
# scoring the model against races it has already seen, silently.
#
# An ID exclusion cannot be defeated by a date being off by a few days, and the
# assertion below turns any future leak into a loud failure rather than a
# quietly excellent-looking forecast.
#
# flag_implausible() is NOT applied here: it is a GLOBAL operation (median and
# MAD per event across the whole corpus) and the deployed store has it applied
# at build time. Re-running it on a slice would compute different thresholds.
clean <- deployed_history(OUT, events = unique(entries[!is.na(event_id)]$event_id),
                          from = GAMES_DATE - DEPLOYED$history_days,
                          to = GAMES_DATE - 1L)
clean <- clean[!is.na(event_id) & !is.na(perf) &
                 (is.na(competition_id) | competition_id != GLASGOW_2026)]
leak <- champs[competition_id == GLASGOW_2026 & date < GAMES_DATE]
if (nrow(leak)) {
  cli::cli_alert_warning(
    "Excluded {nrow(leak)} in-Games result{?s} that the date cut would have admitted."
  )
}
stopifnot(
  "history must not contain the competition being predicted" =
    !any(clean$competition_id == GLASGOW_2026, na.rm = TRUE)
)
ability <- deployed_ability(clean, as_of = GAMES_DATE, calibration = calibration)

# Age on the day, carried forward from each athlete's last recorded age. Applied
# per field inside the loop rather than globally, because the field prior runs
# first and project_ability() scales its shift by (1 - shrinkage) -- so it must
# see the shrinkage the prior produced. That ordering is what the backtest does.
ages <- clean[!is.na(age), .(age_last = max(age), age_asof = max(date)),
              by = .(athlete_id = as.character(athlete_id), event_id)]
ages[, age_now := age_last + as.numeric(GAMES_DATE - age_asof) / 365.25]

# --- simulate ----------------------------------------------------------------
res <- list()
for (ev in unique(entries$event_id)) {
  field_ids <- entries[event_id == ev & !is.na(athlete_id)]$athlete_id
  ent <- ability[event_id == ev & athlete_id %in% field_ids]
  if (nrow(ent) < 3L) next
  ent <- deployed_field(ent, aging = aging, ages = ages[event_id == ev, .(athlete_id, age_now)])
  sim <- simulate_event(ent, n_sims = N_SIMS, calibration = calibration, seed = 20260727L)
  mp <- medal_probs(sim)
  mp[, event_id := ev]
  res[[length(res) + 1L]] <- mp
}
pred <- rbindlist(res, fill = TRUE)

info <- unique(entries[, .(athlete_id, athlete, nation, event_id)])
pred <- merge(pred, info, by = c("athlete_id", "event_id"), all.x = TRUE)
pred <- merge(pred, citius_events()[, .(event_id, discipline, sex)], by = "event_id")
pred[, `:=`(generated_at = Sys.time(), field_type = "official_entry_list",
            half_life = DEPLOYED$half_life, config = DEPLOYED$stamp)]

stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
arrow::write_parquet(pred, file.path(OUT, paste0("glasgow2026_entrylist_predictions_", stamp, ".parquet")))
cli::cli_alert_success("{nrow(pred)} prediction row{?s} across {uniqueN(pred$event_id)} event{?s}.")

# --- reports -----------------------------------------------------------------
setorder(pred, event_id, -p_gold)
cat("\n=== TOP 3 PER EVENT ===\n")
top3 <- pred[, .SD[1:min(3, .N)], by = event_id]
print(top3[, .(discipline, sex, athlete, nation,
               gold = round(p_gold, 3), medal = round(p_medal, 3))], nrows = 200)

cat("\n=== EXPECTED MEDALS BY NATION ===\n")
nat <- pred[, .(E_gold = sum(p_gold), E_medals = sum(p_medal),
                entries = .N), by = nation]
setorder(nat, -E_gold)
print(nat[1:20, .(nation, E_gold = round(E_gold, 2), E_medals = round(E_medals, 2), entries)])
