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
HALF_LIFE <- 730          # tuned on ranking skill, not next-result MAE
N_SIMS <- 20000L

champs      <- readRDS(file.path(OUT, "championship_results.rds"))
calibration <- readRDS(file.path(OUT, "calibration.rds"))
aging       <- readRDS(file.path(OUT, "aging.rds"))

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
clean <- flag_implausible(champs)[!is.na(event_id) & !is.na(perf) & date < GAMES_DATE]
ability <- estimate_ability(clean, as_of = GAMES_DATE, half_life = HALF_LIFE,
                            calibration = calibration)

ages <- clean[!is.na(age), .(age_last = max(age), age_asof = max(date)),
              by = .(athlete_id = as.character(athlete_id), event_id)]
ability <- merge(ability, ages, by = c("athlete_id", "event_id"), all.x = TRUE)
ability[, age_now := age_last + as.numeric(GAMES_DATE - age_asof) / 365.25]
ability <- suppressWarnings(project_ability(ability[!is.na(age_ref) & !is.na(age_now)], aging))

# --- simulate ----------------------------------------------------------------
res <- list()
for (ev in unique(entries$event_id)) {
  field_ids <- entries[event_id == ev & !is.na(athlete_id)]$athlete_id
  ent <- ability[event_id == ev & athlete_id %in% field_ids]
  if (nrow(ent) < 3L) next
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
            half_life = HALF_LIFE)]

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
