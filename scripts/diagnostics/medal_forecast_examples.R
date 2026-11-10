# Example rows for the medal FORECAST on the athlete ratings page (citius#5).
#
# DECIDED 2026-11-08 (Pete, replacing the 2026-10-27 frozen-field rating): the
# page shows a forecast, not a rating. For each event, today's best athletes
# go into ONE simulated final, the same size as that event's Paris 2024 final
# (8 sprints/hurdles/800, 12 for the 1500m and field events, 16 for the 5000m,
# ~80 marathon), and each gets a chance of gold (sums to 1 per event) and of
# any medal (sums to 3). Athletes outside the final get no number. The
# simulation is citius::simulate_event() + medal_probs(), never re-implemented.
#
# Field sizes come from data/medal_reference_fields.parquet (the Paris/Tokyo
# finalist counts); only the counts are used now, not the frozen abilities.
# Publishes nothing. Run from C:/dev/citiusverse.

suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages({ library(data.table); library(arrow) })
source(here::here("citiusdata", "scripts", "_deployed.R"))
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
OUT <- here::here("citiusdata", "data")

EVENTS <- c("AT-100Metres-M", "AT-100Metres-W", "AT-400MetresHurdles-M", "AT-1500Metres-W",
            "AT-HighJump-W", "AT-JavelinThrow-M", "AT-Marathon-M", "AT-35KilometresRaceWalk-M")
N_SIMS <- 10000L

final_size <- setDT(read_parquet(file.path(OUT, "medal_reference_fields.parquet")))[, .(n_final = .N), by = event_id]
calibration <- deployed_calibration(OUT)
clean_all <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))
clean_all <- clean_all[!is.na(event_id) & !is.na(perf)]
AS_OF <- max(clean_all$date, na.rm = TRUE)
ab <- deployed_ability(clean_all, as_of = AS_OF, calibration = calibration)
rm(clean_all); invisible(gc())
ab[, athlete_id := as.character(athlete_id)]
ab <- ab[event_id %in% EVENTS & !is.na(last_date) & as.numeric(AS_OF - last_date) <= 730]

nm <- unique(setDT(readRDS(file.path(OUT, "championship_results.rds")))[!is.na(athlete_name),
          .(athlete_id = as.character(athlete_id), athlete_name)])[, .(athlete_name = athlete_name[1]), by = athlete_id]
ev <- citius_events()[, .(event_id, orientation)]

rows <- rbindlist(lapply(EVENTS, function(e) {
  # NOT `n`: deployed_ability() returns a column called n, which would shadow
  # it inside the data.table brackets (first run died on exactly that).
  size_final <- final_size[event_id == e, n_final]
  if (!length(size_final)) { cli::cli_alert_warning("{e}: no final size"); return(NULL) }
  field <- head(ab[event_id == e][order(-ability)], size_final)
  sim <- simulate_event(field, n_sims = N_SIMS, calibration = calibration,
                        context = list(meet_tier = "M1", round_class = "final"), seed = 1L)
  mp <- as.data.table(medal_probs(sim))[, .(athlete_id, p_gold, p_medal)]
  o <- ev[event_id == e, orientation]
  out <- merge(field[, .(event_id, athlete_id, ability, ability_se, last_date)], mp, by = "athlete_id")
  out[, `:=`(typical_mark = round(perf_to_mark(ability, o), 2), n_final = size_final)]
  setorder(out, -p_medal)
  out[, rank := seq_len(.N)]
  cli::cli_alert_info("{e}: final of {size_final}, gold sums to {round(sum(out$p_gold), 3)}, medals to {round(sum(out$p_medal), 3)}")
  out
}))
rows <- merge(rows, nm, by = "athlete_id", all.x = TRUE)
setcolorder(rows, c("event_id", "rank", "athlete_name", "typical_mark", "p_gold", "p_medal", "n_final"))
setorder(rows, event_id, rank)
f <- file.path(OUT, "medal_forecast_examples_20261108.csv")
fwrite(rows, f)
cli::cli_alert_success("wrote {.file {f}} ({nrow(rows)} rows, as of {AS_OF})")
