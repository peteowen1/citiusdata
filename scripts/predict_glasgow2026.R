# Glasgow 2026 Commonwealth Games — athletics predictions.
#
# SUPERSEDED by predict_glasgow_entries.R, which uses real start lists and the
# deployed configuration. Kept because METHODOLOGY.md and MODEL-LOG.md cite it as
# the only place the `momentum` adjustment was ever applied.
#
# DO NOT QUOTE ITS OUTPUT AS THE MODEL. It predates _deployed.R (2026-07-30) and
# names every input as a literal: athletics_history.rds (the 308k harvest, not
# the 6.6M corpus), calibration.rds (2026-07-28), half_life.rds. That is exactly
# the configuration the 2026-07-31 audit found shipping numbers nobody had
# validated. Left as-is rather than rewired — rewiring dead code hides that it is
# dead, and the runtime warning below is what a reader actually sees.
#
# Run BEFORE results exist. The point is a locked, timestamped forecast that can
# be scored out-of-sample as results land, so the output file is written with
# the generation time embedded and must never be regenerated after results are
# known.
#
# IMPORTANT — these are PROJECTED fields, not entry lists. World Athletics had
# no start list for competition 7187518 while the Games were under way, so the
# field for each event is the strongest Commonwealth-eligible athletes in the
# harvest. Athletes withdraw, are not selected, or contest a different event,
# and none of that is visible here. Scoring must account for it: an event whose
# actual field differs substantially from the projection tests nothing.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

GAMES_DATE <- as.Date("2026-07-30")     # mid-Games, for age projection
FIELD_SIZE <- 8L
N_SIMS     <- 50000L
OUT <- here::here("citiusdata", "data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

cli::cli_alert_warning(c(
  "SUPERSEDED script: this is NOT the deployed configuration. ",
  "It reads athletics_history.rds and calibration.rds, both pre-2026-07-31. ",
  "Use predict_glasgow_entries.R for anything published."))

history     <- readRDS(file.path(OUT, "athletics_history.rds"))
calibration <- readRDS(file.path(OUT, "calibration.rds"))
aging       <- readRDS(file.path(OUT, "aging.rds"))
profiles    <- readRDS(file.path(OUT, "profiles.rds"))
half_life   <- readRDS(file.path(OUT, "half_life.rds"))

athlete_countries <- unique(profiles[, .(athlete_id = as.character(athlete_id), country)])

ability <- estimate_ability(
  history[!is.na(perf) & !is.na(event_id)],
  as_of = GAMES_DATE,
  half_life = half_life,        # fitted per family, not assumed
  calibration = calibration
)

# Project each athlete's ability to the age they will be at the Games.
#
# `age_ref` comes from estimate_ability() and is the *weighted* mean age behind
# the estimate. Do not substitute a career mean: recency decay already makes the
# estimate reflect current form, so projecting from a junior-heavy career mean
# applies the ageing improvement twice. That bug once projected a sprinter
# faster than his personal best and made a 4-result junior the 100m favourite.
#
# There is deliberately no staleness cutoff. estimate_ability() shrinks on total
# weight, so an athlete whose last race was a decade ago carries almost no
# evidence and regresses to the event mean by itself. A cutoff would be a guess;
# the decay driving that shrinkage is fitted by fit_half_life().
ages <- history[!is.na(age), .(age_last = max(age), age_asof = max(date)),
                by = .(athlete_id = as.character(athlete_id), event_id)]
ability <- merge(ability, ages, by = c("athlete_id", "event_id"), all.x = TRUE)
ability[, age_now := age_last + as.numeric(GAMES_DATE - age_asof) / 365.25]
ability <- project_ability(ability[!is.na(age_ref) & !is.na(age_now)], aging)

events <- ability[, .N, by = event_id][N >= FIELD_SIZE]$event_id
cli::cli_alert_info("Predicting {length(events)} event{?s}.")

predictions <- rbindlist(lapply(events, function(ev) {
  field <- project_field(ability, ev,
                         nations = commonwealth_nations(),
                         athlete_countries = athlete_countries,
                         size = FIELD_SIZE,
                         as_of = GAMES_DATE)
  if (nrow(field) < 4L) return(NULL)

  sim <- simulate_event(field, n_sims = N_SIMS, calibration = calibration, seed = 20260727L)
  out <- medal_probs(sim)
  out <- merge(out, field[, .(athlete_id, country, implied = perf_to_mark(ability, sim$orientation))],
               by = "athlete_id")
  out[, event_id := ev][, field_size := nrow(field)]
  out[]
}), use.names = TRUE, fill = TRUE)

predictions[, generated_at := Sys.time()]
predictions[, field_type := "projected"]

stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
path <- file.path(OUT, paste0("glasgow2026_predictions_", stamp, ".parquet"))
arrow::write_parquet(predictions, path)
cli::cli_alert_success("Wrote {nrow(predictions)} prediction row{?s} to {.path {path}}.")

setorder(predictions, event_id, -p_gold)
print(predictions[, .SD[1:3], by = event_id][
  , .(event_id, athlete_id, country, p_gold = round(p_gold, 3),
      p_medal = round(p_medal, 3))])
