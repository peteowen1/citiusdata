# Harvest World Athletics result histories and fit a calibration.
#
# Race groupings are reconstructed from athlete histories, so calibration
# quality scales with overlap: the more athletes harvested, the more
# championship fields are recovered whole, and the better the shared-shock and
# sensitivity estimates become.
#
# Results are written unfiltered. Missing marks are meaningful — a no-mark in a
# technical event is how the foul rate is measured — so filtering happens
# downstream, never here.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT_DIR <- here::here("citiusdata", "data")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Seed athlete ids from championship fields, then harvest each history in full.
SEED_COMPETITIONS <- c(
  7190593,  # World Athletics Championships, Tokyo 2025
  7153115,  # Olympic Games, Paris 2024
  7147633   # XXII Commonwealth Games, Birmingham 2022
)

seed_ids <- unique(unlist(lapply(SEED_COMPETITIONS, function(cid) {
  r <- tryCatch(competition_results(cid), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) return(NULL)
  r$athlete_id
})))
seed_ids <- seed_ids[!is.na(seed_ids)]
cli::cli_alert_info("Seeded {length(seed_ids)} athlete{?s} from competition fields.")

histories <- rbindlist(lapply(seed_ids, function(id) {
  tryCatch(athlete_results(as.integer(id)), error = function(e) NULL)
}), use.names = TRUE, fill = TRUE)

cli::cli_alert_success(
  "Harvested {nrow(histories)} performance{?s} from {uniqueN(histories$athlete_id)} athlete{?s}."
)

keyed <- add_race_key(histories)
calibration <- calibrate(keyed)

arrow::write_parquet(keyed, file.path(OUT_DIR, "athletics_results.parquet"))
saveRDS(calibration, file.path(OUT_DIR, "calibration.rds"))

print(calibration)
