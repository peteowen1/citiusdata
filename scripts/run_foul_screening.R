# Build calibration with foul_round and run 50-meet screening backtest
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("Re-building calibration_corpus_coasting.rds to include foul_round...")
champs <- readRDS(file.path(OUT, "championship_results.rds"))
champs <- flag_implausible(champs)

# Attach meet_tier if available
cat_file <- file.path(OUT, "competition_catalogue.parquet")
if (file.exists(cat_file)) {
  cat_dt <- setDT(arrow::read_parquet(cat_file))[, .(competition_id = as.character(competition_id), meet_tier)]
  champs[, competition_id := as.character(competition_id)]
  champs <- merge(champs, cat_dt, by = "competition_id", all.x = TRUE)
}

cal <- calibrate(champs, min_races = 30L)
cal$coasting_trait <- fit_coasting_trait(champs, min_heats = 2L, shrink_k = 5.0)

cal_path <- file.path(OUT, "calibration_corpus_coasting.rds")
saveRDS(cal, cal_path)
say("Updated calibration_corpus_coasting.rds with foul_round!")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR ROUND-DEPENDENT FOUL RATES ===")
CACHE_BASE <- file.path(OUT, "backtest_cache_coasting")
CACHE_FOUL <- file.path(OUT, "backtest_cache_foul")

Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_foul",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== QUICK COMPARE: coasting vs foul_round ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_foul")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
