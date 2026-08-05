# Run 50-meet screening backtest for Asymmetric Huber Robust Location Estimation (robust_location = TRUE)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR ROBUST LOCATION ESTIMATION ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_robloc",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_ROBUST_LOCATION = "TRUE",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== QUICK COMPARE: coasting vs robust_location ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_robloc")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
