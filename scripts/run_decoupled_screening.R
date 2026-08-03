# Run 50-meet screening backtest for Dual-Path Decoupling
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR DUAL-PATH DECOUPLING ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_decoupled",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_DECOUPLE_PEAK = "TRUE",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== QUICK COMPARE: coasting vs dual-path decoupled ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_decoupled")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
