# Run screening backtest for the coasting arm and compare against mtierf baseline.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")

Sys.setenv(CITIUS_BT_CACHE = "backtest_cache_coasting")
Sys.setenv(CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds")
Sys.setenv(CITIUS_BT_MEETS = "50")

cat("\n=== RUNNING 50-MEET SCREENING BACKTEST FOR COASTING ARM ===\n")
source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

cat("\n=== RUNNING QUICK COMPARE: mtierf vs coasting ===\n")
Sys.setenv(CITIUS_QC_A = "backtest_cache_mtierf")
Sys.setenv(CITIUS_QC_B = "backtest_cache_coasting")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
