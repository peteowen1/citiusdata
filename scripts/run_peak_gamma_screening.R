# Run 50-meet screening backtest for peak_gamma = 1.0 (Exponential Quantile Weighting)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE_BASE <- file.path(OUT, "backtest_cache_coasting")
CACHE_PEAK <- file.path(OUT, "backtest_cache_peak1")

say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR PEAK_GAMMA = 1.0 ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_peak1",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_PEAK_GAMMA = "1.0",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== RUNNING QUICK COMPARE: coasting vs peak_gamma=1.0 ===")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
run_quick_compare(CACHE_BASE, CACHE_PEAK)
