# Run 50-meet screening backtest for milder peak_gamma values (0.25 and 0.50)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE_BASE  <- file.path(OUT, "backtest_cache_coasting")
CACHE_P025  <- file.path(OUT, "backtest_cache_peak025")
CACHE_P050  <- file.path(OUT, "backtest_cache_peak050")

say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR PEAK_GAMMA = 0.25 ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_peak025",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_PEAK_GAMMA = "0.25",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)
source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR PEAK_GAMMA = 0.50 ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_peak050",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_PEAK_GAMMA = "0.50",
  CITIUS_BT_CALIBRATION = "calibration_corpus_coasting.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)
source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== QUICK COMPARE: coasting vs peak_gamma = 0.25 ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_peak025")
source(here::here("citiusdata", "scripts", "quick_compare.R"))

say("=== QUICK COMPARE: coasting vs peak_gamma = 0.50 ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_peak050")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
