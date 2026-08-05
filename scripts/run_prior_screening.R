# Run 50-meet screening backtest comparing baseline vs condition_prior_weight = 0.5
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
CACHE_BASE  <- file.path(OUT, "backtest_cache_coasting")
CACHE_PRIOR <- file.path(OUT, "backtest_cache_prior")

say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR CONDITION PRIOR ARM ===")
backtest_athletics(
  cache_dir   = CACHE_PRIOR,
  n_sims      = 4000L,
  max_meets   = 50L,
  calibration = "calibration_corpus_coasting.rds",
  half_life   = "half_life_fitted.rds"
)

say("=== RUNNING QUICK COMPARE: coasting vs prior ===")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
run_quick_compare(CACHE_BASE, CACHE_PRIOR)
