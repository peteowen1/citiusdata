# Fit athlete_foul traits and run 50-meet screening backtest
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("Fitting athlete_foul trait on championship corpus...")
champs <- readRDS(file.path(OUT, "championship_results.rds"))
champs <- flag_implausible(champs)

cal <- readRDS(file.path(OUT, "calibration_corpus_coasting.rds"))
cal$athlete_foul <- fit_athlete_foul_trait(champs, shrink_k = 5.0)

cal_path <- file.path(OUT, "calibration_corpus_athfoul.rds")
saveRDS(cal, cal_path)
say("Saved calibration_corpus_athfoul.rds with athlete_foul traits!")

say("=== RUNNING 50-MEET SCREENING BACKTEST FOR ATHLETE FOUL TRAIT ===")
Sys.setenv(
  CITIUS_BT_CACHE = "backtest_cache_athfoul",
  CITIUS_BT_MEETS = "50",
  CITIUS_BT_CALIBRATION = "calibration_corpus_athfoul.rds",
  CITIUS_BT_HALF_LIFE = "half_life_fitted.rds"
)

source(here::here("citiusdata", "scripts", "backtest_athletics.R"))

say("=== QUICK COMPARE: coasting vs athlete_foul ===")
Sys.setenv(CITIUS_QC_A = "backtest_cache_coasting", CITIUS_QC_B = "backtest_cache_athfoul")
source(here::here("citiusdata", "scripts", "quick_compare.R"))
