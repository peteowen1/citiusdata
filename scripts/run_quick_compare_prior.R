# Paired statistical compare: coasting (A) vs prior (B)
source(here::here("citiusdata", "scripts", "quick_compare.R"))
OUT <- here::here("citiusdata", "data")
run_quick_compare(file.path(OUT, "backtest_cache_coasting"), file.path(OUT, "backtest_cache_prior"))
