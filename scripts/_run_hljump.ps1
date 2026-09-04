# Jump half-life confirmatory test: identical to _run_hltest.ps1 (which
# already carries the DEPLOYED road=1095/walk=730/hurdles=180) plus jump=180
# added. Vectorized per-event screen (vectorized_hl_sweep_all.R, 2026-09-02)
# found men's jump events want longer half-lives (150-210d) than the
# family's implicit 365 default and than women's jump events (mostly ~90) --
# this tests a single pooled jump=180 as the cheapest real-pipeline check,
# NOT split by sex (hl_family has no sex axis). Control is backtest_hltest_t1t2.rds
# itself, not backtest_hlctrl_t1t2.rds -- hlctrl predates the hurdles=180 adoption
# and would confound the jump variable with that already-decided change.
#
# _t1t2 suffix added 2026-09-02: an earlier re-run with no CITIUS_BT_TIER
# sampled TARGET=900 evenly BY ROW across the full pool, not by date, and
# landed entirely in 2016-2018 -- the wrong population to compare against the
# original hurdles=180 evidence (277 T1+T2 meets, 2023-01-01 holdout).
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_hljump_t1t2"
$env:CITIUS_BT_OUT           = "backtest_hljump_t1t2.rds"
$env:CITIUS_BT_MEETS         = "50"
$env:CITIUS_BT_TARGET        = "900"
$env:CITIUS_BT_WORKERS       = "4"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180,jump=180"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_TIER          = "T1_elite,T2_strong"
Remove-Item Env:\CITIUS_BT_SIGMA_SCALE -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$LOG = "C:\dev\citiusverse\citiusdata\hljump_t1t2_log.txt"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$n = (Get-ChildItem "citiusdata\data\backtest_cache_hljump_t1t2" -Filter *.rds |
        Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
Write-Host "HLJUMP DONE: cached=$n"
