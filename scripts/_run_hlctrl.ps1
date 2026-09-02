# Control for the hurdles half-life test: project_tier + family debias, NO
# sigma scale (irrelevant to marks, already flagged unready), default
# half-life (road=1095, walk=730 only -- the deployed defaults).
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_hlctrl_t1t2"
$env:CITIUS_BT_OUT           = "backtest_hlctrl_t1t2.rds"
$env:CITIUS_BT_MEETS         = "50"
$env:CITIUS_BT_TARGET        = "900"
$env:CITIUS_BT_WORKERS       = "4"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
# Match the original hurdles=180 adoption evidence's scope (T1+T2 meets, both
# sides of the 2023-01-01 holdout) -- an earlier re-run with no tier filter
# sampled TARGET=900 evenly BY ROW, not by date, and landed entirely in
# 2016-2018 because that's where most rows in the unrestricted pool sit.
$env:CITIUS_BT_TIER          = "T1_elite,T2_strong"
Remove-Item Env:\CITIUS_BT_SIGMA_SCALE -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_HALF_LIFE_FAMILY -ErrorAction SilentlyContinue

$LOG = "C:\dev\citiusverse\citiusdata\hlctrl_t1t2_log.txt"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$n = (Get-ChildItem "citiusdata\data\backtest_cache_hlctrl_t1t2" -Filter *.rds |
        Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
Write-Host "HLCTRL DONE: cached=$n"
