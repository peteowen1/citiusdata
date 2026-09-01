# Finishes backtest_combined_full.rds (project_tier 0.5 + family debias fit
# 2016-17/applied 2018+ + sigma scale 0.785) -- 201 of 277 meets already
# cached, this resumes and completes the remaining ~76 at full 12-worker
# parallelism now that the two bugs that forced manual foreground chunking
# (ErrorActionPreference=Stop, FAMILY_DEBIAS export) are both fixed.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_combined_full"
$env:CITIUS_BT_OUT           = "backtest_combined_full.rds"
$env:CITIUS_BT_MEETS         = "300"
$env:CITIUS_BT_TARGET        = "900"
$env:CITIUS_BT_WORKERS       = "12"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_BT_SIGMA_SCALE   = "0.785"
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$LOG = "C:\dev\citiusverse\citiusdata\combined_full_rest_log.txt"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$n = (Get-ChildItem "citiusdata\data\backtest_cache_combined_full" -Filter *.rds |
        Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
Write-Host "COMBINED_FULL_REST DONE: cached=$n"
