# One-off verification run: MARKS_ONLY fast path against the SAME config as
# backtest_combined_full.rds (calibration_corpus_csigma_coast, tier=0.5,
# family_debias=1, sigma_scale=0.785). Fresh empty cache -> todo = the full
# 277-meet pool in the same deterministic order combined_full used, so the
# first N meets picked here are guaranteed already present in
# backtest_cache_combined_full for a direct median_mark comparison.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_marksonly_verify"
$env:CITIUS_BT_OUT           = "backtest_marksonly_verify.rds"
$env:CITIUS_BT_MEETS         = "8"
$env:CITIUS_BT_TARGET        = "900"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_BT_SIGMA_SCALE   = "0.785"
$env:CITIUS_BT_MARKS_ONLY    = "1"
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$LOG = "C:\dev\citiusverse\citiusdata\marksonly_verify_log.txt"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Encoding utf8 $LOG
Write-Host "MARKS_ONLY_VERIFY DONE"
