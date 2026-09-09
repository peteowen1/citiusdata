# Smoke test for CITIUS_BT_TRAIN_TIERS: prove the code path RUNS, not just
# parses. One meet, MARKS_ONLY (no simulation), throwaway cache and output.
# "Parses is not runs" -- a rename sweep verified only by parsing left a guard
# suite broken for a day.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\smoke_train_tiers_log.txt"
if (Test-Path $LOG) { Remove-Item $LOG }

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_smoke_traintiers"
$env:CITIUS_BT_OUT           = "backtest_smoke_traintiers.rds"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_TRAIN_TIERS   = "T1_elite,T2_strong"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "1"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_SCORE_MIN_RACES  = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"

& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Write-Host "SMOKE DONE"
