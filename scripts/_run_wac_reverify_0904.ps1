# Re-verify the 2026-08-29 WAC-tier (meet_tier) context-adjustment rejection
# against today's rebuilt catalogue/corpus. Three steps, sequential (control
# and treatment share the cache-fingerprint mechanism and must not run
# concurrently with anything writing to the same corpus/catalogue files):
#   1. build_calibration_wac_coast_0904.R  -- fresh WAC-tier+coasting calibration
#   2. control arm  -- deployed feed-tier calibration, T1_elite population
#   3. treatment arm -- WAC-tier calibration + CITIUS_BT_MEET_TIER=1, same population
# Per-event scoring (score_wac_by_event.R) is a separate manual step once both
# arms finish, so a crash here doesn't lose completed arms.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\wac_reverify_0904_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[1/3] building WAC-tier+coasting calibration..."
& Rscript "citiusdata\scripts\build_calibration_wac_coast_0904.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
if (-not (Test-Path "citiusdata\data\calibration_corpus_wac_coast_0904.rds")) {
  "=== ABORT: calibration build did not produce output, stopping before any backtest ===" | Out-File -Append -Encoding utf8 $LOG
  Write-Host "ABORT: calibration build failed, see log"
  exit 1
}
"=== calibration build done $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[2/3] control arm (deployed feed-tier, T1_elite)..."
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE       = "athletics_corpus_store"
$env:CITIUS_BT_CACHE       = "bt_cache_wac_ctrl_0904"
$env:CITIUS_BT_OUT         = "backtest_wac_ctrl_0904.rds"
$env:CITIUS_BT_TIER        = "T1_elite"
$env:CITIUS_BT_MEETS       = "200"
$env:CITIUS_BT_WORKERS     = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_MEET_TIER -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== control arm done $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[3/3] treatment arm (WAC meet_tier, T1_elite)..."
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_CACHE       = "bt_cache_wac_trt_0904"
$env:CITIUS_BT_OUT         = "backtest_wac_trt_0904.rds"
$env:CITIUS_BT_MEET_TIER   = "1"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== treatment arm done $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "WAC_REVERIFY DONE"
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
