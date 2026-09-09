# Marks correction with offsets fitted on a WIDER pre-test window.
#
# Pete's point, 2026-09-05: marks is not the hard half. It is a level error you
# can measure and subtract, so the question is how much correction the data
# supports out of sample -- not whether marks is "the constraint".
#
# The earlier run fitted offsets on [2016, 2018): 641 races, ~12 per event, so
# empirical-Bayes shrinkage pulled most events most of the way back to their
# family mean and the correction applied was weak. This fits [2016, 2020):
# 1,544 races, 19,627 rows, 64 events, shrinkage weights ~0.90. Still strictly
# out of sample -- the fit ends the day the test set begins.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\debias_wide_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_debias_wide"
$env:CITIUS_BT_OUT           = "backtest_debias_wide.rds"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_TRAIN_TIERS -ErrorAction SilentlyContinue

for ($i = 1; $i -le 6; $i++) {
  "--- attempt $i $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_debias_wide.rds") {
    $f = Get-Item "citiusdata\data\backtest_debias_wide.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) {
      "=== ARM DONE $(Get-Date) (attempt $i) ===" | Out-File -Append -Encoding utf8 $LOG
      break
    }
  }
  Start-Sleep -Seconds 30
}

"--- scoring the marks half $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_debias_wide.rds"
$env:CITIUS_GOAL_MARKS_ONLY = "1"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "DEBIAS WIDE DONE"
