# The goal's training design PLUS the marks fix: T1+T2-only training,
# project_tier + family-pool debias, scored on T1 since 2020.
#
# Offsets were refitted on backtest_traintiers_t1t2.rds -- NOT the full-history
# arm's offsets. A differently-trained model has a different level bias, so
# applying the full-history offsets here would be correcting for a bias this
# model does not have.
#
# CEILING WORTH KNOWING BEFORE READING THE RESULT: the T1+T2 arm beats last-5
# on medal logloss in only 35 of 54 events (against 42 for full history), and
# these marks levers cannot change a probability. So the combined goal for this
# arm CANNOT exceed 35 of 54 no matter how good the marks half is -- already
# below what full-history + debias reached (22 of 54 with a 42 ceiling).
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\traintiers_debias_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_traintiers_debias"
$env:CITIUS_BT_OUT           = "backtest_traintiers_debias.rds"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_TRAIN_TIERS   = "T1_elite,T2_strong"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"

for ($i = 1; $i -le 6; $i++) {
  "--- attempt $i $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_traintiers_debias.rds") {
    $f = Get-Item "citiusdata\data\backtest_traintiers_debias.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) {
      "=== ARM DONE $(Get-Date) (attempt $i) ===" | Out-File -Append -Encoding utf8 $LOG
      break
    }
  }
  "attempt $i incomplete; resuming from the per-meet cache" | Out-File -Append -Encoding utf8 $LOG
  Start-Sleep -Seconds 30
}

"--- scoring the marks half $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_traintiers_debias.rds"
$env:CITIUS_GOAL_MARKS_ONLY = "1"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "TRAINTIERS+DEBIAS DONE"
