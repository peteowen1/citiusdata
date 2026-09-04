# project_tier + family-pool debias, scored on the goal's marks half.
#
# MARKS_ONLY is legitimate here and nowhere else: both levers are proven
# bit-for-bit incapable of moving a probability (p_medal identical whether on
# or off), so the medal-logloss column from the full-sim arm still applies and
# re-simulating it would burn hours reproducing identical numbers.
#
# Offsets were refitted on backtest_wac_trt_0904.rds over [2016, 2018), which
# does not overlap the 2020+ test set -- no leakage.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\debias_marks_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_debias_marks"
$env:CITIUS_BT_OUT           = "backtest_debias_marks.rds"
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
  if (Test-Path "citiusdata\data\backtest_debias_marks.rds") {
    $f = Get-Item "citiusdata\data\backtest_debias_marks.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) {
      "=== ARM DONE $(Get-Date) (attempt $i) ===" | Out-File -Append -Encoding utf8 $LOG
      break
    }
  }
  "attempt $i incomplete; resuming from the per-meet cache" | Out-File -Append -Encoding utf8 $LOG
  Start-Sleep -Seconds 30
}

"--- scoring the marks half $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_debias_marks.rds"
$env:CITIUS_GOAL_MARKS_ONLY = "1"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "DEBIAS MARKS DONE"
