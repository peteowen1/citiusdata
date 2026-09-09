# Does removing shared race conditions from history predict better?
#
# The deployed model ignores all 663,716 fitted race effects (adjust_race off),
# so wind/altitude/track/weather sit inside every athlete's ability estimate.
# This arm applies them, restricted to races with >= 8 athletes -- 27.9% of
# them -- because 40% of races have fewer than 5 athletes and a race that small
# cannot separate "the race was fast" from "the athlete was fast".
#
# FULL SIMULATION, not marks-only: this changes ability AND sigma, so it moves
# probabilities. A marks-only read would miss half of what it does.
#
# Scored against the same T1 2020+ set as everything else this session, so it
# is comparable to the 18/42/16 baseline and the 26/42/23 debias result.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\condsd_arm_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_condsd.rds"
Remove-Item Env:CITIUS_BT_ADJUST_RACE -ErrorAction SilentlyContinue
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_condsd"
$env:CITIUS_BT_OUT           = "backtest_condsd.rds"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_MARKS_ONLY -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_TRAIN_TIERS -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_FAMILY_DEBIAS -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_PROJECT_TIER -ErrorAction SilentlyContinue

for ($i = 1; $i -le 6; $i++) {
  "--- attempt $i $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_condsd.rds") {
    $f = Get-Item "citiusdata\data\backtest_condsd.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) {
      "=== ARM DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
      break
    }
  }
  Start-Sleep -Seconds 30
}

"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_condsd.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_condsd.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "RACEFILT ARM DONE"
