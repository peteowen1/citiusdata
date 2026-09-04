# The goal's training design (Pete, 2026-09-05): train on T1+T2 only, score on
# T1. Full simulation -- NOT marks-only -- because the goal needs medal
# logloss, and N_SIMS stays at the 10,000 default so this arm is directly
# comparable to backtest_wac_trt_0904.rds, which ran at 10,000. Screening at
# 2,500 would inflate this arm's logloss by Monte Carlo noise alone and bias
# the comparison against it.
#
# Retry loop: the per-meet cache makes a kill cost one meet, so relaunching is
# safe and this box has killed long jobs repeatedly under memory pressure.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\traintiers_arm_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "bt_cache_traintiers_t1t2"
$env:CITIUS_BT_OUT           = "backtest_traintiers_t1t2.rds"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_TRAIN_TIERS   = "T1_elite,T2_strong"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_MARKS_ONLY -ErrorAction SilentlyContinue

for ($i = 1; $i -le 6; $i++) {
  "--- attempt $i $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_traintiers_t1t2.rds") {
    $f = Get-Item "citiusdata\data\backtest_traintiers_t1t2.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) {
      "=== ARM DONE $(Get-Date) (attempt $i) ===" | Out-File -Append -Encoding utf8 $LOG
      break
    }
  }
  "attempt $i incomplete; resuming from the per-meet cache" | Out-File -Append -Encoding utf8 $LOG
  Start-Sleep -Seconds 30
}

# Score it against last-5 on the same test set, so the only thing that differs
# from the 16-of-54 result is the training restriction.
"--- scoring the arm $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_traintiers_t1t2.rds"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "TRAINTIERS ARM DONE"
