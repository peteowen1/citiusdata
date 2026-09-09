# ARM: two-sided sigma estimator (sigma_raw, SIGMA_PARTS=weight) with pseudo-n 40.
#
# WHY THESE TWO TOGETHER. sigma_estimator_shootout.R (2026-09-06): the deployed
# one-sided sigma_rob ranks athletes by hold-out consistency at Spearman 0.07
# and scores 0.92 on Gaussian log score after a per-event level fit; the
# two-sided sd scores 2.10 and, shrunk toward the event with pseudo-n 40,
# 2.24 -- tying the event constant while keeping a weak real signal (0.13).
# PIT coverage (pit_coverage_log_sg_*.txt) shows neither knob moves the WIDTH
# of the simulated spread (cover50 0.626 -> 0.630 for pseudo-n 40), so this arm
# is a pure allocation test: does giving the per-athlete spread to the right
# people improve medal logloss / Brier? Marks MAE cannot move (sigma is a
# spread parameter) and is reported only as a sanity check.
#
# The refuted event-constant arm (pre-08-11, gold Brier +2.93%) says the
# per-athlete term carries something that wins races. If this arm ALSO loses,
# that something is upside, not consistency, and the next move is
# decouple_peak -- not another sigma knob.
#
# Compare to backtest_ctrl_tierfix.rds (18 / 40 / 16).
#
#   schtasks /create /tn citius_sigma_raw_pn40 /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_sigma_raw_pn40_arm.ps1"
#   schtasks /run /tn citius_sigma_raw_pn40
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\sigma_raw_pn40_log.txt"
# Wait for the PIT grid to finish (memory), up to 3 hours.
$GRID = "C:\dev\citiusverse\citiusdata\pit_grid_log.txt"
"=== QUEUED $(Get-Date), waiting for the PIT grid ===" | Out-File -Append -Encoding utf8 $LOG
for ($w = 0; $w -lt 360; $w++) {
  if ((Test-Path $GRID) -and (Select-String -Path $GRID -Pattern "ALL DONE" -Quiet)) { break }
  Start-Sleep -Seconds 30
}
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_sigma_raw_pn40"
$env:CITIUS_BT_OUT            = "backtest_sigma_raw_pn40.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "1"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_SIGMA_PARTS    = "weight"
$env:CITIUS_SIGMA_PSEUDO_N    = "40"
foreach ($v in "CITIUS_BT_ADJUST_RACE", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_MARKS_ONLY",
              "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SIGMA_MODE",
              "CITIUS_SIGMA_SCALE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_sigma_raw_pn40.rds") {
    $f = Get-Item "citiusdata\data\backtest_sigma_raw_pn40.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_sigma_raw_pn40.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_sigma_raw_pn40.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
