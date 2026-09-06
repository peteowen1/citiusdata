# ARM: race-shock EXCESS strip with fitted persistence (Pete's design, 2026-09-06).
#
# calibration_race_eb_perevent_persist.rds = the deployed calibration with the
# EB-shrunk race effects and $race_shock (expected effect per event x tier x
# round cell; beta by tier class: top 0.53, high 0.72, mid 0.86, low 1.02).
# CITIUS_BT_ADJUST_RACE=1 now strips (1 - beta) * (c_r - expected) from each
# historical mark instead of the whole effect minus a top-final reference.
# No add-back.
#
# MARKS ONLY: the strip moves ability levels (a level change per athlete), so
# marks MAE is the metric; the analytic median is exact for this. Placings are
# not scored here -- a full-sim arm follows if marks win.
#
# Compare to backtest_ctrl_tierfix.rds (18 of 54 events beat last-5 on marks;
# per-family MAE sprint 1.532, hurdles 1.590, jump 2.310, throw 3.221,
# middle 1.420, distance 1.719, road 3.063).
#
#   schtasks /create /tn citius_excess_strip /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_excess_strip_arm.ps1"
#   schtasks /run /tn citius_excess_strip
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\excess_strip_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_race_eb_perevent_persist.rds"
$env:CITIUS_BT_ADJUST_RACE    = "1"
$env:CITIUS_BT_MARKS_ONLY     = "1"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_excess_strip"
$env:CITIUS_BT_OUT            = "backtest_excess_strip.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "2"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
foreach ($v in "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
              "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
              "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_excess_strip.rds") {
    $f = Get-Item "citiusdata\data\backtest_excess_strip.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM        = "backtest_excess_strip.rds"
$env:CITIUS_GOAL_MARKS_ONLY = "1"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_excess_strip.csv" -Force
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
$env:CITIUS_BIAS_ARM = "backtest_excess_strip.rds"
& Rscript "citiusdata\scripts\diagnostics\bias_by_context.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
