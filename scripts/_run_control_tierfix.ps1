# FRESH CONTROL for the deployed configuration, after the 2026-09-06 tier-weight
# fix (citius@1f27c4b). backtest_wac_trt_0904.rds -- the control every arm has
# been compared to -- was computed while result_weight() still looked up a
# "high" bucket the WAC promotion had deleted (~469k rows weighted +29% too
# heavily). The race-shock arm and the sigma arms run on the FIXED code, so
# comparing them to that file confounds the arm with the fix. This is the same
# config, same code, no arm.
#
# Deliberately NOT setting CITIUS_BT_FAMILY_DEBIAS: it is now in _deployed.R,
# which the backtest does not source, and it cannot move a probability anyway.
# Marks are judged from the gated debias's own measured arm.
#
#   schtasks /create /tn citius_ctrl_tierfix /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_control_tierfix.ps1"
#   schtasks /run /tn citius_ctrl_tierfix
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\ctrl_tierfix_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_ctrl_tierfix"
$env:CITIUS_BT_OUT            = "backtest_ctrl_tierfix.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
# ONE worker: at launch the box had 585 MB available with two arms in flight,
# which is the exact condition that killed the shock arm silently at 240/394.
$env:CITIUS_BT_WORKERS        = "1"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
foreach ($v in "CITIUS_BT_ADJUST_RACE", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_MARKS_ONLY",
              "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SIGMA_MODE",
              "CITIUS_BT_SIGMA_PARTS") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_ctrl_tierfix.rds") {
    $f = Get-Item "citiusdata\data\backtest_ctrl_tierfix.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_ctrl_tierfix.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_ctrl_tierfix.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
