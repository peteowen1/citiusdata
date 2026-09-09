# ARM: context-conditional condition_sd (calibration_corpus_wac_coast_0904_ctxsd.rds
# with CITIUS_BT_COND_CONTEXT=1). Otherwise the deployed configuration.
#
# WHAT IT CAN AND CANNOT SHOW. A shared shock cancels from every pairwise
# comparison, so medal logloss / Brier should be a near-tie against the control
# (backtest_ctrl_tierfix.rds, 18 / 40 / 16); the only channel to placings is
# athlete-specific sensitivity, which scales with it. Marks MAE is a median
# property and cannot move either. This arm is therefore a DO-NO-HARM check on
# the medal metrics; the thing it fixes -- the width of the marks distribution
# in finals -- is judged by pit_coverage_check.R (pit_coverage_log_finals_ctx_on.txt).
#
# If medal logloss moves by more than noise in either direction, sensitivity is
# doing something the shared-shock rule says it should not, and that is a
# finding in its own right.
#
#   schtasks /create /tn citius_cond_context /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_cond_context_arm.ps1"
#   schtasks /run /tn citius_cond_context
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\cond_context_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904_ctxsd.rds"
$env:CITIUS_BT_COND_CONTEXT   = "1"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_cond_context"
$env:CITIUS_BT_OUT            = "backtest_cond_context.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "1"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
foreach ($v in "CITIUS_BT_ADJUST_RACE", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_MARKS_ONLY",
              "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SIGMA_MODE",
              "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N", "CITIUS_SIGMA_SCALE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_cond_context.rds") {
    $f = Get-Item "citiusdata\data\backtest_cond_context.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_cond_context.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_cond_context.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
