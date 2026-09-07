# CONFIRMING ARM: the deployed model as it now stands -- race-shock strip ON,
# family debias OFF -- on the standard apparatus.
#
# WHY. The debias was disabled 2026-09-07 13:30 on the marks lab, which is
# gate-verified exact against estimate_ability() but scores a 2025-2026 window
# of 39 events. The launch gate is the 2020+ T1_elite set of 54 events scored
# by score_goal_by_event.R. This run puts the decision on that apparatus.
#
# Read against:
#   backtest_ctrl_tierfix.rds        18 marks / 40 logloss / 16 both (no strip, no debias)
#   backtest_strip_fullsim.rds       18 / 38 / 15 (strip + debias, the config
#                                    that was deployed between 05:15 and 13:30)
# The question is whether dropping the debias recovers marks events on the
# 2020+ set the way it does on 2025+.
#
# MARKS ONLY: the debias provably cannot move a probability, and the strip's
# medal effect is already measured by backtest_strip_fullsim.rds. So this is
# the analytic-median path, ~1h rather than ~2.5h.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\confirm_nodebias_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904_full2.rds"
$env:CITIUS_BT_ADJUST_RACE    = "1"
$env:CITIUS_BT_MARKS_ONLY     = "1"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_confirm_nodebias"
$env:CITIUS_BT_OUT            = "backtest_confirm_nodebias.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "2"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
# The debias is OFF: this is the whole point of the arm.
foreach ($v in "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS",
              "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
              "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_confirm_nodebias.rds") {
    $f = Get-Item "citiusdata\data\backtest_confirm_nodebias.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM        = "backtest_confirm_nodebias.rds"
$env:CITIUS_GOAL_MARKS_ONLY = "1"
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_confirm_nodebias.csv" -Force
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
$env:CITIUS_BIAS_ARM = "backtest_confirm_nodebias.rds"
& Rscript "citiusdata\scripts\diagnostics\bias_by_context.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
