# ARM: the race-shock EXCESS strip, family-gated, FULL SIMULATION -- the medal read.
#
# calibration_corpus_wac_coast_0904_full2.rds = the deployed variance
# calibration (context condition_sd, spread scales, sigma_marks) plus the
# EB-shrunk race table and $race_shock with per-race beta (tier, family, PB
# share, wind, size of the excess) gated to sprint / hurdles / jump / throw.
# CITIUS_BT_ADJUST_RACE=1 strips (1 - beta_race) * excess from history in
# those families; CITIUS_BT_COND_CONTEXT=1 is the deployed simulation path.
#
# The two marks-only arms (2026-09-07) put the gated strip at sprint -1.4%,
# jump -1.4%, throw -1.5%, hurdles -0.4% marks MAE with endurance untouched.
# A level change per athlete CAN reorder a field, so this is the arm that
# decides adjust_race: medal logloss / Brier vs backtest_ctrl_tierfix.rds
# (18 / 40 / 16), read per event with score_goal_by_event.R.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\strip_fullsim_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904_full2.rds"
$env:CITIUS_BT_ADJUST_RACE    = "1"
$env:CITIUS_BT_COND_CONTEXT   = "1"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_strip_fullsim"
$env:CITIUS_BT_OUT            = "backtest_strip_fullsim.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "2"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
foreach ($v in "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_MARKS_ONLY", "CITIUS_BT_TRAIN_TIERS",
              "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS",
              "CITIUS_SIGMA_PSEUDO_N", "CITIUS_SIGMA_SCALE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_strip_fullsim.rds") {
    $f = Get-Item "citiusdata\data\backtest_strip_fullsim.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_strip_fullsim.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_strip_fullsim.csv" -Force
$env:CITIUS_BIAS_ARM = "backtest_strip_fullsim.rds"
& Rscript "citiusdata\scripts\diagnostics\bias_by_context.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
