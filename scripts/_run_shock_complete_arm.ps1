# The COMPLETE race-shock fix, relaunched after the meet_tier collision fix.
#   strip    -- calibration_race_eb_perevent.rds (evidence-weighted shrinkage)
#   add back -- expected_race_shock.csv (T1 final +1.51%, by tier x round)
#
# FRESH CACHE (bt_cache_shock_v3). The first launch ran with CITIUS_BT_MEET_TIER=1
# silently degraded to the feed tier by a meet_tier.x/.y merge collision. The arm
# fingerprint records use_meet_tier=1, which was ALSO 1 during the broken run --
# the behaviour changed but the fingerprint did not, so reusing the old cache
# would have blended 4 feed-tier meets into a catalogue-tier arm undetectably.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\shock_complete_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_race_eb_perevent.rds"
$env:CITIUS_BT_ADJUST_RACE    = "1"
$env:CITIUS_BT_SHOCK_ADDBACK  = "expected_race_shock.csv"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_shock_v3"
$env:CITIUS_BT_OUT            = "backtest_shock_complete.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "1"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_MARKS_ONLY -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_TRAIN_TIERS -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_FAMILY_DEBIAS -ErrorAction SilentlyContinue
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_shock_complete.rds") {
    $f = Get-Item "citiusdata\data\backtest_shock_complete.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_shock_complete.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_shock_complete.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "SHOCK COMPLETE DONE"
