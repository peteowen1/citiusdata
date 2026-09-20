# Smoke for CITIUS_BT_SPREAD_SCALE (2026-09-20): the deployed configuration of
# _run_sigma_scale_arm.ps1 on a few meets. Run it TWICE on the same script
# version -- once with a scale, once with "" -- and the second log must say
#   "Ability cache <same key>: N meets already cached"
# The key carries the script's md5, so an old control's cache never matches a
# freshly edited script; sharing is only between arms on one script version.
# Passed 2026-09-20 (1 worker scale 0.8, then 2 workers control: 3 of 3 cached).
# Usage: powershell -NoProfile -File citiusdata\scripts\_smoke_spread_scale.ps1 [workers] [meets] [scale|""]
param([string]$Workers = "1", [string]$Meets = "3", [string]$Scale = "0.8")
Set-Location "C:\dev\citiusverse"
$env:CITIUS_BT_ADJUST_RACE        = "1"
$env:CITIUS_BT_STORE              = "athletics_corpus_store"
$env:CITIUS_BT_TIER               = "M1"
$env:CITIUS_BT_MEET_TIER          = "1"
$env:CITIUS_BT_TARGET             = "60"
$env:CITIUS_BT_MEETS              = $Meets
$env:CITIUS_BT_WORKERS            = $Workers
$env:CITIUS_HALF_LIFE_FAMILY      = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CALIBRATION        = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad.rds"
$env:CITIUS_EVENT_PARAMS          = "event_params.rds"
$env:CITIUS_BT_NEIGHBOUR_COMBINE  = "1"
$env:CITIUS_BT_NEIGHBOUR_COMBINE_EVENTS = "AT-800Metres-M,AT-1500Metres-M,AT-3000Metres-M,AT-5000Metres-M,AT-10000Metres-M"
$env:CITIUS_BT_MIN_FREE_MB        = "4000"
if ($Scale -ne "") { $env:CITIUS_BT_SPREAD_SCALE = $Scale } else { Remove-Item Env:CITIUS_BT_SPREAD_SCALE -ErrorAction SilentlyContinue }
$env:CITIUS_BT_CACHE = "bt_cache_sp_smoke$Scale"
$env:CITIUS_BT_OUT   = "backtest_sp_smoke$Scale.rds"
foreach ($v in "CITIUS_BT_MARKS_ONLY", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_ADJ_MARKS", "CITIUS_BT_SIGMA_SCALE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\spread_smoke_log.txt"
