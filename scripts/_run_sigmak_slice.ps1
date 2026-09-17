# One resumable SLICE of the per-event sigma-scale k arm.
#
# WHAT IS BEING TESTED. k = median(sigma_raw / sigma_rob) is fitted per EVENT
# by default now (pooled across a meet's events); this tests fitting it per
# event instead. Measured 2026-09-18: within-event spread over six years 1.94%,
# between-event 19.75% -- a 10x separation, so a meet contesting 800m and 5000m
# currently applies ONE blended k to both, wrong in opposite directions. This is
# an ACCURACY question, unrelated to altitude and independent of it.
#
# BOTH ARMS USE TODAY'S DEPLOYED CALIBRATION (the banded, road-excluded
# altitude one, promoted 2026-09-18), not the pre-altitude one. altitude is now
# in production; a control built on the old calibration would be a stale
# baseline for THIS comparison.
#
# WHY SLICES, WHY THE MEMORY WAIT, WHY THE CHUNKED CACHE: see
# _run_altitude_slice.ps1's header -- identical reasoning, not repeated here.
#
#   powershell -NoProfile -File citiusdata\scripts\_run_sigmak_slice.ps1 pooled
#   powershell -NoProfile -File citiusdata\scripts\_run_sigmak_slice.ps1 byevent
param([ValidateSet("pooled","byevent")][string]$Arm = "pooled")

$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$stale = @("CITIUS_BT_SHOCK_ADDBACK","CITIUS_BT_TRAIN_TIERS","CITIUS_BT_FAMILY_DEBIAS",
           "CITIUS_BT_SIGMA_MODE","CITIUS_BT_SIGMA_PARTS","CITIUS_SIGMA_PSEUDO_N",
           "CITIUS_SIGMA_SCALE","CITIUS_BT_COND_CONTEXT","CITIUS_BT_ALT_ADDBACK",
           "CITIUS_BT_SIGMA_K_HOIST")
foreach ($v in $stale) { if (Test-Path "Env:\$v") { Remove-Item "Env:\$v" } }

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad.rds"
if ($Arm -eq "byevent") { $env:CITIUS_SIGMA_K_BY_EVENT = "1" }
else { Remove-Item Env:\CITIUS_SIGMA_K_BY_EVENT -ErrorAction SilentlyContinue }
$env:CITIUS_BT_ADJUST_RACE   = "1"
Remove-Item Env:\CITIUS_BT_MARKS_ONLY -ErrorAction SilentlyContinue  # full sim: placings are the question
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "M1"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_TARGET        = "120"
$env:CITIUS_BT_MEETS         = "150"
$env:CITIUS_BT_WORKERS       = "2"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CACHE         = "bt_cache_sigmak_$Arm"
$env:CITIUS_BT_OUT           = "backtest_sigmak_$Arm.rds"

$t0 = Get-Date
$before = (Get-ChildItem "citiusdata\data\bt_cache_sigmak_$Arm" -ErrorAction SilentlyContinue).Count

$FLOOR = 7500
$WAIT_MAX_SEC = 240
$w0 = Get-Date
while ($true) {
  $avail = [math]::Round((Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue)
  if ($avail -ge $FLOOR) { "window open: $avail MB available, launching"; break }
  if (((Get-Date) - $w0).TotalSeconds -ge $WAIT_MAX_SEC) {
    "no window in $WAIT_MAX_SEC s (last $avail MB, need $FLOOR) -- nothing run, nothing lost. Try again later."
    exit 0
  }
  Start-Sleep -Seconds 15
}

& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
  Select-String -Pattern "remaining|chunking|SKIPPED|meet_tier:|Loop wall|Error|available|floor|wrote|brier" |
  Select-Object -Last 12
$after = (Get-ChildItem "citiusdata\data\bt_cache_sigmak_$Arm" -ErrorAction SilentlyContinue).Count
$mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
"SLICE arm=$Arm  cache $before -> $after files  in $mins min"
$total = 121
if ($after -gt $before) {
  $rate = [math]::Round($mins * 60 / ($after - $before), 1)
  $left = [math]::Max(0, $total - $after)
  "  $rate s/meet; $left meets left, roughly $([math]::Round($left * $rate / 60, 1)) min"
} elseif ($after -ge $total) {
  "  ARM COMPLETE: $($after - 1) meets cached."
}
