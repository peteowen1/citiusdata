# One resumable SLICE of an altitude arm, sized to run in the foreground.
#
# WHY SLICES. Background jobs here are killed by the harness low-memory
# watchdog -- it took this arm out twice on 2026-09-17 while another verse's
# 8 GB job was running -- while foreground calls survive. Foreground calls are
# capped at ten minutes, and an arm is ~50. That combination is only workable
# because backtest_athletics.R now writes its per-meet cache after every chunk
# (aa3d961): each slice picks up exactly where the last one stopped, and being
# killed costs at most one chunk.
#
# Run it repeatedly until it reports nothing remaining. Identical env to
# _run_altitude_arm.ps1, deliberately -- the cache fingerprint is checked
# against the arm stamp, so a single differing variable would reject the cache
# and silently restart from zero.
#
#   powershell -NoProfile -File citiusdata\scripts\_run_altitude_slice.ps1 ctrl
#   powershell -NoProfile -File citiusdata\scripts\_run_altitude_slice.ps1 on
param([ValidateSet("ctrl","on")][string]$Arm = "ctrl")

$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$stale = @("CITIUS_BT_SHOCK_ADDBACK","CITIUS_BT_TRAIN_TIERS","CITIUS_BT_FAMILY_DEBIAS",
           "CITIUS_BT_SIGMA_MODE","CITIUS_BT_SIGMA_PARTS","CITIUS_SIGMA_PSEUDO_N",
           "CITIUS_SIGMA_SCALE","CITIUS_BT_COND_CONTEXT")
foreach ($v in $stale) { if (Test-Path "Env:\$v") { Remove-Item "Env:\$v" } }

if ($Arm -eq "ctrl") {
  $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2.rds"
} else {
  $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2_altitude.rds"
}
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "M1"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "150"
$env:CITIUS_BT_WORKERS       = "2"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CACHE         = "bt_cache_alt_$Arm"
$env:CITIUS_BT_OUT           = "backtest_alt_$Arm.rds"

$t0 = Get-Date
$before = (Get-ChildItem "citiusdata\data\bt_cache_alt_$Arm" -ErrorAction SilentlyContinue).Count
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
  Select-String -Pattern "remaining|chunking|SKIPPED|meet_tier:|Loop wall|Error|wrote|brier" |
  Select-Object -Last 10
$after = (Get-ChildItem "citiusdata\data\bt_cache_alt_$Arm" -ErrorAction SilentlyContinue).Count
$mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
"SLICE arm=$Arm  cache $before -> $after files  in $mins min"
if ($after -gt $before) {
  $rate = [math]::Round($mins * 60 / ($after - $before), 1)
  "  $rate s/meet; $([math]::Max(0, 151 - $after)) meets left, roughly $([math]::Round(($ (151 - $after)) * $rate / 60, 1)) min"
}
