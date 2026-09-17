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
param([ValidateSet("ctrl","on","noroad")][string]$Arm = "ctrl")

$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$stale = @("CITIUS_BT_SHOCK_ADDBACK","CITIUS_BT_TRAIN_TIERS","CITIUS_BT_FAMILY_DEBIAS",
           "CITIUS_BT_SIGMA_MODE","CITIUS_BT_SIGMA_PARTS","CITIUS_SIGMA_PSEUDO_N",
           "CITIUS_SIGMA_SCALE","CITIUS_BT_COND_CONTEXT")
foreach ($v in $stale) { if (Test-Path "Env:\$v") { Remove-Item "Env:\$v" } }

# noroad = the altitude calibration with road's beta zeroed. Road is excluded
# because its REGRESSOR is invalid for the family -- alt_m is the venue city's
# point elevation and a road course climbs and descends away from it -- not
# because its coefficient is small. It is the strongest coefficient in the fit
# (t = -17.7) and the one family the 2026-09-17 arm showed significantly worse
# out of sample. See docs/reviews/altitude-arm-2026-09-17.md.
#
# The CONTROL IS REUSED across both altitude arms: its calibration is the
# deployed one, unchanged, so bt_cache_alt_ctrl is valid for this comparison
# too and does not need re-running. That is only safe because the pool target
# is pinned below -- a different CITIUS_BT_TARGET would select different meets.
switch ($Arm) {
  "ctrl"   { $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2.rds" }
  "on"     { $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2_altitude.rds" }
  "noroad" { $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2_altitude_noroad.rds" }
}
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "M1"
$env:CITIUS_BT_MEET_TIER     = "1"
# POOL SIZE. Not the same thing as CITIUS_BT_MEETS, which is meets per
# INVOCATION. Unset, CITIUS_BT_TARGET defaults to 900, which backtest_athletics.R
# itself prices at ~7.5 hours per arm -- and an A/B is two of those. This ran
# unset for hours on 2026-09-17 while the logs said "150", because 150 was the
# per-run cap and the cache quietly passed 151 meets.
#
# BOTH ARMS MUST SHARE THIS NUMBER. The pool is an evenly spaced sample of the
# meet list, so a different target selects DIFFERENT MEETS and the arms stop
# being comparable -- which score_arm.R's vintage guard does NOT catch, because
# the history is identical either way. 120 is the documented sensible size:
# ~1,500 finals, and about 36 min per arm at the 18s/meet measured today.
$env:CITIUS_BT_TARGET        = "120"
$env:CITIUS_BT_MEETS         = "150"
$env:CITIUS_BT_WORKERS       = "2"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CACHE         = "bt_cache_alt_$Arm"
$env:CITIUS_BT_OUT           = "backtest_alt_$Arm.rds"

$t0 = Get-Date
$before = (Get-ChildItem "citiusdata\data\bt_cache_alt_$Arm" -ErrorAction SilentlyContinue).Count

# WAIT FOR A WINDOW BEFORE LAUNCHING. Available memory swings by gigabytes while
# another verse's job iterates -- measured 948 MB to 9,817 MB within minutes on
# 2026-09-17 -- so a hand check can pass and R's own check fail seconds later.
# That happened twice. Poll here instead, and require headroom over the floor so
# the two checks agree.
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

# Do NOT over-filter: the reason a slice refused to start is the single most
# useful line it can print, and an earlier version of this filter swallowed it
# and reported a bare "Error:".
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
  Select-String -Pattern "remaining|chunking|SKIPPED|meet_tier:|Loop wall|Error|available|floor|wrote|brier" |
  Select-Object -Last 12
$after = (Get-ChildItem "citiusdata\data\bt_cache_alt_$Arm" -ErrorAction SilentlyContinue).Count
$mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
"SLICE arm=$Arm  cache $before -> $after files  in $mins min"
if ($after -gt $before) {
  $rate = [math]::Round($mins * 60 / ($after - $before), 1)
  "  $rate s/meet; $([math]::Max(0, 151 - $after)) meets left, roughly $([math]::Round(($ (151 - $after)) * $rate / 60, 1)) min"
}
