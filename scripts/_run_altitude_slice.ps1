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
param([ValidateSet("ctrl","on","noroad","midonly")][string]$Arm = "ctrl",
      [switch]$Placings,
      [switch]$AddBack)

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
  # midonly zeroes EIGHT families to isolate the one with a measured marks gain:
  # middle, -0.0283pp at t = -5.01. Those eight are excluded to isolate, NOT
  # because their regressors are invalid the way road's is. The noroad arm
  # improved marks and did not improve concordance (74.4484% -> 74.3762%,
  # p = 0.26, 610 of 780 races identical), so the open question is whether the
  # middle gain survives on its own or was part of the same level-only shift.
  "midonly" { $env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2_altitude_midonly.rds" }
}
# The COMPLETE altitude design: history subtraction (from the calibration) plus
# the target-venue add-back. Shipping only the first half is a known defect --
# ability is made altitude-neutral and nothing puts the venue back, so a race AT
# altitude is predicted from sea-level-neutral ability. The three arms measured
# on 2026-09-17 all ran the half-design.
if ($AddBack) { $env:CITIUS_BT_ALT_ADDBACK = "1" }
else { Remove-Item Env:\CITIUS_BT_ALT_ADDBACK -ErrorAction SilentlyContinue }
$env:CITIUS_BT_ADJUST_RACE   = "1"
# MARKS_ONLY off for a placings arm. The two share an ABILITY CACHE -- marks_only
# is in backtest_athletics.R's ABIL_EXCLUDE list precisely because it cannot
# change an ability -- so a placings arm reuses whatever the marks arm already
# computed for the same calibration and pays only for the simulation.
#
# The cache is also why placings need asking at all. "Altitude is a shared
# whole-field shock so it cannot reorder a field" is TRUE of a shock applied to
# the race being predicted and FALSE here: this term corrects an athlete's
# HISTORY, moving each athlete's ability by an amount that depends on their own
# altitude exposure, so two athletes in one final get different corrections.
if ($Placings) {
  Remove-Item Env:\CITIUS_BT_MARKS_ONLY -ErrorAction SilentlyContinue
} else {
  $env:CITIUS_BT_MARKS_ONLY  = "1"
}
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
# Separate backtest cache per MODE as well as per arm: a marks-only cache holds
# no placings, so reading it back for a placings arm would score NA gold/medal
# and report a dead heat. The ability cache is deliberately shared; this one is
# deliberately not.
$suffix = if ($Placings) { "$($Arm)_sim" } else { $Arm }
if ($AddBack) { $suffix = "$($suffix)_ab" }
$env:CITIUS_BT_CACHE         = "bt_cache_alt_$suffix"
$env:CITIUS_BT_OUT           = "backtest_alt_$suffix.rds"

$t0 = Get-Date
$before = (Get-ChildItem "citiusdata\data\bt_cache_alt_$suffix" -ErrorAction SilentlyContinue).Count

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
  Select-String -Pattern "remaining|chunking|SKIPPED|meet_tier:|Loop wall|Error|available|floor|wrote|brier|add-back" |
  Select-Object -Last 12
$after = (Get-ChildItem "citiusdata\data\bt_cache_alt_$suffix" -ErrorAction SilentlyContinue).Count
$mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
"SLICE arm=$suffix  cache $before -> $after files  in $mins min"
# The target is 120 meets plus one _arm.rds stamp. This said 151 for a while --
# left over from when CITIUS_BT_MEETS=150 was mistaken for the pool size -- and
# a stray `$ (` in the interpolation made every slice print a PowerShell error
# after its result. Cosmetic, but a runner that errors on every successful run
# teaches you to ignore its output, which is how a real error gets missed.
$total = [int]$env:CITIUS_BT_TARGET + 1
if ($after -gt $before) {
  $rate = [math]::Round($mins * 60 / ($after - $before), 1)
  $left = [math]::Max(0, $total - $after)
  $eta  = [math]::Round($left * $rate / 60, 1)
  "  $rate s/meet; $left meets left, roughly $eta min"
} elseif ($after -ge $total) {
  "  ARM COMPLETE: $($after - 1) meets cached."
}
