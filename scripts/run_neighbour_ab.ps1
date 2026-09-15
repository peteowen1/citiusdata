# Run the cross-event A/B (combine_neighbour_ability) unattended, waiting for
# memory rather than dying on it.
#
# WHY A RUNNER. Three separate things have each cost a run here:
#
#   1. MEMORY CONTENTION. The backtest needs ~8 GB and refuses to start below
#      CITIUS_BT_MIN_FREE_MB (9,000). On 2026-09-15 a pannaverse job was holding
#      12.9 GB and the probe aborted in 3s. The floor is CORRECT -- being
#      OOM-killed half-way is worse -- but "abort" is the wrong response to a
#      transient dip, exactly as it was for the overnight backfill. This WAITS.
#      It never kills anything: the processes holding the memory belong to other
#      verses and other sessions.
#   2. RESUMPTION. backtest_athletics.R scores CITIUS_BT_MEETS (default 25) per
#      invocation and caches each meet, so it must be run repeatedly until it
#      reports nothing remaining. That loop is here rather than in a human's
#      hands.
#   3. COMPARABILITY. An A/B is a measurement of the mechanism only if the other
#      51 resolved settings are identical. compare_arm_fingerprints.R asserts
#      that from the _arm.rds each arm writes, and this refuses to report a
#      result if it fails.
#
# Caches are FRESH and dated. Every input moved on 2026-09-15 -- the corpus, the
# catalogue, meet_tier and the stores -- so every pre-existing backtest cache is
# stale. Reusing one would compare today's arm against a control scored on
# different data, which is the "baseline must share the code" failure.
#
# Deliberately NO $ErrorActionPreference = "Stop": with *>&1 a native command's
# stderr arrives as ErrorRecord objects and Stop turns the harmless data.table
# build warning into a terminating error (it killed the catalogue chain earlier
# the same day). $LASTEXITCODE is the real signal.
$ErrorActionPreference = "Continue"

$verse = "C:\dev\citiusverse"
$data  = "$verse\citiusdata\data"
$stamp = Get-Date -Format 'yyyyMMdd'
$log   = "$data\neighbour_ab_$stamp.log"
Set-Location $verse

# --- knobs -------------------------------------------------------------------
$FloorMB      = 9000    # must match CITIUS_BT_MIN_FREE_MB below
$HeadroomMB   = 1000    # start only with room to spare, so we don't race the floor
$PatienceMin  = 240     # give up waiting after this long with no window
$PollSec      = 60
$MeetsPerCall = 25
# Per arm. The pool grew to 900 meets once the 2026-09-15 catalogue rebuild
# widened T1+T2, so an uncapped run is ~15 hours for the pair. Both arms score
# the SAME meets in the same deterministic order, so a cap gives a smaller but
# still valid comparison -- and the equal-count check at the end enforces that
# the two arms actually covered the same ground. Raise it to extend a promising
# result rather than starting wide.
$MaxCalls     = if ($env:AB_MAX_CALLS) { [int]$env:AB_MAX_CALLS } else { 4 }

$CTRL_CACHE = "backtest_cache_ab${stamp}_ctrl"
$ARM_CACHE  = "backtest_cache_ab${stamp}_nbcomb"

# Held equal across both arms. Taken from the 2026-09-14 trial's fingerprint so
# this is a continuation of that comparison, not a new one with drifted settings.
$CALIB  = "calibration_corpus_wac_coast_0904_full2.rds"
# Men's distance events: every one is both a candidate target and a candidate
# neighbour of the others, which is the point -- Ingebrigtsen's 1500m evidence
# should inform his 5000m rating.
$EVENTS = "AT-10000Metres-M,AT-1500Metres-M,AT-3000Metres-M,AT-5000Metres-M,AT-800Metres-M"

function Say($m) {
  $line = "[{0}] {1}" -f (Get-Date -Format 'HH:mm:ss'), $m
  # Out-Null is LOAD-BEARING, not tidiness. A PowerShell function returns
  # everything written to its output stream, and Tee-Object passes its input
  # through -- so without this every Say line became part of Run-Arm's return
  # value. `$okCtrl` was then a non-empty array rather than $false, `-not
  # ($okCtrl -and $okArm)` was false, and on 2026-09-15 this runner printed
  # "AB COMPLETE" after BOTH arms had failed with rc=1 and cached zero meets.
  # A runner whose entire job is refusing to report a bad result reported one.
  $line | Tee-Object -FilePath $log -Append | Out-Null
  Write-Host $line
}

function Get-AvailMB {
  # Available MBytes, NOT FreePhysicalMemory. Free memory excludes the standby
  # list, reads far lower than what a process can actually get, and led to a
  # 3.6h arm being killed on a number that was noise.
  try {
    [int](Get-Counter '\Memory\Available MBytes' -ErrorAction Stop).CounterSamples[0].CookedValue
  } catch { -1 }
}

function Wait-ForMemory($needMB) {
  $deadline = (Get-Date).AddMinutes($PatienceMin)
  $first = $true
  while ($true) {
    $avail = Get-AvailMB
    if ($avail -lt 0) { Say "  cannot read Available MBytes; proceeding and letting the backtest's own floor decide"; return $true }
    if ($avail -ge $needMB) {
      if (-not $first) { Say "  memory available again: $avail MB" }
      return $true
    }
    if ((Get-Date) -gt $deadline) {
      Say "  GAVE UP waiting: $avail MB available, need $needMB MB, waited $PatienceMin min"
      return $false
    }
    if ($first) {
      $top = Get-Process | Sort-Object WorkingSet64 -Descending |
             Select-Object -First 3 |
             ForEach-Object { "{0}({1}) {2}MB" -f $_.Name, $_.Id, [int]($_.WorkingSet64/1MB) }
      Say "  waiting for memory: $avail MB available, need $needMB MB. Holding it: $($top -join ', ')"
      Say "  (not killing anything -- these belong to other sessions)"
      $first = $false
    }
    Start-Sleep -Seconds $PollSec
  }
}

function Run-Arm($label, $cache, [bool]$combineOn) {
  Say "=== ARM: $label (cache $cache) ==="
  # backtest_athletics.R prints the remaining count BEFORE it scores, so a run
  # that finishes the panel still reports a non-zero number and only the NEXT
  # call reports 0. Tracking the cached-meet count as well means we stop as soon
  # as a call adds nothing, instead of paying a full corpus load to be told.
  $prevN = -1
  for ($i = 1; $i -le $MaxCalls; $i++) {
    if (-not (Wait-ForMemory ($FloorMB + $HeadroomMB))) {
      Say "  $label stopped at call $i for lack of memory; cached meets are kept and it resumes."
      return $false
    }

    # Set EVERY variable every time. A leaked value from the previous arm is the
    # whole reason the fingerprint check exists; not leaking it is cheaper.
    $env:CITIUS_BT_CACHE       = $cache
    $env:CITIUS_BT_MEETS       = "$MeetsPerCall"
    $env:CITIUS_BT_CALIBRATION = $CALIB
    $env:CITIUS_BT_MIN_FREE_MB = "$FloorMB"
    if ($combineOn) {
      $env:CITIUS_BT_NEIGHBOUR_COMBINE        = "1"
      $env:CITIUS_BT_NEIGHBOUR_COMBINE_EVENTS = $EVENTS
    } else {
      $env:CITIUS_BT_NEIGHBOUR_COMBINE        = ""
      $env:CITIUS_BT_NEIGHBOUR_COMBINE_EVENTS = ""
    }
    # Never both mechanisms at once; the script aborts if they are, but be explicit.
    $env:CITIUS_BT_NEIGHBOUR_TRANSFER = ""

    $t0 = Get-Date
    $out = & Rscript "citiusdata/scripts/backtest_athletics.R" *>&1
    $rc  = $LASTEXITCODE
    $out | Tee-Object -FilePath $log -Append | Out-Null
    $secs = [int]((Get-Date) - $t0).TotalSeconds

    $remain = ($out | Select-String -Pattern '(\d+) of (\d+) meets? remaining' |
               Select-Object -Last 1)
    $left = if ($remain) { [int]$remain.Matches[0].Groups[1].Value } else { -1 }
    $n = (Get-ChildItem "$data\$cache" -Filter '*.rds' -ErrorAction SilentlyContinue |
          Where-Object { $_.Name -ne '_arm.rds' }).Count
    # ${i} not $i -- "$i:" parses as a drive-qualified variable reference.
    Say "  call ${i}: rc=$rc in ${secs}s, $n meet(s) cached, $left remaining"

    if ($rc -ne 0) {
      # An out-of-memory abort is a wait-and-retry, not a failure. Anything else
      # is a real error and retrying it just burns the call budget.
      if ($out -match 'available; this backtest needs') {
        Say "  hit the memory floor mid-run; will wait and retry"
        continue
      }
      Say "  $label FAILED with rc=$rc -- stopping this arm. See $log"
      return $false
    }
    if ($left -eq 0) { Say "  $label COMPLETE: $n meets cached"; return $true }
    if ($n -eq $prevN) {
      # A clean exit that cached nothing new means the panel is done (or nothing
      # in it can be scored). Either way, looping again cannot help.
      Say "  $label COMPLETE: $n meets cached (a call added nothing new)"
      return $true
    }
    $prevN = $n
    if ($left -lt 0) { Say "  could not parse a remaining-count; stopping rather than looping blind"; return $false }
  }
  # Hitting the cap is a DELIBERATE partial run, not a failure: the pool is 900
  # meets and a full pass is ~15h for the pair. Both arms walk the same
  # deterministic order, and the equal-count check after both arms finish is
  # what guarantees they covered the same meets. Raise AB_MAX_CALLS and rerun
  # to extend -- the caches resume rather than restart.
  Say "  $label stopped at the $MaxCalls-call cap ($n meets); raise AB_MAX_CALLS to extend"
  return $true
}

Say "neighbour A/B starting. control=$CTRL_CACHE arm=$ARM_CACHE"
Say "events under test: $EVENTS"

$okCtrl = Run-Arm "CONTROL"  $CTRL_CACHE $false
$okArm  = Run-Arm "COMBINE"  $ARM_CACHE  $true

# Belt and braces: trust the meet COUNTS, not only the booleans. The boolean
# path is what failed silently once already, and a count of zero is the thing
# that actually makes a result meaningless.
$nCtrl = (Get-ChildItem "$data\$CTRL_CACHE" -Filter '*.rds' -ErrorAction SilentlyContinue |
          Where-Object { $_.Name -ne '_arm.rds' }).Count
$nArm  = (Get-ChildItem "$data\$ARM_CACHE"  -Filter '*.rds' -ErrorAction SilentlyContinue |
          Where-Object { $_.Name -ne '_arm.rds' }).Count
Say "cached meets: control=$nCtrl arm=$nArm"

if (-not ($okCtrl -eq $true -and $okArm -eq $true)) {
  Say "AB INCOMPLETE -- control ok=$okCtrl, arm ok=$okArm. Caches are kept; rerun to resume."
  exit 1
}
if ($nCtrl -eq 0 -or $nArm -eq 0) {
  Say "AB INVALID -- an arm cached ZERO meets (control=$nCtrl, arm=$nArm). Nothing was measured."
  exit 1
}
if ($nCtrl -ne $nArm) {
  # Scoring different meets in the two arms compares panels, not mechanisms.
  Say "AB INVALID -- the arms scored DIFFERENT meet counts (control=$nCtrl, arm=$nArm)."
  exit 1
}

Say "=== fingerprint check ==="
& Rscript "citiusdata/scripts/compare_arm_fingerprints.R" "$data\$CTRL_CACHE" "$data\$ARM_CACHE" *>&1 |
  Tee-Object -FilePath $log -Append
if ($LASTEXITCODE -ne 0) {
  Say "FINGERPRINT CHECK FAILED -- the arms are not comparable. Do not report a result from this run."
  exit 1
}

Say "AB COMPLETE. Caches: $CTRL_CACHE, $ARM_CACHE. Log: $(Split-Path $log -Leaf)"
