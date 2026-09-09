# CHUNKED variant of _run_event_params_arm.ps1 -- same test, same two arms,
# same scoring step. The only difference is HOW the ~450-meet backtest runs:
# in small batches instead of one 12GB shot.
#
# WHY THIS EXISTS. The original refuses to start below 12 GB available,
# measured from two OOM kills on 2026-09-07. Built 2026-09-08 after a live
# memory test on THIS machine showed available memory can be far below that
# on a normal evening (SlayTheSpire2, NordVPN and a browser session were
# enough on their own), and a follow-up chunk-size probe (10 meets) drove
# available memory to 3 MB within 90 seconds before being killed by hand --
# still inside the calibration/store LOAD phase, before a single meet had
# been scored. That is the load-bearing fact this script is built around:
# the ~5.85 GB "before any meet runs" floor documented in the original is a
# FIXED cost per Rscript invocation (calibration + corpus store), not
# something that grows with how many meets that invocation processes.
# Chunking cannot shrink that floor -- it can only avoid paying whatever
# ADDITIONAL memory a long single run accumulates on top of it (the classic
# data.table "gc() can't see this" RSS growth this project has hit before).
#
# HOW CHUNKING WORKS, FOR FREE. backtest_athletics.R already caches one .rds
# per meet in CITIUS_BT_CACHE and builds `todo` as the meets NOT yet cached
# (line ~843: `todo <- pool[!file.exists(...)]`). A second invocation with
# the same CITIUS_BT_CACHE resumes rather than restarts. Every invocation
# also rescoring ALL cached meets unconditionally at the end (not just the
# ones it just processed), so the output file after any chunk is a real,
# usable partial result -- never a placeholder.
#
# THE UNTESTED PART. This script has NOT been run end to end. It is safe to
# launch (small chunks, a real memory gate before every one, easy to kill),
# but "safe" is not the same as "measured" -- watch the first few chunks of
# a real run before trusting it unattended overnight.
#
# NOT A SEPARATE ARM DESIGN. Same calibration, same adjust_race, same tier
# filter, same event_params.rds (now carrying half_life/races_half_life/
# trim_tactical/context_scale/peak_gamma -- five parameters, since
# fit_event_params.R was extended 2026-09-08). If you change what the
# non-chunked script tests, mirror it here or the two stop being comparable.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\event_params_arm_chunked_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

# CONSERVATIVE, not the original's 12000. The documented floor is 5.85 GB;
# this leaves real margin above it rather than the razor-thin gap that let
# tonight's live probe hit 3 MB available. Raise this if a watched run still
# gets close.
#
# LOWERED 8000 -> 5500 after fixing the actual memory floor (see
# backtest_athletics.R's NEED_FULL_CORPUS comment): a live 1-meet run with
# the fix in place measured a peak of 3.9GB, not the pre-fix ~12GB this
# script's original margin was sized against. 5500 keeps real headroom above
# that measured number without the old, now-unfounded 8000 padding -- raise
# again if a real 25-meet chunk (not yet measured, only 1 meet) turns out to
# need more.
$MIN_AVAIL_MB   = 5500
$CHUNK_MEETS    = 25      # backtest_athletics.R's own MAX_PER_RUN default
$MAX_CHUNKS     = 30      # generous margin above ceil(450/25) = 18
$WAIT_SECS      = 120     # between memory-gate retries
$MAX_WAITS      = 60      # 60 * 120s = up to 2h of waiting per gate check

function Wait-ForMemory {
  param([string]$label)
  for ($w = 1; $w -le $MAX_WAITS; $w++) {
    $avail = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
    if ($avail -ge $MIN_AVAIL_MB) {
      "$label -- $avail MB available (>= $MIN_AVAIL_MB), proceeding" | Out-File -Append -Encoding utf8 $LOG
      return $true
    }
    "$label -- only $avail MB available (need $MIN_AVAIL_MB), waiting ${WAIT_SECS}s ($w/$MAX_WAITS)" |
      Out-File -Append -Encoding utf8 $LOG
    Start-Sleep -Seconds $WAIT_SECS
  }
  "$label -- gave up waiting for memory after $MAX_WAITS attempts" | Out-File -Append -Encoding utf8 $LOG
  return $false
}

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904_full2.rds"
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "$CHUNK_MEETS"
$env:CITIUS_BT_WORKERS       = "1"
foreach ($v in "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_MARKS_ONLY",
               "CITIUS_MARKS_BLEND") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

$arms = @(
  @{ name = "ctrl";  params = "" },
  @{ name = "event"; params = "event_params.rds" }
)
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_HALF_LIFE = "365"
Remove-Item "Env:\CITIUS_RACES_HALF_LIFE" -ErrorAction SilentlyContinue

foreach ($arm in $arms) {
  "--- arm $($arm.name): event_params '$($arm.params)', chunked $CHUNK_MEETS meets/run $(Get-Date) ---" |
    Out-File -Append -Encoding utf8 $LOG
  if ($arm.params) { $env:CITIUS_EVENT_PARAMS = $arm.params }
  else { Remove-Item "Env:\CITIUS_EVENT_PARAMS" -ErrorAction SilentlyContinue }
  $cacheDir = "citiusdata\data\bt_cache_evparams_$($arm.name)"
  $env:CITIUS_BT_CACHE = "bt_cache_evparams_$($arm.name)"
  $env:CITIUS_BT_OUT   = "backtest_evparams_$($arm.name).rds"
  $out = "citiusdata\data\backtest_evparams_$($arm.name).rds"
  New-Item -ItemType Directory -Force -Path $cacheDir | Out-Null

  $prevCount = -1
  $stalled = 0
  $completionReason = "reached MAX_CHUNKS ($MAX_CHUNKS) -- population may be PARTIAL, check meets-cached count above"
  for ($c = 1; $c -le $MAX_CHUNKS; $c++) {
    if (-not (Wait-ForMemory "arm $($arm.name) chunk $c")) {
      $completionReason = "memory never recovered -- population is PARTIAL"
      break
    }
    "  chunk $c/$MAX_CHUNKS starting $(Get-Date)" | Out-File -Append -Encoding utf8 $LOG
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    $rscriptExit = $LASTEXITCODE

    $nowCount = (Get-ChildItem -Path $cacheDir -Filter "*.rds" -ErrorAction SilentlyContinue).Count
    "  chunk $c done: exit code $rscriptExit, $nowCount meets cached (was $prevCount)" |
      Out-File -Append -Encoding utf8 $LOG
    # A crashed Rscript (OOM, R error) can produce zero new cached meets, the
    # SAME observable shape as genuine completion (nothing left in `todo`).
    # Without checking the exit code, two crashes in a row would trip the
    # stall-detector below and get logged as "population COMPLETE", which is
    # a false positive a human or a later automated step would trust. Found
    # by silent-failure-hunter review, 2026-09-08 -- this runner had never
    # been run end to end at the time.
    if ($rscriptExit -ne 0) {
      $completionReason = "Rscript exited $rscriptExit on chunk $c -- population PARTIAL, NOT a real stall"
      break
    }
    if ($nowCount -eq $prevCount) {
      $stalled++
      if ($stalled -ge 2) {
        $completionReason = "no new meets cached for 2 consecutive clean-exit chunks -- population COMPLETE at $nowCount meets"
        break
      }
    } else {
      $stalled = 0
    }
    $prevCount = $nowCount
  }
  # THE LINE TO CHECK before trusting this arm's numbers. A partial population
  # is still a real, scored result -- backtest_athletics.R rescoring ALL cached
  # meets every invocation makes that safe -- but it is not the SAME result as
  # the non-chunked arm's 450-meet run, and the two are not comparable until
  # both say COMPLETE.
  "  arm $($arm.name) STOPPED: $completionReason" | Out-File -Append -Encoding utf8 $LOG

  if (-not (Test-Path $out)) {
    "!!! arm $($arm.name) produced NO OUTPUT -- not scoring it" | Out-File -Append -Encoding utf8 $LOG
    continue
  }
  "--- scoring $($arm.name) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_GOAL_ARM = "backtest_evparams_$($arm.name).rds"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" `
            "citiusdata\data\goal_by_event_evparams_$($arm.name).csv" -Force
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
