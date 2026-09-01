# Run the tier05 arm ONLY, in parallel. The ctrl arm is already complete (278
# cached meets), and _run_tier_arms.ps1 loops ctrl first -- which would pay the
# ~190s startup twice to discover it has nothing left to do.
#
# Env below is copied from _run_tier_arms.ps1 DELIBERATELY, not re-derived. The
# arm fingerprint in backtest_athletics.R compares 29 fields and aborts on any
# mismatch, and a verified stamp diff (2026-09-01) showed ctrl vs tier05 differ
# in exactly one: project_tier. Changing anything here silently confounds the
# A/B, so the two non-default settings carry their original reasons:
#
#  * CITIUS_BT_CALIBRATION -- defaults to calibration_corpus.rds, but DEPLOYED
#    is calibration_corpus_csigma_coast.rds. The default scores a model nobody
#    runs, and says so only as a passing "carries no provenance stamp" warning.
#  * CITIUS_BT_STORE -- USE_STORE requires this to be set explicitly, or the
#    store is silently bypassed. ctrl ran history_source=store, so leaving it
#    unset is both slower AND a different arm.
#
# CITIUS_BT_WORKERS is NOT in the fingerprint, correctly: it changes how fast
# the arm computes, not what it computes. Parallel mode needed the export_vars
# fix (apply_parallel_export_fix.R) -- without it every PSOCK worker dies with
# "object 'TIER_SHRINK' not found".
#
# Usage:  pwsh citiusdata\scripts\_run_tier05_parallel.ps1
# was "Stop": native Rscript stderr chatter (e.g. R-version package warnings)
# gets treated as a terminating error under Stop, killing the job on benign
# output -- confirmed 2026-09-01 via two real backtest kills traced to this.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

# Pool is 278 meets. 300/chunk means chunk 1 takes the lot and chunk 2 just
# confirms exhaustion -- 2 startups rather than the 3 that 200/chunk costs.
$MEETS_PER_CHUNK = 300
$MAX_CHUNKS      = 3
$WORKERS         = 12
$HB  = "C:\dev\citiusverse\citiusdata\data\tier05_heartbeat.txt"
$LOG = "C:\dev\citiusverse\citiusdata\tier05_parallel_log.txt"

$env:CITIUS_BT_CALIBRATION  = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE        = "athletics_corpus_store"
$env:CITIUS_BT_CACHE        = "backtest_cache_tier05"
$env:CITIUS_BT_OUT          = "backtest_tier05.rds"
$env:CITIUS_BT_MEETS        = "$MEETS_PER_CHUNK"
$env:CITIUS_BT_PROJECT_TIER = "0.5"
$env:CITIUS_BT_WORKERS      = "$WORKERS"
# The ctrl arm ran with no round projection; an inherited value from a previous
# shell would be a second differing field and confound the comparison.
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$cacheDir = "C:\dev\citiusverse\citiusdata\data\backtest_cache_tier05"
"START $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss')) workers=$WORKERS" |
  Out-File -Append -Encoding utf8 $HB

$prev = -1
for ($i = 1; $i -le $MAX_CHUNKS; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
    Out-File -Append -Encoding utf8 $LOG
  $n = 0
  if (Test-Path $cacheDir) {
    # _arm.rds is a stamp, not a meet -- counting it reports 1 meet on an empty
    # cache and makes the "no progress" test fire a chunk late.
    $n = (Get-ChildItem $cacheDir -Filter *.rds |
            Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
  }
  $stamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
  "HEARTBEAT $stamp | chunk=$i | cached=$n" | Out-File -Append -Encoding utf8 $HB
  Write-Host "  chunk $i : cached=$n"
  if ($n -eq $prev) { Write-Host "  tier05 complete ($n meets)"; break }
  $prev = $n
}

"DONE $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" | Out-File -Append -Encoding utf8 $HB
Write-Host "TIER05 DONE"
