# Run the project_tier arm A/B: control vs tier@0.5 (and a shrink sweep if the
# first two land cleanly). Chunked invocations rather than one long process --
# backtest_athletics.R is cache-resumable per meet, and a background R process
# died silently in this environment on 2026-08-30 and cost hours. Frequent
# checkpoints mean a death loses one chunk, not a run.
#
# Every arm MUST use its own CITIUS_BT_CACHE and CITIUS_BT_OUT. The script's own
# _arm.rds fingerprint aborts if a cache is reused across arms, which is the
# guard against reading the control's cached meets back as the tier arm's.
#
# Usage:  pwsh _run_tier_arms.ps1
$ErrorActionPreference = "Stop"
Set-Location "C:\dev\citiusverse"

# MEASURED 2026-09-01: a 3-meet invocation took 222s wall, of which the timing
# block accounted for only 32s of actual work -- so startup (package load, store
# read, 1.4M history rows) is ~190s and is paid ONCE per invocation. Small
# chunks would spend most of the run restarting.
#
# Large chunks are safe ONLY ON THE SERIAL PATH, and the original wording of
# this comment did not say so. Serial (backtest_athletics.R:946-948) writes one
# .rds per meet inside the loop, so a death loses the meet in flight. PARALLEL
# (:921) runs the whole parLapply to completion and only then writes the cache
# at :925-927 -- nothing is checkpointed until every meet is done, so a death
# loses the entire chunk, and the cached-file count stays flat throughout and is
# NOT a progress signal. If CITIUS_BT_WORKERS > 1, size the chunk to the loss
# you can afford to repeat, and read liveness from the process list instead.
#
# Startup also scales with workers: each PSOCK worker pays its own
# devtools::load_all(). MEASURED 2026-09-01: ~190s at 4 workers, ~9 min at 12.
$MEETS_PER_CHUNK = 200
$MAX_CHUNKS      = 4
$HB              = "C:\dev\citiusverse\citiusdata\data\tier_arm_heartbeat_tmp.txt"

# name, cache dir, output file, tier shrink ("" = off), round shrink ("" = off)
$arms = @(
  @{ n = "ctrl";   cache = "backtest_cache_tierctrl"; out = "backtest_tierctrl.rds"; tier = "";    round = "" },
  @{ n = "tier05"; cache = "backtest_cache_tier05";   out = "backtest_tier05.rds";   tier = "0.5"; round = "" }
)

# MATCH THE DEPLOYED CONFIG. Both of these are non-default and both were caught
# by a first run that had to be killed (2026-09-01):
#
#  * CALIBRATION defaults to calibration_corpus.rds, but DEPLOYED$calibration is
#    calibration_corpus_csigma_coast.rds (_deployed.R:70) -- which is also what
#    backtest_ctrl_now.rds's meta records. Leaving the default would have scored
#    an arm of a model nobody runs, and the log said so only as a passing
#    "carries no provenance stamp" warning.
#  * USE_STORE is `dir.exists(STORE) && (identical(HISTORY, OUTCOMES) ||
#    nzchar(CITIUS_BT_STORE))`. HISTORY and OUTCOMES differ by default, so the
#    store is silently bypassed unless CITIUS_BT_STORE is set explicitly --
#    "No parquet store; filtering the in-memory corpus" in the log. The control
#    ran with history_source = store, so an unset store is both slower AND a
#    different arm.
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE       = "athletics_corpus_store"

foreach ($a in $arms) {
  Write-Host "=== ARM $($a.n) ==="
  $env:CITIUS_BT_CACHE = $a.cache
  $env:CITIUS_BT_OUT   = $a.out
  $env:CITIUS_BT_MEETS = "$MEETS_PER_CHUNK"
  if ($a.tier -ne "")  { $env:CITIUS_BT_PROJECT_TIER  = $a.tier }  else { Remove-Item Env:\CITIUS_BT_PROJECT_TIER  -ErrorAction SilentlyContinue }
  if ($a.round -ne "") { $env:CITIUS_BT_PROJECT_ROUND = $a.round } else { Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue }

  $cacheDir = "C:\dev\citiusverse\citiusdata\data\$($a.cache)"
  $prev = -1
  for ($i = 1; $i -le $MAX_CHUNKS; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
      Out-File -Append -Encoding utf8 "C:\dev\citiusverse\citiusdata\tier_arm_log_tmp.txt"
    $n = 0
    if (Test-Path $cacheDir) { $n = (Get-ChildItem $cacheDir -Filter *.rds | Measure-Object).Count }
    $stamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    "HEARTBEAT $stamp | arm=$($a.n) | chunk=$i | cached=$n" | Out-File -Append -Encoding utf8 $HB
    Write-Host "  chunk $i : cached=$n"
    # No new meets cached means the pool is exhausted for this arm.
    if ($n -eq $prev) { Write-Host "  arm $($a.n) complete ($n meets)"; break }
    $prev = $n
  }
}

"DONE $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" | Out-File -Append -Encoding utf8 $HB
Write-Host "ALL ARMS DONE"
