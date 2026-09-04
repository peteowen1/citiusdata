# Shrink sweep for the project_tier() arm: 0.3, 0.7, 1.0 (0.5 already ran as
# tier05 / backtest_tier05.rds, ctrl already ran as backtest_tierctrl.rds).
# Each value is its own arm with its own cache dir and output file, run
# sequentially -- not concurrently -- to avoid oversubscribing the same 12
# workers each arm wants, matching _run_tier05_parallel.ps1's own reasoning.
#
# Env carried over unchanged from _run_tier05_parallel.ps1 / _run_tier_arms.ps1:
# DEPLOYED calibration and the parquet store, both non-default and both
# required for this to be the same arm family as ctrl/tier05.
#
# Usage:  pwsh citiusdata\scripts\_run_tier_shrink_sweep.ps1
# was "Stop": native Rscript stderr chatter (e.g. R-version package warnings)
# gets treated as a terminating error under Stop, killing the job on benign
# output -- confirmed 2026-09-01 via two real backtest kills traced to this.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$MEETS_PER_CHUNK = 300
$MAX_CHUNKS      = 3
$WORKERS         = 12
$HB  = "C:\dev\citiusverse\citiusdata\data\tier_shrink_sweep_heartbeat.txt"
$LOG = "C:\dev\citiusverse\citiusdata\tier_shrink_sweep_log.txt"

$env:CITIUS_BT_CALIBRATION  = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE        = "athletics_corpus_store"
$env:CITIUS_BT_MEETS        = "$MEETS_PER_CHUNK"
$env:CITIUS_BT_WORKERS      = "$WORKERS"
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$shrinks = @("0.3", "0.7", "1.0")

foreach ($s in $shrinks) {
  $tag = "tier" + ($s -replace '\.', '')
  $env:CITIUS_BT_PROJECT_TIER = "$s"
  $env:CITIUS_BT_CACHE        = "backtest_cache_$tag"
  $env:CITIUS_BT_OUT          = "backtest_$tag.rds"
  $cacheDir = "C:\dev\citiusverse\citiusdata\data\backtest_cache_$tag"

  "START $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss')) shrink=$s tag=$tag workers=$WORKERS" |
    Out-File -Append -Encoding utf8 $HB
  Write-Host "=== ARM $tag (shrink=$s) ==="

  $prev = -1
  for ($i = 1; $i -le $MAX_CHUNKS; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 |
      Out-File -Append -Encoding utf8 $LOG
    $n = 0
    if (Test-Path $cacheDir) {
      $n = (Get-ChildItem $cacheDir -Filter *.rds |
              Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
    }
    $stamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    "HEARTBEAT $stamp | tag=$tag | chunk=$i | cached=$n" | Out-File -Append -Encoding utf8 $HB
    Write-Host "  chunk $i : cached=$n"
    if ($n -eq $prev) { Write-Host "  $tag complete ($n meets)"; break }
    $prev = $n
  }
}

"DONE $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" | Out-File -Append -Encoding utf8 $HB
Write-Host "SWEEP DONE"
