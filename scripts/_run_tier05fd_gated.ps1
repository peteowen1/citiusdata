# Re-run of the tier05fd confirmation arm AFTER the apply-date gate fix
# (backtest_athletics.R, 2026-09-01). The previous arm applied the family-pool
# offsets to every meet including ones predating the fit window, so its cache
# holds leakage-contaminated per-meet results.
#
# A FRESH CACHE IS NOT OPTIONAL. The gate is a CODE change, and the arm
# fingerprint can only see env/data, so re-running into backtest_cache_tier05fd
# would read the contaminated meets straight back and print "cached=277" -- a
# completed-looking run with none of the fix in it.
#
# Chunked with a resume loop because a long background Rscript on this machine
# has been killed mid-run before with no explanation; each chunk resumes from
# the cache, so a kill costs one chunk rather than the run.
$ErrorActionPreference = "Stop"
Set-Location "C:\dev\citiusverse"

$MEETS_PER_CHUNK = 300
$MAX_CHUNKS      = 6
$WORKERS         = 12
$HB  = "C:\dev\citiusverse\citiusdata\data\tier05fd2_heartbeat.txt"
$LOG = "C:\dev\citiusverse\citiusdata\tier05fd2_log.txt"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_tier05fd2"
$env:CITIUS_BT_OUT           = "backtest_tier05fd2.rds"
$env:CITIUS_BT_MEETS         = "$MEETS_PER_CHUNK"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_WORKERS       = "$WORKERS"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_TARGET -ErrorAction SilentlyContinue

$cacheDir = "C:\dev\citiusverse\citiusdata\data\backtest_cache_tier05fd2"
"START $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" | Out-File -Append -Encoding utf8 $HB

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
  "HEARTBEAT $stamp | chunk=$i | cached=$n" | Out-File -Append -Encoding utf8 $HB
  Write-Host "  chunk $i : cached=$n"
  if ($n -eq $prev) { Write-Host "  complete ($n meets)"; break }
  $prev = $n
}
"DONE $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" | Out-File -Append -Encoding utf8 $HB
Write-Host "TIER05FD2 DONE"
