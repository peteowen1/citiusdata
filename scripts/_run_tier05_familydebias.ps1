# Confirmation arm: project_tier(shrink=0.5) + family-pool debias, wired
# together in the actual pipeline (not the post-hoc check). Same env as
# _run_tier05_parallel.ps1 plus CITIUS_BT_FAMILY_DEBIAS=1.
# was "Stop": native Rscript stderr chatter (e.g. R-version package warnings)
# gets treated as a terminating error under Stop, killing the job on benign
# output -- confirmed 2026-09-01 via two real backtest kills traced to this.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$MEETS_PER_CHUNK = 300
$MAX_CHUNKS      = 3
$WORKERS         = 12
$HB  = "C:\dev\citiusverse\citiusdata\data\tier05fd_heartbeat.txt"
$LOG = "C:\dev\citiusverse\citiusdata\tier05fd_log.txt"

$env:CITIUS_BT_CALIBRATION  = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE        = "athletics_corpus_store"
$env:CITIUS_BT_CACHE        = "backtest_cache_tier05fd"
$env:CITIUS_BT_OUT          = "backtest_tier05fd.rds"
$env:CITIUS_BT_MEETS        = "$MEETS_PER_CHUNK"
$env:CITIUS_BT_PROJECT_TIER = "0.5"
$env:CITIUS_BT_WORKERS      = "$WORKERS"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$cacheDir = "C:\dev\citiusverse\citiusdata\data\backtest_cache_tier05fd"
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
Write-Host "TIER05FD DONE"
