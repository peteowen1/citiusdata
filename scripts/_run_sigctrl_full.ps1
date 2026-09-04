# Sigma-context-scale test on the FULL T1 population (~900 meets, ~1,285+ T1
# races), mirroring backtest_tierctrl.rds's own config exactly (same
# calibration, no project_tier, no family_debias) plus the corrected
# sigma_context scale on top. Isolates the ONE thing being tested against the
# population that already gave -0.63% medal Brier / -4.95% gold Brier without
# it, per Pete's direct request to run this on the full 1,285-race population
# rather than the 246-276 race slice.
# NOT "Stop": Rscript's own benign stderr chatter (e.g. "package 'data.table'
# was built under R version 4.5.2") gets piped through 2>&1 below, and under
# ErrorActionPreference=Stop PowerShell treats ANY native-command stderr
# output as a terminating error -- which killed this exact job twice today
# (task notifications reported "failed"/"killed" while cache stalled at 1
# meet, no R error anywhere in the log). $LASTEXITCODE is checked explicitly
# instead of relying on stop-on-error for a native command.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$MEETS_PER_CHUNK = 300
$MAX_CHUNKS      = 6
$WORKERS         = 10
$HB  = "C:\dev\citiusverse\citiusdata\data\sigctrl_full_heartbeat.txt"
$LOG = "C:\dev\citiusverse\citiusdata\sigctrl_full_log.txt"

$env:CITIUS_BT_CALIBRATION  = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE        = "athletics_corpus_store"
$env:CITIUS_BT_CACHE        = "backtest_cache_sigctrl_full"
$env:CITIUS_BT_OUT          = "backtest_sigctrl_full.rds"
$env:CITIUS_BT_MEETS        = "$MEETS_PER_CHUNK"
$env:CITIUS_BT_TARGET       = "900"
$env:CITIUS_BT_WORKERS      = "$WORKERS"
$env:CITIUS_BT_SIGMA_SCALE  = "0.785"
Remove-Item Env:\CITIUS_BT_PROJECT_TIER -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_FAMILY_DEBIAS -ErrorAction SilentlyContinue

$cacheDir = "C:\dev\citiusverse\citiusdata\data\backtest_cache_sigctrl_full"
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
Write-Host "SIGCTRL_FULL DONE"
