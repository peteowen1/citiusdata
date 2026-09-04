# Resume after the first chain was killed mid-treatment-arm (118/200 done).
# Per-meet cache is resumable, so this picks up where it left off rather than
# redoing anything. MEETS raised to 450 (> the 394-meet T1 pool) so each arm
# finishes its FULL population in one invocation instead of hitting the
# 200-meet cap again and leaving a chronological gap.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\wac_reverify_0904_log.txt"
"=== RESUME $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[1/3] control arm, filling remaining T1 meets..."
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE       = "athletics_corpus_store"
$env:CITIUS_BT_CACHE       = "bt_cache_wac_ctrl_0904"
$env:CITIUS_BT_OUT         = "backtest_wac_ctrl_0904.rds"
$env:CITIUS_BT_TIER        = "T1_elite"
$env:CITIUS_BT_MEETS       = "450"
$env:CITIUS_BT_WORKERS     = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_MEET_TIER -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== control arm complete $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[2/3] treatment arm, filling remaining T1 meets..."
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_CACHE       = "bt_cache_wac_trt_0904"
$env:CITIUS_BT_OUT         = "backtest_wac_trt_0904.rds"
$env:CITIUS_BT_MEET_TIER   = "1"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== treatment arm complete $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

Write-Host "[3/3] per-event scoring..."
& Rscript "citiusdata\scripts\diagnostics\score_wac_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "WAC_REVERIFY_RESUME DONE"
