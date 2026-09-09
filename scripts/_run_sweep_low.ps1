$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\sweep_low_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_SWEEP_EVENTS = "AT-100Metres-M,AT-ShotPut-M"
$env:CITIUS_SWEEP_ITERS  = "1,2,3,5,8,12,18,25"
& Rscript "citiusdata\scripts\diagnostics\sweep_decompose_iters.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "SWEEP LOW DONE"
