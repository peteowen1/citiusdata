$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\sweep_iters_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_SWEEP_EVENTS = "AT-100Metres-M,AT-ShotPut-M"
$env:CITIUS_SWEEP_ITERS  = "25,50,100,200,400,1000,2000"
& Rscript "citiusdata\scripts\diagnostics\sweep_decompose_iters.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "SWEEP DONE"
