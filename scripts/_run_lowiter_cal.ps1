$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\lowiter_cal_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_LOWITER = "2"
& Rscript "citiusdata\scripts\build_calibration_lowiter.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "LOWITER CAL DONE"
