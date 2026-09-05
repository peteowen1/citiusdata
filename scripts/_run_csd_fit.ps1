$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\csd_fit_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\fit_condition_sd_from_total.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "CSD FIT DONE"
