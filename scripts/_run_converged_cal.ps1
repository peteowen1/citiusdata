# Build the converged calibration (centre = "auto", max_iter 2500).
# Detached via Task Scheduler, which is what has survived on this box all week.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\converged_cal_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\build_calibration_converged.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "CONVERGED CAL DONE"
