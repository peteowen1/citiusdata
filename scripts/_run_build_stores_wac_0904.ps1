# Rebuild the athletics stores with join_tier = TRUE, so the history carries
# the meet_tier labels the newly-promoted WAC-fitted calibration expects.
# Task Scheduler rather than a session-backgrounded job: session-scoped
# background tasks were killed four times on 2026-09-04 under memory pressure.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\build_stores_wac_0904_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\build_stores.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "BUILD_STORES DONE"
