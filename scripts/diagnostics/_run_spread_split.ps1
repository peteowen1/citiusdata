# Runs the shared-vs-individual spread split detached from any Claude session
# (Task Scheduler), so a session's background-task cycling cannot kill it.
#   schtasks /create /tn citius_spread_split /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\diagnostics\_run_spread_split.ps1"
#   schtasks /run /tn citius_spread_split
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\spread_split_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\split_spread_shared_vs_individual.R" 2>&1 |
  Out-File -Append -Encoding utf8 $LOG
"=== END $(Get-Date) exit=$LASTEXITCODE ===" | Out-File -Append -Encoding utf8 $LOG
