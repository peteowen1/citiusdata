# Score the standing goal (Pete, 2026-09-05): does every event beat last-5 on
# BOTH marks MAE and medal logloss, WAC T1 test set since 2020?
#
# Loops the scorer until it reports ALL DONE. The baseline simulation is
# checkpointed per 250-race chunk, so a memory kill costs one chunk and the
# next attempt resumes -- which is why relaunching in a loop is safe here and
# was not safe before the checkpoint existed.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\goal_by_event_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

for ($i = 1; $i -le 8; $i++) {
  "--- attempt $i $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
    Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\goal_by_event.csv") {
    $csv = Get-Item "citiusdata\data\goal_by_event.csv"
    if ($csv.LastWriteTime -gt (Get-Date).AddMinutes(-10)) {
      "=== ALL DONE $(Get-Date) (attempt $i) ===" | Out-File -Append -Encoding utf8 $LOG
      Write-Host "GOAL SCORER DONE"
      exit 0
    }
  }
  "attempt $i did not finish; retrying from checkpoint" | Out-File -Append -Encoding utf8 $LOG
  Start-Sleep -Seconds 20
}
"=== GAVE UP after 8 attempts $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "GOAL SCORER GAVE UP"
