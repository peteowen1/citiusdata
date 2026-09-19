# Waits for _run_noleak_chain.ps1 to finish (ALL DONE in its log), then runs the
# sigma-scale 0.9 arm on the same 60-meet pool (ctrl cache reused, so one arm).
# 2026-09-19: queued so the machine is never idle overnight and never runs two
# 4-6 GB jobs at once.
$LOG = "C:\dev\citiusverse\citiusdata\noleak_chain_log.txt"
while (-not (Select-String -Path $LOG -Pattern "ALL DONE|!!! step" -Quiet)) { Start-Sleep -Seconds 60 }
Start-Sleep -Seconds 30
$env:CITIUS_SS_SCALES  = "0.9"
$env:CITIUS_SS_WORKERS = "1"
$env:CITIUS_SS_TARGET  = "60"
$env:CITIUS_BT_MIN_FREE_MB = "5500"
& powershell -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\_run_sigma_scale_arm.ps1"
