# 2026-09-20 morning: the refit chain's arm refused on the 7000 MB preflight
# (a peer session's three backtests hold memory). Wait for the sigma 0.9 arm,
# then resume the refit chain at step 7 with the floor lowered knowingly.
$LOG = "C:\dev\citiusverse\citiusdata\sigma_scale_arm_log.txt"
$t0 = Get-Date
while (-not ((Get-Item $LOG).LastWriteTime -gt $t0 -and (Select-String -Path $LOG -Pattern "ALL DONE" | Select-Object -Last 1).Line -match "2026-09-20")) { Start-Sleep -Seconds 60 }
Start-Sleep -Seconds 30
$env:CITIUS_NL_TAG = "refit"; $env:CITIUS_NL_EXCLUDE = "0"; $env:CITIUS_NL_FROM = "7"
$env:CITIUS_BT_MIN_FREE_MB = "5500"
& powershell -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\_run_noleak_chain.ps1"
