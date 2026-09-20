# 2026-09-20: 1.0 -> 0.9 -> 0.8 improved monotonically, so 0.8 is the edge of
# the tested range, not an optimum. Wait for the refit arm, then run 0.7.
$OUT = "C:\dev\citiusverse\citiusdata\data\backtest_refit.rds"
$LOG = "C:\dev\citiusverse\citiusdata\refit_chain_log.txt"
while (-not (Test-Path $OUT) -and -not ((Select-String -Path $LOG -Pattern "!!! step 7|produced no output" | Measure-Object).Count -gt 0 -and (Get-Item $LOG).LastWriteTime -gt (Get-Date).AddMinutes(-1))) { Start-Sleep -Seconds 60 }
Start-Sleep -Seconds 30
$env:CITIUS_SS_SCALES  = "0.7"
$env:CITIUS_SS_WORKERS = "1"
$env:CITIUS_SS_TARGET  = "60"
$env:CITIUS_BT_MIN_FREE_MB = "5500"
& powershell -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\_run_sigma_scale_arm.ps1"
