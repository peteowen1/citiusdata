# Overnight queue (2026-09-20), one job at a time so two 4-6 GB fits never overlap:
#   1. wait for the no-leak chain (ALL DONE in noleak_chain_log.txt)
#   2. the REFIT control: same chain, same corpus, nothing excluded (tag "refit")
#      -- deployed vs no-leak confounds corpus vintage (32,089 vs 33,372 meets)
#      with the exclusion; refit vs no-leak isolates the leak
#   3. the sigma-scale 0.9 arm on the same 60-meet pool (ctrl cache reused)
$LOG = "C:\dev\citiusverse\citiusdata\noleak_chain_log.txt"
while (-not (Select-String -Path $LOG -Pattern "ALL DONE|!!! step" -Quiet)) { Start-Sleep -Seconds 60 }
Start-Sleep -Seconds 30
$env:CITIUS_NL_TAG = "refit"; $env:CITIUS_NL_EXCLUDE = "0"
& powershell -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\_run_noleak_chain.ps1"
Remove-Item Env:\CITIUS_NL_TAG, Env:\CITIUS_NL_EXCLUDE -ErrorAction SilentlyContinue
Start-Sleep -Seconds 30
$env:CITIUS_SS_SCALES  = "0.9"
$env:CITIUS_SS_WORKERS = "1"
$env:CITIUS_SS_TARGET  = "60"
$env:CITIUS_BT_MIN_FREE_MB = "5500"
& powershell -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\_run_sigma_scale_arm.ps1"
