# Autonomous overnight runner. Waits for the in-flight uniform-sigma arm, scores
# it, then sweeps the remaining arms. Launched detached so it survives the
# session ending.
Set-Location C:\dev\citiusverse
$log = "citiusdata\data\overnight.log"
function Say($m) { "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $m | Tee-Object -FilePath $log -Append }

Say "runner started"

# 1. wait for the arm already running
$p = "citiusdata\data\backtest_flat.rds"
$n = 0
while (-not (Test-Path $p) -and $n -lt 120) { Start-Sleep -Seconds 30; $n++ }
if (Test-Path $p) {
  Say "flat arm complete; scoring"
  $env:CITIUS_SCORE_ARM = "backtest_flat.rds"; $env:CITIUS_SCORE_VS = ""
  $env:CITIUS_SCORE_BASE = "event"; $env:CITIUS_SCORE_HOLDOUT = "2023-01-01"
  Rscript citiusdata\scripts\score_arm.R *> "citiusdata\data\score_flat.log"
  Get-Content "citiusdata\data\score_flat.log" |
    Select-String -Pattern "DECISIONS|PRIMARY|marks MAE ctr|gold logloss|gold Brier|favourite" |
    ForEach-Object { Say "  flat | $_" }
} else { Say "flat arm did not finish in time; continuing" }

# 2. sweep the rest
& "citiusdata\scripts\overnight_arms.ps1"
Say "runner finished"
