# Treatment for the hurdles half-life test: identical to _run_hlctrl.ps1
# except HL_BY_FAMILY adds hurdles=180, matching fit_half_life()'s own
# measured optimum for the family (documented in backtest_athletics.R's own
# half-life comment block, never adopted -- only road/walk were).
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_csigma_coast.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_CACHE         = "backtest_cache_hltest"
$env:CITIUS_BT_OUT           = "backtest_hltest.rds"
$env:CITIUS_BT_MEETS         = "300"
$env:CITIUS_BT_TARGET        = "900"
$env:CITIUS_BT_WORKERS       = "10"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_SIGMA_SCALE -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_BT_PROJECT_ROUND -ErrorAction SilentlyContinue

$LOG = "C:\dev\citiusverse\citiusdata\hltest_log.txt"
& Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$n = (Get-ChildItem "citiusdata\data\backtest_cache_hltest" -Filter *.rds |
        Where-Object { $_.Name -ne "_arm.rds" } | Measure-Object).Count
Write-Host "HLTEST DONE: cached=$n"
