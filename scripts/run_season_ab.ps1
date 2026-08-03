# Season + indoor A/B, run as two backtest arms off ONE calibration.
#
# PowerShell, not bash, and that is not a style choice: `arrow` hard-segfaults
# under Git Bash R on this machine, with no message, so anything reading parquet
# must be launched from PowerShell. A .sh version of this reads as a crash in
# the backtest itself. See the Windows notes in ~/.claude/CLAUDE.md.
#
# Run from C:\dev\citiusverse.
#
#   pwsh citiusdata\scripts\run_season_ab.ps1
#   pwsh citiusdata\scripts\run_season_ab.ps1 -Meets 250 -EliteHistory
#
# -EliteHistory is a SCREENING mode: history is restricted to championship-
# calibre athletes, which cuts rows ~4.7x and runtime ~4.8x. Median ability
# change is 0.014% of a mark against the ~0.8% effects being chased, so a
# screening verdict nearly always survives -- but the maximum is 3.68%, so
# confirm the winner on full history before adopting anything, and never quote
# screening-mode absolute numbers.
param(
  [int]$Meets = 250,
  [switch]$EliteHistory,
  [string]$LogDir = "$env:TEMP\citius_season_ab"
)

$ErrorActionPreference = "Stop"
Set-Location C:\dev\citiusverse
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

# Refuse to run alongside a heavy foreign R job. Three attempts at this A/B were
# OOM-killed by a concurrent torpverse build holding 5+ GB on a box with 31.5 GB
# total; the kill is silent and looks exactly like a crash in this script.
$free = [math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1MB, 1)
Write-Host "Free physical memory: $free GB"
if ($free -lt 6) {
  Write-Warning "Under 6 GB free. Other R processes:"
  Get-CimInstance Win32_Process -Filter "Name='Rscript.exe'" |
    Select-Object ProcessId, @{n='RSS_GB';e={[math]::Round($_.WorkingSetSize/1GB,2)}}, CommandLine |
    Format-List
  Write-Warning "Runs at this level have been OOM-killed. Continuing anyway - watch for a silent death."
}

Write-Host "`n=== building the matched calibration pair ==="
Rscript citiusdata\scripts\make_season_arm_calibrations.R
if ($LASTEXITCODE -ne 0) { throw "calibration pair build failed" }

$env:CITIUS_BT_MEETS = "$Meets"
if ($EliteHistory) { $env:CITIUS_BT_ELITE_HISTORY = "1" }
else { Remove-Item Env:\CITIUS_BT_ELITE_HISTORY -ErrorAction SilentlyContinue }

foreach ($arm in @("off", "on")) {
  Write-Host "`n=== arm: season_$arm ($Meets meets) ==="
  $env:CITIUS_BT_CACHE = "backtest_cache_season_$arm"
  $env:CITIUS_BT_CALIBRATION = "calibration_season_$arm.rds"
  $env:CITIUS_BT_OUT = "backtest_season_$arm.rds"
  Rscript citiusdata\scripts\backtest_athletics.R > "$LogDir\arm_$arm.log" 2>&1
  if ($LASTEXITCODE -ne 0) {
    Write-Warning "arm $arm exited $LASTEXITCODE - tail of $LogDir\arm_$arm.log:"
    Get-Content "$LogDir\arm_$arm.log" -Tail 20
    throw "arm $arm failed"
  }
  Write-Host "arm $arm complete."
}

Write-Host "`n=== scoring: season_on against season_off ==="
# score_arm.R takes env vars, not positional arguments, and resolves paths with
# here::here() -- so it must run from C:\dev\citiusverse.
$env:CITIUS_SCORE_ARM = "backtest_season_on.rds"
$env:CITIUS_SCORE_VS  = "backtest_season_off.rds"
Rscript citiusdata\scripts\score_arm.R
