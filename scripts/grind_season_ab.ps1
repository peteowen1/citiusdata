# Grind the season A/B through in small batches, coexisting with other jobs.
#
# Why batches rather than one long run: this box runs another project queue that
# launches multi-GB R jobs back to back. Four attempts at a single 250-meet run
# were OOM-killed, each losing 20-30 minutes of wall clock. The kill is silent --
# no error, no exit code, the log simply stops mid-meet.
#
# The backtest caches per meet and rebuilds `todo` from what is missing on disk,
# so progress is monotone: a killed batch costs only the meet in flight. Running
# 25 meets at a time turns "compete for a 45-minute window and lose" into "take
# whatever 3-minute windows are going". Slower in wall clock, but it converges
# without supervision instead of restarting from a kill.
#
# Between batches it pauses if memory is tight, so it yields to the other queue
# rather than racing it. It never kills a foreign process.
param(
  [int]$BatchMeets = 25,
  [int]$TargetMeets = 250,
  [int]$MaxBatches = 60,
  [int]$MinFreeGB = 4,
  # Was a session scratchpad path, which ceased to exist with that session.
  [string]$LogDir = $(if ($env:GRIND_LOG_DIR) { $env:GRIND_LOG_DIR } else { Join-Path $env:TEMP "citius-grind" })
)

Set-Location C:\dev\citiusverse
$env:CITIUS_BT_ELITE_HISTORY = "1"
$env:CITIUS_BT_MEETS = "$BatchMeets"

function Cached($arm) {
  @(Get-ChildItem "citiusdata\data\backtest_cache_season_$arm" -ErrorAction SilentlyContinue).Count
}
function FreeGB { [math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1MB, 1) }

for ($i = 1; $i -le $MaxBatches; $i++) {
  $off = Cached "off"; $on = Cached "on"
  if ($off -ge $TargetMeets -and $on -ge $TargetMeets) {
    Write-Output "both arms complete: off=$off on=$on"; break
  }
  $arm = if ($off -lt $TargetMeets) { "off" } else { "on" }

  # Yield rather than race. A batch started at 0 GB free just dies and wastes
  # the corpus load, so skipping is strictly better than trying.
  $free = FreeGB
  if ($free -lt $MinFreeGB) {
    Write-Output ("[{0}] batch {1}: only {2} GB free - skipping this window (off={3} on={4})" -f (Get-Date -Format HH:mm:ss), $i, $free, $off, $on)
    Start-Sleep -Seconds 90
    continue
  }

  Write-Output ("[{0}] batch {1}: arm={2} off={3}/{4} on={5}/{6} free={7}GB" -f (Get-Date -Format HH:mm:ss), $i, $arm, $off, $TargetMeets, $on, $TargetMeets, $free)
  $env:CITIUS_BT_CACHE = "backtest_cache_season_$arm"
  $env:CITIUS_BT_CALIBRATION = "calibration_season_$arm.rds"
  $env:CITIUS_BT_OUT = "backtest_season_$arm.rds"
  Rscript citiusdata\scripts\backtest_athletics.R > "$LogDir\batch_$arm.log" 2>&1
  $rc = $LASTEXITCODE
  $now = Cached $arm
  Write-Output ("    -> exit={0}, {1} now at {2} meets (+{3})" -f $rc, $arm, $now, ($now - $(if ($arm -eq 'off') { $off } else { $on })))
  # A non-zero exit is expected when the OS kills it; the cache keeps the gain,
  # so this loops rather than aborting.
}

$off = Cached "off"; $on = Cached "on"
if ($off -lt $TargetMeets -or $on -lt $TargetMeets) {
  Write-Output "INCOMPLETE after $MaxBatches batches: off=$off on=$on - re-run this script to continue."
  exit 2
}

Write-Output "=== verifying the adjustment actually fired ==="
Rscript citiusdata\scripts\verify_season_arm_fired.R
if ($LASTEXITCODE -ne 0) { Write-Output "VERIFY FAILED - do not read the scorecard"; exit 1 }

Write-Output "=== scoring: season_on vs season_off ==="
$env:CITIUS_SCORE_ARM = "backtest_season_on.rds"
$env:CITIUS_SCORE_VS  = "backtest_season_off.rds"
Rscript citiusdata\scripts\score_arm.R
