# WEEKLY: merge every pending-harvest meet not yet in championship_results.rds, then
# rebuild corpus -> catalogue -> stores (merge_missing_meets_batch.R's own chain).
# Pete 2026-11-13: "weekly, auto-pausing", after live ratings sat frozen at 2026-11-06 for a
# week because the daily harvest only STAGES meets and nobody ran the merge.
#
# Registered as the scheduled task "citius weekly merge" (Sundays 05:30, before the 07:30
# blog refresh; the 13 Nov merge took 24 min). It SKIPS, logging why and exiting 0, when:
#   - a backtest or arm is running: the merge changes the corpus every backtest reads, and
#     an arm whose data moves mid-run cannot be compared to its control;
#   - under 7000 MB is free (the corpus build loads ~8M rows);
#   - another copy of this script or the merge script is already running.
# A skipped week is caught up the next week: the merge takes every staged meet, not one week's.
# Each merge writes a ~150 MB time-stamped rollback copy of championship_results.rds
# (data/championship_results_premerge_missingmeets_*.rds). Nothing here deletes them.
#
#   log: citiusdata\weekly_merge_log.txt
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\weekly_merge_log.txt"
$D   = "C:\dev\citiusverse\citiusdata\data"
function Say([string]$m) { "[$(Get-Date -Format s)] $m" | Out-File -Append -Encoding utf8 $LOG }
Say "START"

$busy = Get-CimInstance Win32_Process -Filter "name='Rscript.exe' or name='powershell.exe'" |
  Where-Object { $_.ProcessId -ne $PID -and $_.CommandLine -match 'backtest_athletics|compare_thin_record_arms|compare_arm_fingerprints|_run_thin_|merge_missing_meets_batch|weekly_merge_pending|build_athletics_corpus|build_stores' }
if ($busy) {
  Say "SKIP: running job(s) that read or rebuild the corpus: $((($busy | ForEach-Object { ($_.CommandLine -split '\\')[-1] }) | Select-Object -Unique) -join '; ')"
  exit 0
}
$avail = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
if ($avail -lt 7000) { Say "SKIP: only $avail MB free (need 7000)"; exit 0 }

# Fresh copies of every staged meet: a multi-day meet can be re-staged fuller later.
& gh release download pending-harvest --repo peteowen1/citiusdata --pattern 'meet_*_raw_results.rds' --dir $D --clobber 2>&1 | Out-File -Append -Encoding utf8 $LOG
if ($LASTEXITCODE -ne 0) { Say "FAIL: gh release download exited $LASTEXITCODE; nothing merged"; exit 1 }
$ids = Get-ChildItem -Path $D -Filter 'meet_*_raw_results.rds' | ForEach-Object { $_.Name -replace '^meet_', '' -replace '_raw_results\.rds$', '' }
if (-not $ids) { Say "nothing staged"; exit 0 }
$ids | Set-Content -Encoding ascii (Join-Path $D 'pending_ids.txt')
Say "$($ids.Count) staged ids (the merge skips those already in championship_results.rds)"

Remove-Item Env:\CITIUS_MERGE_DRYRUN -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_MERGE_SKIP_CHAIN -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\merge_missing_meets_batch.R" pending_ids.txt 2>&1 | Out-File -Append -Encoding utf8 $LOG
$rc = $LASTEXITCODE
if ($rc -ne 0) { Say "FAIL: merge_missing_meets_batch.R exited $rc (read the lines above; championship_results.rds may be updated with the corpus NOT rebuilt)"; exit 1 }
Say "OK"
