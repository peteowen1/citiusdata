# WEEKLY: merge every pending-harvest meet not yet in championship_results.rds, then
# rebuild corpus -> catalogue -> stores (merge_missing_meets_batch.R's own chain).
# Pete 2026-11-13: "weekly, auto-pausing", after live ratings sat frozen at 2026-11-06 for a
# week because the daily harvest only STAGES meets and nobody ran the merge.
#
# Registered as the scheduled task "citius weekly merge" (Sundays 05:30, before the 07:30
# blog refresh; the 13 Nov merge took 24 min). It SKIPS, logging why (exit 0, but the second
# skip in a row is a FAIL, exit 1), when:
#   - a backtest or arm is running: the merge changes the corpus every backtest reads, and
#     an arm whose data moves mid-run cannot be compared to its control;
#   - under 7000 MB is free (the corpus build loads ~8M rows);
#   - another copy of this script or the merge script is already running.
# A skipped week is caught up the next week: the merge takes every staged meet, not one week's.
# Each merge writes a ~150 MB time-stamped rollback copy of championship_results.rds
# (data/championship_results_premerge_missingmeets_*.rds); after a good run only the newest 3 are kept.
#
#   log: citiusdata\weekly_merge_log.txt
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\weekly_merge_log.txt"
$D   = "C:\dev\citiusverse\citiusdata\data"
function Say([string]$m) { "[$(Get-Date -Format s)] $m" | Out-File -Append -Encoding utf8 $LOG }
Say "START"

$SKIPS = Join-Path $D 'weekly_merge_skips.txt'
# A skip is fine once; skipping week after week is the silent freeze this job exists to
# prevent (review 2026-11-13), so the second skip in a row fails loudly.
function Skip([string]$why) {
  $n = 1; if (Test-Path $SKIPS) { $n = 1 + [int](Get-Content $SKIPS -TotalCount 1) }
  Set-Content -Path $SKIPS -Value $n
  if ($n -ge 2) { Say "FAIL: skipped $n weeks running, latest because $why"; exit 1 }
  Say "SKIP ($n in a row): $why"; exit 0
}

# Busy = any Rscript that reads or rebuilds the corpus, or ANY scripts\_run_*.ps1 runner:
# arm runners sleep between chunks with no Rscript alive (review 2026-11-13).
$busy = Get-CimInstance Win32_Process -Filter "name='Rscript.exe' or name='powershell.exe' or name='pwsh.exe'" |
  Where-Object { $_.ProcessId -ne $PID -and $_.CommandLine -match 'backtest_athletics|compare_thin_record_arms|compare_arm_fingerprints|scripts[^a-z]_run_|merge_missing_meets_batch|weekly_merge_pending|build_athletics_corpus|build_stores' }
if ($busy) {
  Skip "running job(s) that read or rebuild the corpus: $((($busy | ForEach-Object { ($_.CommandLine -split '[\\/]')[-1] }) | Select-Object -Unique) -join '; ')"
}
$avail = $null
try { $avail = (Get-Counter '\Memory\Available MBytes' -ErrorAction Stop).CounterSamples.CookedValue } catch { Skip "could not read free memory ($($_.Exception.Message))" }
if ($avail -lt 7000) { Skip "only $avail MB free (need 7000)" }
# Past the checks: this run is not a skip, so the skip streak ends here, whatever happens next.
Remove-Item $SKIPS -ErrorAction SilentlyContinue

# Fresh copies of every staged meet: a multi-day meet can be re-staged fuller later, so the
# merge (CITIUS_MERGE_MIN_AGE_DAYS below) leaves meets that started within 7 days for next week.
& gh release download pending-harvest --repo peteowen1/citiusdata --pattern 'meet_*_raw_results.rds' --dir $D --clobber 2>&1 | Out-File -Append -Encoding utf8 $LOG
if ($LASTEXITCODE -ne 0) { Say "FAIL: gh release download exited $LASTEXITCODE; nothing merged"; exit 1 }
$ids = Get-ChildItem -Path $D -Filter 'meet_*_raw_results.rds' | ForEach-Object { $_.Name -replace '^meet_', '' -replace '_raw_results\.rds$', '' }
if (-not $ids) { Say "nothing staged"; Remove-Item $SKIPS -ErrorAction SilentlyContinue; exit 0 }
$ids | Set-Content -Encoding ascii (Join-Path $D 'pending_ids.txt')
Say "$($ids.Count) staged ids (the merge skips those already in championship_results.rds)"

Remove-Item Env:\CITIUS_MERGE_DRYRUN -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_MERGE_SKIP_CHAIN -ErrorAction SilentlyContinue
$env:CITIUS_MERGE_MIN_AGE_DAYS = "7"   # same window harvest_pending_meets.R re-stages within
$out = Join-Path $env:TEMP "citius_weekly_merge_out.txt"
& Rscript "citiusdata\scripts\merge_missing_meets_batch.R" pending_ids.txt 2>&1 | Out-File -Encoding utf8 $out
$rc = $LASTEXITCODE
Get-Content $out | Out-File -Append -Encoding utf8 $LOG
if ($rc -ne 0) { Say "FAIL: merge_missing_meets_batch.R exited $rc (read the lines above; championship_results.rds may be updated with the corpus NOT rebuilt)"; exit 1 }
if (Select-String -Path $out -Pattern 'DuckDB is now BEHIND' -Quiet) { Say "FAIL: the merge landed but citius.duckdb did not update (build_stores reads it)"; exit 1 }
# The corpus must be at least as new as championship_results.rds, or the rebuild chain did not run.
$ch = (Get-Item (Join-Path $D 'championship_results.rds')).LastWriteTime
$co = (Get-Item (Join-Path $D 'athletics_corpus.rds')).LastWriteTime
if ($co -lt $ch) { Say "FAIL: athletics_corpus.rds ($co) is older than championship_results.rds ($ch); the rebuild chain did not run"; exit 1 }
Remove-Item $SKIPS -ErrorAction SilentlyContinue
# Keep the newest 3 of the merge's own rollback copies (Pete 2026-11-13); only after a good run,
# so a failed merge never costs a rollback point.
$old = Get-ChildItem -Path $D -Filter 'championship_results_premerge_missingmeets_*.rds' | Sort-Object LastWriteTime -Descending | Select-Object -Skip 3
foreach ($f in $old) { Remove-Item $f.FullName; Say "pruned rollback copy $($f.Name)" }
Say "OK"
