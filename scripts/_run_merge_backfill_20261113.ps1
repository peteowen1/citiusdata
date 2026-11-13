# Merge the 1,030 September backfill meets (65,155 results) staged 2026-09-15 that never
# reached championship_results.rds, then corpus -> catalogue -> stores. Pete's go 2026-11-13.
# Old-shape files (tier / comp_ranking_category) are renamed in memory by
# merge_missing_meets_batch.R (pure rename, checked on 300 merged meets / 18,097 rows).
# Dry run 9:59am: 65,155 rows, 1,030 meets (9 empty files skipped). Rollback: the script's own
# time-stamped championship_results.rds copy.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\merge_backfill_20261113_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
# Wait for 7000 MB free (same floor as weekly_merge_pending.ps1), up to 2 h.
for ($w = 1; $w -le 60; $w++) {
  $avail = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
  if ($avail -ge 7000) { "$(Get-Date -Format s) $avail MB free, starting" | Out-File -Append -Encoding utf8 $LOG; break }
  if ($w -eq 60) { "=== END $(Get-Date -Format s) SKIPPED: memory never reached 7000 MB ===" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
  if ($w % 5 -eq 1) { "$(Get-Date -Format s) only $avail MB free, waiting" | Out-File -Append -Encoding utf8 $LOG }
  Start-Sleep -Seconds 120
}
Remove-Item Env:\CITIUS_MERGE_DRYRUN -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_MERGE_SKIP_CHAIN -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\merge_missing_meets_batch.R" backfill_ids_20261113.txt 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== END $(Get-Date -Format s) exit $LASTEXITCODE ===" | Out-File -Append -Encoding utf8 $LOG
