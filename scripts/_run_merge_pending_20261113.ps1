# Merge the 42 pending-harvest meets not yet in championship_results.rds (staged 6-11 Nov),
# then corpus -> catalogue -> stores (merge_missing_meets_batch.R's own chain). Pete's go
# 2026-11-13. Dry run 8:30am: 119 staged ids, 77 already merged (7 Nov), 4,024 rows to append;
# discipline_code / value_raw / birthdate_year_only 0% are the mapper's documented exemptions.
# Rollback: the script writes a copy of championship_results.rds before replacing it.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\merge_pending_20261113_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
Remove-Item Env:\CITIUS_MERGE_DRYRUN -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_MERGE_SKIP_CHAIN -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\merge_missing_meets_batch.R" pending_ids.txt 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== END $(Get-Date -Format s) exit $LASTEXITCODE ===" | Out-File -Append -Encoding utf8 $LOG
