# Rebuild athletics_corpus_store from citius.duckdb, then a local export with upload OFF
# (2026-11-10 padded-id fix; the runner copy that ran predates its build_stores step).
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\padded_id_fix_log.txt"
function Say([string]$m) { "[$(Get-Date -Format s)] $m" | Out-File -Append -Encoding utf8 $LOG }
Say "STEP build_stores start (_run_build_stores_after_fix.ps1)"
& Rscript "citiusdata\scripts\build_stores.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$code = $LASTEXITCODE
Say "STEP build_stores exit $code"
if ($code -ne 0) { Say "STOPPED at build_stores"; exit 1 }
$env:CITIUS_SKIP_UPLOAD = "1"
Say "STEP local_export2 start (CITIUS_SKIP_UPLOAD=1, nothing uploaded)"
& Rscript "citiusdata\scripts\export_blog_data.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Say "STEP local_export2 exit $LASTEXITCODE"
Say "AFTER-FIX DONE"
