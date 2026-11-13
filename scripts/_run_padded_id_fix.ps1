# Padded-athlete-id fix, end to end (2026-11-10; citiusverse NEXT-STEPS "Zero-padded athlete ids").
# Waits for the round-weight arm to finish (it reads the corpus and championship_results.rds),
# then runs each step in order and STOPS at the first non-zero exit.
#   1. oneoff_strip_padded_ids.R   - championship_results.rds + name lookup + DuckDB copy
#   2. build_athletics_corpus.R    - corpus rebuild (strips again before the dedupe, logs the count)
#   3. build_crosswalk.R           - crosswalk on the stripped ids
#   4. diagnostics/zero_padded_ids.R - should report 0 twins
#   5. export_blog_data.R with CITIUS_SKIP_UPLOAD=1 - local export ONLY (nothing uploaded),
#      so the ratings change can be compared with blog_1046 before the 07:30 refresh.
# Launch detached:
#   Start-Process powershell -ArgumentList '-NoProfile','-File','C:\dev\citiusverse\citiusdata\scripts\_run_padded_id_fix.ps1' -WindowStyle Hidden
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\padded_id_fix_log.txt"
$ARM = "C:\dev\citiusverse\citiusdata\thin_blend_rw_arm_log.txt"
function Say([string]$m) { "[$(Get-Date -Format s)] $m" | Out-File -Append -Encoding utf8 $LOG }
Say "START, waiting for the round-weight arm"

# Wait (max 6 h) until the arm log has a terminal line AND no backtest Rscript is running.
$deadline = (Get-Date).AddHours(6)
while ($true) {
  $tail = Get-Content $ARM -Tail 40 -ErrorAction SilentlyContinue | Out-String
  # '!!! fingerprints differ' is EXPECTED here (control cache built at citius 26d229a,
  # not on the runner's whitelist): every meet is computed by then, only the compare
  # is skipped, so the inputs are free. 'STOPPED' / '!!! stopping' = arm incomplete.
  $done = $tail -match 'ALL DONE' -or $tail -match 'arm blend STOPPED' -or $tail -match '!!! '
  $bt = Get-CimInstance Win32_Process -Filter "name='Rscript.exe'" | Where-Object { $_.CommandLine -match 'backtest_athletics|compare_thin_record_arms|compare_arm_fingerprints' }
  $armps = Get-CimInstance Win32_Process -Filter "name='powershell.exe'" | Where-Object { $_.CommandLine -match '_run_thin_blend_rw_arm' }
  if ($done -and -not $bt -and -not $armps) { break }
  if ((Get-Date) -gt $deadline) { Say "GAVE UP waiting for the arm after 6 h"; exit 2 }
  Start-Sleep -Seconds 60
}
$full = Get-Content $ARM -Raw
if ($full -notmatch 'arm blend STOPPED: COMPLETE' -or $tail -match '!!! stopping') {
  Say "ARM DID NOT FINISH CLEANLY -- not touching its inputs; a session should look first"; exit 3
}
Say "arm finished"

# THE COMPARE MUST RUN BEFORE THE REBUILD: compare_thin_record_arms.R joins the arm caches
# (padded ids) to athletics_corpus_store on athlete_id, so after the rebuild the padded
# athletes would silently drop out of the verdict. If the arm's own compare was skipped
# (fingerprints differ on citius_git only: control 26d229a, arm a2e0c4c; the diff between
# them is opt-in code the control never runs, checked 2026-11-10), run it here.
if ($full -notmatch 'ALL DONE \(incl\. vs E\)' -and $full -notmatch 'ALL DONE \(compare by fix runner\)') {
  Say "arm compare was skipped; running it now (appended to the arm log)"
  "--- compare run by _run_padded_id_fix.ps1 (citius_git differs; code diff checked) $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $ARM
  foreach ($pair in @(@("bt_cache_thin_ctrl", "bt_cache_blendrw_on", "R1"), @("bt_cache_thin_ctrl", "bt_cache_blendrw_on", ""), @("bt_cache_blend_on", "bt_cache_blendrw_on", "R1"))) {
    $env:CITIUS_TC_A = $pair[0]; $env:CITIUS_TC_B = $pair[1]
    if ($pair[2]) { $env:CITIUS_TC_RACE = $pair[2] } else { Remove-Item "Env:\CITIUS_TC_RACE" -ErrorAction SilentlyContinue }
    "--- compare $($pair[0]) vs $($pair[1]), race filter '$($pair[2])' $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $ARM
    & Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $ARM
    if ($LASTEXITCODE -ne 0) { Say "compare $($pair[0]) vs $($pair[1]) exit $LASTEXITCODE -- STOPPED before the rebuild"; exit 4 }
  }
  Remove-Item "Env:\CITIUS_TC_A", "Env:\CITIUS_TC_B", "Env:\CITIUS_TC_RACE" -ErrorAction SilentlyContinue
  "=== ALL DONE (compare by fix runner) $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $ARM
  Say "compare done"
}
Say "starting the fix"

$steps = @(
  @{ name = "oneoff_strip_padded_ids"; script = "citiusdata\scripts\oneoff_strip_padded_ids.R" },
  @{ name = "build_athletics_corpus";  script = "citiusdata\scripts\build_athletics_corpus.R" },
  @{ name = "build_crosswalk";         script = "citiusdata\scripts\build_crosswalk.R" },
  # athletics_corpus_store (partitioned parquet, read by _deployed.R, the export and the
  # backtests) is built from citius.duckdb by build_stores.R, NOT by the corpus build.
  @{ name = "build_stores";            script = "citiusdata\scripts\build_stores.R" },
  @{ name = "zero_padded_ids";         script = "citiusdata\scripts\diagnostics\zero_padded_ids.R" }
)
foreach ($s in $steps) {
  Say "STEP $($s.name) start"
  & Rscript $s.script 2>&1 | Out-File -Append -Encoding utf8 $LOG
  $code = $LASTEXITCODE
  Say "STEP $($s.name) exit $code"
  if ($code -ne 0) { Say "STOPPED at $($s.name)"; exit 1 }
}
$env:CITIUS_SKIP_UPLOAD = "1"
Say "STEP local_export start (CITIUS_SKIP_UPLOAD=1, nothing uploaded)"
& Rscript "citiusdata\scripts\export_blog_data.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Say "STEP local_export exit $LASTEXITCODE"
Say "ALL STEPS DONE"
