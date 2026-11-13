# CONTROL REBUILD for the 0.5/0 round-rule arm (Pete 2026-11-12: "launch overnight").
# Version E with NO round weighting (CITIUS_TB_RW unset = default 1), identical env to
# _run_thin_blend_arm.ps1 (which built bt_cache_blend_on), but on the corpus AFTER the
# zero-padded-id fix, so it matches bt_cache_blendrw05_on's data exactly:
#   history_md5 fd277b52d6bf7b1b72a4061e1529d09c, store_md5 30cd6631a8c289530aa70ec2bb706ff9
#   (both re-checked against the live files 2026-11-12 10pm).
# The old bt_cache_blend_on predates the fix, which is why the rw05 runner's fingerprint
# check refused to compare (citiusdata/thin_blend_rw05_arm_log.txt, 11 Nov 5:29am).
#
# Expected fingerprint differences vs the arm: thin_blend_par (rw), script_md5, and
# citius_git: the arm ran at citius a2e0c4c; HEAD 84607ba only adds an OPTIONAL
# world_records argument to flag_implausible() (default NULL = old behaviour), and
# backtest_athletics.R:1039/1046 calls it without that argument.
# DO NOT COMMIT TO citius OR citiusdata/scripts WHILE THIS RUNS: citius_git and
# script_md5 are read per chunk (memory commits-under-a-running-arm).
#
# Runtime: the identical rw05 workload took 6 h 40 min (22:49 -> 05:29, 9 chunks, 725 meets).
#   Start-Process powershell -ArgumentList '-NoProfile','-File','citiusdata\scripts\_run_thin_blend_on_postfix.ps1'
#   log: citiusdata\thin_blend_on_postfix_log.txt
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\thin_blend_on_postfix_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
# MAX_WAITS 180 x 120 s = 6 h: another session's R job held memory under the floor at launch.
# The runner waits for MIN_AVAIL_MB + 500 so a small swing between its check and the backtest's
# own 6000 MB preflight does not abort the chunk (12 Nov 10:26pm: 6077 here, 5819 in R seconds later).
# Floor lowered from 6000 to 5000 by Pete 2026-11-12 ~10:45pm (Chrome held 6 GB; chunk peak never measured).
$MIN_AVAIL_MB = 5000; $LAUNCH_MB = 5500; $CHUNK_MEETS = 120; $MAX_CHUNKS = 10; $WAIT_SECS = 120; $MAX_WAITS = 180
$MAX_PREFLIGHT_RETRIES = 10
function Wait-ForMemory([string]$label) {
  for ($w = 1; $w -le $MAX_WAITS; $w++) {
    $avail = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
    if ($avail -ge $LAUNCH_MB) { "$label -- $avail MB available, proceeding" | Out-File -Append -Encoding utf8 $LOG; return $true }
    if ($w % 15 -eq 1) { "$label -- only $avail MB available (need $LAUNCH_MB), waiting ($w/$MAX_WAITS)" | Out-File -Append -Encoding utf8 $LOG }
    Start-Sleep -Seconds $WAIT_SECS
  }
  return $false
}
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad_refit1031_ss080.rds"
$env:CITIUS_EVENT_PARAMS   = "event_params.rds"
$env:CITIUS_BT_ADJUST_RACE = "1"
$env:CITIUS_BT_STORE       = "athletics_corpus_store"
$env:CITIUS_BT_TIER        = "M1"
$env:CITIUS_BT_MEETS       = "$CHUNK_MEETS"
$env:CITIUS_BT_WORKERS     = "1"
$env:CITIUS_BT_MARKS_ONLY  = "1"
$env:CITIUS_BT_MIN_FREE_MB = "$MIN_AVAIL_MB"
foreach ($v in "CITIUS_BT_ABILITY_CACHE", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK",
               "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_NEIGHBOUR_COMBINE", "CITIUS_BT_NEIGHBOUR_TRANSFER",
               "CITIUS_SIGMA_SCALE", "CITIUS_MARKS_BLEND", "CITIUS_THIN_RECORD",
               "CITIUS_TB_RW", "CITIUS_TB_WINW", "CITIUS_TC_RACE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
$env:CITIUS_THIN_BLEND = "1"; $env:CITIUS_TB_HR = "2"; $env:CITIUS_TB_MR = "0.5"; $env:CITIUS_TB_H = "365"
$env:CITIUS_BT_CACHE = "bt_cache_blend_on_postfix"; $env:CITIUS_BT_OUT = "backtest_blend_on_postfix.rds"
$cacheDir = "citiusdata\data\bt_cache_blend_on_postfix"
New-Item -ItemType Directory -Force -Path $cacheDir | Out-Null
$prev = -1; $stalled = 0; $reason = "reached MAX_CHUNKS -- PARTIAL"; $retries = 0
$chunkOut = Join-Path $env:TEMP "citius_ctrl_postfix_chunk.txt"
for ($c = 1; $c -le $MAX_CHUNKS; $c++) {
  if (-not (Wait-ForMemory "control chunk $c")) { $reason = "memory never recovered -- PARTIAL"; break }
  "  chunk $c starting $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Encoding utf8 $chunkOut
  $rc = $LASTEXITCODE
  Get-Content $chunkOut | Out-File -Append -Encoding utf8 $LOG
  # The backtest's own memory preflight refusing is a wait, not a failure: nothing ran.
  if ($rc -ne 0 -and (Select-String -Path $chunkOut -Pattern 'MB available; this backtest needs' -Quiet)) {
    $retries++
    if ($retries -gt $MAX_PREFLIGHT_RETRIES) { $reason = "preflight refused $retries times on chunk $c -- PARTIAL"; break }
    "  chunk $c refused by the backtest's memory preflight; retry $retries/$MAX_PREFLIGHT_RETRIES after waiting" | Out-File -Append -Encoding utf8 $LOG
    Start-Sleep -Seconds $WAIT_SECS; $c--; continue
  }
  $now = (Get-ChildItem -Path $cacheDir -Filter "*.rds" -ErrorAction SilentlyContinue).Count
  "  chunk $c done: exit $rc, $now meets cached (was $prev)" | Out-File -Append -Encoding utf8 $LOG
  if ($rc -ne 0) { $reason = "Rscript exited $rc on chunk $c -- PARTIAL, not a stall"; break }
  if ($now -eq $prev) { $stalled++; if ($stalled -ge 2) { $reason = if ($now -gt 0) { "COMPLETE at $now meets" } else { "no meets cached after 2 chunks -- PARTIAL (check CITIUS_BT_CACHE / the backtest log)" }; break } } else { $stalled = 0 }
  $prev = $now
}
"  control STOPPED: $reason" | Out-File -Append -Encoding utf8 $LOG
if ($reason -notlike "COMPLETE*") { "!!! stopping: control is not complete" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
"--- fingerprints $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\compare_arm_fingerprints.R" "citiusdata\data\bt_cache_blend_on_postfix" "citiusdata\data\bt_cache_blendrw05_on" thin_blend_par script_md5 citius_git 2>&1 | Out-File -Append -Encoding utf8 $LOG
if ($LASTEXITCODE -ne 0) { "!!! fingerprints differ beyond the expected fields -- not comparing" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
$env:CITIUS_TC_A = "bt_cache_blend_on_postfix"; $env:CITIUS_TC_B = "bt_cache_blendrw05_on"
"--- compare (E vs E + 0.5/0), R1 races $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_TC_RACE = "R1"
& Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$rcR1 = $LASTEXITCODE
if ($rcR1 -ne 0) { "!!! R1 compare exited $rcR1" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
"--- compare (E vs E + 0.5/0), every race $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
Remove-Item "Env:\CITIUS_TC_RACE" -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
$rcAll = $LASTEXITCODE
"=== ALL DONE $(Get-Date -Format s) (compare exit $rcAll) ===" | Out-File -Append -Encoding utf8 $LOG
if ($rcAll -ne 0) { exit 1 }
