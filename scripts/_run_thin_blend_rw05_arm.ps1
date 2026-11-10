# 0.5/0 ROUND RULE (clean-split re-pick 2026-11-10, Pete chose "arm 0.5/0, then decide"); copy of _run_thin_blend_rw_arm.ps1.
# NOTE: runs on the corpus AFTER the zero-padded-id fix, while bt_cache_thin_ctrl / bt_cache_blend_on were built
# before it; the fix touched 11 of 170,549 thin-event fit rows in the grid, so the thin-event compare is barely
# affected, but the other-event invariance line WILL show movement (data changed), and the fingerprint check fails.
# Thin-record BLEND arm ("version E + ROUND WEIGHTING", Pete 2026-11-10) against the CACHED control
# bt_cache_thin_ctrl (deployed config, 719 M1 meets, marks-only).
#
# Why no new control: the script edit changed script_md5, but a 12-meet run of the
# edited script with the switch OFF reproduced bt_cache_thin_ctrl exactly (1,942
# predictions, max diff 0, 2026-11-09 11:21pm), so the cached control is the control.
# The fingerprint check therefore expects script_md5 to differ as well as the blend
# fields. The arm recomputes its abilities (new ability-cache key): ~6 h.
#
# Settings (tuned on R1 racers, diagnostics/tune_thin_record_E_R1.R): recent half-life
# x2, kappa_R x0.5, long half-life 365 d, no floor. Verdict: R1 races, 2025+ meets
# (compare_thin_record_arms.R with CITIUS_TC_RACE=R1), plus every race for reference.
#
#   powershell -NoProfile -File citiusdata\scripts\_run_thin_blend_arm.ps1
#   log: citiusdata\thin_blend_rw05_arm_log.txt
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\thin_blend_rw05_arm_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
$MIN_AVAIL_MB = 6000; $CHUNK_MEETS = 120; $MAX_CHUNKS = 10; $WAIT_SECS = 120; $MAX_WAITS = 60
function Wait-ForMemory([string]$label) {
  for ($w = 1; $w -le $MAX_WAITS; $w++) {
    $avail = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
    if ($avail -ge $MIN_AVAIL_MB) { "$label -- $avail MB available, proceeding" | Out-File -Append -Encoding utf8 $LOG; return $true }
    "$label -- only $avail MB available (need $MIN_AVAIL_MB), waiting ${WAIT_SECS}s ($w/$MAX_WAITS)" | Out-File -Append -Encoding utf8 $LOG
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
# The backtest's own preflight floor (7000 MB) refused the 2026-11-09 11:43pm
# relaunch at 6906 MB while other sessions' jobs ran. Marks-only chunks peaked
# well under that all evening, and Wait-ForMemory below already gates each chunk
# at $MIN_AVAIL_MB, so the two floors are set equal.
$env:CITIUS_BT_MIN_FREE_MB = "$MIN_AVAIL_MB"
foreach ($v in "CITIUS_BT_ABILITY_CACHE", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK",
               "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_NEIGHBOUR_COMBINE", "CITIUS_BT_NEIGHBOUR_TRANSFER",
               "CITIUS_SIGMA_SCALE", "CITIUS_MARKS_BLEND", "CITIUS_THIN_RECORD") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
$env:CITIUS_THIN_BLEND = "1"; $env:CITIUS_TB_HR = "2"; $env:CITIUS_TB_MR = "0.5"; $env:CITIUS_TB_H = "365"; $env:CITIUS_TB_RW = "0.5"; $env:CITIUS_TB_WINW = "0"
$env:CITIUS_BT_CACHE = "bt_cache_blendrw05_on"; $env:CITIUS_BT_OUT = "backtest_blendrw05_on.rds"
$cacheDir = "citiusdata\data\bt_cache_blendrw05_on"
New-Item -ItemType Directory -Force -Path $cacheDir | Out-Null
$prev = -1; $stalled = 0; $reason = "reached MAX_CHUNKS -- PARTIAL"
for ($c = 1; $c -le $MAX_CHUNKS; $c++) {
  if (-not (Wait-ForMemory "arm blend chunk $c")) { $reason = "memory never recovered -- PARTIAL"; break }
  "  chunk $c starting $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  $rc = $LASTEXITCODE
  $now = (Get-ChildItem -Path $cacheDir -Filter "*.rds" -ErrorAction SilentlyContinue).Count
  "  chunk $c done: exit $rc, $now meets cached (was $prev)" | Out-File -Append -Encoding utf8 $LOG
  if ($rc -ne 0) { $reason = "Rscript exited $rc on chunk $c -- PARTIAL, not a stall"; break }
  if ($now -eq $prev) { $stalled++; if ($stalled -ge 2) { $reason = "COMPLETE at $now meets"; break } } else { $stalled = 0 }
  $prev = $now
}
"  arm blend STOPPED: $reason" | Out-File -Append -Encoding utf8 $LOG
if ($reason -notlike "COMPLETE*") { "!!! stopping: arm is not complete" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
"--- fingerprints $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\compare_arm_fingerprints.R" "citiusdata\data\bt_cache_thin_ctrl" "citiusdata\data\bt_cache_blendrw05_on" thin_blend thin_blend_par script_md5 2>&1 | Out-File -Append -Encoding utf8 $LOG
if ($LASTEXITCODE -ne 0) { "!!! fingerprints differ beyond the expected fields -- not comparing" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
$env:CITIUS_TC_A = "bt_cache_thin_ctrl"; $env:CITIUS_TC_B = "bt_cache_blendrw05_on"
"--- compare, R1 races $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_TC_RACE = "R1"
& Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"--- compare, every race $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
Remove-Item "Env:\CITIUS_TC_RACE" -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date -Format s) (compare exit $LASTEXITCODE) ===" | Out-File -Append -Encoding utf8 $LOG
# Marginal effect of the round rule alone: E with the rule (this arm) vs E without (bt_cache_blend_on).
"--- fingerprints vs E-without-rule $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\compare_arm_fingerprints.R" "citiusdata\data\bt_cache_blend_on" "citiusdata\data\bt_cache_blendrw05_on" thin_blend_par script_md5 2>&1 | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_TC_A = "bt_cache_blend_on"; $env:CITIUS_TC_B = "bt_cache_blendrw05_on"; $env:CITIUS_TC_RACE = "R1"
"--- compare E vs E+rule, R1 races $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\compare_thin_record_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE (incl. vs E) $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
