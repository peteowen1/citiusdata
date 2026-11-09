# Thin-record arm ("version C", Pete 2026-11-09) against a same-corpus control,
# both on the deployed config (_deployed.R: calibration ..._refit1031_ss080,
# event_params.rds, race effect on), M1 meets, MARKS-ONLY: marks + concordance
# judge an ability change (citiusverse CLAUDE.md), so no simulation is needed.
#
# ORDER MATTERS: control first. The thin-record fields are in ABIL_EXCLUDE, so
# the arm shares the control's ability cache and pays only for the long refit
# (smoke 2026-11-09: 12 meets, 239 s ability with the arm on).
#
# Then compare_arm_fingerprints.R (the arms must differ ONLY in thin_record*)
# and compare_marks_arms.R (paired marks, concordance, zero-movement bucket).
# Every non-middle-distance prediction must be IDENTICAL between the arms; the
# invariance bucket is that check.
#
#   powershell -NoProfile -File citiusdata\scripts\_run_thin_record_arm.ps1
#   log: citiusdata\thin_record_arm_log.txt
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\thin_record_arm_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG

$MIN_AVAIL_MB = 6000    # backtest's own preflight floor is 7000; this gates each chunk
$CHUNK_MEETS  = 120     # ~4 min startup per chunk, so fewer, bigger chunks
$MAX_CHUNKS   = 10      # 719 M1 meets / 120 = 6
$WAIT_SECS    = 120
$MAX_WAITS    = 60

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
foreach ($v in "CITIUS_BT_ABILITY_CACHE", "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK",
               "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_NEIGHBOUR_COMBINE", "CITIUS_BT_NEIGHBOUR_TRANSFER",
               "CITIUS_SIGMA_SCALE", "CITIUS_MARKS_BLEND", "CITIUS_THIN_RECORD") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

$arms = @(@{ name = "ctrl"; thin = "0" }, @{ name = "on"; thin = "1" })
foreach ($arm in $arms) {
  $env:CITIUS_THIN_RECORD = $arm.thin
  $env:CITIUS_BT_CACHE = "bt_cache_thin_$($arm.name)"
  $env:CITIUS_BT_OUT   = "backtest_thin_$($arm.name).rds"
  $cacheDir = "citiusdata\data\bt_cache_thin_$($arm.name)"
  New-Item -ItemType Directory -Force -Path $cacheDir | Out-Null
  "--- arm $($arm.name) (CITIUS_THIN_RECORD=$($arm.thin)) $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $prev = -1; $stalled = 0; $reason = "reached MAX_CHUNKS -- PARTIAL"
  for ($c = 1; $c -le $MAX_CHUNKS; $c++) {
    if (-not (Wait-ForMemory "arm $($arm.name) chunk $c")) { $reason = "memory never recovered -- PARTIAL"; break }
    "  chunk $c starting $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    $rc = $LASTEXITCODE
    $now = (Get-ChildItem -Path $cacheDir -Filter "*.rds" -ErrorAction SilentlyContinue).Count
    "  chunk $c done: exit $rc, $now meets cached (was $prev)" | Out-File -Append -Encoding utf8 $LOG
    # A crash also caches nothing new; never let it read as completion.
    if ($rc -ne 0) { $reason = "Rscript exited $rc on chunk $c -- PARTIAL, not a stall"; break }
    if ($now -eq $prev) { $stalled++; if ($stalled -ge 2) { $reason = "COMPLETE at $now meets"; break } } else { $stalled = 0 }
    $prev = $now
  }
  "  arm $($arm.name) STOPPED: $reason" | Out-File -Append -Encoding utf8 $LOG
  if ($reason -notlike "COMPLETE*") { "!!! stopping: arm $($arm.name) is not complete" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
}

"--- fingerprints $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\compare_arm_fingerprints.R" "citiusdata\data\bt_cache_thin_ctrl" "citiusdata\data\bt_cache_thin_on" thin_record thin_record_md5 2>&1 | Out-File -Append -Encoding utf8 $LOG
if ($LASTEXITCODE -ne 0) { "!!! fingerprints differ beyond thin_record -- not comparing" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
"--- compare $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_MA_A = "bt_cache_thin_ctrl"; $env:CITIUS_MA_B = "bt_cache_thin_on"
& Rscript "citiusdata\scripts\diagnostics\compare_marks_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date -Format s) (compare exit $LASTEXITCODE) ===" | Out-File -Append -Encoding utf8 $LOG
