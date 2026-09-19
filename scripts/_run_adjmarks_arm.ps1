# Adjusted-marks arm for the CAREER model (the ITG cards), 2026-09-19.
#
#   ctrl : deployed calibration (banded altitude, road excluded), raw history
#   adjm : same calibration, history perf replaced by the conditions-cleaned
#          perf from adjusted_marks_v8.parquet (wind + venue/altitude + indoor),
#          calibration$altitude switched off inside the script so altitude is
#          corrected once. Race shock is untouched: calibrate()'s c_r stays.
#
# Judged on MARKS + CONCORDANCE (an ability-level change), never Brier alone:
# citiusverse/CLAUDE.md "Which metric judges which change". Compare with
#   CITIUS_MA_A=bt_cache_adjm_ctrl CITIUS_MA_B=bt_cache_adjm_on Rscript citiusdata/scripts/diagnostics/compare_marks_arms.R
#
# Same shape as _run_altitude_arm.ps1: ctrl first, separate cache per arm,
# per-meet cache so a kill loses one meet, 6 retries, runtime logged.
#   powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_adjmarks_arm.ps1
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\adjmarks_arm_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "M1"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_TARGET        = "120"     # BOTH arms share the pool -- see _run_altitude_arm.ps1
$env:CITIUS_BT_MEETS         = "150"
$env:CITIUS_BT_WORKERS       = "2"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad.rds"
foreach ($v in "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_ADJ_MARKS") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

$ARMS = @(
  @{ tag = "adjm_ctrl"; adj = "" },
  @{ tag = "adjm_on";   adj = "adjusted_marks_v8.parquet" }
)

foreach ($a in $ARMS) {
  $tag = $a.tag
  "--- ARM $tag (adj='$($a.adj)') start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $t0 = Get-Date
  if ($a.adj -ne "") { $env:CITIUS_BT_ADJ_MARKS = $a.adj } else { Remove-Item Env:\CITIUS_BT_ADJ_MARKS -ErrorAction SilentlyContinue }
  $env:CITIUS_BT_CACHE = "bt_cache_$tag"
  $env:CITIUS_BT_OUT   = "backtest_$tag.rds"
  $out = "citiusdata\data\backtest_$tag.rds"

  for ($i = 1; $i -le 6; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path $out) { $f = Get-Item $out; if ($f.LastWriteTime -gt $t0) { break } }
    "    retry $i for $tag at $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
    Start-Sleep -Seconds 30
  }

  $mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
  "--- ARM $tag done in $mins min ---" | Out-File -Append -Encoding utf8 $LOG
  $secs = [int]((Get-Date) - $t0).TotalSeconds
  $csv  = Join-Path $env:USERPROFILE ".claude\runtime-log.csv"
  if (-not (Test-Path $csv)) { "ts_end,secs,repo,tool,source,label" | Out-File -Encoding utf8 $csv }
  "$((Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')),$secs,citiusdata,PowerShell,arm,backtest_athletics.R / $tag (target $env:CITIUS_BT_TARGET M1 meets, marks-only)" |
    Out-File -Append -Encoding utf8 $csv
  if (-not (Test-Path $out)) {
    "!!! $tag produced no output after 6 attempts -- stopping, not scoring a half-run comparison." | Out-File -Append -Encoding utf8 $LOG
    break
  }
}
"=== ALL DONE $(Get-Date -Format s) === compare: CITIUS_MA_A=bt_cache_adjm_ctrl CITIUS_MA_B=bt_cache_adjm_on Rscript citiusdata/scripts/diagnostics/compare_marks_arms.R" | Out-File -Append -Encoding utf8 $LOG
