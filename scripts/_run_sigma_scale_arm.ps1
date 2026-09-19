# Spread arm for the CAREER model's finals (2026-09-19). The deployed cards
# are under-confident on the favourite: over 2,083 M1 finals the favourite's
# mean p_gold is 0.430 and they win 48.4% (+5.4pp; +7 to +11pp in the 0.4-0.6
# band), medals near-calibrated (diagnostics/career_finals_calibration.R).
# That is a spread-too-wide signature, so the lever is sigma. Arms:
#   ss_ctrl : deployed configuration, placings simulated (NOT marks-only)
#   ss_090  : CITIUS_BT_SIGMA_SCALE=0.9  (multiplies calibration$sigma_context$ratio)
#   ss_080  : CITIUS_BT_SIGMA_SCALE=0.8
# A SPREAD change is judged on Brier / log-loss and the calibration curve,
# not marks or concordance (citiusverse/CLAUDE.md). Read with
#   CITIUS_CAL_CACHE=bt_cache_ss_090 Rscript citiusdata/scripts/diagnostics/career_finals_calibration.R
# and quick_compare.R for gold/medal Brier against ss_ctrl.
#   powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_sigma_scale_arm.ps1
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\sigma_scale_arm_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_ADJUST_RACE        = "1"
$env:CITIUS_BT_STORE              = "athletics_corpus_store"
$env:CITIUS_BT_TIER               = "M1"
$env:CITIUS_BT_MEET_TIER          = "1"
$env:CITIUS_BT_TARGET             = "120"
$env:CITIUS_BT_MEETS              = "150"
$env:CITIUS_BT_WORKERS            = "2"
$env:CITIUS_HALF_LIFE_FAMILY      = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_CALIBRATION        = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad.rds"
$env:CITIUS_EVENT_PARAMS          = "event_params.rds"
$env:CITIUS_BT_NEIGHBOUR_COMBINE  = "1"
foreach ($v in "CITIUS_BT_MARKS_ONLY", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_ADJ_MARKS", "CITIUS_BT_SIGMA_SCALE") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

$ARMS = @(
  @{ tag = "ss_ctrl"; scale = "" },
  @{ tag = "ss_090";  scale = "0.9" },
  @{ tag = "ss_080";  scale = "0.8" }
)

foreach ($a in $ARMS) {
  $tag = $a.tag
  "--- ARM $tag (sigma scale '$($a.scale)') start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $t0 = Get-Date
  if ($a.scale -ne "") { $env:CITIUS_BT_SIGMA_SCALE = $a.scale } else { Remove-Item Env:\CITIUS_BT_SIGMA_SCALE -ErrorAction SilentlyContinue }
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
  "$((Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')),$secs,citiusdata,PowerShell,arm,backtest_athletics.R / $tag (target $env:CITIUS_BT_TARGET M1 meets, placings)" |
    Out-File -Append -Encoding utf8 $csv
  if (-not (Test-Path $out)) { "!!! $tag produced no output after 6 attempts -- stopping." | Out-File -Append -Encoding utf8 $LOG; break }
}
"=== ALL DONE $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
