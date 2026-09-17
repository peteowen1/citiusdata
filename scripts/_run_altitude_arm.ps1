# ARM: per-family altitude adjustment in estimate_ability() (2026-09-17).
#
# WHAT IS BEING TESTED. calibration_corpus_wac_coast_0904_full2_altitude.rds is
# the deployed calibration with ONE element added, $altitude, and everything
# else byte-identical (compose_altitude_calibration.R asserts this). So the only
# difference between the two arms below is whether ability.R's altitude block
# fires. That isolation is the whole point -- re-running calibrate() to "add
# altitude" would also refit wind, the round/tier precisions and the race table,
# which is the shared-vintage confound that once made six single-variable arms
# all score ~1.7% for the same wrong reason.
#
# BOTH ARMS RUN HERE, from the same code, in the same session. An older control
# file on disk would be the cheaper comparison and the wrong one: same schema and
# same row count are not the same thing as comparable, and this repo has drawn
# two wrong conclusions in one day from exactly that shortcut.
#
# MARKS ONLY, and that is not a shortcut either. Altitude is a whole-field
# shock: every athlete in a race at Sestriere gets the same push. A shared shock
# CANNOT reorder a field -- it cancels out of every pairwise comparison -- so it
# moves absolute marks and nothing else. Marks MAE is therefore the metric that
# can see this, and a large medal/gold move would be a WARNING that something
# leaked, not a win.
#
# ADJUST_RACE=1 matches the deployed pipeline, and matters more than usual here:
# the residual coefficients were fitted per has_cr, where has_cr is exactly
# "did estimate_ability() apply a field-size-shrunk race effect to this row".
# Running the arm with the race strip off would score a coefficient fitted under
# a different regime.
#
#   powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_altitude_arm.ps1
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"

$LOG = "C:\dev\citiusverse\citiusdata\altitude_arm_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG

# Shared across both arms. Anything set here must NOT be set per-arm below.
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "M1"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "2"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
# Cleared for the same reason the excess-strip arm clears them: a stale variable
# from an earlier arm in the same shell silently changes the model under test.
foreach ($v in "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

# ctrl first so that if the run is interrupted the baseline exists: a treatment
# arm with no baseline is unreadable, a baseline with no treatment is still a
# usable control for the retry.
$ARMS = @(
  @{ tag = "alt_ctrl"; cal = "calibration_corpus_wac_coast_0904_full2.rds" },
  @{ tag = "alt_on";   cal = "calibration_corpus_wac_coast_0904_full2_altitude.rds" }
)

foreach ($a in $ARMS) {
  $tag = $a.tag
  "--- ARM $tag ($($a.cal)) start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $t0 = Get-Date
  $env:CITIUS_BT_CALIBRATION = $a.cal
  # Separate cache per arm. A shared cache is how two arms silently become one
  # arm scored twice.
  $env:CITIUS_BT_CACHE = "bt_cache_$tag"
  $env:CITIUS_BT_OUT   = "backtest_$tag.rds"
  $out = "citiusdata\data\backtest_$tag.rds"

  for ($i = 1; $i -le 6; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path $out) {
      $f = Get-Item $out
      if ($f.LastWriteTime -gt $t0) { break }
    }
    "    retry $i for $tag at $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
    Start-Sleep -Seconds 30
  }

  $mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
  "--- ARM $tag done in $mins min ---" | Out-File -Append -Encoding utf8 $LOG
  # Runtime goes in the same central log the hook and runtime_log.R write, so
  # "how long does an arm take" stays a query rather than a memory.
  $secs = [int]((Get-Date) - $t0).TotalSeconds
  $csv  = Join-Path $env:USERPROFILE ".claude\runtime-log.csv"
  if (-not (Test-Path $csv)) { "ts_end,secs,repo,tool,source,label" | Out-File -Encoding utf8 $csv }
  "$((Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')),$secs,citiusdata,PowerShell,arm,backtest_athletics.R / $tag (450 M1 meets, marks-only)" |
    Out-File -Append -Encoding utf8 $csv

  if (-not (Test-Path $out)) {
    "!!! $tag produced no output after 6 attempts -- stopping, not scoring a half-run comparison." | Out-File -Append -Encoding utf8 $LOG
    break
  }

  "--- scoring $tag $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_GOAL_ARM        = "backtest_$tag.rds"
  $env:CITIUS_GOAL_MARKS_ONLY = "1"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_$tag.csv" -Force
  Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
}

"=== ALL DONE $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
