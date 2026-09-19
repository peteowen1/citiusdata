# The no-leak calibration chain (docs/plans/leakage-noleak-chain-2026-09-19.md).
#
# Refits the deployed calibration's whole composition with the SCORED
# COMPETITIONS of one backtest pool removed, then runs the deployed backtest
# configuration with that calibration on the same pool. The finished
# deployed-calibration arm on that pool is the control, so this runs ONE arm.
#
#   control : bt_cache_ss_ctrl / backtest_ss_ctrl.rds  (60 M1 meets, 2026-09-19)
#   arm     : bt_cache_noleak  / backtest_noleak.rds   (same 60 meets, no-leak cal)
#
# Read with
#   Rscript citiusdata/scripts/diagnostics/compare_marks_arms.R   (marks, concordance)
#   CITIUS_CAL_CACHE=bt_cache_noleak Rscript citiusdata/scripts/diagnostics/career_finals_calibration.R
#   CITIUS_QC_A=bt_cache_ss_ctrl CITIUS_QC_B=bt_cache_noleak Rscript citiusdata/scripts/quick_compare.R
#
# Accepted residual leaks, recorded here so the read is honest:
#   - race_reliability_by_event.csv (per-event, 663k races; 60 meets are noise in it)
#   - altitude_effect.parquet (per family x sex x band, fitted on the leaky base)
#   - the PIT residual rows behind the spread scales were produced with a
#     calibration that saw the pool; the scored ROWS are dropped (hook), the
#     calibration that made them is not refitted
#
#   powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_noleak_chain.ps1
# Env: CITIUS_NL_POOL (default backtest_ss_ctrl.rds), CITIUS_NL_TARGET (60),
#      CITIUS_NL_FROM (step number to resume from, default 0), CITIUS_NL_WORKERS (1)
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
# CITIUS_NL_TAG names the chain's artefacts (default "noleak"); CITIUS_NL_EXCLUDE=0
# runs the SAME chain on the SAME corpus with nothing excluded -- the refit
# control. Needed because the deployed calibration was fitted on the 09-04
# corpus (32,089 meets) and a refit today sees 33,372: deployed vs no-leak
# confounds corpus vintage with the exclusion, refit vs no-leak does not.
$TAG    = $(if ($env:CITIUS_NL_TAG)    { $env:CITIUS_NL_TAG }    else { "noleak" })
$EXCL   = $(if ($env:CITIUS_NL_EXCLUDE) { $env:CITIUS_NL_EXCLUDE } else { "1" })
$LOG = "C:\dev\citiusverse\citiusdata\${TAG}_chain_log.txt"
$D   = "C:\dev\citiusverse\citiusdata\data"
"=== START $(Get-Date -Format s) (tag $TAG, exclude $EXCL) ===" | Out-File -Append -Encoding utf8 $LOG
$POOL   = $(if ($env:CITIUS_NL_POOL)   { $env:CITIUS_NL_POOL }   else { "backtest_ss_ctrl.rds" })
$TARGET = $(if ($env:CITIUS_NL_TARGET) { $env:CITIUS_NL_TARGET } else { "60" })
$FROM   = [int]$(if ($env:CITIUS_NL_FROM) { $env:CITIUS_NL_FROM } else { "0" })
if (-not (Test-Path (Join-Path $D $POOL))) { "!!! pool artefact $POOL missing" | Out-File -Append $LOG; exit 1 }

$BASE   = "calibration_corpus_wac_coast_0904_$TAG.rds"
$CTXSD  = "calibration_corpus_wac_coast_0904_${TAG}_ctxsd.rds"
$SCALED = "calibration_corpus_wac_coast_0904_${TAG}_ctxsd_scaled_all.rds"
$EB     = "calibration_race_eb_perevent_$TAG.rds"
$PERS   = "calibration_race_eb_perevent_${TAG}_persist5.rds"
$FULL2  = "calibration_corpus_wac_coast_0904_full2_$TAG.rds"
$FINAL  = "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad_$TAG.rds"
$X0 = @{ CITIUS_WAC_OUT = $BASE }
$X2 = @{ CITIUS_SCALES_SRC = $CTXSD; CITIUS_SCALES_OUT = $SCALED }
if ($EXCL -eq "1") { $X0.CITIUS_EXCLUDE_SCORED = $POOL; $X2.CITIUS_EXCLUDE_SCORED = $POOL }

function Step($n, $label, $script, $envs, $expect) {
  if ($n -lt $FROM) { "--- step $n $label SKIPPED (resume from $FROM)" | Out-File -Append $LOG; return }
  "--- step $n $label start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $t0 = Get-Date
  foreach ($k in $envs.Keys) { Set-Item -Path "Env:\$k" -Value $envs[$k] }
  & Rscript $script 2>&1 | Out-File -Append -Encoding utf8 $LOG
  foreach ($k in $envs.Keys) { Remove-Item "Env:\$k" -ErrorAction SilentlyContinue }
  $mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
  $csv  = Join-Path $env:USERPROFILE ".claude\runtime-log.csv"
  "$((Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')),$([int]((Get-Date) - $t0).TotalSeconds),citiusdata,PowerShell,chain,$TAG step $n $label" | Out-File -Append -Encoding utf8 $csv
  $f = Join-Path $D $expect
  if (-not (Test-Path $f) -or (Get-Item $f).LastWriteTime -lt $t0) {
    "!!! step $n $label did not write $expect -- stopping ($mins min)" | Out-File -Append $LOG; exit 1
  }
  "--- step $n $label done in $mins min -> $expect ---" | Out-File -Append -Encoding utf8 $LOG
}

Step 0 $(if ($EXCL -eq "1") { "base calibrate (scored competitions removed)" } else { "base calibrate (REFIT control, nothing removed)" }) "citiusdata\scripts\build_calibration_wac_coast_0904.R" `
  $X0 $BASE
Step 1 "context condition_sd" "citiusdata\scripts\build_calibration_condsd_context.R" `
  @{ CITIUS_CTXSD_SRC = $BASE; CITIUS_CTXSD_OUT = $CTXSD } $CTXSD
Step 2 "spread scales (scored PIT rows removed)" "citiusdata\scripts\fit_spread_scales.R" `
  $X2 $SCALED
Step 3 "EB race shrinkage" "citiusdata\scripts\build_calibration_race_eb.R" `
  @{ CITIUS_EB_SRC = $BASE; CITIUS_EB_OUT = $EB } $EB
Step 4 "shock persistence (family-gated)" "citiusdata\scripts\fit_race_shock_persistence.R" `
  @{ CITIUS_SHOCK_CAL = $EB; CITIUS_SHOCK_OUT = $PERS; CITIUS_COMPOSE_SHOCK = $PERS; CITIUS_SHOCK_FAMILIES = "sprint,hurdles,jump,throw" } $PERS
Step 5 "compose scales + shock" "citiusdata\scripts\build_calibration_compose.R" `
  @{ CITIUS_COMPOSE_SCALES = $SCALED; CITIUS_COMPOSE_SHOCK = $PERS; CITIUS_COMPOSE_OUT = $FULL2 } $FULL2
Step 6 "altitude (road zeroed)" "citiusdata\scripts\compose_altitude_calibration.R" `
  @{ CITIUS_ALT_BASE = $FULL2; CITIUS_ALT_OUT = $FINAL; CITIUS_ALT_ZERO_FAMILIES = "road" } $FINAL

# Step 7: the arm. Same configuration as _run_sigma_scale_arm.ps1's control.
if (7 -ge $FROM) {
  "--- step 7 backtest arm (no-leak calibration) start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  $t0 = Get-Date
  $env:CITIUS_BT_ADJUST_RACE        = "1"
  $env:CITIUS_BT_STORE              = "athletics_corpus_store"
  $env:CITIUS_BT_TIER               = "M1"
  $env:CITIUS_BT_MEET_TIER          = "1"
  $env:CITIUS_BT_TARGET             = $TARGET
  $env:CITIUS_BT_MEETS              = "150"
  $env:CITIUS_BT_WORKERS            = $(if ($env:CITIUS_NL_WORKERS) { $env:CITIUS_NL_WORKERS } else { "1" })
  $env:CITIUS_HALF_LIFE_FAMILY      = "road=1095,walk=730,hurdles=180"
  $env:CITIUS_BT_CALIBRATION        = $FINAL
  $env:CITIUS_EVENT_PARAMS          = "event_params.rds"
  $env:CITIUS_BT_NEIGHBOUR_COMBINE  = "1"
  $env:CITIUS_BT_NEIGHBOUR_COMBINE_EVENTS = "AT-800Metres-M,AT-1500Metres-M,AT-3000Metres-M,AT-5000Metres-M,AT-10000Metres-M"
  foreach ($v in "CITIUS_BT_MARKS_ONLY", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS",
                 "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
                 "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_ADJ_MARKS", "CITIUS_BT_SIGMA_SCALE") {
    Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
  }
  $env:CITIUS_BT_CACHE = "bt_cache_$TAG"
  $env:CITIUS_BT_OUT   = "backtest_$TAG.rds"
  $out = Join-Path $D "backtest_$TAG.rds"
  for ($i = 1; $i -le 6; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path $out) { $f = Get-Item $out; if ($f.LastWriteTime -gt $t0) { break } }
    "    retry $i for $TAG arm at $(Get-Date -Format s)" | Out-File -Append -Encoding utf8 $LOG
    Start-Sleep -Seconds 30
  }
  $mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
  $csv  = Join-Path $env:USERPROFILE ".claude\runtime-log.csv"
  "$((Get-Date).ToUniversalTime().ToString('yyyy-MM-ddTHH:mm:ssZ')),$([int]((Get-Date) - $t0).TotalSeconds),citiusdata,PowerShell,arm,backtest_athletics.R / $TAG (target $TARGET M1 meets, placings)" | Out-File -Append -Encoding utf8 $csv
  "--- step 7 arm done in $mins min ---" | Out-File -Append -Encoding utf8 $LOG
}
"=== ALL DONE $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
