# CONFIRMING ARM for the marks recency blend, on the standard apparatus.
#
# WHY. The blend was licensed in the marks lab, which is gate-verified exact
# against estimate_ability() but scores 35 events on a 2024+ window. The launch
# gate is the 2020+ T1_elite set of 54 events scored by score_goal_by_event.R.
# This run puts the decision on that apparatus.
#
# Two arms, and the PAIR is the point -- a blend arm alone cannot separate the
# blend from anything else that changed today:
#
#   blend0   CITIUS_MARKS_BLEND=0    the control: identical code, blend off
#   blend6   CITIUS_MARKS_BLEND=0.6  what is deployed
#
# Read against each other, not against an older stored arm. Everything else --
# calibration, race-shock strip, debias off, family half-lives -- is held
# identical between them, which is what makes the difference attributable.
#
# MARKS ONLY, so ~1h per arm rather than ~2.5h. The blend provably cannot move
# a probability (tests/testthat/test-marks-blend.R), so there is nothing for a
# medal arm to find here; a separate medal arm is queued as a NULL check, where
# any movement at all would mean the marks/ranking separation leaked.
#
# NOTE the marks-only branch in backtest_athletics.R applies the blend BY HAND,
# because it skips simulate_event(). Without that, this arm would measure the
# unblended model while claiming to test the blended one and come back showing
# no effect -- a false negative indistinguishable from a real null.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\marks_blend_arm_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904_full2.rds"
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
# ONE worker, not two. The 2-worker run of this arm was OOM-killed at 16:30 on
# 2026-09-07 with ~1 of 394 meets cached after an hour: each worker holds its
# own copy of the narrowed history, and 3.1 GB free at fork time is not enough
# for a second. Serial is roughly twice the wall clock and finishes, which beats
# half the speed of nothing. Raise it again only after checking
# `Get-Counter '\Memory\Available MBytes'` -- FreePhysicalMemory reads high
# while the machine is already thrashing.
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
foreach ($v in "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

# Do not even start if the machine cannot hold the run. An arm that dies at
# meet 240 costs an hour and leaves a half-built cache; a refusal costs nothing.
#
# 12 GB, MEASURED, not guessed. Two runs were OOM-killed on 2026-09-07:
#   2 workers, 3.1 GB available at fork  -> died after ~55 min, 1/394 cached
#   1 worker,  5.3 GB available at start -> died, nothing cached
# The parent alone reached 5.85 GB during "Reading history from the parquet
# store" (705 Mb narrowed, several times that once materialised in R), and that
# read happens before any meet is processed, so it is a floor rather than a
# peak. An earlier 5000 threshold passed at 5309 MB and the run still died --
# which is why this number comes from the failures, not from the read size.
#
# Chunking does NOT help: CITIUS_BT_MEETS caps meets per run and the cache
# resumes, but the store read is a fixed per-run cost and it is the thing that
# blows up. Nor do CITIUS_HISTORY_DAYS or CITIUS_BT_ELITE_HISTORY -- both cut
# memory by changing which history the model sees, which confounds the arm.
$availMB = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
"available memory at start: $availMB MB" | Out-File -Append -Encoding utf8 $LOG
if ($availMB -lt 12000) {
  "!!! only $availMB MB available, need 12000. NOT STARTING." | Out-File -Append -Encoding utf8 $LOG
  Write-Host "Refusing to start: $availMB MB available, need 12000."
  exit 1
}

foreach ($arm in @(@{name="blend0"; blend="0"}, @{name="blend6"; blend="0.6"})) {
  "--- arm $($arm.name), blend $($arm.blend), $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_MARKS_BLEND = $arm.blend
  $env:CITIUS_BT_CACHE    = "bt_cache_marks_$($arm.name)"
  $env:CITIUS_BT_OUT      = "backtest_marks_$($arm.name).rds"
  $out = "citiusdata\data\backtest_marks_$($arm.name).rds"

  for ($i = 1; $i -le 6; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path $out) {
      $f = Get-Item $out
      if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
    }
    Start-Sleep -Seconds 30
  }

  if (-not (Test-Path $out)) {
    "!!! arm $($arm.name) produced NO OUTPUT -- not scoring it" | Out-File -Append -Encoding utf8 $LOG
    continue
  }
  "--- scoring $($arm.name) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_GOAL_ARM        = "backtest_marks_$($arm.name).rds"
  $env:CITIUS_GOAL_MARKS_ONLY = "1"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" `
            "citiusdata\data\goal_by_event_marks_$($arm.name).csv" -Force
  Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
