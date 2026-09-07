# MEDAL ARM for races-since decay. Does the marks gain survive contact with
# finishing positions?
#
# WHAT IS BEING TESTED. `races_half_life = 5` discounts a result once the
# athlete has run five more races in that event, on top of the calendar decay.
# On the marks lab, held out on 2024+ against a like-for-like last-5 baseline,
# it takes events beaten from 28 of 44 to 37 and pooled mark error from 2.1487
# to 2.0791. But it changes `ability`, which is what orders a field, so nothing
# about marks licenses deploying it. This arm asks the only question that
# matters for medals: does the ordering get better or worse?
#
# THE PAIR MOVES TOGETHER, ON PURPOSE. The treatment sets races_half_life 5 AND
# a calendar half-life of 730. That is two changes in one arm, which is normally
# exactly the mistake this repo keeps making -- and here it is the finding
# rather than sloppiness. Measured:
#
#   half_life 365, races Inf   28 of 44   <- control, what runs today
#   half_life 730, races Inf   17 of 44   <- worse than control ALONE
#   half_life 365, races 5     36 of 44
#   half_life 730, races 5     37 of 44   <- treatment
#
# 365 days had been doing two jobs, discounting stale form and crudely capping
# how many results accumulate. Splitting the arm would test a config nobody
# proposes and that is known to be worse. If this arm wins, the follow-up is to
# re-fit the per-family half-lives with races decay on, since road=1095,
# walk=730 and hurdles=180 were all fitted without it.
#
# FULL SIMULATION, not marks-only: the whole point is p_gold and p_medal.
# Roughly 2.5h per arm, serial.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\races_decay_arm_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

# 12 GB, measured from two OOM kills on 2026-09-07 rather than guessed. The
# parent alone reaches 5.85 GB during the store read, before any meet runs.
$availMB = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
"available memory at start: $availMB MB" | Out-File -Append -Encoding utf8 $LOG
if ($availMB -lt 12000) {
  "!!! only $availMB MB available, need 12000. NOT STARTING." | Out-File -Append -Encoding utf8 $LOG
  Write-Host "Refusing to start: $availMB MB available, need 12000."
  exit 1
}

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904_full2.rds"
$env:CITIUS_BT_ADJUST_RACE   = "1"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
foreach ($v in "CITIUS_BT_FAMILY_DEBIAS", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_TRAIN_TIERS",
               "CITIUS_BT_SIGMA_MODE", "CITIUS_BT_SIGMA_PARTS", "CITIUS_SIGMA_PSEUDO_N",
               "CITIUS_SIGMA_SCALE", "CITIUS_BT_COND_CONTEXT", "CITIUS_BT_MARKS_ONLY",
               "CITIUS_MARKS_BLEND") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}

# Per-family half-lives are HELD CONSTANT across both arms, so the treatment
# differs in exactly two things: the global calendar half-life and races-since
# decay. The lab measured its 37-of-44 with no family overrides at all, but
# clearing CITIUS_HALF_LIFE_FAMILY does not disable them -- backtest_athletics.R
# falls back to "road=1095,walk=730" when the variable is unset, so an arm that
# merely cleared it would silently run DIFFERENT overrides rather than none, and
# the comparison would carry a third uncontrolled change.
#
# Re-fitting the family half-lives with races decay on is the follow-up. All
# three were fitted without it, so they are very likely wrong now in the same
# way the global 365 was.
$arms = @(
  @{ name = "ctrl";  races = "Inf"; hl = "365" },
  @{ name = "races"; races = "5";   hl = "730" }
)
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
foreach ($arm in $arms) {
  "--- arm $($arm.name): races_half_life $($arm.races), half_life $($arm.hl) $(Get-Date) ---" |
    Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_RACES_HALF_LIFE = $arm.races
  $env:CITIUS_HALF_LIFE        = $arm.hl
  $env:CITIUS_BT_CACHE = "bt_cache_races_$($arm.name)"
  $env:CITIUS_BT_OUT   = "backtest_races_$($arm.name).rds"
  $out = "citiusdata\data\backtest_races_$($arm.name).rds"

  for ($i = 1; $i -le 6; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path $out) {
      $f = Get-Item $out
      if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-20)) { break }
    }
    Start-Sleep -Seconds 30
  }
  if (-not (Test-Path $out)) {
    "!!! arm $($arm.name) produced NO OUTPUT -- not scoring it" | Out-File -Append -Encoding utf8 $LOG
    continue
  }
  "--- scoring $($arm.name) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_GOAL_ARM = "backtest_races_$($arm.name).rds"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" `
            "citiusdata\data\goal_by_event_races_$($arm.name).csv" -Force
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
