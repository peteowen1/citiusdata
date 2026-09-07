# MEDAL ARM for the per-event parameter tables. Does the marks gain survive
# contact with finishing positions?
#
# WHAT IS BEING TESTED. scripts/fit_event_params.R fits four parameters per
# event -- half_life, races_half_life, trim_tactical and context_scale -- each
# shrunk twice, event toward family and family toward global, so a thin event
# inherits rather than invents. On the marks lab, held out on 2024+ against a
# like-for-like last-5 baseline, that takes pooled mark error from -6.28% to
# -7.57%. But every one of those four changes `ability`, which is what orders a
# field, so nothing about marks licenses deploying them. This arm asks the only
# question that matters for medals: does the ordering get better or worse?
#
# AN EARLIER VERSION OF THIS ARM TESTED THE WRONG THING. It ran a scalar
# races_half_life = 5 with a calendar half-life of 730, which was the proposal
# for about an hour before per-event tables superseded it. Left alone it would
# have burned two hours measuring a config nobody intends to ship. The lesson is
# cheap here and expensive later: re-read what an arm tests before running it,
# not after.
#
# FOUR CHANGES IN ONE ARM, and that is deliberate rather than sloppy. They are
# fitted jointly and shrunk against each other; splitting them would test four
# configs nobody proposes. If the arm loses, scripts/diagnostics/ can attribute
# it afterwards -- but a win on the bundle is what promotion needs.
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

# The control is the deployed config exactly: per-family half-lives, no
# per-event table, races decay off. The treatment swaps in the table, which also
# disables the per-family map -- the table carries a half-life per event, and
# applying the family override on top would stack two corrections fitted
# independently of one another.
$arms = @(
  @{ name = "ctrl";  params = "" },
  @{ name = "event"; params = "event_params.rds" }
)
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
$env:CITIUS_HALF_LIFE = "365"
Remove-Item "Env:\CITIUS_RACES_HALF_LIFE" -ErrorAction SilentlyContinue

foreach ($arm in $arms) {
  "--- arm $($arm.name): event_params '$($arm.params)' $(Get-Date) ---" |
    Out-File -Append -Encoding utf8 $LOG
  if ($arm.params) { $env:CITIUS_EVENT_PARAMS = $arm.params }
  else { Remove-Item "Env:\CITIUS_EVENT_PARAMS" -ErrorAction SilentlyContinue }
  $env:CITIUS_BT_CACHE = "bt_cache_evparams_$($arm.name)"
  $env:CITIUS_BT_OUT   = "backtest_evparams_$($arm.name).rds"
  $out = "citiusdata\data\backtest_evparams_$($arm.name).rds"

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
  $env:CITIUS_GOAL_ARM = "backtest_evparams_$($arm.name).rds"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" `
            "citiusdata\data\goal_by_event_evparams_$($arm.name).csv" -Force
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
