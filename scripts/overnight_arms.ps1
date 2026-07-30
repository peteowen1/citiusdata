# Overnight arm sweep toward the standing target:
#   centred marks MAE on T1 must beat the five-race baseline (currently +2.78%).
#
# Each arm is a single flip of one thing, so a result is attributable. All read
# the same calibration and corpus, so the only difference is the flip.
#
#   flat      sigma_mode = event      per-athlete sigma removed at simulation
#   prior0    prior_weight = 0        no shrink toward the field
#   meettier  meet_tier context       the broken tier label replaced
#   hl180     half_life = 180         less dilution from old marks
#   noage     aging off               is the age projection earning its place
#
# Sequential, because each holds ~3GB and two at once has OOM-killed runs here
# before. Scores after each so a morning reader sees results, not just caches.
Set-Location C:\dev\citiusverse
$log = "citiusdata\data\overnight.log"
function Say($m) { "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $m | Tee-Object -FilePath $log -Append }

$arms = @(
  @{ name = "prior0";   vars = @{ CITIUS_PRIOR_WEIGHT = "0" } },
  @{ name = "meettier"; vars = @{ CITIUS_BT_MEET_TIER = "1" } },
  @{ name = "hl180";    vars = @{ CITIUS_HALF_LIFE = "180" } },
  @{ name = "noage";    vars = @{ CITIUS_BT_AGING = "" } }
)

foreach ($a in $arms) {
  $n = $a.name
  if (Test-Path "citiusdata\data\backtest_$n.rds") { Say "$n already done, skipping"; continue }
  Say "=== ARM $n starting ==="
  # reset the knobs every time so arms cannot contaminate each other
  $env:CITIUS_PRIOR_WEIGHT = "0.5"
  $env:CITIUS_BT_MEET_TIER = ""
  $env:CITIUS_HALF_LIFE    = "365"
  $env:CITIUS_BT_AGING     = "aging.rds"
  $env:CITIUS_BT_SIGMA_MODE = "athlete"
  foreach ($k in $a.vars.Keys) { Set-Item -Path "env:$k" -Value $a.vars[$k] }
  $env:CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds"
  $env:CITIUS_BT_MEETS = "3000"
  $env:CITIUS_BT_CACHE = "backtest_cache_$n"
  $env:CITIUS_BT_OUT   = "backtest_$n.rds"
  Rscript citiusdata\scripts\backtest_athletics.R *> "citiusdata\data\$n.log"
  if (-not (Test-Path "citiusdata\data\backtest_$n.rds")) { Say "$n FAILED - see $n.log"; continue }
  Say "=== ARM $n scoring ==="
  $env:CITIUS_SCORE_ARM = "backtest_$n.rds"
  $env:CITIUS_SCORE_VS = ""
  $env:CITIUS_SCORE_BASE = "event"
  $env:CITIUS_SCORE_HOLDOUT = "2023-01-01"
  Rscript citiusdata\scripts\score_arm.R *> "citiusdata\data\score_$n.log"
  Get-Content "citiusdata\data\score_$n.log" | Select-String -Pattern "DECISIONS|marks MAE ctr|gold logloss|favourite" |
    ForEach-Object { Say "  $n | $_" }
}
Say "=== overnight sweep complete ==="
