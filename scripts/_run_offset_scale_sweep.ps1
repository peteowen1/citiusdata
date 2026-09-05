# Sweep the offset SCALE. The [2016, 2020) fit over-corrects the 2020+ era --
# every family flipped from over-optimistic to pessimistic after the debias --
# so the question is how much of that fitted correction to apply.
#
# 1.00 is the unscaled wide-window arm already measured (marks 26 of 54).
# This runs 0.75 and 0.60. Marks-only: these levers cannot move a probability.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\offset_scale_sweep_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

$env:CITIUS_BT_CALIBRATION   = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE         = "athletics_corpus_store"
$env:CITIUS_BT_TIER          = "T1_elite"
$env:CITIUS_BT_MEET_TIER     = "1"
$env:CITIUS_BT_MEETS         = "450"
$env:CITIUS_BT_WORKERS       = "1"
$env:CITIUS_BT_MARKS_ONLY    = "1"
$env:CITIUS_BT_PROJECT_TIER  = "0.5"
$env:CITIUS_BT_FAMILY_DEBIAS = "1"
$env:CITIUS_HALF_LIFE_FAMILY = "road=1095,walk=730,hurdles=180"
Remove-Item Env:\CITIUS_BT_TRAIN_TIERS -ErrorAction SilentlyContinue

foreach ($s in @("0.75", "0.6")) {
  $tag = "s" + ($s -replace "\.", "")
  "=== SCALE $s ($tag) $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_BT_FAMILY_DEBIAS_FILE = "family_pool_offsets_s$s.rds"
  $env:CITIUS_BT_CACHE = "bt_cache_debias_$tag"
  $env:CITIUS_BT_OUT   = "backtest_debias_$tag.rds"

  for ($i = 1; $i -le 4; $i++) {
    & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
    if (Test-Path "citiusdata\data\backtest_debias_$tag.rds") {
      $f = Get-Item "citiusdata\data\backtest_debias_$tag.rds"
      if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
    }
    Start-Sleep -Seconds 30
  }

  "--- scoring scale $s $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  $env:CITIUS_GOAL_ARM = "backtest_debias_$tag.rds"
  $env:CITIUS_GOAL_MARKS_ONLY = "1"
  & Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 |
    Out-File -Append -Encoding utf8 $LOG
  Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_$tag.csv" -Force
  "=== SCALE $s DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
Write-Host "OFFSET SCALE SWEEP DONE"
