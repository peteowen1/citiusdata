# ARM: event-level sigma instead of the per-athlete estimator.
#
# WHY. sigma_estimator_shootout.R (2026-09-06, 24 events, 2024+ hold-out): the
# deployed per-athlete sigma ranks athletes by consistency at Spearman 0.069
# against their own hold-out scatter, and its Gaussian log score is WORSE than
# handing every athlete the event constant even after fitting the best level
# for it (2.137 vs 2.240). The one-sided sigma_rob estimator scores 0.918.
# Shrinking the two-sided race-demeaned sd toward the event with pseudo-n 40
# ties the constant (2.239); pseudo-n 2 (the current .CITIUS_SIGMA_PSEUDO_N) is
# well behind. So: per-athlete spread as currently estimated is noise handed
# to the wrong people, and placings are driven by spread.
#
# CITIUS_BT_SIGMA_MODE=event gives every athlete the event's sigma_target;
# adding "target" to SIGMA_PARTS makes that target the calibration's measured
# sigma_within rather than the registry's cv_prior. No code change, so it is
# safe to launch while another arm has backtest_athletics.R in flight.
#
# THIS IS A RE-TEST. refuted-hypotheses.md records that `sigma_mode = "event"`
# (arm backtest_flat.rds, pre-2026-08-11) LOST on every probability metric,
# gold Brier +2.93% p=5.9e-07, and the write-up concluded the per-athlete
# term is the model pricing consistency. The shoot-out says it does NOT track
# consistency, so one of those readings is wrong or the term carries something
# else (upside/peak). What differs from the refuted run: the target is now the
# measured sigma_within ("target" in SIGMA_PARTS) rather than the registry's
# cv_prior, and the model underneath is the coasting + WAC calibration. If it
# loses again, the next move is decouple_peak, not another sigma knob.
#
# Compare to backtest_ctrl_tierfix.rds (same code, same config, no arm), NOT
# to backtest_wac_trt_0904.rds, which predates the tier-weight fix.
# Marks MAE cannot move (sigma is a spread parameter); this arm is judged on
# medal logloss / Brier per event.
#
#   schtasks /create /tn citius_sigma_event /sc once /st 00:00 /f `
#     /tr "powershell -NoProfile -File C:\dev\citiusverse\citiusdata\scripts\_run_sigma_event_arm.ps1"
#   schtasks /run /tn citius_sigma_event
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\sigma_event_log.txt"
# WAIT for the race-shock arm to finish before starting: three arms at once
# left 585 MB available on 2026-09-06, and memory pressure is what kills these
# runs silently. Polls the shock log for its ALL DONE line, up to 6 hours.
$SHOCK = "C:\dev\citiusverse\citiusdata\shock_complete_log.txt"
"=== QUEUED $(Get-Date), waiting for the shock arm ===" | Out-File -Append -Encoding utf8 $LOG
for ($w = 0; $w -lt 720; $w++) {
  if ((Test-Path $SHOCK) -and (Select-String -Path $SHOCK -Pattern "ALL DONE" -Quiet)) { break }
  Start-Sleep -Seconds 30
}
# The PIT coverage check (diagnostics/pit_coverage_check.R, ~10 min, ~2.5 GB) runs
# here, between the shock arm ending and this arm starting, because it could not
# survive alongside two arms and another session's job (39 MB available at 14:20).
"=== PIT coverage check $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
  Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_BT_CALIBRATION    = "calibration_corpus_wac_coast_0904.rds"
$env:CITIUS_BT_STORE          = "athletics_corpus_store"
$env:CITIUS_BT_CACHE          = "bt_cache_sigma_event"
$env:CITIUS_BT_OUT            = "backtest_sigma_event.rds"
$env:CITIUS_BT_TIER           = "T1_elite"
$env:CITIUS_BT_MEET_TIER      = "1"
$env:CITIUS_BT_MEETS          = "450"
$env:CITIUS_BT_WORKERS        = "1"
$env:CITIUS_HALF_LIFE_FAMILY  = "road=1095,walk=730,hurdles=180"
$env:CITIUS_BT_SIGMA_MODE     = "event"
$env:CITIUS_BT_SIGMA_PARTS    = "estimator,weight,target"
foreach ($v in "CITIUS_BT_ADJUST_RACE", "CITIUS_BT_SHOCK_ADDBACK", "CITIUS_BT_MARKS_ONLY",
              "CITIUS_BT_TRAIN_TIERS", "CITIUS_BT_FAMILY_DEBIAS") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
for ($i = 1; $i -le 6; $i++) {
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Test-Path "citiusdata\data\backtest_sigma_event.rds") {
    $f = Get-Item "citiusdata\data\backtest_sigma_event.rds"
    if ($f.LastWriteTime -gt (Get-Date).AddMinutes(-15)) { break }
  }
  Start-Sleep -Seconds 30
}
"--- scoring $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_GOAL_ARM = "backtest_sigma_event.rds"
Remove-Item Env:\CITIUS_GOAL_MARKS_ONLY -ErrorAction SilentlyContinue
& Rscript "citiusdata\scripts\diagnostics\score_goal_by_event.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Copy-Item "citiusdata\data\goal_by_event.csv" "citiusdata\data\goal_by_event_sigma_event.csv" -Force
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
