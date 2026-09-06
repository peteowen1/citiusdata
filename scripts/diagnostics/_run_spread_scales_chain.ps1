# VARIANCE CHAIN (2026-09-06 evening): fit per-family spread scales on 2023
# finals, validate on 2024 and 2025. Every run is the deployed path (aging on,
# gated debias on) with the context-conditional condition_sd and, since the
# package change tonight, sigma_marks driving the mark distribution.
#
#   1. 2024 finals, ctxsd calibration, context on, no scales  -> _finals_ctx_marks
#      (re-baseline: same as _finals_ctx_on but with sigma_marks on perf_std)
#   2. 2023 finals, same                                        -> _finals_2023_ctx  (FIT set)
#   3. fit_spread_scales.R on run 2's rows                      -> ..._ctxsd_scaled.rds
#   4. 2024 finals, scaled calibration                          -> _finals_2024_scaled
#   5. 2025 finals, scaled calibration                          -> _finals_2025_scaled
# Judge: cover50 -> 0.50, cover90 -> 0.90, PIT sd -> 0.289, by family, on 4 and 5.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\spread_chain_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_PIT_ARMS         = "athlete"
$env:CITIUS_PIT_REFIT        = "monthly"
$env:CITIUS_PIT_ROUND        = "final"
$env:CITIUS_PIT_AGING        = "1"
$env:CITIUS_PIT_DEBIAS       = "1"
$env:CITIUS_PIT_DEBIAS_FILE  = ""
$env:CITIUS_PIT_COND_CONTEXT = "1"
foreach ($v in "CITIUS_SIGMA_PSEUDO_N", "CITIUS_SIGMA_SCALE", "CITIUS_PIT_SIGMA_PARTS") {
  Remove-Item "Env:\$v" -ErrorAction SilentlyContinue
}
function Run-Pit($from, $cal, $tag) {
  $env:CITIUS_PIT_FROM = $from; $env:CITIUS_PIT_CAL = $cal; $env:CITIUS_PIT_TAG = $tag
  "--- $tag ($from, $cal) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$tag.txt"
}
Run-Pit "2024-01-01" "calibration_corpus_wac_coast_0904_ctxsd.rds" "_finals_ctx_marks"
Run-Pit "2023-01-01" "calibration_corpus_wac_coast_0904_ctxsd.rds" "_finals_2023_ctx"
"--- fit_spread_scales $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_SCALES_ROWS = "pit_coverage_rows_finals_2023_ctx.csv"
$env:CITIUS_SCALES_SRC  = "calibration_corpus_wac_coast_0904_ctxsd.rds"
$env:CITIUS_SCALES_OUT  = "calibration_corpus_wac_coast_0904_ctxsd_scaled.rds"
& Rscript "citiusdata\scripts\fit_spread_scales.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Run-Pit "2024-01-01" "calibration_corpus_wac_coast_0904_ctxsd_scaled.rds" "_finals_2024_scaled"
Run-Pit "2025-01-01" "calibration_corpus_wac_coast_0904_ctxsd_scaled.rds" "_finals_2025_scaled"
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
