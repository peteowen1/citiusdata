# VARIANCE CHAIN, second pass (2026-09-06, 22:45). The first pass fitted the
# per-family scales on H1 2023 alone (18-52 races per family) and hit the sd
# floor for sprints; pooled coverage landed on target (0.487 / 0.895) but the
# families scattered. This fits on THREE half-seasons (2022, 2023, 2025) with
# sigma_marks as the individual term, and validates on 2024 alone.
#
#   1. 2022 finals, ctxsd, context on, unscaled   -> _finals_2022_ctx  (fit)
#   2. 2025 finals, same                           -> _finals_2025_ctx  (fit; 2023 exists)
#   3. 2023 finals rerun with the sigma_marks column -> _finals_2023_ctx (overwrites)
#   4. fit_spread_scales.R on the three             -> ..._ctxsd_scaled2.rds
#   5. 2024 finals, scaled2                          -> _finals_2024_scaled2
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\spread_chain2_log.txt"
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
Run-Pit "2022-01-01" "calibration_corpus_wac_coast_0904_ctxsd.rds" "_finals_2022_ctx"
Run-Pit "2025-01-01" "calibration_corpus_wac_coast_0904_ctxsd.rds" "_finals_2025_ctx"
Run-Pit "2023-01-01" "calibration_corpus_wac_coast_0904_ctxsd.rds" "_finals_2023_ctx"
"--- fit_spread_scales (2022+2023+2025) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_SCALES_ROWS = "pit_coverage_rows_finals_2022_ctx.csv,pit_coverage_rows_finals_2023_ctx.csv,pit_coverage_rows_finals_2025_ctx.csv"
$env:CITIUS_SCALES_SRC  = "calibration_corpus_wac_coast_0904_ctxsd.rds"
$env:CITIUS_SCALES_OUT  = "calibration_corpus_wac_coast_0904_ctxsd_scaled2.rds"
& Rscript "citiusdata\scripts\fit_spread_scales.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
Run-Pit "2024-01-01" "calibration_corpus_wac_coast_0904_ctxsd_scaled2.rds" "_finals_2024_scaled2"
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
