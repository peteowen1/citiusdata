# PIT coverage grid over the sigma knobs, then a debias-by-year check.
# Population: T1 finals, monthly as-of refit, athlete arm only, debias OFF for
# the sigma grid (the width question is independent of the centring one).
#
# Sigma grid (4 runs, ~12 min each):
#   parts estimator,weight (deployed one-sided sigma_rob)  x pseudo-n 2 (deployed) / 40
#   parts weight           (two-sided sigma_raw)           x pseudo-n 2 / 40
# Read cover50 / cover90 / pit_sd against 0.50 / 0.90 / 0.289 and the by-family
# spread. The scale to apply, if any, is chosen from these and run afterwards.
#
# Debias-by-year (4 runs): finals in H1 2023 and H1 2025, debias on and off,
# to see whether the 2024 finals pessimism under the debias is a 2024 thing.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\pit_grid_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_PIT_ARMS  = "athlete"
$env:CITIUS_PIT_REFIT = "monthly"
$env:CITIUS_PIT_ROUND = "final"
Remove-Item Env:\CITIUS_SIGMA_SCALE -ErrorAction SilentlyContinue

$env:CITIUS_PIT_DEBIAS = "0"
$env:CITIUS_PIT_FROM   = "2024-01-01"
foreach ($v in @(@("estimator,weight", "2",  "_sg_rob_pn2"),
                 @("estimator,weight", "40", "_sg_rob_pn40"),
                 @("weight",           "2",  "_sg_raw_pn2"),
                 @("weight",           "40", "_sg_raw_pn40"))) {
  $env:CITIUS_PIT_SIGMA_PARTS = $v[0]
  $env:CITIUS_SIGMA_PSEUDO_N  = $v[1]
  $env:CITIUS_PIT_TAG         = $v[2]
  "--- $($v[2]) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$($v[2]).txt"
}

Remove-Item Env:\CITIUS_SIGMA_PSEUDO_N -ErrorAction SilentlyContinue
$env:CITIUS_PIT_SIGMA_PARTS = "estimator,weight"
foreach ($v in @(@("2023-01-01", "1", "_finals_debias1_2023"),
                 @("2023-01-01", "0", "_finals_debias0_2023"),
                 @("2025-01-01", "1", "_finals_debias1_2025"),
                 @("2025-01-01", "0", "_finals_debias0_2025"))) {
  $env:CITIUS_PIT_FROM   = $v[0]
  $env:CITIUS_PIT_DEBIAS = $v[1]
  $env:CITIUS_PIT_TAG    = $v[2]
  "--- $($v[2]) $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$($v[2]).txt"
}
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
