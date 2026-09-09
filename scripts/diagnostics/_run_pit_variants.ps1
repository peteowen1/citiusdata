# PIT coverage, three variants in sequence (each ~4 min, ~2.5 GB):
#   finals, debias OFF   -- how much of the finals pessimism is the new debias
#   heats,  debias ON    -- is the centring round-dependent (finals fast, heats slow)
#   all,    debias ON    -- the pooled picture the goal metric sees
# Athlete arm only: the event arm is already measured wider on every cut.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$env:CITIUS_PIT_ARMS = "athlete"
# Second pass (16:20): the once-only fit at FROM read six months of in-season
# progression as pessimism. CITIUS_PIT_REFIT=monthly (now the script default)
# refits at each month start. Tags carry _monthly so the first pass stays readable.
$env:CITIUS_PIT_REFIT = "monthly"
$env:CITIUS_PIT_FROM  = "2024-01-01"
$env:CITIUS_PIT_ROUND = "final"
Remove-Item Env:\CITIUS_SIGMA_PSEUDO_N -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_SIGMA_SCALE -ErrorAction SilentlyContinue
Remove-Item Env:\CITIUS_PIT_SIGMA_PARTS -ErrorAction SilentlyContinue
# Fourth pass (20:40): the context-conditional condition_sd. Same deployed path
# (aging on, debias on) on the _ctxsd calibration with the context passed, on
# finals and on heats. The baseline is pit_coverage_log_finals_aging1_debias1.txt
# (same code path, context off).
$env:CITIUS_PIT_CAL          = "calibration_corpus_wac_coast_0904_ctxsd.rds"
$env:CITIUS_PIT_AGING        = "1"
$env:CITIUS_PIT_DEBIAS       = "1"
$env:CITIUS_PIT_DEBIAS_FILE  = ""
$env:CITIUS_PIT_COND_CONTEXT = "1"
foreach ($v in @(@("final", "_finals_ctx_on"), @("heat", "_heats_ctx_on"))) {
  $env:CITIUS_PIT_ROUND = $v[0]
  $env:CITIUS_PIT_TAG   = $v[1]
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$($v[1]).txt"
}
"PIT VARIANTS DONE $(Get-Date)" | Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_variants_done.txt"
