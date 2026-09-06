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
# Third pass (18:05): the aging projection was missing from this harness while
# the backtest applies it. Deployed path exactly (aging on, debias on), then
# aging on with debias off, then the recent-window offsets file.
foreach ($v in @(@("1", "1", "", "_finals_aging1_debias1"),
                 @("1", "0", "", "_finals_aging1_debias0"),
                 @("1", "1", "family_pool_offsets_recent.rds", "_finals_aging1_recent"))) {
  $env:CITIUS_PIT_AGING       = $v[0]
  $env:CITIUS_PIT_DEBIAS      = $v[1]
  $env:CITIUS_PIT_DEBIAS_FILE = $v[2]
  $env:CITIUS_PIT_TAG         = $v[3]
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$($v[3]).txt"
}
"PIT VARIANTS DONE $(Get-Date)" | Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_variants_done.txt"
