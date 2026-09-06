# PIT coverage, three variants in sequence (each ~4 min, ~2.5 GB):
#   finals, debias OFF   -- how much of the finals pessimism is the new debias
#   heats,  debias ON    -- is the centring round-dependent (finals fast, heats slow)
#   all,    debias ON    -- the pooled picture the goal metric sees
# Athlete arm only: the event arm is already measured wider on every cut.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$env:CITIUS_PIT_ARMS = "athlete"
foreach ($v in @(@("final", "0", "_finals_debias0"), @("heat", "1", "_heats_debias1"), @("all", "1", "_all_debias1"))) {
  $env:CITIUS_PIT_ROUND  = $v[0]
  $env:CITIUS_PIT_DEBIAS = $v[1]
  $env:CITIUS_PIT_TAG    = $v[2]
  & Rscript "citiusdata\scripts\diagnostics\pit_coverage_check.R" 2>&1 |
    Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_coverage_log$($v[2]).txt"
}
"PIT VARIANTS DONE $(Get-Date)" | Out-File -Encoding utf8 "C:\dev\citiusverse\citiusdata\pit_variants_done.txt"
