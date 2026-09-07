# Build the marks-lab cache over T1 AND T2, for statistical power.
#
# WHY. The T1_elite test set gives 1,641 held-out finals since 2024, a median of
# 13 races per event. That is why 21 of 44 events come back "not separated" and
# why no amount of parameter tuning can settle them -- the answer is not in the
# data at that size. T2_strong adds 44,944 held-out finals, 27x more, taking the
# median event to 233 races and the men's 100m from 60 to 1,770.
#
# T2 IS A DIFFERENT POPULATION and must not become the headline: fields average
# 7.5 athletes against T1's 18.6, so they are weaker and shallower. The design
# that makes it useful is to FIT on T2, where the sample is large, and SCORE on
# T1, which is what a championship forecast actually predicts. Those sets are
# disjoint, so the overfitting objection does not apply -- and `meet_tier` now
# survives into the cache so a scorer can split them.
#
# This writes to a SEPARATE cache directory. The T1 cache stays exactly as it is
# so every number measured today remains reproducible.
#
# Cost: the prep reads the parquet store and computes a sigma reference per
# month for every tested athlete, and T2 multiplies that population. Expect
# hours. It is resumable -- each month is cached on its own -- so a kill costs
# one month, not the run.
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\t2_lab_build_log.txt"
"=== START $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG

# Same 12 GB gate as the medal arm, measured from three OOM kills on 2026-09-07.
$availMB = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
"available memory at start: $availMB MB" | Out-File -Append -Encoding utf8 $LOG
if ($availMB -lt 12000) {
  "!!! only $availMB MB available, need 12000. NOT STARTING." | Out-File -Append -Encoding utf8 $LOG
  Write-Host "Refusing to start: $availMB MB available, need 12000."
  exit 1
}

$env:CITIUS_LAB_TIERS = "T1_elite,T2_strong"
$env:CITIUS_LAB_CACHE = "marks_lab_cache_t1t2"
$env:CITIUS_LAB_SIGMA_EVERY = "1"

# Resumable by month, so retry rather than restart. Each pass picks up where the
# last left off; the loop exists because a kill mid-month is the expected
# failure, not an unexpected one.
for ($i = 1; $i -le 12; $i++) {
  "--- prep pass $i, $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
  & Rscript "citiusdata\scripts\diagnostics\marks_lab_prep.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if (Select-String -Path $LOG -Pattern "PREP COMPLETE" -Quiet) { break }
  Start-Sleep -Seconds 60
}
if (-not (Select-String -Path $LOG -Pattern "PREP COMPLETE" -Quiet)) {
  "!!! prep did not complete after 12 passes" | Out-File -Append -Encoding utf8 $LOG
  exit 1
}
"--- building the pair table $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\marks_pairs.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"--- building the fair baseline $(Get-Date) ---" | Out-File -Append -Encoding utf8 $LOG
& Rscript "citiusdata\scripts\diagnostics\build_fair_baseline.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date) ===" | Out-File -Append -Encoding utf8 $LOG
