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

# THE GATE IS 3 GB HERE, NOT THE ARM'S 12, AND THAT IS A CORRECTION.
#
# The 12 GB figure was measured from the medal ARM, which OOM-died twice, and
# then applied to this script because both are "the heavy job". They are not the
# same shape. The arm holds a full simulation per meet across 394 meets; this
# reads the parquet store once and then loops months, caching each one, and an
# equivalent T1 build has already completed on this machine. There was never
# evidence this needed 12 GB -- the number was inherited, and inheriting a
# threshold is how a job sits blocked for hours on a constraint it does not have.
#
# So: a floor low enough to start, plus RSS logging per pass, so the real
# requirement is MEASURED rather than assumed. Resumable by month means a kill
# costs one month, which is what makes trying it cheap.
$availMB = (Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue
"available memory at start: $availMB MB" | Out-File -Append -Encoding utf8 $LOG
# 1.2 GB. The prep is now chunked by family, so peak is the largest single
# family rather than the whole corpus, and the run reports its own peak RSS so
# this floor stops being a guess after the first pass.
if ($availMB -lt 1200) {
  "!!! only $availMB MB available, need 1200. NOT STARTING." | Out-File -Append -Encoding utf8 $LOG
  Write-Host "Refusing to start: $availMB MB available, need 1200."
  exit 1
}

# AFTER THIS BUILD, the fit must be weighted or T2 decides everything: 44,944
# held-out T2 finals against T1's 1,641 is 27 to 1. fit_event_params.R takes
# CITIUS_LAB_TIER_WEIGHT, defaulting to 0.037 = 1641/44944, which gives the two
# tiers equal TOTAL evidence -- T2 buys stability without steering the answer.
# marks_scorecard.R scores T1 only regardless, so the headline stays comparable
# with everything measured before today.
$env:CITIUS_LAB_TIERS = "T1_elite,T2_strong"
$env:CITIUS_LAB_CACHE = "marks_lab_cache_t1t2"
$env:CITIUS_LAB_SIGMA_EVERY = "1"

# Resumable by month, so retry rather than restart. Each pass picks up where the
# last left off; the loop exists because a kill mid-month is the expected
# failure, not an unexpected one.
for ($i = 1; $i -le 12; $i++) {
  $free0 = [math]::Round((Get-Counter '\Memory\Available MBytes').CounterSamples.CookedValue)
  "--- prep pass $i, $(Get-Date), available ${free0} MB ---" | Out-File -Append -Encoding utf8 $LOG
  $job = Start-Process -FilePath "Rscript" -ArgumentList "citiusdata\scripts\diagnostics\marks_lab_prep_chunked.R" `
                       -PassThru -NoNewWindow -RedirectStandardOutput "$LOG.pass$i" -RedirectStandardError "$LOG.pass$i.err"
  # Watch peak working set, so the next run knows what this actually costs
  # rather than inheriting a number from a different job.
  $peak = 0
  while (-not $job.HasExited) {
    Start-Sleep -Seconds 10
    try { $job.Refresh(); $m = [math]::Round($job.WorkingSet64/1MB) } catch { $m = 0 }
    if ($m -gt $peak) { $peak = $m }
  }
  Get-Content "$LOG.pass$i" -ErrorAction SilentlyContinue | Out-File -Append -Encoding utf8 $LOG
  Get-Content "$LOG.pass$i.err" -ErrorAction SilentlyContinue | Out-File -Append -Encoding utf8 $LOG
  Remove-Item "$LOG.pass$i", "$LOG.pass$i.err" -ErrorAction SilentlyContinue
  "    pass $i exit $($job.ExitCode), PEAK RSS ${peak} MB" | Out-File -Append -Encoding utf8 $LOG
  Write-Host "pass $i exit $($job.ExitCode), peak RSS ${peak} MB" 
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
