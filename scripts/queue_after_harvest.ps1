# Everything that has to happen after the referenced-competition harvest, in
# order, unattended.
#
# Written as a script rather than run by hand because the order is load-bearing
# and the failure mode is silent: a rebuild step skipped leaves a corpus that
# looks completely normal and is stale, and an arm run against the wrong
# calibration produces a clean scorecard measuring the wrong thing.
#
# Three arms, all on the SAME new corpus so they are mutually comparable:
#   ref3    reference on the new data. Also the pre/post-harvest read, since the
#           old reference is a different vintage and score_arm.R will refuse an
#           arm-vs-arm comparison across that boundary.
#   cevent  round and tier offsets fitted per EVENT instead of per family.
#   noctx   context adjustment off entirely.
#
# ref3 vs noctx bounds how much the context layer is worth at all; cevent sits
# between them and says whether the layer is salvageable by fitting it at the
# right grain. Running noctx matters even though we expect it to lose: if it
# WINS, the answer is to delete the layer, not to refit it.
#
# Usage:  powershell -File scripts/queue_after_harvest.ps1
$ErrorActionPreference = "Stop"
$repo = "C:\dev\citiusverse"
$scripts = "$repo\citiusdata\scripts"
$log = "$repo\citiusdata\data\queue_after_harvest.log"

function Say($m) {
  $line = "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $m
  Write-Output $line
  Add-Content -Path $log -Value $line -Encoding utf8
}

function Run($script, $envs) {
  Say "=== $script"
  foreach ($k in $envs.Keys) {
    # A PowerShell env var set to "" is UNSET, so Sys.getenv(x, default) in R
    # returns the DEFAULT and the arm silently runs as the reference. That is
    # exactly how the `noage` arm came back bit-identical to ref2 with a
    # straight face. Flags therefore carry real sentinel values, never "".
    Set-Item -Path "Env:$k" -Value $envs[$k]
    Say "    $k=$($envs[$k])"
  }
  & Rscript "$scripts\$script"
  if ($LASTEXITCODE -ne 0) { Say "FAILED ($LASTEXITCODE): $script"; exit 1 }
  foreach ($k in $envs.Keys) { Remove-Item -Path "Env:$k" -ErrorAction SilentlyContinue }
}

Set-Location $repo
Say "queue starting"

# 1. Wait out the harvest. merge_referenced.R refuses to swap the store while
#    another Rscript is running, so this is belt and braces rather than the only
#    guard.
while (Get-Process Rscript -ErrorAction SilentlyContinue) {
  Start-Sleep -Seconds 60
}
Say "harvest finished"

# 2. Merge and rebuild: corpus -> stores -> catalogue -> calibration chain.
Run "merge_referenced.R" @{}

# 3. The per-event calibration, built by refitting ONLY the context block on top
#    of csigma so cevent differs from ref3 in exactly one thing.
Run "build_calibration_cevent.R" @{}

# 4. The arms. Fresh cache directories every time: the runner's skip-if-exists
#    check matched a stale file from a previous day once and an arm never ran.
$arms = @(
  @{ name = "ref3";   env = @{ CITIUS_BT_OUT = "backtest_ref3.rds"
                              CITIUS_BT_CACHE = "backtest_cache_ref3"
                              CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds" } },
  @{ name = "cevent"; env = @{ CITIUS_BT_OUT = "backtest_cevent.rds"
                              CITIUS_BT_CACHE = "backtest_cache_cevent"
                              CITIUS_BT_CALIBRATION = "calibration_corpus_cevent.rds" } },
  @{ name = "noctx";  env = @{ CITIUS_BT_OUT = "backtest_noctx.rds"
                              CITIUS_BT_CACHE = "backtest_cache_noctx"
                              CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds"
                              CITIUS_BT_CONTEXT = "off" } }
)
foreach ($a in $arms) {
  Say "--- arm $($a.name)"
  Run "backtest_athletics.R" $a.env
}

# 5. Score. Each arm against the last-five baseline, then the two candidates
#    against ref3 -- same corpus, so the vintage guard passes and the difference
#    is the variable rather than the data.
foreach ($a in $arms) {
  Say "--- score $($a.name) vs baseline"
  Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$($a.name).rds" }
}
foreach ($n in @("cevent", "noctx")) {
  Say "--- score $n vs ref3"
  Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$n.rds"
                       CITIUS_SCORE_VS = "backtest_ref3.rds" }
}

# 6. The per-family breakdown, which is the reason the arms were run. The
#    aggregate can move a tenth of a percent while the 400m moves eight, and the
#    aggregate is not what is being tested here.
foreach ($n in @("ref3", "cevent", "noctx")) {
  Say "--- family diagnostic $n"
  Run "diagnose_marks.R" @{ CITIUS_DIAG_ARM = "backtest_$n.rds" }
}

Say "queue complete"
