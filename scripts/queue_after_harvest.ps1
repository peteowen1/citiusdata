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

# $Fatal separates the two kinds of step. A rebuild step failing invalidates
# everything after it, so the queue stops. An ARM failing costs only that arm --
# stopping there would throw away the other two for no reason, and an unattended
# run that dies at step one of three has wasted the night.
# Result is reported through $script:LastOk, NOT a return value. In PowerShell a
# function emits everything it writes, so `return $true` would come back as
# [all of Rscript's stdout..., $true] and `if (Run ...)` would test a non-empty
# ARRAY -- always truthy, so a failed arm would count as succeeded. The stdout
# has to keep flowing to the console and log, so the flag goes out of band.
$script:LastOk = $true
function Run($script, $envs, $Fatal = $true) {
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
  $code = $LASTEXITCODE
  foreach ($k in $envs.Keys) { Remove-Item -Path "Env:$k" -ErrorAction SilentlyContinue }
  if ($code -ne 0) {
    Say "FAILED ($code): $script"
    if ($Fatal) { Say "fatal - queue stopped"; exit 1 }
    $script:LastOk = $false
    return
  }
  $script:LastOk = $true
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
#
#    CITIUS_BT_MEETS IS NOT OPTIONAL. It defaults to 25, so leaving it unset
#    scores 25 meets instead of ~3,000 -- and nothing fails. Each arm finishes
#    early and produces a complete-looking scorecard on a sample far too small to
#    separate anything, which is worse than an error because it gets believed.
#    The previous overnight runner set 3000; rebaseline_chain.R's own closing
#    instructions say 2000. Set it explicitly on every arm.
$MEETS = "3000"
$arms = @(
  @{ name = "ref3";   env = @{ CITIUS_BT_OUT = "backtest_ref3.rds"
                              CITIUS_BT_CACHE = "backtest_cache_ref3"
                              CITIUS_BT_MEETS = $MEETS
                              CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds" } },
  @{ name = "cevent"; env = @{ CITIUS_BT_OUT = "backtest_cevent.rds"
                              CITIUS_BT_CACHE = "backtest_cache_cevent"
                              CITIUS_BT_MEETS = $MEETS
                              CITIUS_BT_CALIBRATION = "calibration_corpus_cevent.rds" } },
  @{ name = "noctx";  env = @{ CITIUS_BT_OUT = "backtest_noctx.rds"
                              CITIUS_BT_CACHE = "backtest_cache_noctx"
                              CITIUS_BT_MEETS = $MEETS
                              CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds"
                              CITIUS_BT_CONTEXT = "off" } }
)
$ran = @()
foreach ($a in $arms) {
  Say "--- arm $($a.name)"
  Run "backtest_athletics.R" $a.env -Fatal:$false
  if ($script:LastOk) { $ran += $a.name }
  else { Say "arm $($a.name) failed; continuing with the rest" }
}
Say "arms completed: $($ran -join ', ')"
if ($ran.Count -eq 0) { Say "no arms completed - nothing to score"; exit 1 }

# 5. Score. Each arm against the last-five baseline, then the two candidates
#    against ref3 -- same corpus, so the vintage guard passes and the difference
#    is the variable rather than the data.
foreach ($n in $ran) {
  Say "--- score $n vs baseline"
  Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$n.rds" } -Fatal:$false
}
# Arm-vs-arm only where ref3 actually exists to compare against.
if ($ran -contains "ref3") {
  foreach ($n in @("cevent", "noctx")) {
    if ($ran -notcontains $n) { continue }
    Say "--- score $n vs ref3"
    Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$n.rds"
                         CITIUS_SCORE_VS = "backtest_ref3.rds" } -Fatal:$false
  }
} else {
  Say "ref3 missing; skipping arm-vs-arm scoring"
}

# 6. The per-family breakdown, which is the reason the arms were run. The
#    aggregate can move a tenth of a percent while the 400m moves eight, and the
#    aggregate is not what is being tested here.
foreach ($n in $ran) {
  Say "--- family diagnostic $n"
  Run "diagnose_marks.R" @{ CITIUS_DIAG_ARM = "backtest_$n.rds" } -Fatal:$false
}

# 7. The pre-registered rules, applied by code rather than by whoever reads the
#    numbers first. Requires all three arms, so it is skipped if any failed.
if ($ran.Count -eq 3) {
  Say "--- pre-registered evaluation"
  Run "evaluate_prereg.R" @{} -Fatal:$false
} else {
  Say "skipping pre-registered evaluation: needs all three arms, got $($ran -join ', ')"
}

Say "queue complete"
