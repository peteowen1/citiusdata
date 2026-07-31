# Resume the run after the background tasks were killed at 17:13.
#
# noctx died at 378 of 380 meets and never wrote its artefact. backtest_athletics.R
# caches per meet and skips what it already has, so re-running costs the two
# remaining meets plus assembly rather than another 90 minutes -- the same
# property that recovered the `flat` arm on 2026-07-30.
#
# Structured so each stage is independently resumable, because whatever killed
# the last run can kill this one.
$ErrorActionPreference = "Stop"
$repo = "C:\dev\citiusverse"
$scripts = "$repo\citiusdata\scripts"
$log = "$repo\citiusdata\data\queue_after_harvest.log"
function Say($m) {
  $line = "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $m
  Write-Output $line; Add-Content -Path $log -Value $line -Encoding utf8
}
function Run($script, $envs) {
  Say "=== $script"
  foreach ($k in $envs.Keys) { Set-Item -Path "Env:$k" -Value $envs[$k] }
  & Rscript "$scripts\$script"
  $code = $LASTEXITCODE
  foreach ($k in $envs.Keys) { Remove-Item -Path "Env:$k" -ErrorAction SilentlyContinue }
  if ($code -ne 0) { Say "FAILED ($code): $script"; exit 1 }
}
Set-Location $repo
Say "resume: finishing noctx from cache"

Run "backtest_athletics.R" @{ CITIUS_BT_OUT = "backtest_noctx.rds"
                             CITIUS_BT_CACHE = "backtest_cache_noctx"
                             CITIUS_BT_MEETS = "3000"
                             CITIUS_BT_CALIBRATION = "calibration_corpus_csigma.rds"
                             CITIUS_BT_CONTEXT = "off" }

foreach ($n in @("ref3", "cevent", "noctx")) {
  Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$n.rds" }
}
foreach ($n in @("cevent", "noctx")) {
  Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_$n.rds"; CITIUS_SCORE_VS = "backtest_ref3.rds" }
}
Run "evaluate_prereg.R" @{}

Say "=== mtier: calibration then arm"
Run "build_calibration_mtier.R" @{}
Run "backtest_athletics.R" @{ CITIUS_BT_OUT = "backtest_mtier.rds"
                              CITIUS_BT_CACHE = "backtest_cache_mtier"
                              CITIUS_BT_MEETS = "3000"
                              CITIUS_BT_CALIBRATION = "calibration_corpus_mtier.rds"
                              CITIUS_BT_MEET_TIER = "on" }
Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_mtier.rds" }
Run "score_arm.R" @{ CITIUS_SCORE_ARM = "backtest_mtier.rds"; CITIUS_SCORE_VS = "backtest_ref3.rds" }
Say "resume complete"
