# The arm that should have been in the first queue: meet_tier ON.
#
# citius has carried a coherent meet tier since this morning -- T1/T2/T3 on
# competition_catalogue.parquet, anchor-guarded -- and estimate_ability() reads
# it when the results carry a `meet_tier` column. backtest_athletics.R gates that
# behind CITIUS_BT_MEET_TIER, which DEFAULTS TO OFF, and queue_after_harvest.ps1
# never set it. So ref3, cevent and noctx all ran on the feed's `tier` label:
# per-RESULT, varying within a single meet, and labelling Diamond League "low"
# so its marks are adjusted UPWARD by 1.69%.
#
# Built the fix, measured it, left it behind a flag nobody turned on. Same shape
# as the deployed-versus-validated gap _deployed.R exists to close.
#
# It matters more now than when it was measured. The `meettier` arm scored gold
# Brier -0.42% and marks -0.10% on the pre-harvest corpus -- small enough to
# ignore. That corpus did not contain the 2.27M newly harvested results, which
# are overwhelmingly low-tier and are now the bulk of the history feeding every
# ability estimate. A correction worth 0.1% against a little bad history is worth
# re-measuring against a lot of it.
#
# Waits for the running queue so the two never contend for memory: one arm holds
# ~3.7 GB and only ~6.9 GB is free.
#
# Usage:  powershell -File scripts/queue_mtier.ps1
$ErrorActionPreference = "Stop"
$repo = "C:\dev\citiusverse"
$scripts = "$repo\citiusdata\scripts"
$log = "$repo\citiusdata\data\queue_after_harvest.log"

function Say($m) {
  $line = "[{0}] {1}" -f (Get-Date -Format "HH:mm:ss"), $m
  Write-Output $line
  Add-Content -Path $log -Value $line -Encoding utf8
}

Set-Location $repo
Say "mtier queue waiting for the main queue to finish"
while (Get-Process Rscript -ErrorAction SilentlyContinue) { Start-Sleep -Seconds 60 }
Say "main queue done; starting mtier"

# BOTH halves, or neither. Fitting the offsets on meet_tier without applying by
# meet_tier -- or the reverse -- is the mismatch this arm exists to remove.
Say "=== build_calibration_mtier.R (fit tier offsets on meet_tier)"
& Rscript "$scripts\build_calibration_mtier.R"
if ($LASTEXITCODE -ne 0) { Say "FAILED ($LASTEXITCODE): mtier calibration"; exit 1 }

$env:CITIUS_BT_OUT = "backtest_mtier.rds"
$env:CITIUS_BT_CACHE = "backtest_cache_mtier"
$env:CITIUS_BT_MEETS = "3000"
$env:CITIUS_BT_CALIBRATION = "calibration_corpus_mtier.rds"
$env:CITIUS_BT_MEET_TIER = "on"      # a real sentinel: "" would read as UNSET
Say "=== backtest_athletics.R (meet_tier ON)"
& Rscript "$scripts\backtest_athletics.R"
if ($LASTEXITCODE -ne 0) { Say "FAILED ($LASTEXITCODE): mtier arm"; exit 1 }
Remove-Item Env:CITIUS_BT_MEET_TIER -ErrorAction SilentlyContinue

Say "=== score mtier vs baseline"
$env:CITIUS_SCORE_ARM = "backtest_mtier.rds"
& Rscript "$scripts\score_arm.R"
Say "=== score mtier vs ref3"
$env:CITIUS_SCORE_VS = "backtest_ref3.rds"
& Rscript "$scripts\score_arm.R"
Remove-Item Env:CITIUS_SCORE_VS -ErrorAction SilentlyContinue

Say "=== family diagnostic mtier"
$env:CITIUS_DIAG_ARM = "backtest_mtier.rds"
& Rscript "$scripts\diagnose_marks.R"
Say "mtier queue complete"
