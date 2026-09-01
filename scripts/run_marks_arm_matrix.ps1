# The 2x2 arm matrix for the marks-vs-last-5 goal.
#
#   pwsh citiusdata/scripts/run_marks_arm_matrix.ps1
#   pwsh citiusdata/scripts/run_marks_arm_matrix.ps1 -Only shrink
#
# TWO INDEPENDENT MECHANISMS, hence four cells rather than a single A/B. The
# marks bias decomposes into a near-CONSTANT model-specific offset (+1.546pp,
# matching the tier+round lift that estimate_ability() removes from history and
# only project_championship() ever puts back) and a GRADIENT that the naive
# last-5 baseline shares -- cor(model bias, last-5 bias) = +0.884, driven by
# sigma_within at r=+0.706, the signature of regression to the mean under
# selection. project_tier/project_round target the first; selection shrinkage
# targets the second. Running them together without the singles would leave us
# unable to say which did the work, or whether they double-count.
#
# RUN ORDER IS NOT ARBITRARY: ctrl first. If ctrl does not reproduce the
# deployed numbers, everything after it is measuring a setup bug, and there is
# no point spending hours to find that out at the end.
[CmdletBinding()]
param(
  [ValidateSet("all", "ctrl", "proj", "shrink", "both")] [string]$Only = "all",
  [string]$TierShrink  = "0.5",   # project_tier()'s MEASURED default
  [string]$RoundShrink = "1",     # project_round() has no measured shrink
  [string]$SelShrink   = "1.4",   # lambda -- NOT validated, needs its own sweep
  [string]$SelSigma    = "event", # "event" (measured) or "athlete" (variant)
  [switch]$SkipPrecheck
)
$ErrorActionPreference = "Stop"
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

$VERSE = "C:\dev\citiusverse"
Set-Location $VERSE

function Fail($m) { Write-Host "`n  ABORTED: $m" -ForegroundColor Red; exit 1 }

# ---- refuse to start on top of a live run -----------------------------------
$live = @(Get-Process -Name Rscript -ErrorAction SilentlyContinue)
if ($live.Count -gt 0) {
  Fail "$($live.Count) Rscript process(es) already running. Two backtests on one box contend for CPU and the timings become meaningless; a previous session also lost hours to an OOM kill from exactly this. Wait."
}

# ---- the patch must be applied ----------------------------------------------
$bt = Get-Content "citiusdata\scripts\backtest_athletics.R" -Raw
if ($bt -notmatch "CITIUS_BT_SEL_SHRINK") {
  Fail "backtest_athletics.R has no CITIUS_BT_SEL_SHRINK. Run: Rscript citiusdata\scripts\apply_selection_shrinkage_patch.R"
}

# ---- precondition: sigma coverage -------------------------------------------
# An event with no fitted sigma shrinks by zero, which makes the shrink arms
# silently PARTIAL. Assert before spending the compute, not after.
if (-not $SkipPrecheck) {
  Write-Host "`n  sigma coverage precheck..." -NoNewline
  & Rscript "citiusdata\scripts\check_sigma_coverage.R" | Out-Host
  if ($LASTEXITCODE -ne 0) { Fail "sigma coverage check failed -- the shrink arms would be partial." }
}

# ---- the matrix --------------------------------------------------------------
# CACHE AND OUT MOVE TOGETHER. backtest_athletics.R documents a collision where
# an A/B came back a dead heat because two arms shared one of them; each cell
# gets its own of both, named identically, so they cannot drift apart.
$arms = @(
  @{ name = "ctrl";   tier = "";           round = "";            sel = ""         },
  @{ name = "proj";   tier = $TierShrink;  round = $RoundShrink;  sel = ""         },
  @{ name = "shrink"; tier = "";           round = "";            sel = $SelShrink },
  @{ name = "both";   tier = $TierShrink;  round = $RoundShrink;  sel = $SelShrink }
)
if ($Only -ne "all") { $arms = $arms | Where-Object { $_.name -eq $Only } }

foreach ($a in $arms) {
  $n = $a.name
  Write-Host "`n=== arm: $n ===" -ForegroundColor Cyan
  $env:CITIUS_BT_CACHE         = "backtest_cache_mk_$n"
  $env:CITIUS_BT_OUT           = "backtest_mk_$n.rds"
  $env:CITIUS_BT_PROJECT_TIER  = $a.tier
  $env:CITIUS_BT_PROJECT_ROUND = $a.round
  $env:CITIUS_BT_SEL_SHRINK    = $a.sel
  $env:CITIUS_BT_SEL_SIGMA     = $SelSigma

  $t0 = Get-Date
  & Rscript "citiusdata\scripts\backtest_athletics.R" 2>&1 | Tee-Object -FilePath "citiusdata\marks_arm_${n}_log.txt" | Out-Host
  if ($LASTEXITCODE -ne 0) { Fail "arm '$n' exited $LASTEXITCODE. Nothing downstream is trustworthy." }
  Write-Host ("  {0} done in {1:N1} min" -f $n, ((Get-Date) - $t0).TotalMinutes) -ForegroundColor Green
}

# clear, so a later interactive run in this shell is not silently an arm
Remove-Item Env:\CITIUS_BT_PROJECT_TIER, Env:\CITIUS_BT_PROJECT_ROUND, `
            Env:\CITIUS_BT_SEL_SHRINK, Env:\CITIUS_BT_SEL_SIGMA, `
            Env:\CITIUS_BT_CACHE, Env:\CITIUS_BT_OUT -ErrorAction SilentlyContinue

# ---- score ------------------------------------------------------------------
# T1 is what decides (OPTIMISATION-FRAMEWORK.md). Report all six metrics: a
# level fix that improves marks while costing gold Brier is not a win, and the
# selection arm in particular is NOT uniform across a field, so it can move the
# ordering-sensitive metrics too.
Write-Host "`n=== scoring vs ctrl ===" -ForegroundColor Cyan
foreach ($n in @("proj", "shrink", "both")) {
  if (-not (Test-Path "citiusdata\data\backtest_mk_$n.rds")) { continue }
  Write-Host "`n--- $n vs ctrl ---" -ForegroundColor Yellow
  $env:CITIUS_SCORE_ARM = "backtest_mk_$n.rds"
  $env:CITIUS_SCORE_VS  = "backtest_mk_ctrl.rds"
  & Rscript "citiusdata\scripts\score_arm.R" 2>&1 | Out-Host
}
Remove-Item Env:\CITIUS_SCORE_ARM, Env:\CITIUS_SCORE_VS -ErrorAction SilentlyContinue

# ---- the actual goal: beat the last-5 baseline -------------------------------
# score_arm.R compares arm against arm. The GOAL is stated against a naive
# mean-of-last-5, so score every cell against that too, or we will have
# optimised a number nobody asked about.
Write-Host "`n=== vs naive last-5 / SB / PB baselines ===" -ForegroundColor Cyan
foreach ($n in @("ctrl", "proj", "shrink", "both")) {
  if (-not (Test-Path "citiusdata\data\backtest_mk_$n.rds")) { continue }
  Write-Host "`n--- $n ---" -ForegroundColor Yellow
  $env:CITIUS_MARKS_ARM = "backtest_mk_$n.rds"
  & Rscript "citiusdata\scripts\check_marks_vs_naive_baselines.R" 2>&1 | Out-Host
}
Remove-Item Env:\CITIUS_MARKS_ARM -ErrorAction SilentlyContinue

Write-Host "`nDone. Decide on T1 centred marks MAE against last-5, with bias and the" -ForegroundColor Green
Write-Host "probability metrics read alongside it -- not MAE alone." -ForegroundColor Green
