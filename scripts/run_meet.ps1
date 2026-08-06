# One command to produce and publish a meet's card.
#
#   pwsh run_meet.ps1 birmingham2026
#   pwsh run_meet.ps1 birmingham2026 -SkipUpload
#
# WHY THIS EXISTS. The chain is only ~4 minutes of compute, so the cost was never
# the machine -- it is remembering six scripts, in order, at the right moment,
# for six meets in five weeks, four of them inside seven days. The failure mode
# of getting that wrong is not an error: it is SILENT STALENESS, a page that
# looks fresh over week-old numbers. That is what bit the Glasgow export, and it
# is the failure `_deployed.R` exists to prevent one level down.
#
# It aborts on the FIRST failing step and never proceeds to publish. A partial
# run that uploads is worse than no run at all, because the site then serves a
# self-inconsistent set with a brand-new "as at" stamp over it.
#
# SCOPE: this orchestrates the BIRMINGHAM chain. The Diamond League meets are
# not a parameterisation of it -- no rounds, no entry-list PDF, start lists ~7
# days out -- so they get their own step list when that card is built. The meet
# id is still taken as an argument rather than hardcoded, so adding one is
# editing a table below rather than copying this file.

[CmdletBinding()]
param(
  [Parameter(Mandatory = $true, Position = 0)][string]$MeetId,
  [switch]$SkipUpload,
  [switch]$SkipEntries   # the entry list rarely changes; skip the PDF re-parse
)

$ErrorActionPreference = "Stop"
$VERSE   = "C:\dev\citiusverse"
$SCRIPTS = Join-Path $VERSE "citiusdata\scripts"
$CAL     = Join-Path $VERSE "citiusdata\data\athletics_calendar.csv"

function Fail($msg) { Write-Host "`n  ABORTED: $msg" -ForegroundColor Red; exit 1 }

# --- the meet must be on the calendar ----------------------------------------
# The calendar is the single source of the cutoff, the competition id and how the
# entry list is obtained. Six hardcoded GAMES_START literals is what this
# replaces.
if (-not (Test-Path $CAL)) { Fail "calendar not found at $CAL" }
$meet = Import-Csv $CAL | Where-Object { $_.meet_id -eq $MeetId }
if (-not $meet) {
  $known = (Import-Csv $CAL | ForEach-Object { $_.meet_id }) -join ", "
  Fail "'$MeetId' is not in the calendar. Known meets: $known"
}

# --- step list per meet shape -------------------------------------------------
$STEPS = switch ($MeetId) {
  "birmingham2026" {
    @(
      @{ n = "parse entry list";  f = "parse_birmingham_entries.R";    optional = $SkipEntries },
      @{ n = "resolve athletes";  f = "resolve_birmingham_athletes.R" },
      @{ n = "build rounds";      f = "build_birmingham_rounds.R" },
      @{ n = "predict";           f = "predict_birmingham2026.R" },
      @{ n = "sanity";            f = "sanity_birmingham_card.R" },
      @{ n = "export + publish";  f = "export_athletics_blog.R" }
    )
  }
  default {
    Fail "No step list defined for '$MeetId' yet. Diamond League meets need their own chain (no rounds, no entry PDF) -- see ticket 12."
  }
}

Write-Host ""
Write-Host "  $($meet.name)" -ForegroundColor Cyan
Write-Host "  $($meet.city), $($meet.country)  ·  $($meet.date_start) to $($meet.date_end)"
Write-Host "  cutoff $($meet.prediction_cutoff)  ·  competition $($meet.wa_competition_id)  ·  state $($meet.state)"
if ($SkipUpload) { Write-Host "  UPLOAD SKIPPED" -ForegroundColor Yellow }
Write-Host ""

# A cutoff that does not precede the meet means the forecast could see the meet
# it is forecasting. Check before spending four minutes, not after.
if ([datetime]$meet.prediction_cutoff -ge [datetime]$meet.date_start) {
  Fail "prediction_cutoff ($($meet.prediction_cutoff)) does not precede date_start ($($meet.date_start))"
}

if ($SkipUpload) { $env:CITIUS_SKIP_UPLOAD = "1" }
else { Remove-Item Env:\CITIUS_SKIP_UPLOAD -ErrorAction SilentlyContinue }

$t0 = Get-Date
foreach ($s in $STEPS) {
  if ($s.optional) { Write-Host ("  {0,-20} skipped" -f $s.n) -ForegroundColor DarkGray; continue }
  $path = Join-Path $SCRIPTS $s.f
  if (-not (Test-Path $path)) { Fail "missing script: $($s.f)" }
  $st = Get-Date
  Write-Host ("  {0,-20} running..." -f $s.n) -NoNewline
  $out = & Rscript $path 2>&1
  $code = $LASTEXITCODE
  $sec = [math]::Round(((Get-Date) - $st).TotalSeconds, 1)
  if ($code -ne 0) {
    Write-Host ("`r  {0,-20} FAILED after {1}s" -f $s.n, $sec) -ForegroundColor Red
    Write-Host ""
    $out | Select-Object -Last 25 | ForEach-Object { Write-Host "    $_" }
    Fail "$($s.f) exited $code. Nothing was published beyond this point."
  }
  Write-Host ("`r  {0,-20} ok  {1,6}s" -f $s.n, $sec) -ForegroundColor Green
}

$mins = [math]::Round(((Get-Date) - $t0).TotalMinutes, 1)
Write-Host ""
Write-Host "  Done in $mins min." -ForegroundColor Green
if (-not $SkipUpload) {
  Write-Host "  Published. Verify: https://pub-ee4bf5b599a047f9ac2b9facc1587008.r2.dev/athletics/athletics-manifest.json"
}
Remove-Item Env:\CITIUS_SKIP_UPLOAD -ErrorAction SilentlyContinue
