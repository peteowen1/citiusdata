# After a promotion in _deployed.R, every published card is stale until it is
# re-predicted under the new stamp -- the sanity scripts catch it, the site does
# not. This runs run_meet.ps1 for every card meet (entries kept, upload held),
# then publishes once. ~4 min per meet.
#
# MEETS THAT HAVE STARTED ARE SKIPPED (red-team F1, 2026-10-25). On 2026-09-30
# this script rebuilt all six cards 17-51 days after their meets, under a
# calibration whose corpus contained those meets, and overwrote the pre-meet
# cards the site called "locked and never rewritten". A meet whose date_start
# (athletics_calendar.csv, the source of calendar.parquet) is on or before
# today is now skipped and logged. -AllowPostMeet rebuilds them anyway, for a
# deliberate retrospective; export_athletics_blog.R then stamps those cards
# forecast_kind = "retrospective" from their own generated_at, so they cannot
# be shown as locked.
#
#   powershell -NoProfile -File citiusdata\scripts\refresh_all_cards.ps1
#   ... -DryRun          print what would run and what is skipped; runs nothing
#   ... -AllowPostMeet   also rebuild meets that have started (retrospective)
#   log: citiusdata\refresh_cards_log.txt ; aborts on the first failing meet
param(
  [string[]]$Meets = @("birmingham2026", "lausanne2026", "silesia2026", "zurich2026", "brussels2026", "budapest2026"),
  [switch]$AllowPostMeet,
  [switch]$DryRun,
  # UTC date, matching _forecast_kind.R's 00:00 UTC cut-off on date_start.
  [string]$Today = ((Get-Date).ToUniversalTime().ToString("yyyy-MM-dd"))
)
$ROOT = "C:\dev\citiusverse"
$LOG  = "$ROOT\citiusdata\refresh_cards_log.txt"
$CAL  = "$ROOT\citiusdata\data\athletics_calendar.csv"
if ($DryRun) { $LOG = [System.IO.Path]::GetTempFileName() }

function Log([string]$msg, [string]$level = "info") {
  $msg | Out-File -Append -Encoding utf8 $LOG
  switch ($level) {
    "warn"  { Write-Warning $msg }
    "error" { Write-Host $msg -ForegroundColor Red }
    default { Write-Host $msg }
  }
}

"=== START $(Get-Date -Format s) (today = $Today, AllowPostMeet = $AllowPostMeet, DryRun = $DryRun) ===" | Out-File -Encoding utf8 $LOG

$cal = Import-Csv $CAL
$todayD = [datetime]::ParseExact($Today, "yyyy-MM-dd", $null)
$run = @()
foreach ($m in $Meets) {
  $row = $cal | Where-Object { $_.meet_id -eq $m }
  if (-not $row) { Log "!!! $m is not in $CAL -- stopping" "error"; exit 1 }
  $start = [datetime]::ParseExact($row.date_start, "yyyy-MM-dd", $null)
  if ($start -le $todayD) {
    if ($AllowPostMeet) {
      Log "!!! $m started $($row.date_start) (on/before $Today): REBUILDING ANYWAY (-AllowPostMeet). The card will publish as forecast_kind = retrospective." "warn"
      $run += $m
    } else {
      Log "!!! SKIP $m -- meet started $($row.date_start), on/before $Today. A card rebuilt now is not a forecast. Pass -AllowPostMeet for a deliberate retrospective rebuild." "warn"
    }
  } else {
    $run += $m
  }
}

if ($run.Count -eq 0) {
  Log "Nothing to refresh: every requested meet has started. Not publishing." "warn"
  "=== ALL DONE $(Get-Date -Format s) (nothing run) ===" | Out-File -Append -Encoding utf8 $LOG
  exit 0
}
Log "Will refresh: $($run -join ', ')"
if ($DryRun) {
  Log "DRY RUN: would run run_meet.ps1 <meet> -SkipUpload -SkipEntries -SkipExport for each, then export_athletics_blog.R $($run -join ' ')"
  Log "(dry-run log: $LOG)"
  exit 0
}

Set-Location $ROOT
foreach ($m in $run) {
  "--- $m start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  & pwsh -NoProfile -File "$ROOT\citiusdata\scripts\run_meet.ps1" $m -SkipUpload -SkipEntries -SkipExport 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if ($LASTEXITCODE -ne 0) { "!!! $m failed (exit $LASTEXITCODE) -- stopping before publish" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
  "--- $m done $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
}
# Publish ONLY the meets refreshed here. With no arguments the export rebuilds
# every card on disk, which would republish the skipped meets too.
"--- publish start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
Set-Location $ROOT
& Rscript citiusdata\scripts\export_athletics_blog.R @run 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date -Format s) (exit $LASTEXITCODE) ===" | Out-File -Append -Encoding utf8 $LOG
