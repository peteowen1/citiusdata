# Harvest every meet in wa_calendar_missing_meets_priority.csv that this
# repo doesn't have yet, in tier-priority order (OW/GW/GL/DF/A/B/C/D/E),
# skipping F -- the lowest code, ~80% of the gap, lowest model value (see
# docs/reference/modelling-traps.md, "R1 vs M1").
#
# One Rscript process per meet via the existing, proven harvest_wa_results.R
# -- reused unmodified rather than re-implemented, at the cost of each call
# re-discovering the WA endpoint (small, ~1-2s overhead per meet). Writes
# <competition_id>_raw_results.rds per meet; does NOT touch
# championship_results.rds -- that is a separate, single combined append
# step once harvesting is done, not 265 individual appends (each of which
# would pay its own ~25s load/save + corpus rebuild cost).
#
# Usage: powershell -File citiusdata/scripts/harvest_missing_by_tier.ps1
$ErrorActionPreference = "Continue"
$verse = "C:\dev\citiusverse"
Set-Location $verse
$csv = Import-Csv "citiusdata\data\wa_calendar_missing_meets_priority.csv"
$order = @("OW","GW","GL","DF","A","B","C","D","E")   # F excluded deliberately
# BUG FIXED 2026-09-16: `Sort-Object key1, key2 -Descending` applies
# -Descending to EVERY key, not just the trailing one -- the first run of
# this script sorted tier priority DESCENDING (E first, OW last), the exact
# opposite of "in order of tier". All 265 meets still got harvested (fast
# enough that ordering didn't matter that run), but a future run relying on
# priority-if-interrupted needs this fixed. Per-key direction requires a
# hashtable, not a second positional argument.
$queue = $csv | Where-Object { $order -contains $_.meet_code } |
  Sort-Object @{Expression={$order.IndexOf($_.meet_code)}; Ascending=$true},
              @{Expression="start_date"; Descending=$true}

$log = "citiusdata\data\harvest_missing_by_tier_20260916.log"
$total = $queue.Count
$i = 0
$ok = 0; $fail = 0
"[$('{0:HH:mm:ss}' -f (Get-Date))] starting: $total meets, tiers $($order -join ',')" | Tee-Object -FilePath $log -Append

foreach ($row in $queue) {
  $i++
  $comp = $row.competition_id
  $out = "citiusdata\data\$($comp)_raw_results.rds"
  if (Test-Path $out) {
    "[$('{0:HH:mm:ss}' -f (Get-Date))] $i/$total [$($row.meet_code)] $comp already harvested, skipping" | Tee-Object -FilePath $log -Append | Out-Null
    $ok++
    continue
  }
  $env:CITIUS_COMP = $comp
  $env:CITIUS_MEET = $comp
  $t0 = Get-Date
  $res = & Rscript "citiusdata/scripts/harvest_wa_results.R" *>&1
  $rc = $LASTEXITCODE
  $secs = [int]((Get-Date) - $t0).TotalSeconds
  $status = if ($rc -eq 0) { $ok++; "OK" } else { $fail++; "FAIL" }
  "[$('{0:HH:mm:ss}' -f (Get-Date))] $i/$total [$($row.meet_code)] $comp ($($row.name)): $status in ${secs}s (ok=$ok fail=$fail)" |
    Tee-Object -FilePath $log -Append | Out-Null
  if ($rc -ne 0) { ($res -join "`n") | Out-File -FilePath $log -Append }
}
"[$('{0:HH:mm:ss}' -f (Get-Date))] DONE: $ok ok, $fail failed, of $total" | Tee-Object -FilePath $log -Append
