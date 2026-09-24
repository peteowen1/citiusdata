cd 'C:/dev/citiusverse'
$guards = @(
  'check_form_anchors','check_marks_vs_wr','check_race_key_contiguity',
  'check_form_seed_collisions','check_dropped_events','check_ceil_ranking',
  'check_coldstart_share','check_cold_coverage','check_elite_panel',
  'check_panel_marks','check_seeded_best','check_export_blend',
  'check_level_by_rank','check_thin_evidence','check_script_hygiene',
  'check_top3_2026','check_active_filter','check_concordance_by_event'
)
# Wait for free memory before each guard. Every one of these loads the full
# corpus, and running the suite alongside a display rebuild produced two
# failures at exit -1 - process termination, not an assertion - on scripts that
# pass standalone. A suite that fails falsely under load is as damaging as one
# that passes falsely, because the next real failure gets waved through as
# "probably memory again".
foreach ($g in $guards) {
  $waited = 0
  while ($waited -lt 120) {
    $freeGB = [math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1MB, 1)
    if ($freeGB -ge 4) { break }
    Start-Sleep -Seconds 10; $waited += 10
  }
  # Guards live in scripts/diagnostics/ since de4d1c0 (the reorganise). This
  # runner kept the old path, and Rscript answers a missing file with a
  # SEGFAULT (exit -1073741819), not "cannot open file" -- so every guard read
  # as an access-violation FAIL and the suite verified nothing until
  # 2026-09-19. A missing script is now its own loud failure.
  $path = "citiusdata/scripts/diagnostics/$g.R"
  if (-not (Test-Path $path)) { Write-Output "===== $g : FAIL (script not found at $path) ====="; continue }
  $out = & Rscript $path 2>&1 | Out-String
  $code = $LASTEXITCODE
  $status = if ($code -eq 0) { 'PASS' } else { 'FAIL' }
  Write-Output "===== $g : $status (exit $code) ====="
  if ($code -ne 0) {
    ($out -split "`n" | Select-String -Pattern 'Error|error:|stopifnot|FAIL' | Select-Object -First 4) -join "`n"
  }
}
Write-Output "GUARDS COMPLETE"
