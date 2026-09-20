# After adjusted_marks.parquet changes: rebuild everything downstream of the
# form engine, in the order docs/reference/build-recipes.md gives, then the
# forecast table, the params export and the publish. One runner owns the order.
#
#   powershell -NoProfile -File citiusdata\scripts\_promote_form_file.ps1
#   log: citiusdata\promote_form_log.txt   ~15 min (engine 3.5, guards ~3, publish ~6)
#   env it needs: OFFSETS_TAG (venue offsets build) and INDOOR_TAG (indoor
#   coefficients build) for export_conditions_params.R.
param([string]$OffsetsTag = "adjusted_marks_v8", [string]$IndoorTag = "adjusted_marks_v12",
      [string]$From = "")   # resume: skip steps until this label matches (e.g. -From "paris medallists")
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\promote_form_log.txt"
"=== START $(Get-Date -Format s) === offsets $OffsetsTag, indoor $IndoorTag$(if ($From) { ", from '$From'" })" | Out-File -Append -Encoding utf8 $LOG

$script:started = ($From -eq "")
function Step($label, $cmd, $envs, $expect) {
  if (-not $script:started) { if ($label -eq $From) { $script:started = $true } else { "--- $label skipped (-From $From) ---" | Out-File -Append -Encoding utf8 $LOG; return } }
  "--- $label start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  foreach ($k in $envs.Keys) { Set-Item -Path "Env:$k" -Value $envs[$k] }
  $t0 = Get-Date
  & $cmd 2>&1 | Out-File -Append -Encoding utf8 $LOG
  $ok = ($LASTEXITCODE -eq 0) -and ((-not $expect) -or ((Test-Path $expect) -and ((Get-Item $expect).LastWriteTime -gt $t0)))
  "--- $label $(if ($ok) {'done'} else {'FAILED'}) in $([math]::Round(((Get-Date)-$t0).TotalMinutes,1)) min ---" | Out-File -Append -Encoding utf8 $LOG
  if (-not $ok) { "!!! stopping at $label" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
}
foreach ($v in "SEQ_ADJFILE", "SEQ_ADJ") { Remove-Item "Env:\$v" -ErrorAction SilentlyContinue }

Step "form engine (final)" { Rscript citiusdata\scripts\form_ratings.R } @{
  SEQ_TAG = "final"; SEQ_HIST = "1"; SEQ_SEED_XEV = "1"; SEQ_SEED_XEV_NE = "0.2"; SEQ_DEBUT_PRIOR = "replacement"
} "citiusdata\data\seqv3_history_final.parquet"
Step "combined simulation" { Rscript citiusdata\scripts\build_combined_simulation.R } @{ STATE_TAG = "final" } $null
Step "display marks" { Rscript citiusdata\scripts\form_display_marks.R } @{ FORM_TAG = "final" } $null
Step "guard suite" { powershell -NoProfile -File citiusdata\scripts\run_guard_suite.ps1 } @{ FORM_TAG = "final" } $null
Step "paris medallists" { Rscript citiusdata\scripts\diagnostics\check_paris_medallists.R } @{ FORM_TAG = "final" } $null
Step "forecast marks" { Rscript citiusdata\scripts\build_forecast_marks.R } @{ FORM_TAG = "final" } "citiusdata\data\forecast_marks_final.parquet"
Step "conditions params" { Rscript citiusdata\scripts\export_conditions_params.R } @{ OFFSETS_TAG = $OffsetsTag; INDOOR_TAG = $IndoorTag; FORECAST_TAG = "final" } "citiusdata\data\conditions_params\_all.json"
Step "publish" { Rscript citiusdata\scripts\export_athletics_blog.R } @{ FORM_TAG = "final" } $null
"=== ALL DONE $(Get-Date -Format s) ===" | Out-File -Append -Encoding utf8 $LOG
