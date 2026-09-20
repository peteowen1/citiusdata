# One adjusted-marks arm on the FORM engine, end to end, judged on forecast
# error against the August file and v7 (docs/reviews/adjusted-marks-arms-2026-09-19.md
# was assembled by hand; this is the same chain as a script).
#
#   build_adjusted_marks.R  (ADJ_OUT=adjusted_marks_<tag>.parquet, plus whatever
#                            build env the caller sets: LEVEL_BY, VENUE_INDOOR, ...)
#   form_ratings.R          (SEQ_ADJFILE=that file, SEQ_TAG=adj<tag>, deployed knobs)
#   build_forecast_marks.R  (FORM_TAG=adj<tag>, ADJ_FILE=that file)
#   diagnostics/compare_forecast_arms.R  (paired |error| by family, 2025-26)
#
#   $env:VENUE_INDOOR="1"; $env:LEVEL_BY="career"
#   powershell -NoProfile -File citiusdata\scripts\_run_form_adj_arm.ps1 -Tag v9
#   log: citiusdata\form_adj_arm_<tag>_log.txt     ~45 min (build 4, engine ~35, rest 3)
param([Parameter(Mandatory)][string]$Tag, [string[]]$Vs = @("adjold", "adjv7"))
$ErrorActionPreference = "Continue"
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\form_adj_arm_${Tag}_log.txt"
$ADJ = "adjusted_marks_$Tag.parquet"
"=== START $Tag $(Get-Date -Format s) === build env: LEVEL_BY=$env:LEVEL_BY VENUE_INDOOR=$env:VENUE_INDOOR TIER_DEMEAN=$env:TIER_DEMEAN" | Out-File -Encoding utf8 $LOG

function Step($label, $script, $envs, $expect) {
  "--- $label start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  foreach ($k in $envs.Keys) { Set-Item -Path "Env:$k" -Value $envs[$k] }
  $t0 = Get-Date
  & Rscript $script 2>&1 | Out-File -Append -Encoding utf8 $LOG
  $ok = ($LASTEXITCODE -eq 0) -and (Test-Path $expect) -and ((Get-Item $expect).LastWriteTime -gt $t0)
  "--- $label $(if ($ok) {'done'} else {'FAILED'}) in $([math]::Round(((Get-Date)-$t0).TotalMinutes,1)) min ---" | Out-File -Append -Encoding utf8 $LOG
  if (-not $ok) { "!!! stopping at $label" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
}

Step "build adjusted marks" "citiusdata\scripts\build_adjusted_marks.R" @{ ADJ_OUT = $ADJ } "citiusdata\data\$ADJ"
Step "form engine" "citiusdata\scripts\form_ratings.R" @{
  SEQ_ADJFILE = $ADJ; SEQ_TAG = "adj$Tag"; SEQ_HIST = "1"
  SEQ_SEED_XEV = "1"; SEQ_SEED_XEV_NE = "0.2"; SEQ_DEBUT_PRIOR = "replacement"
} "citiusdata\data\seqv3_history_adj$Tag.parquet"
Step "forecast marks" "citiusdata\scripts\build_forecast_marks.R" @{ FORM_TAG = "adj$Tag"; ADJ_FILE = $ADJ } "citiusdata\data\forecast_marks_adj$Tag.parquet"
"--- compare start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
$env:CITIUS_FA_ARM = "adj$Tag"; $env:CITIUS_FA_VS = ($Vs -join ",")
& Rscript "citiusdata\scripts\diagnostics\compare_forecast_arms.R" 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $Tag $(Get-Date -Format s) (compare exit $LASTEXITCODE) ===" | Out-File -Append -Encoding utf8 $LOG
