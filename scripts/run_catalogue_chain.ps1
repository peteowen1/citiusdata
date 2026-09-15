# Run the six-script catalogue chain in order, stopping at the first failure.
#
# WHY A RUNNER. The chain is not optional and the order matters:
# docs/plans/post-reharvest-runbook.md:66-72. Running ONLY the base builder
# drops the catalogue from 32,086 competitions to 6,800, which happened on
# 2026-08-19. augment_catalogue_depth.R must run last.
#
# Every step's stdout AND stderr go to one log. A previous run of the corpus
# rebuild wrote stderr to a separate file, so a hard failure left an empty
# stdout log and looked identical to a long-running job for 40 minutes.
#
# The catalogue is a deployed input (DEPLOYED$calibration is fitted on its
# meet_tier), so it is backed up before anything runs.
#
# DO NOT set $ErrorActionPreference = "Stop" here. With `*>&1`, a native
# command's stderr arrives in the pipeline as ErrorRecord objects, and under
# "Stop" ANY of them terminates the script -- so on 2026-09-15 the harmless
# "package 'data.table' was built under R version 4.5.2" warning aborted the
# chain after step 1, leaving the catalogue at 13,671 rows / 20 cols instead
# of 32,088 / 26. $LASTEXITCODE, checked below, is the only thing that
# actually says whether Rscript failed.
$ErrorActionPreference = "Continue"
$verse = "C:\dev\citiusverse"
$data  = "$verse\citiusdata\data"
$log   = "$data\catalogue_chain.log"
Set-Location $verse

# THE ORDER IS LOAD-BEARING, and the six-step version of this list was wrong.
# post-reharvest-runbook.md:66-72 documents the AUGUST chain; three more
# scripts were added on 2026-09-03/04 and never added to it. Running only the
# six on 2026-09-15 produced a catalogue with 20 of 26 columns -- no
# strength_pb, strength_ew, races_won_ew, wac, tier_source or tier_pre_wa --
# while every step exited 0 and the row count actually went UP.
#
# Why this order and not another:
#   * apply_strength_ew.R RE-DERIVES meet_tier from scratch, so it must run
#     BEFORE the one-way floors in wa_codes and depth, or it erases them
#     silently. It also needs `class` settled, so it runs after the road steps.
#   * augment_catalogue_wa_codes.R states its own prerequisite chain
#     (build -> coverage -> road_majors -> road_half_majors -> this).
#   * augment_catalogue_depth.R runs after wa_codes; it is the last FLOOR.
#   * apply_wa_category_tier.R is last overall: it snapshots `tier_pre_wa`
#     from whatever meet_tier everything else concluded, and on a rerun resets
#     meet_tier back to that snapshot first. It is only idempotent in that slot.
#
# repair_catalogue_metadata.R is deliberately NOT here. It is a one-off repair
# of damage the augment scripts did before they were fixed, and it restores
# from a .bak that predates that damage -- running it against a fresh build
# would reinstate stale values. Check comp_name coverage after a build instead.
$steps = @(
  "build_competition_catalogue.R",
  "augment_catalogue_coverage.R",
  "augment_catalogue_road_majors.R",
  "augment_catalogue_road_half_majors.R",
  "build_strength_ew.R",               # writes strength_ew.parquet
  "apply_strength_ew.R",               # re-derives meet_tier -- before the floors
  "augment_catalogue_wa_codes.R",      # floor
  "augment_catalogue_depth.R",         # floor, last of them
  "apply_wa_category_tier.R"           # MUST be last overall
)

# Back up first: a half-run chain leaves a catalogue with 79% of its rows gone.
$bk = "$data\competition_catalogue.pre_chain_$(Get-Date -Format 'yyyyMMdd').parquet"
if (-not (Test-Path $bk)) {
  Copy-Item "$data\competition_catalogue.parquet" $bk
  "backed up to $(Split-Path $bk -Leaf)" | Tee-Object -FilePath $log -Append
}

$before = (Get-Item "$data\competition_catalogue.parquet").Length
"chain starting $(Get-Date -Format 'HH:mm:ss'), catalogue $([int]($before/1KB)) KB" |
  Tee-Object -FilePath $log -Append

foreach ($s in $steps) {
  $t0 = Get-Date
  "=== $s ===" | Tee-Object -FilePath $log -Append
  # 2>&1 so a failure reaches the log rather than a separate stream nobody reads.
  & Rscript "citiusdata/scripts/$s" *>&1 | Tee-Object -FilePath $log -Append
  $rc = $LASTEXITCODE
  $secs = [int]((Get-Date) - $t0).TotalSeconds
  if ($rc -ne 0) {
    "FAILED: $s exited $rc after ${secs}s -- STOPPING. The catalogue is now PARTIAL; restore from $(Split-Path $bk -Leaf)." |
      Tee-Object -FilePath $log -Append
    exit 1
  }
  $sz = [int]((Get-Item "$data\competition_catalogue.parquet").Length / 1KB)
  "  ok in ${secs}s, catalogue now $sz KB" | Tee-Object -FilePath $log -Append
}

# A chain that exits 0 and still ruins the catalogue has happened twice
# (2026-08-19 rows, 2026-09-15 columns), so verify against the backup rather
# than trusting nine zero exit codes. NOT by file size: bytes cannot tell
# "1,030 new meets" from "six columns gone", and on 2026-09-15 it reported the
# shrink and named the wrong cause.
"=== verify_catalogue_chain.R ===" | Tee-Object -FilePath $log -Append
& Rscript "citiusdata/scripts/verify_catalogue_chain.R" $bk *>&1 |
  Tee-Object -FilePath $log -Append
if ($LASTEXITCODE -ne 0) {
  "CHAIN VERIFY FAILED -- the catalogue is NOT fit to deploy. Restore from $(Split-Path $bk -Leaf)." |
    Tee-Object -FilePath $log -Append
  exit 1
}

"CHAIN COMPLETE $(Get-Date -Format 'HH:mm:ss')" | Tee-Object -FilePath $log -Append
