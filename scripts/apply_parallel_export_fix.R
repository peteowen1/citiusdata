# Fix: the parallel path in backtest_athletics.R is broken for tier/round arms.
#
# run_meet() reads TIER_SHRINK and ROUND_SHRINK at the project_tier()/
# project_round() calls, but neither was added to `export_vars` when the tier
# arm landed (2026-09-01). The serial path finds them by lexical scoping, so it
# works; every PSOCK worker dies with:
#
#   Error in checkForRemoteErrors(val) :
#     6 nodes produced errors; first error: object 'TIER_SHRINK' not found
#
# Measured, not assumed: CITIUS_BT_WORKERS=8 on 6 meets fails outright on the
# unpatched script and succeeds on the patched copy, producing output that is
# identical() to the serial run across 116 races / 16,548 cells.
#
# REFUSES TO RUN WHILE AN Rscript IS LIVE. Rscript parses incrementally, so
# editing this file mid-run corrupts the running job and surfaces as a syntax
# error at the END of a long run (see r-datatable-gotchas.md, 2026-08-21).
#
# Idempotent: re-running is a no-op once applied.
#
# Usage:  Rscript citiusdata/scripts/apply_parallel_export_fix.R

TARGET <- file.path("citiusdata", "scripts", "backtest_athletics.R")
stopifnot("run from C:/dev/citiusverse" = file.exists(TARGET))

# Count only processes actually RUNNING THE TARGET, not every Rscript on the
# machine. The original guard counted `Get-Process -Name Rscript`, which is both
# too broad and genuinely blocking: this box routinely has unrelated R jobs from
# sibling verses (pannaverse was running two when this was written), none of
# which read backtest_athletics.R and none of which this edit can corrupt.
# Matching on the command line keeps the safety intent exactly -- refuse while a
# job is reading THIS file -- without waiting on jobs that have nothing to do
# with it. It also removes the "1 is us" subtlety: this script's own command
# line names apply_parallel_export_fix.R, so it never self-matches.
# Single quotes only inside the PowerShell -- an earlier version used escaped
# double quotes for -Filter, which system2() mangled into `-Filter
# Name='Rscript.exe'`. PowerShell rejected it, live_n came back NA, and the
# `!is.na()` test then let the edit through: the guard FAILED OPEN, which is
# strictly worse than no guard because it looks like protection. Hence also the
# fail-closed branch below.
live <- suppressWarnings(system2("powershell", c("-NoProfile", "-Command",
  paste0("(Get-CimInstance Win32_Process | ",
         "Where-Object { $_.Name -eq 'Rscript.exe' -and ",
         "$_.CommandLine -like '*backtest_athletics.R*' } | ",
         "Measure-Object).Count")),
  stdout = TRUE, stderr = TRUE))
live_n <- suppressWarnings(as.integer(tail(live, 1)))
if (is.na(live_n)) {
  stop("Could not determine whether backtest_athletics.R is running (the process ",
       "check failed). Refusing to edit rather than guess -- a guard that cannot ",
       "verify must fail closed.\n  Check output: ", paste(live, collapse = " | "))
}
if (live_n > 0L) {
  stop(sprintf(paste0("%d process(es) are running backtest_athletics.R. Editing it ",
                      "now would corrupt the running job. Wait for it to finish."), live_n))
}

src <- readLines(TARGET, warn = FALSE)

if (any(grepl('"TIER_SHRINK", "ROUND_SHRINK"', src, fixed = TRUE))) {
  cat("Already applied - no change.\n"); quit(save = "no")
}

anchor <- grep('^\\s*"ROBUST_LOCATION", "DECOUPLE_PEAK"\\)\\s*$', src)
if (length(anchor) != 1L) {
  stop(sprintf("Expected exactly 1 export_vars anchor, found %d. Apply by hand.",
               length(anchor)))
}

replacement <- c(
  '                    "ROBUST_LOCATION", "DECOUPLE_PEAK",',
  '                    # run_meet() reads these at the project_tier()/',
  '                    # project_round() calls. Without them here the serial path',
  '                    # works (lexical scoping) and every PSOCK worker dies with',
  '                    # "object \'TIER_SHRINK\' not found" -- so parallel mode was',
  '                    # silently broken for exactly the arms it was needed for.',
  '                    "TIER_SHRINK", "ROUND_SHRINK")')

out <- append(src[-anchor], replacement, after = anchor - 1L)

# Must still parse, or we have traded a runtime error for a worse one.
tmp <- tempfile(fileext = ".R"); on.exit(unlink(tmp), add = TRUE)
writeLines(out, tmp)
ok <- tryCatch({ parse(tmp); TRUE }, error = function(e) { message(conditionMessage(e)); FALSE })
if (!ok) stop("Patched result does not parse; TARGET left untouched.")

bak <- paste0(TARGET, ".bak-", format(Sys.Date(), "%Y%m%d"))
if (!file.exists(bak)) file.copy(TARGET, bak)
writeLines(out, TARGET)
cat("Applied. Backup:", bak, "\n")
cat("Parallel mode now usable:  CITIUS_BT_WORKERS=12\n")
