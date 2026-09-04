# Apply the FAMILY-POOL DEBIAS arm to backtest_athletics.R.
#
# WHAT THIS ADDS. check_family_partial_pool_debias.R validated 2026-09-01: a
# hierarchical empirical-Bayes offset (family x sex -> global, event -> its
# family x sex, both fit on the all-tiers-pooled population -- T1-only fitting
# was tested and rejected, see fit_family_pool_offsets.R's header) flips T1
# elite raw marks MAE from losing to last-5 to beating it: -2.96% to -3.10%
# across 5 different train/test split dates (3 of 5 significant at p<0.03),
# and stacks cleanly with project_tier (tier05: -3.01% combined vs -1.33% for
# the debias alone on that split). The offset table is fit ONCE by
# fit_family_pool_offsets.R and read from citiusdata/data/family_pool_offsets.rds
# -- this patch only wires the LOOKUP into the backtest loop, it does not fit
# anything itself.
#
# CITIUS_BT_FAMILY_DEBIAS=1   turn it on. Empty = OFF, control path unchanged.
#
# UNITS. The offset table is in "100 x oriented log mark" -- the same %-of-
# mark convention as project_tier's own offsets in this file -- so it is
# divided by 100 before being subtracted from `ability`, which is on the
# unscaled oriented-log scale. Getting this wrong by a factor of 100 would
# still run and still print, which is exactly the kind of silent-wrong-scale
# bug this repo has been bitten by before; CITIUS_BT_FAMILY_DEBIAS_SELFTEST
# below exists so it does not happen a second time here.
#
# REFUSES TO RUN WHILE AN Rscript IS LIVE -- same guard, same reasoning, as
# apply_parallel_export_fix.R and apply_selection_shrinkage_patch.R. Narrowed
# to processes actually running backtest_athletics.R (not every Rscript on the
# box) and fails CLOSED on an unparseable process count, not open.
#
#   Rscript citiusdata/scripts/apply_family_pool_debias_patch.R
#
# It edits ONE file (citiusdata/scripts/backtest_athletics.R) and makes no
# other change. A timestamped backup is written beside it first. Idempotent.

TARGET <- file.path("citiusdata", "scripts", "backtest_athletics.R")
if (!file.exists(TARGET)) stop("run from the citiusverse root; not found: ", TARGET)

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

if (any(grepl("CITIUS_BT_FAMILY_DEBIAS", src, fixed = TRUE))) {
  message("Already applied (CITIUS_BT_FAMILY_DEBIAS present). Nothing to do.")
  quit(save = "no", status = 0)
}

# ---- block 1: env-var parsing + offset table load, after the project_round alert ----
ANCHOR1 <- '  "project_round ON: shrink {.val {ROUND_SHRINK}} (no-op on a finals-only pool).")'
i1 <- which(src == ANCHOR1)
if (length(i1) != 1L) stop("anchor 1 not found exactly once (found ", length(i1), ")")

BLOCK1 <- c(
'',
'# FAMILY-POOL DEBIAS. Offsets fit OFFLINE by fit_family_pool_offsets.R and read',
'# here as a fixed lookup, not refit per meet -- same "single global correction"',
'# shape as TIER_SHRINK/ROUND_SHRINK above and SEL_SHRINK below, if applied.',
'FAMILY_DEBIAS <- nzchar(Sys.getenv("CITIUS_BT_FAMILY_DEBIAS", ""))',
'if (FAMILY_DEBIAS) {',
'  .fp_path <- here::here("citiusdata", "data", "family_pool_offsets.rds")',
'  if (!file.exists(.fp_path)) cli::cli_abort(',
'    "{.envvar CITIUS_BT_FAMILY_DEBIAS} is set but {.file {.fp_path}} does not ",',
'    "exist. Run {.file fit_family_pool_offsets.R} first.")',
'  .fp <- readRDS(.fp_path)',
'  .fp_ev_by <- as.data.table(citius_events())[, .(event_id, fs = paste(family, sex, sep = "|"))]',
'  .fp_fs_by_event <- setNames(.fp_ev_by$fs, .fp_ev_by$event_id)',
'  # Scalar lookup: ev_map (event-level, already shrunk toward its family x sex)',
'  # first, else the family x sex map, else the grand mean -- the same fallback',
'  # chain fit_family_pool_offsets.R used when FITTING, so an event absent from',
'  # both here and there behaves identically to one absent from the fit alone.',
'  family_pool_offset <- function(event_id) {',
'    ev <- .fp$ev_map[event_id]',
'    if (!is.na(ev)) return(unname(ev))',
'    fsv <- .fp_fs_by_event[event_id]',
'    fsm <- if (!is.na(fsv)) .fp$fs_map[fsv] else NA_real_',
'    if (!is.na(fsm)) return(unname(fsm))',
'    .fp$mu0',
'  }',
'  cli::cli_alert_info(',
'    "family-pool debias ON: offsets fit on {.file {(.fp$fit_arm)}}, fit holdout {.val {format((.fp$fit_holdout))}}.")',
'}'
)

# ---- block 2: application, LAST -- after project_tier/project_round, before sim ----
ANCHOR2 <- '    sim <- tick("sim", simulate_event(entrants, n_sims = N_SIMS,'
i2 <- which(src == ANCHOR2)
if (length(i2) != 1L) stop("anchor 2 not found exactly once (found ", length(i2), ")")
if (i2 <= i1) stop("anchors out of order; file is not the expected shape")

BLOCK2 <- c(
'    # FAMILY-POOL DEBIAS, applied LAST -- after aging and the tier/round',
'    # projections, immediately before simulation. Order matters less here than',
'    # for selection shrinkage: this offset was fit against the FINAL predicted',
'    # mark of whatever arm produced the fit data, so it is a residual correction',
'    # meant to sit after everything else, not a footing-sensitive one.',
'    # UNITS: the table is "100 x oriented log mark" (the %-of-mark convention',
'    # used throughout this file), `ability` is NOT scaled by 100 -- divide.',
'    if (FAMILY_DEBIAS && nrow(entrants)) {',
'      entrants[, ability := ability - family_pool_offset(ev) / 100]',
'    }',
ANCHOR2
)

# ---- block 3: export the closure for the PSOCK parallel path ---------------
# Same trap, same day, as TIER_SHRINK/ROUND_SHRINK (apply_parallel_export_fix.R):
# clusterExport() re-homes an exported function's environment to each worker's
# OWN globalenv rather than copying the sender's bindings with it, so
# family_pool_offset()'s free variables (.fp, .fp_fs_by_event) must be exported
# by name too, or every worker dies the moment the function is actually called.
ANCHOR3 <- '  if (!USE_STORE) export_vars <- c(export_vars, "clean")'
i3 <- which(src == ANCHOR3)
if (length(i3) != 1L) stop("anchor 3 not found exactly once (found ", length(i3), ")")

BLOCK3 <- c(
ANCHOR3,
'  if (FAMILY_DEBIAS) export_vars <- c(export_vars, "FAMILY_DEBIAS", "family_pool_offset",',
'                                      ".fp", ".fp_fs_by_event")'
)

# Anchors are patched in DESCENDING file-position order so earlier insertions
# never shift the line numbers the later ones were computed against.
stopifnot("anchors must be strictly increasing in file position" = i1 < i2 && i2 < i3)
out <- c(src[seq_len(i1)], BLOCK1,
         src[(i1 + 1L):(i2 - 1L)], BLOCK2,
         src[(i2 + 1L):(i3 - 1L)], BLOCK3,
         src[(i3 + 1L):length(src)])

bak <- paste0(TARGET, ".pre_familypool_", format(Sys.time(), "%Y%m%d%H%M%S"))
file.copy(TARGET, bak)
writeLines(out, TARGET)

ok <- tryCatch({ parse(TARGET); TRUE },
               error = function(e) { message("PARSE FAILED: ", conditionMessage(e)); FALSE })
if (!ok) {
  file.copy(bak, TARGET, overwrite = TRUE)
  stop("patch produced an unparseable file; reverted from ", bak)
}
message("Patched ", TARGET, " (+", length(BLOCK1) + length(BLOCK2) - 1L, " lines). Backup: ", bak)
message("Parse check: OK. Control path unchanged -- CITIUS_BT_FAMILY_DEBIAS unset = OFF.")
message("Run fit_family_pool_offsets.R first if citiusdata/data/family_pool_offsets.rds does not exist yet.")
