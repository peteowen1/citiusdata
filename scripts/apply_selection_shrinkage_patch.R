# Apply the SELECTION SHRINKAGE arm to backtest_athletics.R.
#
# WHY THIS IS A PATCH SCRIPT AND NOT A DIRECT EDIT. It was written while a
# backtest was running on the target file. Rscript parses top-level expressions
# incrementally as it reads, so editing a script mid-run corrupts the running
# job and surfaces as a syntax error in code that parsed cleanly at launch --
# documented in this repo's own gotchas after a 25-minute backtest died that
# way on 2026-08-21, having computed every result and written none.
#
# So: this script refuses to run while an Rscript process is live, checks its
# anchors, and is idempotent. Run it once the arm currently in flight finishes.
#
#   Rscript citiusdata/scripts/apply_selection_shrinkage_patch.R
#
# It edits ONE file (citiusdata/scripts/backtest_athletics.R) and makes no
# other change. A timestamped backup is written beside it first.

TARGET <- file.path("citiusdata", "scripts", "backtest_athletics.R")
if (!file.exists(TARGET)) stop("run from the citiusverse root; not found: ", TARGET)

# ---- refuse to patch a file that is being read right now --------------------
# Count only processes actually RUNNING THE TARGET, not every Rscript on the
# machine -- matches the fix already proven in apply_parallel_export_fix.R
# (2026-09-01): the original machine-wide `Get-Process -Name Rscript` count
# both blocks on unrelated jobs from sibling verses AND, on a mangled system2()
# quoting, comes back NA -- and `!is.na()`/`is.finite()` on that NA reads
# "could not check" as "nothing is running", failing the guard OPEN. Hence the
# explicit fail-closed branch below rather than folding the NA case into the
# same condition as the live-count case.
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

if (any(grepl("CITIUS_BT_SEL_SHRINK", src, fixed = TRUE))) {
  message("Already applied (CITIUS_BT_SEL_SHRINK present). Nothing to do.")
  quit(save = "no", status = 0)
}

# ---- block 1: env-var parsing, after the project_round alert -----------------
ANCHOR1 <- '  "project_round ON: shrink {.val {ROUND_SHRINK}} (no-op on a finals-only pool).")'
i1 <- which(src == ANCHOR1)
if (length(i1) != 1L) stop("anchor 1 not found exactly once (found ", length(i1), ")")

BLOCK1 <- c(
'',
'# SELECTION SHRINKAGE -- the OTHER half of the marks bias, and a different',
'# mechanism from project_tier/project_round above.',
'#',
'# Measured 2026-09-01 across 33 event x sex groups: per-event marks bias',
'# correlates +0.706 (p=4.5e-06) with calibration$events$sigma_within, and',
'# sigma_within is the ONLY survivor in a multivariate model against tier lift,',
'# venue spread and venue concentration (t=3.80, p=0.0007). It also predicts the',
'# NAIVE last-5 baseline\'s bias (t=4.28, p=0.00017); cor(model bias, last-5',
'# bias) = +0.884, so 78% of the model\'s per-event bias is a gradient BOTH',
'# predictors share. That is the signature of regression to the mean under',
'# SELECTION -- athletes reach a T1 final partly on a lucky recent result, so any',
'# past-performance predictor over-rates them, scaled by their own noise. Throws',
'# sigma_within 0.0506 against sprint 0.0146 matches the family ordering.',
'#',
'# Four alternatives were tested and refuted: venue effect and venue',
'# concentration (both OPPOSITE sign; javelin is the least venue-concentrated',
'# event in the corpus), foul rate (opposite sign; distance has the highest foul',
'# rate and the lowest bias), tier mix (arithmetically insufficient -- the lift',
'# actually applied spans 0.71-1.08% against a 5.21pp gradient), and the',
'# `tactical` registry flag (splits sharply but is 800m-and-up, so it restates',
'# the gradient rather than explaining it).',
'#',
'# THE TARGET IS prior_mu, NOT THE FIELD MEAN, and that is not a detail.',
'# Shrinking toward the field mean cannot correct a LEVEL bias at all: with a',
'# shrink factor constant across the field -- which per-event sigma gives, since',
'# every athlete in a race shares one event -- mean(F + (1-c)(pred - F)) == F',
'# exactly. The field\'s mean prediction is unchanged and only its spread',
'# compresses. prior_mu is the event-population mean the field was SELECTED',
'# FROM, sits below the selected field, and so moves the level in the direction',
'# the defect actually requires.',
'#',
'#   CITIUS_BT_SEL_SHRINK=1.4   lambda. Empty = OFF, so control is',
'#                              byte-identical. SCALE: bias spans ~5.2pp over a',
'#                              sigma range of ~0.036, so lambda near 1.0-1.5 is',
'#                              the order the measurement implies. This is NOT a',
'#                              validated default -- it needs its own sweep, the',
'#                              way project_tier()\'s 0.5 got one.',
'#   CITIUS_BT_SEL_SIGMA=event  which sigma scales the shrink. "event" (default)',
'#                              is calibration$events$sigma_within, the quantity',
'#                              the +0.706 was actually measured on. "athlete"',
'#                              is estimate_ability()\'s per-athlete `sigma`,',
'#                              closer to the mechanism but UNMEASURED -- a',
'#                              variant to sweep, never the default.',
'SEL_SHRINK <- parse_shrink(Sys.getenv("CITIUS_BT_SEL_SHRINK", ""), "CITIUS_BT_SEL_SHRINK")',
'SEL_SIGMA  <- tolower(Sys.getenv("CITIUS_BT_SEL_SIGMA", "event"))',
'if (!SEL_SIGMA %in% c("event", "athlete")) cli::cli_abort(',
'  "{.envvar CITIUS_BT_SEL_SIGMA} must be {.val event} or {.val athlete}, got {.val {SEL_SIGMA}}.")',
'if (!is.na(SEL_SHRINK)) cli::cli_alert_info(',
'  "selection shrinkage ON: lambda {.val {SEL_SHRINK}} on {.val {SEL_SIGMA}} sigma, toward prior_mu.")'
)

# ---- block 2: application, BEFORE project_tier -------------------------------
ANCHOR2 <- '    if (!is.na(TIER_SHRINK)) {'
i2 <- which(src == ANCHOR2)
if (length(i2) != 1L) stop("anchor 2 not found exactly once (found ", length(i2), ")")
if (i2 <= i1) stop("anchors out of order; file is not the expected shape")

BLOCK2 <- c(
'    # SELECTION SHRINKAGE, applied BEFORE the projections below. The order is',
'    # load-bearing: prior_mu is on the top-tier-final footing that',
'    # estimate_ability(adjust_context = TRUE) produced, and project_tier() moves',
'    # `ability` OFF that footing onto the race\'s own. Shrinking afterwards would',
'    # pull ability toward a target on a DIFFERENT footing, injecting a bias of',
'    # (tier offset x shrink weight). Running first keeps ability and prior_mu on',
'    # one footing, and the projections are uniform additive shifts applied after',
'    # -- the same commuting argument the block below already makes for aging.',
'    #',
'    # Unlike those projections this is NOT uniform across the field: the shift is',
'    # cw * (prior_mu - ability), so an athlete further above the event prior is',
'    # discounted more. That is the selection story, and it means this arm can',
'    # move the ordering-sensitive metrics (Brier, logloss, favourite-wins) as',
'    # well as the marks metrics. Read those in the scorecard, not just MAE.',
'    if (!is.na(SEL_SHRINK) && nrow(entrants)) {',
'      if (!"prior_mu" %chin% names(entrants)) cli::cli_abort(',
'        "selection shrinkage needs {.field prior_mu}, absent from the ability table.")',
'      sig <- if (SEL_SIGMA == "athlete") {',
'        if (!"sigma" %chin% names(entrants)) cli::cli_abort(',
'          "{.envvar CITIUS_BT_SEL_SIGMA=athlete} needs {.field sigma} on the ability table.")',
'        entrants$sigma',
'      } else {',
'        .evs <- data.table::as.data.table(calibration$events)',
'        if (!all(c("event_id", "sigma_within") %chin% names(.evs))) cli::cli_abort(',
'          "selection shrinkage needs {.field calibration$events$sigma_within}.")',
'        .evs[data.table::data.table(event_id = entrants$event_id),',
'             on = "event_id", x.sigma_within]',
'      }',
'      # An event with no fitted sigma shrinks by zero rather than erroring, so a',
'      # thin event cannot abort a 200-meet run. Coverage is asserted ONCE before',
'      # the run instead -- see run_marks_arm_matrix.ps1 -- which is the right',
'      # place for a precondition: loudly, up front, not counted in a hot loop',
'      # where a partial arm would look like a completed one.',
'      cw <- pmin(pmax(SEL_SHRINK * sig, 0), 1)',
'      cw[!is.finite(cw)] <- 0',
'      entrants[, ability := ability + cw * (prior_mu - ability)]',
'    }'
)

out <- c(src[seq_len(i1)], BLOCK1,
         src[(i1 + 1L):(i2 - 1L)], BLOCK2,
         src[i2:length(src)])

bak <- paste0(TARGET, ".pre_selshrink_", format(Sys.time(), "%Y%m%d%H%M%S"))
file.copy(TARGET, bak)
writeLines(out, TARGET)

# ---- prove it still parses ---------------------------------------------------
ok <- tryCatch({ parse(TARGET); TRUE },
               error = function(e) { message("PARSE FAILED: ", conditionMessage(e)); FALSE })
if (!ok) {
  file.copy(bak, TARGET, overwrite = TRUE)
  stop("patch produced an unparseable file; reverted from ", bak)
}
message("Patched ", TARGET, " (+", length(BLOCK1) + length(BLOCK2), " lines). Backup: ", bak)
message("Parse check: OK. Control path unchanged -- CITIUS_BT_SEL_SHRINK empty = OFF.")
