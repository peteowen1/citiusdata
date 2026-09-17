# Attach $altitude to a copy of the deployed calibration, and change nothing
# else.
#
# WHY A SEPARATE FILE RATHER THAN A REFIT. The arm this feeds has to isolate
# ONE mechanism. Re-running calibrate() to "add altitude" would also refit the
# wind coefficients, the round/tier precisions and the race table, so the arm
# would carry three changes at once -- the shared-vintage confound that once
# made six single-variable arms all score ~1.7% for the same wrong reason
# (make_season_arm_calibrations.R's header). This copies the deployed object
# and adds one element.
#
# Usage:  Rscript citiusdata/scripts/compose_altitude_calibration.R
# Env:    CITIUS_ALT_BASE  base calibration (default: the deployed one)
#         CITIUS_ALT_OUT   output filename

VERSE <- here::here()
suppressMessages({library(data.table); library(arrow)})
D <- file.path(VERSE, "citiusdata", "data")
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
say <- function(...) cat(sprintf(...), "\n", sep = "")

BASE <- Sys.getenv("CITIUS_ALT_BASE", DEPLOYED$calibration)
OUTF <- Sys.getenv("CITIUS_ALT_OUT",
                   sub("\\.rds$", "_altitude.rds", BASE))

cal <- readRDS(file.path(D, BASE))
stopifnot("base is not a citius_calibration" = inherits(cal, "citius_calibration"))
if (!is.null(cal$altitude))
  cli::cli_abort("{BASE} already carries $altitude -- composing again would stack two vintages.")

ae <- as.data.table(read_parquet(file.path(D, "altitude_effect.parquet")))
# The residual coefficients, split by whether a race effect was applied. The
# `gross` scope rows from the same file are the diagnostic fit, NOT what the
# model applies -- taking them here would double-count the share c_r removed.
alt <- ae[scope %in% c("residual (race effect applied)", "gross (no race effect)"),
          .(family, has_cr, beta, se, t, n_ath_ev)]
if (!nrow(alt)) cli::cli_abort("altitude_effect.parquet carries no residual rows -- re-run fit_altitude_effect.R.")

# A coefficient that is not distinguishable from zero is set to zero rather
# than carried as noise: throw and combined measure null, and applying a
# noisy near-zero to 325k throw rows is a cost with no expected gain.
MIN_T <- as.numeric(Sys.getenv("CITIUS_ALT_MIN_T", "3"))
# `abs(t) < MIN_T` is NA when t is NA, and data.table's `i` treats NA as
# no-match -- so the plain filter would SKIP a row whose t could not even be
# computed, leaving its unvalidated beta in place. That is backwards: a
# coefficient with no t-statistic is the one most in need of zeroing. Same shape
# as the documented `dt[col > k]` trap in C:/dev/.claude/rules/r-datatable-gotchas.md.
alt[!is.finite(t), t := 0]
n_zeroed <- alt[abs(t) < MIN_T, .N]
alt[abs(t) < MIN_T, beta := 0]
say("families zeroed for |t| < %.1f: %d of %d rows", MIN_T, n_zeroed, nrow(alt))

# ZERO A FAMILY ON GROUNDS OTHER THAN SIGNIFICANCE.
#
# |t| is the wrong filter for a family whose REGRESSOR is invalid, and road is
# exactly that. alt_m is the venue city's point elevation; a marathon or half
# climbs and descends away from it, so for road the variable does not measure
# what the coefficient assumes it measures. Its fit significance is among the
# strongest in the table (-1.42%/km, t = -17.7) and it was the one family the
# 2026-09-17 arm showed to be significantly WORSE out of sample: +0.0128pp
# (t = +3.53) overall, and +0.6234pp (t = 5.97) in the >2200m band, a 10%
# relative degradation on exactly the races the term exists for. The fit most
# likely picks up that high-altitude road races are disproportionately run by
# altitude-resident East African fields, and attributes that to metres.
#
# So this is not tuning. A family listed here is excluded because the
# measurement is not valid for it, and no coefficient value would fix that.
# Full evidence: docs/reviews/altitude-arm-2026-09-17.md
ZERO_FAM <- trimws(strsplit(Sys.getenv("CITIUS_ALT_ZERO_FAMILIES", ""), ",")[[1]])
ZERO_FAM <- ZERO_FAM[nzchar(ZERO_FAM)]
if (length(ZERO_FAM)) {
  unknown <- setdiff(ZERO_FAM, unique(alt$family))
  if (length(unknown)) cli::cli_abort(
    "CITIUS_ALT_ZERO_FAMILIES names {length(unknown)} family/families not in the fit: {.val {unknown}}. A typo here silently zeroes nothing.")
  n_fam <- alt[family %chin% ZERO_FAM & beta != 0, .N]
  alt[family %chin% ZERO_FAM, beta := 0]
  say("families zeroed BY NAME (invalid regressor, not significance): %s -- %d row(s) set to 0",
      paste(ZERO_FAM, collapse = ", "), n_fam)
}

# Every (family, has_cr) cell must be present. fit_altitude_effect.R fits per
# cell behind a MIN_PAIRS gate, so one scope of a family can drop out while
# nrow(alt) stays comfortably positive. A missing cell never matches at runtime,
# gets beta NA -> 0, and is then indistinguishable from the deliberate no-op.
# Athletics only: fit_altitude_effect.R fits the athletics corpus, so the
# swimming families are legitimately absent and must not be demanded here. This
# guard failed on its first run for exactly that reason -- a gate is not correct
# until it has been run against known-good data and NOT fired.
ev <- as.data.table(citius::citius_events())
fam_all <- sort(unique(ev[sport == "Athletics", family]))
want <- CJ(family = fam_all, has_cr = c(FALSE, TRUE))
miss <- want[!alt, on = .(family, has_cr)]
if (nrow(miss)) cli::cli_abort(c(
  "altitude_effect.parquet is missing {nrow(miss)} of {nrow(want)} (family, has_cr) cells.",
  i = "Missing: {paste(miss$family, miss$has_cr, collapse = '; ')}",
  x = "A missing cell is silently inert at runtime, not an error -- re-run fit_altitude_effect.R."))

# All-zero is a valid-looking calibration that changes nothing. It passes the
# wiring test (the element exists and has rows) and ability.R's own guard, while
# contributing exactly zero to every prediction -- a setter and a reader both
# present, and numerically inert, which no existing check can see.
if (all(alt$beta == 0)) cli::cli_abort(
  "every family zeroed at |t| < {MIN_T} -- this would write a calibration that is wired but inert. Check the fit before composing.")

cal$altitude <- alt[]
cal$provenance$altitude <- list(
  from = "altitude_effect.parquet", base = BASE,
  min_t = MIN_T, built = Sys.time())

saveRDS(cal, file.path(D, OUTF))
say("\nwrote %s", OUTF)
say("$altitude rows: %d", nrow(alt))
print(alt[order(has_cr, beta), .(family, has_cr, beta = round(beta, 4),
                                 pct_per_km = round(100*(exp(beta)-1), 2), t = round(t, 1))])
say("\nEverything else is byte-identical to %s.", BASE)
