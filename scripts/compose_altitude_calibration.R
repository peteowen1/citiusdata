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
#
# sex, band and scope are SELECTED, not dropped. Banding (2026-09-18) added
# sex and band as lookup keys ability.R now requires, and scope is what the
# completeness check below reads. An earlier version of this line selected
# only (family, has_cr, beta, se, t, n_ath_ev) -- correct for the old linear
# fit, silently wrong the moment the fit gained new columns, because dropping
# a column here is not an error, it just removes a key the model needs to
# match on.
alt <- ae[scope %in% c("residual (race effect applied)", "gross (no race effect)"),
          .(family, sex, has_cr, band, beta, se, t, n_ath_ev, scope)]
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
  # Neutral wording on purpose. This said "(invalid regressor, not
  # significance)", which is the reason ROAD is excluded and not the reason a
  # family is excluded when the list is being used to ISOLATE one family -- the
  # middle-only variant zeroes eight families whose regressors are perfectly
  # valid. A log line that states a justification the caller did not give is a
  # small lie that a later reader will take as a finding.
  say("families zeroed BY NAME via CITIUS_ALT_ZERO_FAMILIES: %s -- %d row(s) set to 0",
      paste(ZERO_FAM, collapse = ", "), n_fam)
  say("  (the REASON is the caller's; road is excluded for an invalid regressor, an isolation run is not)")
}

# Every (family, sex) must have AT LEAST its <200 reference row -- NOT every
# (family, sex, band, has_cr) cell, which the banded refit (2026-09-18) does
# not guarantee: fit_family_sex_band() only emits a band row when that
# contrast clears MIN_PAIRS, so 77 of 90 family x sex x band cells are fitted
# by design, not 90. Demanding all 90 here would abort a healthy fit.
#
# What MUST hold: a (family, sex) that never clears MIN_PAIRS for ANY band
# never appears in the gross table at all -- fit_family_sex_band() only adds
# the explicit <200 reference row for family/sex combos that already have at
# least one fitted band (`unique(gross[, .(family, sex)])`). So a (family, sex)
# missing here means it produced NOTHING, not "produced only the reference" --
# the exact "wired but inert" failure this guard exists to catch, at the
# coarser grain the design now guarantees.
#
# Athletics only: fit_altitude_effect.R fits the athletics corpus, so the
# swimming families are legitimately absent and must not be demanded here. This
# guard failed on its first run for exactly that reason -- a gate is not correct
# until it has been run against known-good data and NOT fired.
ev <- as.data.table(citius::citius_events())
fam_all <- sort(unique(ev[sport == "Athletics", family]))
sex_all <- sort(unique(ev[sport == "Athletics", sex]))
want <- CJ(family = fam_all, sex = sex_all)
# NOT scope == "gross": `alt` was already filtered above to the two RESIDUAL
# scopes ("gross (no race effect)" and "residual (race effect applied)"). The
# plain "gross" diagnostic scope is never in `alt` at all, so checking for it
# here would find zero rows and abort on every run -- caught reading this back
# rather than by the abort firing on known-good data.
have <- unique(alt[, .(family, sex)])
miss <- want[!have, on = .(family, sex)]
if (nrow(miss)) cli::cli_abort(c(
  "altitude_effect.parquet is missing {nrow(miss)} of {nrow(want)} (family, sex) cells entirely -- not even a <200 reference row.",
  i = "Missing: {paste(miss$family, miss$sex, collapse = '; ')}",
  x = "A missing (family, sex) is silently inert at runtime for EVERY band -- re-run fit_altitude_effect.R, or check whether that combination genuinely never clears MIN_PAIRS."))

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
# pct_effect, not pct_per_km: beta is now the level effect of a BAND vs <200m
# (banded, 2026-09-18), not a per-km rate, so there is no "per km" to name.
print(alt[order(has_cr, family, sex, band), .(family, sex, has_cr, band,
                                              beta = round(beta, 4),
                                              pct_effect = round(100*(exp(beta)-1), 2),
                                              t = round(t, 1))])
say("\nEverything else is byte-identical to %s.", BASE)
