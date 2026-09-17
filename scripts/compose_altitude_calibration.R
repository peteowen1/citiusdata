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
n_zeroed <- alt[abs(t) < MIN_T, .N]
alt[abs(t) < MIN_T, beta := 0]
say("families zeroed for |t| < %.1f: %d of %d rows", MIN_T, n_zeroed, nrow(alt))

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
