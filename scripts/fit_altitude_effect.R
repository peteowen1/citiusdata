# Per-(family, sex) altitude effect, estimated WITHIN athlete-event, BANDED not
# linear.
#
# WHY BANDED. A single linear slope was fitted and measured against its own
# banded diagnostic on 2026-09-17, and the diagnostic said the slope was wrong
# at both ends: <200m true effect +0.0012 (essentially zero), >2200m -0.0232 --
# a linear fit through those five points over- corrects sea level by close to
# its entire magnitude and under-corrects extreme altitude by roughly half.
# Confirmed by isolating the add-back's own effect on 2026-09-18: it made 9,677
# SEA-LEVEL (<200m) races significantly WORSE (t=4.36, p<1e-4) while doing
# nothing useful above 2200m, where the road family (the only one with rows
# there) had been zeroed for an unrelated reason. Full story in
# docs/reviews/altitude-arm-2026-09-17.md.
#
# WHY NOT A GAM. The estimator below is within-athlete-event demeaning
# (Frisch-Waugh-Lovell): demean y and demean the regressor(s), then OLS. FWL is
# EXACT only for a linear term -- for a nonlinear f, mean(f(x)) over an
# athlete's races is not f(mean(x)), so "smooth the demeaned residual" is a
# different, wrong model. A correct GAM needs the athlete-event effects and the
# smooth estimated JOINTLY (e.g. mgcv::bam with a random-effect smooth per
# athlete-event), which is materially more infrastructure and carries its own
# bias risk -- unbiased only if the random effect is uncorrelated with altitude
# exposure, which it measurably is not (shift-vs-history correlation -0.940,
# 2026-09-17).
#
# BAND DUMMIES ARE STILL LINEAR. A 0/1 indicator for "is this row in band b" is
# a linear regressor, so FWL applies exactly. Each non-reference band is fit as
# its own two-band comparison against <200m (the reference), reusing fit_one()
# UNCHANGED -- the same tested single-regressor estimator this file has used
# since it was first written, just with a dummy in place of continuous km. This
# is deliberately NOT one joint multi-dummy regression per cell: a pairwise
# contrast against a fixed reference only requires an athlete to have raced in
# EITHER band, not to have crossed several band boundaries at once, which is a
# much weaker and more common pattern in the data.
#
# WHY family x SEX, not family alone. Sized 2026-09-18 before choosing: family
# alone gives 9 cells (all powered); family x sex gives 18 (all powered,
# thousands of athlete-events each); family x sex x band gives 90, 77 of which
# (86%) clear a 200-athlete-event floor. Event-level was close (66 of 83) but
# the failures are exactly the thin events (walks, combined disciplines) that
# family x sex avoids gap-filling for. Sex matters mechanically, not just
# plausibly: East African distance/middle fields are male-skewed relative to
# women's in this corpus, and that population is exactly what drives the
# shift-vs-history correlation above -- a family-only fit blends two different
# selection effects into one number.
#
# Usage:  Rscript citiusdata/scripts/fit_altitude_effect.R
#         CITIUS_ALT_MIN_PAIRS=200  minimum athlete-events per band contrast

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
D <- file.path(VERSE, "citiusdata", "data")
source(file.path(VERSE, "citiusdata", "scripts", "_venue_elevation.R"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
MIN_PAIRS <- as.integer(Sys.getenv("CITIUS_ALT_MIN_PAIRS", "200"))

BAND_BREAKS <- c(-Inf, 200, 800, 1500, 2200, Inf)
BAND_LABS   <- c("<200", "200-800", "800-1500", "1500-2200", ">2200")
REF_BAND    <- "<200"

# Per-stage runtimes, appended to ~/.claude/runtime-log.csv so "what is slow"
# is a query rather than a recollection. See the Long Runs section of
# ~/.claude/CLAUDE.md.
source(file.path(Sys.getenv("USERPROFILE"), ".claude", "lib", "runtime_log.R"))
rt_script("fit_altitude_effect.R")

alt <- venue_elevation(D, quiet = FALSE)[, .(venue_city, alt_m)]

ch <- rt_stage("load corpus (7 cols)", with_citius_db_connection(
  function(conn) load_championship_results(
    conn, columns = c("athlete_id", "event_id", "date", "perf",
                      "venue_city", "indoor", "race_key")), read_only = TRUE))
setDT(ch)
say("corpus rows: %s", format(nrow(ch), big.mark = ","))

# INDOOR IS EXCLUDED, not controlled for. Indoor is already its own term in the
# calibration, and an indoor track at 1,600 m (Albuquerque) mixes two effects
# this fit cannot separate. Better to estimate altitude on the outdoor
# population it will mostly be applied to than to pretend the interaction away.
ch <- ch[!is.na(perf) & (is.na(indoor) | !indoor)]
ch <- merge(ch, alt, by = "venue_city")
say("outdoor rows with a known venue elevation: %s (%.1f%% of corpus)",
    format(nrow(ch), big.mark = ","), 100 * nrow(ch) / 5039647)

# family AND sex from the event registry -- the authoritative source, not a
# regex on event_id. citius_events() carries `sex` directly (used elsewhere as
# `ev$sex == "W"`), so this reads the same column the model itself would.
reg <- as.data.table(citius_events())[, .(event_id, family, sex)]
ch <- merge(ch, reg, by = "event_id")
ch <- ch[!is.na(sex)]
ch[, alt_km := alt_m / 1000]
ch[, band := cut(alt_m, BAND_BREAKS, labels = BAND_LABS)]

# Within athlete-event demeaning, unchanged from the linear version. Groups
# with no altitude variation contribute nothing (every dummy demeans to 0) and
# are dropped explicitly rather than left to contribute zero rows silently.
ch[, `:=`(n_g = .N, alt_sd = stats::sd(alt_km)), by = .(athlete_id, event_id)]
use <- ch[n_g >= 2L & is.finite(alt_sd) & alt_sd > 0]
say("athlete-events with genuine altitude variation: %s (%s rows)",
    format(uniqueN(use, by = c("athlete_id", "event_id")), big.mark = ","),
    format(nrow(use), big.mark = ","))

use[, `:=`(y = perf - mean(perf)), by = .(athlete_id, event_id)]

# fit_one() UNCHANGED from the linear version: FWL-exact OLS of demeaned y on
# a demeaned regressor. Here the regressor is a 0/1 band dummy instead of
# continuous km, and that is the whole change -- band dummies are still linear
# regressors, so the same estimator applies without modification.
fit_one <- function(d) {
  sxx <- sum(d$x^2)
  if (!is.finite(sxx) || sxx <= 0) return(NULL)
  b   <- sum(d$x * d$y) / sxx
  res <- d$y - b * d$x
  dfree <- nrow(d) - uniqueN(d, by = c("athlete_id", "event_id")) - 1L
  if (dfree <= 0) return(NULL)
  se  <- sqrt(sum(res^2) / dfree / sxx)
  data.table(beta = b, se = se, t = b / se,
             n_rows = nrow(d),
             n_ath_ev = uniqueN(d, by = c("athlete_id", "event_id")))
}

# Fit one (family, sex) x (band vs REF_BAND) contrast, on a chosen y column
# (either raw perf-demeaned, or the race-effect residual). `has_cr` labels
# which scope produced `ycol` for the caller; it does not affect the fit.
# .tband, NOT `band` -- the local is deliberately distinct from the `band`
# COLUMN, the same convention ability.R's wind block scar established
# ("Local names are deliberately distinct from any column in dt"). A first
# version used the function parameter name `band` with data.table's `..`
# prefix to disambiguate inside `d[band %in% c(REF_BAND, ..band)]`, and it
# failed outright ("object '..band' not found") -- the `..` mechanism does not
# reliably resolve inside a nested expression like `c(x, ..y)`, only as a bare
# top-level reference. Renaming avoids the ambiguity rather than fighting it.
fit_band_contrast <- function(d, .tband, ycol = "y") {
  sub <- d[band %in% c(REF_BAND, .tband)]
  if (!nrow(sub)) return(NULL)
  sub[, n_bands_here := uniqueN(band), by = .(athlete_id, event_id)]
  sub <- sub[n_bands_here >= 2L]
  if (!nrow(sub)) return(NULL)
  sub[, x := as.numeric(band == .tband) - mean(as.numeric(band == .tband)),
      by = .(athlete_id, event_id)]
  sub[, yv := get(ycol) - mean(get(ycol)), by = .(athlete_id, event_id)]
  fit_one(sub[, .(x, y = yv, athlete_id, event_id)])
}

fit_family_sex_band <- function(d, ycol = "y") {
  cells <- unique(d[, .(family, sex)])
  rbindlist(lapply(seq_len(nrow(cells)), function(i) {
    fam <- cells$family[i]; sx <- cells$sex[i]
    dd <- d[family == fam & sex == sx]
    rows <- lapply(setdiff(BAND_LABS, REF_BAND), function(b) {
      r <- fit_band_contrast(dd, b, ycol = ycol)
      if (is.null(r) || r$n_ath_ev < MIN_PAIRS) return(NULL)
      r[, `:=`(family = fam, sex = sx, band = b)]
      r
    })
    rbindlist(rows, fill = TRUE)
  }), fill = TRUE)
}

say("\n=== fitting gross altitude effect: family x sex x band ===")
gross <- rt_stage("gross fit, family x sex x band", fit_family_sex_band(use, "y"))
# Reference band explicit, not implicit. A downstream lookup that finds no row
# for <200 must not be able to confuse "unfitted / unknown" with "known-zero
# reference band" -- so every (family, sex) cell that produced ANY fitted band
# also gets an explicit <200 row with beta = 0, t = Inf (a sentinel meaning
# "not estimated, defined as the reference", not "insignificant and zeroed").
ref_rows <- unique(gross[, .(family, sex)])[, `:=`(band = REF_BAND, beta = 0,
                    se = NA_real_, t = Inf, n_rows = NA_integer_, n_ath_ev = NA_integer_)]
gross <- rbind(gross, ref_rows, fill = TRUE)
gross[, scope := "gross"]
gross[, pct_effect := round(100 * (exp(beta) - 1), 2)]
setorder(gross, family, sex, band)
say("beta is the log-mark effect of this band vs <200m (higher = better performance).")
say("NEGATIVE beta = this band is worse than sea level for this family/sex.")
print(gross[, .(family, sex, band, beta = round(beta, 4), t = round(t, 1),
                pct_effect, n_ath_ev)])

# --- THE RESIDUAL FIT, which is the one a model can actually use ------------
#
# calibrate()'s per-race shared effect `c_r` ALREADY absorbs part of altitude,
# because altitude is shared by the whole field exactly like wind is. Fitted
# here on perf AFTER the strip estimate_ability() actually applies, so the
# coefficient is by construction what the deployed model has NOT already
# removed -- including the field-size shrink (wt = n_r/(n_r+k)), replicated
# exactly rather than approximated, per the "harness must replicate the
# deployed pipeline" rule this file has gotten wrong before.
CAL <- Sys.getenv("CITIUS_ALT_CAL", "calibration_corpus_wac_coast_0904_full2.rds")
cal <- tryCatch(readRDS(file.path(D, CAL)), error = function(e) NULL)

fam <- gross
if (!is.null(cal) && !is.null(cal$race) && !is.null(cal$race_shock)) {
  rr <- as.data.table(cal$race)[is.finite(c_r)]
  rr[, .rcl := citius:::.round_class(if ("round" %in% names(rr)) round else NA_character_)]
  rr[, .tcl := citius:::.tier_class(if ("tier" %in% names(rr)) tier else NA_character_)]
  ex <- as.data.table(cal$race_shock$expected)
  rr[, e_cell := ex$e_cell[match(paste(event_id, .tcl, .rcl, sep = "|"),
                                 paste(ex$event_id, ex$tier_class, ex$round_class, sep = "|"))]]
  evm <- rr[, .(m = mean(c_r, na.rm = TRUE)), by = event_id]
  rr[!is.finite(e_cell), e_cell := evm$m[match(event_id, evm$event_id)]]
  rr[!is.finite(e_cell), e_cell := 0]
  bt <- as.data.table(cal$race_shock$by_tier)
  rr[, beta_s := bt$beta[match(.tcl, bt$tier_class)]]
  rr[!is.finite(beta_s), beta_s := cal$race_shock$beta]
  rr[, strip := (1 - beta_s) * (c_r - e_cell)]

  if (!"n_in_race" %in% names(rr))
    cli::cli_abort("cal$race has no n_in_race -- cannot replicate estimate_ability()'s field-size shrink.")
  evt <- as.data.table(cal$events)
  if (!all(c("sigma_within", "condition_sd") %in% names(evt)))
    cli::cli_abort("cal$events lacks sigma_within/condition_sd -- same reason.")

  u2 <- merge(use, rr[, .(race_key, strip, n_in_race)], by = "race_key", all.x = TRUE)
  u2 <- merge(u2, evt[, .(event_id, sigma_within, condition_sd)],
              by = "event_id", all.x = TRUE)
  u2[, k := fifelse(is.finite(sigma_within) & is.finite(condition_sd) & condition_sd > 0,
                    (sigma_within / condition_sd)^2, Inf)]
  u2[, n_r := fifelse(is.finite(n_in_race), as.numeric(n_in_race), 0)]
  u2[, wt := n_r / (n_r + k)]
  u2[!is.finite(wt), wt := 0]
  u2[, strip_applied := fifelse(is.finite(strip), strip, 0) * wt]
  u2[, has_cr := is.finite(strip) & wt > 0]
  say("\nrows with a fitted race effect ACTUALLY applied (wt > 0): %.1f%%", 100 * mean(u2$has_cr))

  u2[, perf_adj := perf - strip_applied]
  # Note: perf_adj replaces perf; the per-group re-demeaning happens INSIDE
  # fit_band_contrast(), separately for has_cr TRUE and FALSE, since they are
  # different populations with different reference means.
  u2[, y := perf_adj]

  res_list <- lapply(c(TRUE, FALSE), function(hc) {
    d <- u2[has_cr == hc]
    r <- rt_stage(sprintf("residual fit has_cr=%s, family x sex x band", hc),
                  fit_family_sex_band(d, "y"))
    if (nrow(r)) r[, has_cr := hc]
    r
  })
  res_fam <- rbindlist(res_list, fill = TRUE)
  res_fam[, scope := fifelse(has_cr, "residual (race effect applied)",
                             "gross (no race effect)")]
  res_fam[, pct_effect := round(100 * (exp(beta) - 1), 2)]
  setorder(res_fam, has_cr, family, sex, band)
  say("\n=== residual altitude effect, family x sex x band, split by has_cr ===")
  say("the has_cr = TRUE rows are what a model can still gain from.")
  print(res_fam[, .(family, sex, has_cr, band, beta = round(beta, 4),
                    t = round(t, 1), pct_effect, n_ath_ev)])
  fam <- rbind(gross, res_fam, fill = TRUE)
}

out <- file.path(D, "altitude_effect.parquet")
write_parquet(fam, out)
say("\nwrote %s (%d rows)", basename(out), nrow(fam))
say("NOT wired into any prediction path by this script -- that is a separate,")
say("measured arm. See docs/reference/storage-formats.md on model lifecycle.")
