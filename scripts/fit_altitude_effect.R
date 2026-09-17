# Per-family altitude coefficient, estimated WITHIN athlete-event.
#
# WHY. venue_elevation.parquet has covered 84.4% of corpus rows since
# 2026-08-19 and nothing in the model has ever read it -- two diagnostics and
# no prediction path. Measured within-athlete on 2026-09-17, distance marks are
# ~3.4% slower at altitude while sprints and jumps are FASTER: a real, large,
# entirely unmodelled effect whose SIGN FLIPS by family, so a single global
# coefficient would be worse than nothing.
#
# THE ESTIMATOR IS FIXED-EFFECTS, NOT A RAW REGRESSION. perf is demeaned
# within (athlete_id, event_id) and altitude is demeaned in the same groups, so
# the coefficient is identified only by an athlete's OWN variation in venue
# altitude. A raw regression would mostly measure that altitude venues host
# different athletes -- Kenyan distance fields at Eldoret against European ones
# at sea level -- which is a population difference, not an altitude effect.
#
# KNOWN ATTENUATION, recorded rather than ignored: this repo already documents
# that a single within-athlete centring attenuates when exposure correlates
# with ability, and it does here -- elite East Africans race altitude at home
# and sea level abroad. The magnitudes below are therefore conservative. The
# ORDERING and the SIGNS are what this is for.
#
# Usage:  Rscript citiusdata/scripts/fit_altitude_effect.R
#         CITIUS_ALT_MIN_PAIRS=200  minimum athlete-events per family

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
D <- file.path(VERSE, "citiusdata", "data")
source(file.path(VERSE, "citiusdata", "scripts", "_venue_elevation.R"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
MIN_PAIRS <- as.integer(Sys.getenv("CITIUS_ALT_MIN_PAIRS", "200"))

alt <- venue_elevation(D, quiet = FALSE)[, .(venue_city, alt_m)]

ch <- with_citius_db_connection(function(conn) load_championship_results(
  conn, columns = c("athlete_id", "event_id", "date", "perf",
                    "venue_city", "indoor", "race_key")), read_only = TRUE)
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

reg <- as.data.table(citius_events())[, .(event_id, family)]
ch <- merge(ch, reg, by = "event_id")
ch[, alt_km := alt_m / 1000]

# Within athlete-event demeaning. Groups with no altitude variation contribute
# nothing (demeaned altitude is 0) and are dropped explicitly rather than left
# to contribute zero rows to a denominator.
ch[, `:=`(n_g = .N, alt_sd = stats::sd(alt_km)), by = .(athlete_id, event_id)]
use <- ch[n_g >= 2L & is.finite(alt_sd) & alt_sd > 0]
say("athlete-events with genuine altitude variation: %s (%s rows)",
    format(uniqueN(use, by = c("athlete_id", "event_id")), big.mark = ","),
    format(nrow(use), big.mark = ","))

use[, `:=`(y = perf - mean(perf), x = alt_km - mean(alt_km)),
    by = .(athlete_id, event_id)]

fit_one <- function(d) {
  sxx <- sum(d$x^2)
  if (!is.finite(sxx) || sxx <= 0) return(NULL)
  b   <- sum(d$x * d$y) / sxx
  res <- d$y - b * d$x
  # df loses one per group (the within mean) plus one for the slope.
  dfree <- nrow(d) - uniqueN(d, by = c("athlete_id", "event_id")) - 1L
  if (dfree <= 0) return(NULL)
  se  <- sqrt(sum(res^2) / dfree / sxx)
  data.table(beta = b, se = se, t = b / se,
             n_rows = nrow(d),
             n_ath_ev = uniqueN(d, by = c("athlete_id", "event_id")))
}

fam <- use[, if (uniqueN(.SD, by = c("athlete_id","event_id")) >= MIN_PAIRS) fit_one(.SD),
           by = family, .SDcols = c("x", "y", "athlete_id", "event_id")]
setorder(fam, beta)
fam[, scope := "gross"]
# pct is the effect on the MARK of +1 km of altitude. perf is oriented so
# higher is better, so a negative beta means altitude makes the mark worse.
fam[, pct_per_km := round(100 * (exp(beta) - 1), 2)]

say("\n=== altitude effect per +1 km, by family ===")
say("beta is on the oriented log scale (higher = better performance).")
say("NEGATIVE beta = altitude HURTS that family.")
print(fam[, .(family, beta = round(beta, 4), se = round(se, 4), t = round(t, 1),
              pct_per_km, n_ath_ev, n_rows)])

# LINEARITY CHECK. The physiological response is not linear in metres -- it is
# near-flat to ~1000 m then accelerates -- so a single slope fitted across
# 0-3,640 m will under-correct high venues. Banded means say whether that
# matters here before anyone commits to a functional form.
use[, band := cut(alt_m, c(-Inf, 200, 800, 1500, 2200, Inf),
                  labels = c("<200", "200-800", "800-1500", "1500-2200", ">2200"))]
say("\n=== within-athlete mean demeaned perf by altitude band (distance only) ===")
say("a linear-in-metres term assumes these step evenly; they do not have to.")
print(use[family == "distance", .(rows = .N, mean_y = round(mean(y), 4)), by = band][order(band)])

# --- THE RESIDUAL FIT, which is the one a model can actually use ------------
#
# calibrate()'s per-race shared effect `c_r` ALREADY absorbs part of altitude,
# because altitude is shared by the whole field exactly like wind is. Measured
# 2026-09-17, c_r moves with altitude in the right direction for every family
# but by very different fractions -- ~35% of the distance effect, ~82% of road,
# and it OVERSHOOTS for jump.
#
# So neither obvious option is right. Copying the wind block (suppress wherever
# c_r exists) leaves ~65% of the distance effect uncorrected on the 73% of rows
# that have a race effect. Applying the gross beta everywhere double-counts
# whatever c_r already took -- the "one lever at a time" incident in this
# repo's own history.
#
# Fitted here instead on perf AFTER the strip estimate_ability() actually
# applies, so the coefficient is by construction what the deployed model has
# NOT already removed. Note the strip is (1 - beta_shock) * (c_r - e_cell),
# NOT the full c_r -- replicating what the pipeline does rather than what it
# looks like it does is the "harness must replicate the deployed pipeline"
# rule, which has cost a wrong verdict here before.
CAL <- Sys.getenv("CITIUS_ALT_CAL", "calibration_corpus_wac_coast_0904_full2.rds")
cal <- tryCatch(readRDS(file.path(D, CAL)), error = function(e) NULL)

if (!is.null(cal) && !is.null(cal$race) && !is.null(cal$race_shock)) {
  rr <- as.data.table(cal$race)[is.finite(c_r)]
  rr[, .rcl := citius:::.round_class(if ("round" %in% names(rr)) round else NA_character_)]
  rr[, .tcl := citius:::.tier_class(if ("tier" %in% names(rr)) tier else NA_character_)]
  ex <- as.data.table(cal$race_shock$expected)
  rr[, e_cell := ex$e_cell[match(paste(event_id, .tcl, .rcl, sep = "|"),
                                 paste(ex$event_id, ex$tier_class, ex$round_class, sep = "|"))]]
  # Same fallback ladder ability.R uses when a cell is missing.
  evm <- rr[, .(m = mean(c_r, na.rm = TRUE)), by = event_id]
  rr[!is.finite(e_cell), e_cell := evm$m[match(event_id, evm$event_id)]]
  rr[!is.finite(e_cell), e_cell := 0]
  bt <- as.data.table(cal$race_shock$by_tier)
  rr[, beta_s := bt$beta[match(.tcl, bt$tier_class)]]
  rr[!is.finite(beta_s), beta_s := cal$race_shock$beta]
  rr[, strip := (1 - beta_s) * (c_r - e_cell)]

  u2 <- merge(use, rr[, .(race_key, strip)], by = "race_key", all.x = TRUE)
  u2[, has_cr := is.finite(strip)]
  u2[!is.finite(strip), strip := 0]
  say("\nrows with a fitted race effect: %.1f%%", 100 * mean(u2$has_cr))

  # Re-demean AFTER the strip: the within-athlete mean moves once perf changes.
  u2[, perf_adj := perf - strip]
  u2[, `:=`(y = perf_adj - mean(perf_adj), x = alt_km - mean(alt_km)),
     by = .(athlete_id, event_id, has_cr)]
  u2 <- u2[is.finite(y) & is.finite(x)]

  res_fam <- u2[, if (uniqueN(.SD, by = c("athlete_id","event_id")) >= MIN_PAIRS) fit_one(.SD),
                by = .(family, has_cr), .SDcols = c("x","y","athlete_id","event_id")]
  res_fam[, pct_per_km := round(100 * (exp(beta) - 1), 2)]
  res_fam[, scope := fifelse(has_cr, "residual (race effect applied)",
                             "gross (no race effect)")]
  setorder(res_fam, has_cr, beta)
  say("\n=== residual altitude effect, split by whether a race effect was applied ===")
  say("the `has_cr = TRUE` rows are what a model can still gain from.")
  print(res_fam[, .(family, has_cr, beta = round(beta, 4), se = round(se, 4),
                    t = round(t, 1), pct_per_km, n_ath_ev)])
  fam <- rbind(fam, res_fam, fill = TRUE)
}

out <- file.path(D, "altitude_effect.parquet")
write_parquet(fam, out)
say("\nwrote %s (%d rows)", basename(out), nrow(fam))
say("NOT wired into any prediction path by this script -- that is a separate,")
say("measured arm. See docs/reference/storage-formats.md on model lifecycle.")
