# WAC-CLASS SCORING WEIGHTS: what a race is worth when we judge the model.
#
# The project forecasts LA 2028. Getting an Olympic final wrong costs more than
# getting a category F club meet wrong, and an unweighted mean says they are
# worth the same. Every marks number measured before 2026-09-07 used that
# unweighted mean, which is why the headline read -8.38% while the championship
# races it exists to predict were nearer -6%.
#
# This is the single definition. Sourced by the scorer and by the fitters, so
# "what better means" is stated once rather than re-derived per script.
#
# DISTINGUISH IT FROM THE OTHER TWO TIER LEVERS, which sound alike:
#   w_static                how much a historical MARK counts when estimating
#                           an athlete's ability (swept as ^lambda; 0 won)
#   CITIUS_LAB_TIER_WEIGHT  how much a test row counts when FITTING, so the 27x
#                           more numerous T2 races cannot dominate
#   THIS FILE               how much a race counts when SCORING
#
# The default follows Pete's steer -- top tier worth 10x a club meet:
#
#   OW  Olympic Games / World Championships     10
#   GL  Diamond League                          10
#   GW  Continental Tour Gold / World-tier      10
#   DF  Diamond League Final                    10
#   A   category A meet                          3
#   B   category B meet                          3
#   C, D, E, F   everything below                1
#
# An unknown or missing code gets 1. That is deliberate: an unrecognised code
# should not silently inherit championship weight, and a zero would silently
# drop races. Codes are listed explicitly so a new one shows up as a 1 and gets
# noticed rather than guessed at.
#
# Override wholesale with CITIUS_SCORE_WEIGHTS, e.g.
#   CITIUS_SCORE_WEIGHTS="OW=1,GL=1,GW=1,DF=1,A=0,B=0,C=0,D=0,E=0,F=0"
# scores championships only. Set every weight to 1 to recover the old
# unweighted numbers for comparison with anything measured before today.

.WAC_SCORE_DEFAULT <- c(OW = 10, GL = 10, GW = 10, DF = 10,
                        A = 3, B = 3, C = 1, D = 1, E = 1, F = 1)

# --- WEIGHTS DERIVED FROM MEASURED FIELD STRENGTH ---------------------------
#
# The hand-set table above is a statement of priorities. It broadly tracks field
# strength (Spearman 0.856) but inverts two pairs: GL is weighted 10 and is
# weaker than A at 3, and C is weighted 1 and stronger than B at 3. This derives
# the weights from the measurement instead.
#
# THE METHOD IS EXPONENTIAL TILTING, which is the standard importance weight for
# scoring one population when you sampled another:
#
#   w(z) = exp(beta * z)
#
# where z is the class's mean field strength in within-event standard deviations
# (diagnostics/wac_class_profile.R). Exponential rather than proportional
# because strength goes NEGATIVE -- E is -0.004 and F is -0.205 -- so w ~ z
# would hand out negative weights, and because the weights are used
# multiplicatively.
#
# BETA IS NOT A FREE PARAMETER. It is solved so the WEIGHTED MEAN STRENGTH of
# the corpus equals the strength of the races being forecast:
#
#   sum(n_c * w(z_c) * z_c) / sum(n_c * w(z_c))  =  z_target
#
# That converts "how much is an Olympic final worth?" -- unanswerable -- into
# "what strength of race are we predicting?", which has an answer: OW, the World
# Championships and Olympic Games, at z = 1.256. Every weight follows.
#
# The n_c matter: F is 488,072 races at z = -0.205, so tilting the corpus mean
# up to championship level takes a sharp beta, and the weights come out steeper
# than a naive read of the strength gaps would suggest.
.wac_strength <- c(DF = 2.036, OW = 1.256, GW = 1.042, A = 1.037, GL = 0.860,
                   C = 0.665, B = 0.235, D = 0.205, E = -0.004, F = -0.205)
.wac_races    <- c(DF = 248, OW = 6359, GW = 4251, A = 7523, GL = 6766,
                   C = 20011, B = 53271, D = 50415, E = 39026, F = 488072)

# THE TILT IS SET BY A CAPPED RATIO, NOT BY A TARGET MEAN.
#
# Solving beta so the weighted corpus mean reaches championship strength is the
# textbook importance weight and it is unusable here: F is 488,072 races at
# z = -0.205, so dragging the mean to OW's 1.256 needs beta = 4.27 and makes DF
# worth 4,291x F. The objective becomes 248 races wearing a mask -- the
# effective sample collapses and the metric measures noise.
#
# So beta is set by how far apart the extremes should be:
#
#   beta = log(ratio) / (max(z) - min(z))
#
# ratio = 10 keeps the span Pete asked for -- an Olympic-class race worth ten
# club races -- while the ORDER and the SPACING come from the measurement rather
# than from judgement. That fixes both inversions in the hand-set table: GL was
# weighted 10 while being weaker than A at 3, and C was weighted 1 while being
# stronger than B at 3.
#
# WHAT THIS CANNOT FIX, and it bounds how seriously to take the spacing: the
# classes are not homogeneous. GL's 0.860 averages strong Diamond League meets
# with much weaker continental championships, and the same meeting's main
# programme has been coded GL, GW and DF in different years -- Weltklasse Zurich
# ran as GL through 2015, GW in 2016, DF 2017-2022, GW 2023-24, DF 2025. A
# single strength per class inherits that blur, so the ordering is trustworthy
# and the exact gaps are not.
wac_weights_from_strength <- function(ratio = 10, digits = 2) {
  z <- .wac_strength
  stopifnot("ratio must exceed 1" = is.finite(ratio) && ratio > 1)
  beta <- log(ratio) / (max(z) - min(z))
  w <- exp(beta * z)
  w <- w / min(w)
  structure(round(w, digits), beta = beta, ratio = ratio)
}

# Kept for the record: the target-mean version, which is correct in theory and
# degenerate in practice on this corpus. Never the default.
wac_weights_from_target <- function(z_target = 1.256) {
  z <- .wac_strength; n <- .wac_races
  wm <- function(b) { w <- exp(b * z); sum(n * w * z) / sum(n * w) }
  if (z_target >= max(z)) stop("z_target must be below the strongest class.")
  b <- stats::uniroot(function(b) wm(b) - z_target, interval = c(0, 50), tol = 1e-9)$root
  w <- exp(b * z); w <- w / min(w)
  structure(round(w, 2), beta = b, z_target = z_target, achieved = wm(b))
}

# CITIUS_SCORE_TARGET_Z switches to the derived weights: the strength of the
# races being forecast, e.g. 1.256 for World Championship / Olympic level.
wac_score_weights <- function() {
  spec <- Sys.getenv("CITIUS_SCORE_WEIGHTS", "")
  if (nzchar(spec)) {
    kv <- strsplit(trimws(strsplit(spec, ",")[[1]]), "=")
    bad <- vapply(kv, length, integer(1)) != 2L
    if (any(bad)) stop("CITIUS_SCORE_WEIGHTS wants name=value pairs, comma separated.")
    v <- stats::setNames(as.numeric(vapply(kv, `[`, "", 2)), vapply(kv, `[`, "", 1))
    if (any(!is.finite(v) | v < 0)) stop("CITIUS_SCORE_WEIGHTS values must be finite and >= 0.")
    return(v)
  }
  # DEFAULT: the target-mean tilt at z = 1.0, Pete's call on 2026-09-07.
  #
  # That is "score the corpus as though the average race were a strong
  # international meet" -- between continental championships (0.860) and the
  # World Indoors (1.042). It is a far sharper tilt than the 10:1 cap it
  # replaces, and deliberately so: a 10:1 span barely moved the objective off
  # unweighted, which for a project forecasting LA 2028 understates how much the
  # championship races matter.
  #
  # CITIUS_SCORE_TARGET_Z moves the target; CITIUS_SCORE_RATIO switches back to
  # the capped form; CITIUS_SCORE_WEIGHTS overrides both with a literal table.
  zt <- Sys.getenv("CITIUS_SCORE_TARGET_Z", "")
  rt <- Sys.getenv("CITIUS_SCORE_RATIO", "")
  if (nzchar(rt)) return(wac_weights_from_strength(as.numeric(rt)))
  wac_weights_from_target(if (nzchar(zt)) as.numeric(zt) else 1.0)
}

# Cached race_key -> WAC class, returned as `race_tier`.
#
# THE COLUMN IS RENAMED ON READ, ON PURPOSE. The corpus stores it as `tier`, and
# a bare `tier` is ambiguous in this codebase because two unrelated
# classifications share the word:
#
#   meet_tier   the CATALOGUE's rating of a MEETING: T1_elite, T2_strong,
#               T3_development. What the lab's test set is filtered on.
#   race_tier   the World Athletics category of a RACE: OW, GL, GW, DF, A-F.
#               What the scoring weights use.
#
# They cross: a T1_elite meeting contains races of several WAC classes. Weltklasse
# Zurich's Diamond League disciplines are race_tier GW while its supporting
# programme is race_tier F, and both sit inside meet_tier T1_elite. Ninety of the
# 849 held-out races in the "elite" test set are race_tier F for exactly that
# reason.
#
# The stored column keeps its name -- renaming it would invalidate a 7.5M-row
# parquet store and every cached artefact -- so the disambiguation happens here,
# at the one place every scorer reads it.
.race_tier_lookup <- function(out_dir) {
  f <- file.path(out_dir, "race_tier.rds")
  if (file.exists(f)) return(readRDS(f))
  ch <- data.table::setDT(readRDS(file.path(out_dir, "championship_results.rds")))
  lk <- unique(ch[, .(race_key, race_tier = tier)], by = "race_key")
  rm(ch); invisible(gc())
  saveRDS(lk, f)
  lk
}

# Attach `sw`, the scoring weight, to a table carrying `race_key`.
# Reports the weight actually applied, because a scoring weight that silently
# defaulted to 1 everywhere would make a weighted run look identical to an
# unweighted one with nothing to say which had happened.
attach_score_weight <- function(d, out_dir, quiet = FALSE) {
  w <- wac_score_weights()
  # Accept a legacy `tier` column but work in `race_tier` from here on, so no
  # caller downstream has to guess which classification it is holding.
  if ("tier" %in% names(d) && !"race_tier" %in% names(d))
    data.table::setnames(d, "tier", "race_tier")
  if (!"race_tier" %in% names(d)) {
    d <- merge(d, .race_tier_lookup(out_dir), by = "race_key", all.x = TRUE)
  }
  d[is.na(race_tier), race_tier := "unknown"]
  j <- match(d$race_tier, names(w))
  d[, sw := data.table::fifelse(is.na(j), 1, unname(w[j]))]
  # NORMALISED TO MEAN 1, which matters more than it looks.
  #
  # A weighted mean is invariant to scaling, so the scorer does not care. The
  # FITTERS do: they shrink each event toward its family by an evidence count,
  # and the kappas were tuned against unweighted counts near 1,200. Left raw,
  # weights up to 10 took the effective count from 25,077 to 183,095 and the
  # shrinkage all but vanished -- family ranges for context_scale widened from
  # 0.467-0.598 to 0.321-1.118 with nothing having been decided.
  #
  # That is a side effect of a scoring change reaching into a fitting parameter,
  # silently and in the wrong direction. Normalising keeps "how much evidence is
  # there" on the same scale as before, so the weights change WHICH rows matter
  # without changing HOW MUCH the model is allowed to move.
  mw <- mean(d$sw)
  if (is.finite(mw) && mw > 0) d[, sw := sw / mw]
  if (!quiet) {
    s <- d[, .(races = data.table::uniqueN(race_key), weight = data.table::first(sw)),
           by = race_tier]
    data.table::setorder(s, -weight, -races)
    cat("scoring weights in force:\n")
    print(s)
    cat(sprintf("  effective race count %.0f from %d actual races\n",
                sum(unique(d[, .(race_key, sw)])$sw), data.table::uniqueN(d$race_key)))
  }
  d[]
}
