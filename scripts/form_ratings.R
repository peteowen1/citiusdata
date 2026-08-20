# THE FORM MODEL -- sequential walk-forward ratings. See docs/plans/FORM-MODEL.md
# for the method, its validation, and the adjustment ladder. This answers "how
# good are you RIGHT NOW"; the career model (estimate_ability) answers "how good
# are you over a full record" and keeps the forecasts.
#
# Sequential walk-forward engine v3. All athletics events, T1+T2, 2020->now,
# ONE global chronological sweep so cross-event information is available at the
# moment it is needed. Every adjustment is a flag; 2025 races are the TUNING
# window, 2026 the CONFIRMATION window (score both, tune only ever on 2025).
# v3 over v2: per-athlete variance learned at the same rate as the mean (with a
# floor so a lucky streak cannot collapse it), and a walk-forward majors-finals
# scorecard (concordance, favourite, medal hits) written per run.
#
# Flags (env): SEQ_CENS   censor weight for negative surprise in heats/semis/qual
#                         (1 = off; 0.3 = a cruise counts 30% on the way down)
#              SEQ_AGE    1 = drift ratings along the family aging curve between
#                         appearances (exact curve difference; NA age = no drift)
#              SEQ_STALE  1 = evidence decays with time away (n_eff, family
#                         half-life), so k recovers after a layoff
#              SEQ_XEV    1 = cold-start from a same-family sibling event rating
#                         (mean-shift mapping, blended 50/50 with the first race)
#              SEQ_KT1    k multiplier at T1 meets (1 = off)
#              SEQ_WINDCS 1 = wind-adjust the first race at cold start
suppressMessages(library(data.table)); suppressMessages(library(arrow))
# Numeric knobs from the environment, safely. An env var set to the EMPTY string
# is not unset: Sys.getenv returns "" rather than the default, and as.numeric("")
# is NA — so `SEQ_MAXPLACE=""` silently gave MAXPLACE = NA and the run died deep
# in the loop (2026-08-15). Worse, an empty SEQ_K0 would have run the whole model
# with an NA learning rate. Treat empty as unset, and refuse garbage loudly.
.env_num <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) return(default)
  x <- suppressWarnings(as.numeric(v))
  if (!is.finite(x)) stop(sprintf("%s='%s' is not a finite number", name, v))
  x
}
OUT <- "C:/dev/citiusverse/citiusdata/data"
SC  <- Sys.getenv("FORM_OUT", here::here("citiusdata", "data"))
# Defaults are the 2026-08-14 swept optimum (see docs/plans/FORM-MODEL.md):
# k0 0.95 and floor 0.32 both moved; kappa 3 was already optimal. The old
# eye-chosen 0.55 / 3 / 0.18 scored 68.028 on the 2025 tuning window; these
# score 68.564, and 67.353 -> 68.018 on the sealed 2026 window.
K0 <- .env_num("SEQ_K0", 0.95); KAPPA <- .env_num("SEQ_KAPPA", 3)
KFLOOR <- .env_num("SEQ_KFLOOR", 0.32); CSHRINK <- .env_num("SEQ_C", 4)
# The ladder winners are ON by default, so a bare run IS the chosen model rather
# than the model minus its adjustments. Set SEQ_AGE=0 / SEQ_STALE=0 / SEQ_CENS=1
# to turn them off. (Leaving them opt-in is how 350,401 fitted race effects sat
# inert on every shipped number — dormant by flag, which no wiring guard sees.)
CENS <- .env_num("SEQ_CENS", 0.3); AGEF <- Sys.getenv("SEQ_AGE","1") != "0"
# WINNER CENSORING. SEQ_CENS already says "a slow run you had no reason to win
# fast should not count fully" - it just asks the wrong question, `is this a
# heat?`, when the right one is `did the result beat the time?`. A slow tactical
# WIN is a lower bound on ability, not a measurement of it: the athlete had no
# reason to run faster.
#
# Traced, on the deployed arm: Almgren won the 2025 European 10,000m final in
# 28:53 and it cost him surprise -0.0397, about 4% of rating and ~51 seconds,
# with the race shock at exactly 0.0000 for that race - so nothing absorbed it.
# Two more of his wins carried negative surprise. He ranked 17th having won.
#
# 1 = off. Applied as the MINIMUM of the applicable discounts rather than the
# product, so a heat winner is discounted once, not twice.
# ADOPTED 2026-08-19 at 0.1. Swept, weighted-sealed: 1.0 (off) 73.039,
# 0.5 73.117, 0.3 73.148, 0.1 73.178, 0.0 73.176 - so 0.1 is an interior
# optimum, not a range edge, and the curve is flat below 0.3. Raw sealed rises
# 71.660 to 71.740. Largest single gain measured in this model.
#
# It lands where tactical racing lives: distance +0.177 (7 of 9 events up),
# middle +0.151 (7 of 10), with 1500m M +0.267 over 73,302 pairs against a 0.160
# noise floor. It LOSES slightly in sprints, jumps and hurdles - which is
# mechanistically right, because you cannot coast a 100m, so a slow winning time
# there really is bad news. Worth a per-family value later.
#
# THE RATCHET WORRY, tested and dead: discounting only NEGATIVE surprise for
# winners keeps good news and drops bad, so ratings could inflate. Athlete-events
# whose implied mark beats the world record: 1 at every setting from 1.0 to 0.0,
# p99 as a share of the record moves 94.7 to 94.9 across the whole sweep, median
# 84.5 unchanged. No inflation.
CENSWIN   <- .env_num("SEQ_CENSWIN", 0.1)
CENSWIN_P <- .env_num("SEQ_CENSWIN_PLACE", 1)
STALE <- Sys.getenv("SEQ_STALE","1") != "0"; XEV <- Sys.getenv("SEQ_XEV","") != ""
KT1 <- .env_num("SEQ_KT1", 1); WINDCS <- Sys.getenv("SEQ_WINDCS","") != ""
# SEQ_CEIL  weight on an athlete's BEST MARK SO FAR, blended into the value used
# to ORDER a field: r_use = (1-CEIL)*r_pre + CEIL*best. Season best where the
# athlete has raced this year, career best otherwise, both strictly lagged.
#
# The rating tracks an athlete's AVERAGE; the best mark tracks their CEILING,
# and the two are not redundant — ordering by best mark alone scores 77.22% on
# the 2026 sealed window against the model's 78.05%, i.e. it is nearly as good
# on its own while being wrong in different places. Offline sweep on 2025 gave a
# clean interior peak at 0.30 (+0.332 pp), confirmed at +0.278 pp on 2026.
#
# PREDICTION ONLY. The update below deliberately still runs on r_pre: feeding a
# blended value back into R would make the rating chase its own ceiling, and the
# two would co-drift with nothing anchoring the level.
# ADOPTED 2026-08-15 at 0.30 after an end-to-end A/B: 69.127 -> 69.427 tuning,
# 69.387 -> 69.669 sealed, favourite 52.7% -> 53.2%. SEQ_CEIL=0 is bit-identical
# to the pre-blend engine (verified: it reproduced 69.127 / 69.387 exactly).
CEIL <- .env_num("SEQ_CEIL", 0.30)
# SEQ_BEST_K   how many of an athlete's best marks the ceiling blend averages.
#              1 keeps the original rule exactly (season best if they raced the
#              event this year, career best otherwise).
# SEQ_BEST_HL  half-life in days for down-weighting an old mark inside that
#              average. Inf (the default) means no decay.
#
# WHY MORE THAN ONE MARK. The single best is a maximum, and a maximum cannot
# tell a repeatable level from one outlier. Measured on the current corpus:
#   Kerr    1500m   3:27.79, 3:29.05, 3:29.37, 3:29.38, 3:29.64, 3:30.07 ...
#   Rayner 10,000m  27:09.57, then next best about 28:09
# A top-3 mean costs Kerr about a second and costs Rayner about fifty. That is
# the whole discrimination, and it needs no knowledge of who either man is.
#
# AGE IS THE WRONG DISCRIMINATOR HERE, which is why K comes before HL. Both of
# those bests are roughly two years old, so a decay alone penalises them
# equally. Worse, the obvious proxy fails: Kerr's gap between rating and best is
# 4.1% of his time and Rayner's is 3.4%, so by that measure Kerr looks like the
# bigger outlier. Only counting how many races sit NEAR the best gets it right.
#
# HL still earns its place, for a different case: an athlete who ran three fast
# marks years ago and is now declining. K cannot see that; decay can.
BEST_K  <- max(1L, as.integer(.env_num("SEQ_BEST_K", 1)))
# Not .env_num: that helper rejects non-finite values, which is right for every
# other knob and wrong here - Inf is this one's meaningful default, "no decay".
BEST_HL <- local({
  v <- Sys.getenv("SEQ_BEST_HL", "")
  if (!nzchar(v)) return(Inf)
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x) || x <= 0) stop(sprintf("SEQ_BEST_HL='%s' must be a positive number of days, or Inf", v))
  x
})
# The top-K average of an athlete's marks, weighted by recency. Returns NULL
# when there is nothing recorded, so callers keep their existing NULL handling.
.best_k <- function(K, v, dts, now) {
  if (is.null(v) || !length(v)) return(NULL)
  w <- if (is.finite(BEST_HL)) 2^(-(now - dts) / BEST_HL) else rep(1, length(v))
  if (!sum(w) > 0) return(max(v))
  sum(w * v) / sum(w)
}
# SEQ_SEED  1 = initialise a debut rating from results already held in the
# careers store (4,978,201 rows against the corpus's 1,225,339 — the corpus is
# roughly a quarter of what is on disk). 27.9% of 2026 cold-start athlete-events
# have a prior SAME-EVENT result, 92.2% of those within two years.
#
# Cold starts are 28.7% of the scored metric at 52.94% while every other depth
# band sits at 74–77%, so this is the only large lever left. See
# check_cold_coverage.R / check_cold_recency.R.
#
# The seeded races are NOT added to the corpus — they only set what an athlete
# carries INTO their first scored race. The scored set is unchanged, so the
# metric stays comparable and any corpus-quality reason for their exclusion
# (tier filter is the likely one) stays contained.
# ADOPTED 2026-08-16: on by default. Sealed window 70.267 -> 71.214 (+0.947 pp)
# and MAJORS FINALS 70.84 -> 73.89 (+3.05 pp), favourite 47.1% -> 51.9%, medal
# hits 60.5% -> 64.9%. Set SEQ_SEED=0 to turn it off.
SEEDON <- Sys.getenv("SEQ_SEED","1") != "0"
# 45, not 365 (2026-08-16). Bracketed on the WEIGHTED metric: 365 -> 71.847,
# 180 -> 71.323 (at 20/8), 45 -> 72.019, 21 -> 72.000, 10 -> 71.357 — worse on
# both sides. +0.172 over 365 against a 0.118 pp noise floor. An earlier reading
# that majors preferred 365 was noise: that metric has a ~0.39 pp floor and the
# spread being read off it was 0.35 pp.
SEEDHL <- .env_num("SEQ_SEEDHL", 45)    # half-life in days for the weighted mean
SEEDNE <- .env_num("SEQ_SEEDNE", 5)     # cap on seeded n_eff, so it still learns fast
# SEQ_HUBER  robust update. 0 = off. Otherwise a surprise larger than
# HUBER x the athlete's OWN sd has its step capped there, so a catastrophe moves
# the rating by a bounded amount instead of a proportional one.
#
# Motivating case: Werro, European Indoors final 2025-03-09, ran 2:27.37 off a
# 2:01.39 rating — a 4.9-sigma miss — while the other five finished within 1.3%
# of theirs. She fell. Her rating went 2:01.4 -> 2:07.6 in one afternoon and took
# four races to recover. Results >11% off a rating are 0.81% of the corpus and
# the residual distribution is left-skewed (-0.47): nobody runs 18% FAST.
#
# The tension worth remembering before tuning this: a fall and a genuine
# collapse are IDENTICAL in the data. Clipping the tail also blunts real
# decline, so a lower HUBER is not automatically better even if it scores
# better — check what it does to athletes who really did fall off.
#
# Heat censoring is a crude special case of this (bad qualifiers only); Huber is
# the general form and applies in finals too, which is where falls hurt most.
# 3. Huber 2 scores 0.047 pp higher on the tuning window and 3 scores 0.039 pp
# higher on the sealed one, both inside a 0.118 pp noise floor — so the pair is
# not separable and the score cannot choose. 3 is taken because it is the value
# check_huber_decline.R validated, and because it clips LESS aggressively, which
# is the conservative side of the one failure this knob has that the metric
# cannot see: blunting a genuine collapse. Both clearly beat off (+0.202/+0.155).
HUBER <- .env_num("SEQ_HUBER", 3)
SCORE_MERGED <- Sys.getenv("SEQ_SCORE_MERGED", "0") != "0"
# See the note at the shock estimator: "mean" reproduces the original behaviour,
# "median" makes it robust to athletes who stop racing.
# ADOPTED 2026-08-18 as a trimmed mean at 0.20. The mean is not robust to
# athletes who stop racing, and pacemakers dropping out is routine in distance
# running. Measured, five arms on the same corpus:
#
#   shock        raw 2026   weighted sealed   Barega 1500m   ratings > own best
#   mean          71.711        73.001         3:29.02  (!)       7.64%
#   median        71.633        72.995         3:38.09            6.41%
#   trim 0.10     71.716        72.995         3:31.26  (!)       7.51%
#   trim 0.20     71.680        73.036         3:34.04            6.85%
#   trim 0.30     71.665        73.031         3:37.51            6.59%
#
# Barega's actual 1500m range is 3:32.93-3:37.50, so 3:34.04 is right and
# 3:29.02 was a rating faster than any 1500m he has run. Across the corpus the
# share of athlete-events rated faster than the athlete's own best falls from
# 7.64% to 6.85% - about 2,600 impossible ratings - and the sealed window is
# the best of the five. The median fixes robustness but discards the
# information in every ordinary race, which is what costs it 0.08 pp.
SHOCK <- Sys.getenv("SEQ_SHOCK", "trim")
stopifnot("SEQ_SHOCK must be 'mean', 'median' or 'trim'" =
            SHOCK %in% c("mean", "median", "trim"))
# fraction trimmed from EACH end when SHOCK = "trim"
SHOCK_TRIM <- .env_num("SEQ_SHOCK_TRIM", 0.20)
# HOW MUCH OF A MEASURED RACE EFFECT ACTUALLY GETS APPLIED. Measured 2026-08-18
# on 169,571 races: 49.1% of them receive a shock of effectively zero, and
# 27,262 of those had a measured race effect above 1%. Two causes, both here:
#
#   1. a HARD FLOOR of 3 established athletes (n_eff >= 2), below which S is set
#      to exactly 0. Median field size is 7, so this fails constantly.
#   2. the estimate is multiplied by the established athletes' SHARE of the
#      field. That scales the estimate rather than confidence in it: a shock
#      cleanly measured off 6 known athletes in a field of 30 is cut to a fifth,
#      and the missing four fifths is charged to every athlete as personal form.
#
# Kept fraction of the measured effect, by established share: <10% 0.0%,
# 10-25% 0.4%, 25-50% 2.4%, 50-75% 12.9%, >75% 55.3%.
#
# SEQ_SHOCK_W = "kappa" is now the DEFAULT (adopted 2026-08-19, see below): it
# weights by how many athletes the estimate rests on, m / (m + SEQ_SHOCK_K),
# which does not care how many strangers were also in the race - a shock measured
# off 6 athletes is equally well measured whether the field is 8 or 80.
# SEQ_SHOCK_W = "share" asks for the OLD behaviour explicitly.
# ADOPTED 2026-08-19 after a five-point sweep. Weighted-sealed by K:
#   0.5 -> 72.997 | 1 -> 73.025 | 2 -> 73.039 | 4 -> 73.043, against the old
# share weighting at 73.006 and lowering the floor alone at 73.010. K=4 edges
# the weighted metric by 0.004 (noise) while losing 0.033 on raw, so K=2 is the
# interior optimum: +0.009 raw and +0.033 weighted over the previous default.
#
# The aggregate understates it because the corpus is 98% T2 track, where fields
# are mostly established athletes and the share multiplier barely bit. By family
# the effect lands where the mechanism says it should - road +0.630 (4 of 4
# events up, none down) and distance +0.245 (7 of 9), with 10,000m M +0.804,
# 5000m M +0.336 and 5000m W +0.320 all clearing their noise floors. Every
# family that lost sits inside its own floor.
SHOCK_MINN <- .env_num("SEQ_SHOCK_MINN", 2)
SHOCK_W    <- Sys.getenv("SEQ_SHOCK_W", "kappa")
SHOCK_K    <- .env_num("SEQ_SHOCK_K", 2)
stopifnot("SEQ_SHOCK_W must be 'share' or 'kappa'" = SHOCK_W %chin% c("share", "kappa"))
# SEQ_SLOPE  fit a per-race SLOPE as well as a shift. 0 = shift only (original).
#
# WHY A SHIFT IS NOT ENOUGH. The shock is one number subtracted from everyone,
# so it can model a race being slow but not the field BUNCHING UP - and a
# tactical race does exactly that: the spread of finishing times is far smaller
# than the spread of abilities, because nobody runs to their potential. The
# favourite, expected to beat the field by their full rating gap, comes up
# short; the tail-ender exceeds. Measured in major finals, monotone across five
# bands of rating advantage:
#   weakest 20%  +0.00994      middle -0.00087      strongest 20% -0.00663
# About 1.4 seconds of rating on a 3:30 1500m, every major final, for the best
# athletes. That is why Kerr and Almgren are held down by winning slowly.
#
# SEQ_SLOPE_K shrinks the fitted slope toward 1 (i.e. toward shift-only), because
# a race is a regression with about ten points and the slope is noisy. The
# estimate uses only the trimmed set, for the same reason the shift does.
SLOPE      <- Sys.getenv("SEQ_SLOPE", "0") != "0"
SLOPE_K    <- .env_num("SEQ_SLOPE_K", 8)
SLOPE_MINN <- .env_num("SEQ_SLOPE_MINN", 5)
# SEQ_VPRIOR  1 = derive the thin-record variance prior from WITHIN-ATHLETE
# variation instead of within-race spread. Default off until A/B'd.
#
# The old prior was the median within-race variance of `perf` — how spread out
# DIFFERENT athletes are in one race. `v_pre` is supposed to be ONE athlete's
# race-to-race variation. Those are different quantities, and the first is
# **5.19x larger** than the second at the median event, up to 12.9x for the
# throws (shot put M: prior sd 8.93% of a mark against a learned 2.44%) — for
# the obvious reason that a shot put final spans 15m to 22m while any one
# athlete varies by ~2.4%.
#
# Estimated as median over athletes with >= 8 races of var(diff(perf))/2.
# Differencing removes the athlete's level AND any slow improvement trend, which
# a plain var(perf) would wrongly bank as race-to-race noise. Correlates 0.968
# (log scale) with what deep records actually learn, and unlike the learned
# value it is computable from the corpus, so it is not circular.
#
# VPADJ: the estimator runs 1.63x larger than the learned variance because
# var(diff) retains the race shock while v_pre is the variance of the
# SHOCK-ADJUSTED surprise. Dividing puts the prior on the scale the model
# actually learns on, so a thin record starts where a deep one ends.
# ON by default (2026-08-16). Costs 0.034 pp on the weighted metric — inside its
# 0.118 pp noise floor, so effectively free — and takes the "good day" column
# from being beaten 12.19% of the time to 10.06%, i.e. it becomes a genuine 90th
# percentile rather than one in name. Set SEQ_VPRIOR=0 to revert.
VPRIOR <- Sys.getenv("SEQ_VPRIOR","1") != "0"
# RETUNED 1.63 -> 0.5 on 2026-08-20, on a bracketed sweep against the CALIBRATION
# of the stated uncertainty rather than against the ordering.
#
# The 1.63 above is the ratio by which var(diff) exceeds the learned variance,
# because var(diff) retains the race shock and v_pre is the shock-adjusted
# surprise. Dividing by it puts the prior on the scale the model LEARNS on. That
# reasoning is right for a steady-state athlete and wrong for a debutant, and the
# prior is only ever used for debutants: a first-timer's uncertainty is not just
# their race-to-race variation, it is that PLUS not knowing their level at all.
# Removing the shock and then also charging nothing for unknown level made the
# starting variance far too tight.
#
# Measured as the robust (MAD) scale of z = (perf - r_pre) / sqrt(v_pre + v_shock)
# by evidence band, where 1.000 is honest. Six arms, one engine sha:
#
#   VPADJ   prior sd   cold <1   thin 1-3   mid 3-8   deep 8+   sealed wtd
#   0.35     3.61%      0.912     0.920      1.018     1.065     73.422
#   0.50     3.01%      1.044     0.968      1.037     1.067     73.414
#   0.75     2.45%      1.207     1.020      1.056     1.069     73.403
#   1.00     2.12%      1.329     1.055      1.067     1.069     73.405
#   1.63     1.66%      1.547     1.109      1.082     1.070     73.393
#   2.50     1.34%      1.741     1.153      1.093     1.071     73.388
#
# INTERIOR, not an edge: cold crosses 1.000 between 0.35 and 0.50, thin crosses
# between 0.50 and 0.75, so 0.5 is bracketed on both sides rather than being the
# lowest value anyone tried. The first sweep stopped at 1.0 and would have picked
# it purely for being the end of the range.
#
# The deep band moves 1.071 -> 1.067 across the whole sweep, which is the control:
# a prior that shifted well-evidenced records would be reaching somewhere it has
# no business. It does not.
#
# COSTS NOTHING ON THE ORDERING. Sealed weighted concordance rises 73.393 ->
# 73.414, which is inside its 0.159 pp floor and is therefore reported as "no
# harm" rather than claimed as a gain.
VPADJ  <- .env_num("SEQ_VPADJ", 0.5)
VPMINA <- .env_num("SEQ_VPMINA", 20)   # min athletes before an event is trusted
# SEQ_KPOW  scale the initial learning rate by how NOISY the event is:
#   k0_event = k0 * (median_sd / event_sd) ^ KPOW
#
# One knob, not one per family, because the mechanism says what the shape should
# be rather than leaving it to be fitted. A filter should learn SLOWLY from a
# noisy measurement and FAST from a precise one, and athletics events differ by
# 2.7x in exactly that: measured within-athlete sd is 2.44% of a mark in the
# shot put against 0.90% in the 60m hurdles. One global k0 cannot be right for
# both, and until now every event has used the same one.
#
# KPOW = 0 is the current behaviour exactly (identity check). 1 is full inverse
# scaling. The per-event sd comes from the same within-athlete estimate the
# variance prior uses, so this needs SEQ_VPRIOR on - which it is by default.
KPOW <- .env_num("SEQ_KPOW", 0)
# SEQ_CEILADJ  event-specific ADJUSTMENT to the ceiling blend, on top of the
# baseline CEIL. Pete's framing: baseline parameters, then event-specific
# adjustments - rather than free parameters per event, which is 68 knobs and an
# invitation to overfit.
#
#   technical events (jump, throw)   CEIL + ADJ
#   tactical  events (middle, dist)  CEIL - ADJ
#   everything else                  CEIL
#
# Mechanism, and it is the reason to prefer this over fitting: in a jump or a
# throw a best mark is a TECHNICAL CEILING the athlete can repeat, so it says a
# lot about them. In a tactical 1500m the best mark is a property of how the
# race was run - a sit-and-kick final and a paced meet record produce very
# different marks from the same athlete - so it says less.
#
# `technical` and `tactical` are already in the event registry and have never
# been used by this model. ADJ = 0 is the current behaviour exactly.
CEILADJ <- .env_num("SEQ_CEILADJ", 0)
# SEQ_SEEDHLPOW  scale the SEED half-life by how often the event is contested:
#   hl_event = SEEDHL * (event median race gap / median across events) ^ POW
#
# Pete's observation, and the mechanism is worse than a slightly-wrong half-life.
# The seed weights are 2^(-gap/SEEDHL) and the seeded evidence is
# ne0 = min(sum(weights), SEEDNE). For a marathoner whose previous marathon was
# 200+ days ago every weight underflows, so ne0 ~ 0: they are seeded with a
# VALUE but no EVIDENCE, k runs at its maximum, and their first corpus marathon
# overwrites the seed almost entirely. That is why the marathon top ten sits at
# n_eff 1.0-5.6 with ratings equal to single races.
#
# 45 days is right for a sprinter racing weekly and meaningless for an event
# contested annually. POW = 0 is the current global behaviour exactly.
SEEDHLPOW <- .env_num("SEQ_SEEDHLPOW", 0)
# SEQ_XBLEND  lean a THIN rating on the same athlete's rating in a SIBLING
# EVENT, mapped onto this event's scale. 0 = off.
#
#   w      = XBLEND / (n_eff + XBLEND)
#   mapped = R_sibling - mu_sibling + mu_event
#   r_use  = (1 - w) * r_use + w * mapped
#
# So a deep record ignores its siblings and a thin one leans on them heavily,
# which is the whole point: the fix has to be invisible where evidence exists.
#
# WHY THIS IS NOT `SEQ_XEV`, WHICH WAS REFUTED TWICE. That knob fires only at
# COLD START, once, and needs a sibling with n_eff >= 5 of the SAME FAMILY. It
# was measured on average pairwise concordance both times - a metric dominated
# by sprinters racing twenty times a year - and found worth +0.007 and +0.018 pp.
# Neither measurement looked where the effect lives.
#
# The failures it leaves behind are blatant. Josh Kerr set the mile world record
# and is unranked in the mile, because it is his first mile in the corpus
# (n_eff 0.3) - while he holds 35 races and a 3:27.79 Olympic final at 1500m, a
# distance 8% shorter. Andreas Almgren is 1st at 3000m and 5000m and outside the
# 10,000m top ten on n_eff 1.5. The two 10km race walks correlate 0.614 across a
# 1.1% surface offset and share nothing.
#
# ORDERING ONLY. Like the ceiling blend, this changes what `r_use` compares and
# never what `R` learns - a rating that trained on its own sibling would drift
# toward the family mean and stop being about the event.
# ADOPTED 2026-08-17 at 1. End-to-end arms, all on the same corpus:
#   off                                  71.994 tune / 71.711 sealed
#   on, every family                     72.023 / 71.718
#   on, distance+middle+road+walk        72.013 / 71.746
#   on, distance+middle+sprint+hurdles   72.053 / 71.755   <- adopted
#   same set at strength 2               72.052 / 71.739
# Strength 1 beats 0 below it and 2 above it, so this is an interior optimum
# rather than the highest value that happened to be tried.
XBLEND  <- .env_num("SEQ_XBLEND", 1)
# ADOPTED 2026-08-18: no cutoff. The wall was a lookup optimisation, not a
# modelling decision - the blend weight is already xb/(n_eff + xb), which is
# 4.8% at n_eff 20 and 2% at 50, so deep records self-attenuate without help.
# Measured: cutoff 8 gave 72.073/71.782, cutoff 20 gave 72.070/71.776, none at
# all 72.070/71.770 - all inside noise of each other.
#
# Removed because the wall was excluding real cases while buying nothing. Josh
# Kerr sits at n_eff 10.05 in the 1500m, just past the old cutoff, so it skipped
# him entirely while his Mile (r 0.883, and 2.23 races after Glasgow) had
# something to say. Information should be downweighted, not discarded.
XB_MAXN <- .env_num("SEQ_XB_MAXN", 1e9)
XB_MINS <- .env_num("SEQ_XB_MINS", 2)    # a sibling needs some evidence itself
# SEQ_XB_FAM  which families may borrow from a sibling event.
#
# Measured per event (check_concordance_by_event.R), XBLEND 1 vs 0, deltas that
# beat the event's own noise floor and mostly replicate on the sealed window:
#
#   10,000m W  +1.349 / +1.104     Hammer Throw M   -0.527 / -0.665
#   10,000m M  +0.911              Discus Throw W   -0.686
#   Mile W     +0.686              Weight Throw M   -1.079
#   5000m W    +0.469
#
# AEROBIC ABILITY TRANSFERS AND TECHNICAL SKILL DOES NOT. A 5000m rating says a
# great deal about the same athlete's 10,000m; a discus rating says almost
# nothing about their hammer, and forcing the borrow makes the throw ratings
# worse. Applied globally the two cancelled to +0.029 pp - which is how a real
# effect hides inside an aggregate.
#
# THE SET BELOW WAS CHOSEN ON THREE WINDOWS, NOT TWO. Pooled delta by family
# (score_by_event.R), blend on vs off:
#
#   family     2022-24    2025     2026        family     2022-24   2025     2026
#   sprint      +0.093   +0.151   +0.065       walk        +0.012  -0.051   -0.247
#   hurdles     +0.076   +0.113   +0.036       road        -0.019  +0.199    0.000
#   distance    +0.069   +0.250   +0.296       jump        -0.023  +0.022   -0.063
#   middle      +0.066   +0.139   +0.044       combined    -0.077  +0.316   +0.272
#                                              throw       -0.091  -0.177   -0.265
#
# Four families are positive on all three; throw is negative on all three. The
# previous default carried walk and road - walk is the largest family in the
# sport by event count and the blend actively damages it - and omitted sprint
# and hurdles, which help everywhere. Combined looks strong on the two recent
# windows and goes negative on the earlier one; on four events that is noise, so
# it stays out.
#
# WHY FAMILY AND NOT PER EVENT. pool_event_params.R fits a hierarchical model
# over per-event estimates, shrinking each toward its neighbours by
# w = n/(n + kappa) with kappa estimated by empirical Bayes (measured: tau^2
# 0.0243, kappa 5,935 pairs, weights 0.04-0.87). Its assembled per-event config
# scored 72.061 / 71.753 - a tie with this family gate at 72.053 / 71.755. The
# family is the resolution the data supports; per-event freedom buys nothing.
# Independent per-event fitting would have taken +1.619 on the women's 600m and
# -0.543 on the men's, on ~250 pairs each, as findings.
XB_FAM <- strsplit(Sys.getenv("SEQ_XB_FAM", "distance,middle,sprint,hurdles"), ",")[[1]]

# SEQ_XB_MINCOR  above 0, replace the family gate entirely with MEASURED event
# similarity: borrow from a sibling only where the two events correlate at least
# this much. 0 keeps the family gate.
#
# WHY. Family is a hand-drawn stand-in for similarity, and build_event_similarity.R
# shows it is wrong in both directions. It BLOCKS the 1500m from the 3000m
# (r = 0.91), the 400m from the 400mH (0.82) and the heptathlon from the long
# jump (0.83) because a taxonomy separates them; it PERMITS the hammer to borrow
# from the shot (r = 0.24) and the high jump from the pole vault (0.36) because
# a taxonomy joins them. The rank correlation between a family's internal
# similarity and how much blending helps it is 0.886 - so similarity, not
# family, is what the blend is really keyed on, and the throws were damaged
# because the engine borrowed from siblings that say nothing about the event.
TAG <- Sys.getenv("SEQ_TAG","baseline")   # needed by the log line below
# ADOPTED 2026-08-17 at 0.80, replacing the family gate. Full sweep, all arms on
# the same corpus, with the number of event pairs the gate admits:
#   0.60  93 pairs  72.021 / 71.709      0.85  16 pairs  72.052 / 71.761
#   0.70  57 pairs  72.037 / 71.727      0.90   8 pairs  72.031 / 71.742
#   0.80  28 pairs  72.073 / 71.782      0.95   1 pair   71.995 / 71.713
# Both windows peak at 0.80 with monotone decline either side, so it is an
# interior optimum rather than the edge of the range tried. The 0.95 arm is the
# machinery checking itself: with one eligible pair it reproduces blend-off
# (71.994 / 71.711) to within a thousandth.
#
# For comparison the family gate it replaces scored 72.053 / 71.755.
#
# XB_MAXN is left at 8 although it is redundant: the blend weight is already
# xb/(n_eff + xb), which is 4.8% at n_eff 20 and 2% at 50, so the cutoff is a
# lookup optimisation rather than a modelling decision. Measured - raising it to
# 20 gives 72.070/71.776 and removing it entirely 72.070/71.770, both inside
# noise of keeping it. It stays because it is marginally better and cheaper.
XB_MINCOR <- .env_num("SEQ_XB_MINCOR", 0.80)
# SEQ_XB_PICK  "evidence" (the original rule: the other event with the most
#              races) or "cor" (the most strongly related one).
# SEQ_XB_NSIB  how many other events to combine. 1 reproduces the old shape.
# Both default to the original behaviour so the change is measured, not assumed.
# ADOPTED 2026-08-18, on principle rather than on a score. Measured across four
# arms on the post-harvest corpus, all four land within 0.005 pp of each other
# (raw 2026 concordance 71.541 / 71.540 / 71.542 / 71.542) - so this buys no
# measurable accuracy. It is not inert: pick=cor changes 4,291 rows and nsib=3
# changes 7,450, with a median shift of ~0.001-0.002. It is simply that the
# population the blend can touch is too small to move an aggregate.
#
# Adopted anyway because borrowing from the event that actually relates is the
# correct reasoning, it costs nothing, and it is right per athlete even where it
# is invisible in the mean. Do NOT quote it as an accuracy gain.
XB_PICK <- Sys.getenv("SEQ_XB_PICK", "cor")
XB_NSIB <- max(1L, as.integer(.env_num("SEQ_XB_NSIB", 3)))
stopifnot("SEQ_XB_PICK must be 'evidence' or 'cor'" = XB_PICK %in% c("evidence", "cor"))
SIM <- new.env(hash = TRUE, parent = emptyenv())
if (XB_MINCOR > 0) {
  # Overridable so a rebuilt matrix can be tested against the deployed one in
  # the same batch, rather than by swapping files under a running experiment.
  # DEFAULT CHANGED 2026-08-19 to the specialist matrix: the old 200-pair file
  # contained no road or walk events at all, and its correlations were inflated
  # by athletes a combined event forced into both events.
  sf <- file.path(SC, Sys.getenv("SEQ_SIMFILE", "event_similarity_spec.parquet"))
  # Deliberately a hard stop, not a fallback to the family gate. SEQ_XB_MINCOR
  # is ON BY DEFAULT now, and data files are gitignored, so a fresh clone or a CI
  # runner will land here - and quietly running a DIFFERENT model than the one
  # the caller asked for is the failure mode this repo has been bitten by
  # repeatedly. Build the file, or set SEQ_XB_MINCOR=0 to ask for the family gate
  # explicitly. (TODO: publish event_similarity.parquet via piggyback so this is
  # fetched like every other data artifact rather than rebuilt by hand.)
  if (!file.exists(sf))
    stop("SEQ_XB_MINCOR is ", XB_MINCOR, " (default) but ", basename(sf),
         " is missing.\n  Build it:  Rscript citiusdata/scripts/build_event_similarity.R",
         "\n  Or ask for the old family gate explicitly:  SEQ_XB_MINCOR=0")
  sm <- setDT(read_parquet(sf))
  # Prefer the RELIABILITY-SHRUNK correlation when the matrix carries one. A raw
  # correlation treats a pair measured on 5 shared athletes the same as one
  # measured on 2,699: 100m against 3000m steeplechase came out at r = 0.979 on
  # five athletes, which is significant and meaningless. cor_use shrinks toward
  # zero on sample size in Fisher-z space, so full coverage costs nothing - a
  # pair with no real evidence contributes nothing rather than being excluded.
  simcol <- if ("cor_use" %chin% names(sm)) "cor_use" else "cor"
  if (simcol == "cor")
    cat(sprintf("[%s] NOTE: %s carries no cor_use column, so the raw correlation\n",
                TAG, basename(sf)),
        "        is used and pairs resting on a handful of athletes are NOT\n",
        "        down-weighted.\n", sep = "")
  sm[, corv := as.numeric(get(simcol))]
  sm <- sm[is.finite(corv)]
  stopifnot("the similarity matrix is empty" = nrow(sm) > 0)
  for (i in seq_len(nrow(sm))) {
    e1 <- as.character(sm$e1[i]); e2 <- as.character(sm$e2[i])
    assign(if (e1 < e2) paste0(e1, "|", e2) else paste0(e2, "|", e1), sm$corv[i], envir = SIM)
  }
  XB_PAIRS <- sum(sm$corv >= XB_MINCOR)
  cat(sprintf("[%s] similarity: %s | %d pairs, %d at or above %.2f | column %s\n",
              TAG, basename(sf), nrow(sm), XB_PAIRS, XB_MINCOR, simcol))
  # A gate that admits NOTHING is not a configuration, it is a broken run. The
  # blend would no-op for every athlete while the results row still recorded
  # xblend=1, so an arm with a stale similarity file is indistinguishable from
  # one where the feature simply does not help. That is how a knob that changes
  # nothing gets read as a null result - which this repo did once already, when
  # four XBLEND settings gave byte-identical scores.
  # The likeliest cause is a similarity file built against a different corpus
  # vintage than the one being scored, so rebuild it rather than lowering the gate.
  if (XB_PAIRS == 0)
    stop("SEQ_XB_MINCOR is ", XB_MINCOR, " but NO event pair reaches it (",
         nrow(sm), " pairs, max ", sprintf("%.3f", max(sm$cor)), ").\n",
         "  The blend would silently do nothing. Rebuild the similarity matrix\n",
         "  against the current corpus:  Rscript citiusdata/scripts/build_event_similarity.R")
  rm(sm)
}

# --- PER-EVENT PARAMETER OVERRIDES -------------------------------------------
# SEQ_EVPARAM  path to a parquet with an `event_id` column and one column per
# parameter to override (k0, kfloor, ceil, huber, xblend). NA means "use the
# global value", so a file may cover one event or all of them.
#
# Why this is safe to have, having refused it four times: the DANGER was never
# per-event values, it was per-event FITTING. Eighty-five events, several
# parameters, and events holding a few thousand pairs will produce a "winner"
# for every cell whether or not one exists. optimise_event_params.R is the
# guard - it takes winners only where the same value also wins the SEALED
# window, and reports how many failed that test.
#
# The measurement that changed my mind is cross-event blending: +1.349 on the
# women's 10,000m, -1.079 on the men's weight throw, both replicating, summing
# to +0.029 globally. A single global value there is not a compromise, it is an
# average of two opposite truths.
TAG <- Sys.getenv("SEQ_TAG","baseline")
EVPARAM <- Sys.getenv("SEQ_EVPARAM", "")
EVP <- NULL
# A path that is set but unreadable must be an ERROR, not a shrug. Falling back
# to the global config would produce a run that looks like "the per-event gain
# did not reproduce" when the real answer is "the file was never loaded".
if (nzchar(EVPARAM) && !file.exists(EVPARAM))
  stop(sprintf("SEQ_EVPARAM is set to '%s' but that file does not exist", EVPARAM))
if (nzchar(EVPARAM)) {
  EVP <- setDT(read_parquet(EVPARAM))
  stopifnot("SEQ_EVPARAM file has no event_id column" = "event_id" %in% names(EVP))
  EVP[, event_id := as.character(event_id)]
  ovr <- setdiff(names(EVP), "event_id")
  # a mistyped column would otherwise be silently ignored by .ev_vec()
  known <- c("k0", "kfloor", "ceil", "huber", "xblend", "seedhl",
             "kappa", "cens", "kt1", "atten")
  if (length(setdiff(ovr, known)))
    stop(sprintf("SEQ_EVPARAM has unknown column(s): %s (known: %s)",
                 paste(setdiff(ovr, known), collapse = ", "),
                 paste(known, collapse = ", ")))
  stopifnot("SEQ_EVPARAM has no parameter columns" = length(ovr) > 0)
  cat(sprintf("[%s] per-event overrides from %s: %d events, columns %s\n",
      TAG, basename(EVPARAM), nrow(EVP), paste(ovr, collapse = ", ")))
  for (cn in ovr) cat(sprintf("[%s]   %-8s set on %d events, range %.3f-%.3f\n",
      TAG, cn, sum(!is.na(EVP[[cn]])),
      suppressWarnings(min(EVP[[cn]], na.rm = TRUE)),
      suppressWarnings(max(EVP[[cn]], na.rm = TRUE))))
}
# Resolve a per-event vector for one parameter: the global value everywhere,
# overridden where the file says so.
.ev_vec <- function(nm, global, events) {
  v <- setNames(rep(global, length(events)), events)
  if (!is.null(EVP) && nm %in% names(EVP)) {
    want <- EVP[!is.na(get(nm))]
    e <- want[event_id %chin% events]
    # an event_id that matches nothing applies nothing, silently - say so
    if (nrow(e) < nrow(want))
      cat(sprintf("[%s]   WARNING %s: %d of %d override event_ids match no event: %s\n",
          TAG, nm, nrow(want) - nrow(e), nrow(want),
          paste(head(setdiff(want$event_id, e$event_id), 5), collapse = ", ")))
    if (nrow(e)) {
      v[e$event_id] <- e[[nm]]
      cat(sprintf("[%s]   %s applied to %d event(s)\n", TAG, nm, nrow(e)))
    }
  }
  v
}
# SEQ_WINP  1 = compute win probabilities and Brier. Default OFF: the draws cost
#           ~60s of a ~360s run (measured) and nothing reads the accumulators.
# SEQ_HIST  1 = write the per-race r_pre history (see below).
WINP <- Sys.getenv("SEQ_WINP","") != ""
# SEQ_MAXPLACE  score only finishers placing <= this (0 = off, score everyone).
# A METRIC change, not a model change: updates still use the whole field, so an
# athlete's own rating still learns from their race whatever they placed. The
# question it answers is whether the form model can carry road racing once we
# stop grading it on ordering the back of a 500-runner field.
#
# ON BY DEFAULT AT 12 since 2026-08-15 (Pete's call). The model exists to say who
# wins and who medals; scoring whether it ranked 40th against 41st in a big field
# measures something nobody wants. 12 = a full track final plus a couple of
# places. Set SEQ_MAXPLACE=0 to score the whole field.
#
# CAPPING MAKES THE METRIC EASIER, so a capped number is NOT comparable to an
# uncapped one. Only capped-vs-capped on the same cap is a fair read, and every
# figure recorded before 2026-08-15 is UNCAPPED. The cap value cannot be chosen
# by maximising the metric — a smaller cap scores higher mechanically.
#
# Verified not to move the model: the full knob grid re-run at cap 12 puts every
# optimum where it was uncapped (k0 0.95, kappa 3, floor 0.32), largest deviation
# 0.02, so adopting it required no re-tuning.
MAXPLACE <- as.integer(.env_num("SEQ_MAXPLACE", 12))
HIST <- Sys.getenv("SEQ_HIST","") != ""
# Identify the engine that produced a run, so two arms can be PROVED comparable
# rather than assumed so. Hash the file, not git: this script is routinely run
# uncommitted, so a commit sha would say nothing about what actually ran.
ENGINE_SRC <- local({
  f <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
  if (is.na(f) || !nzchar(f)) f <- here::here("citiusdata", "scripts", "form_ratings.R")
  f
})
ENGINE_SHA <- if (file.exists(ENGINE_SRC))
  digest::digest(file = ENGINE_SRC, algo = "sha256") else "unknown"
FROM <- as.Date("2020-01-01")

cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
cat0 <- cat0[meet_tier %in% c("T1_elite","T2_strong"), .(competition_id, meet_tier, class)]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
ag <- readRDS(file.path(OUT, "aging.rds"))
curves <- as.data.table(ag$curves)
agefun <- lapply(split(curves, curves$family), function(cv) approxfun(cv$age, cv$effect, rule = 2))
cal <- readRDS(file.path(OUT, "calibration_corpus_csigma_coast_keyfix.rds"))
wb <- as.data.table(cal$wind)[, .(event_id, beta)]
HFAM <- c(road = 1095, walk = 730); HDEF <- 365

evs <- setdiff(sub("^event_id=","",list.dirs(file.path(OUT,"athletics_corpus_store"),recursive=FALSE,full.names=FALSE)), "__unmatched__")
dl <- list()
for (EV in evs) {
  x <- tryCatch(setDT(read_parquet(file.path(OUT, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV)),
        col_select = c("athlete_id","competition_id","date","perf","mark","place","race_key","round","age","wind"))),
        error = function(e) NULL)
  if (is.null(x)) next
  x[, `:=`(event_id = EV, athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
  dl[[EV]] <- x
}
d <- rbindlist(dl, fill = TRUE); rm(dl); invisible(gc())
d <- merge(d, cat0, by = "competition_id")
# SPLIT PERFORMANCES, captured before the place > 0 filter below discards them.
#
# A split is a real MARK but not a real RACE. World Athletics records Kerr's
# 1500m of 3:27.62 (2026-07-18) - taken passing 1500m inside the Mile he won,
# and faster than his career best - and it reaches us with no finishing
# position. His whole Mile field is there: 10 rows in the 1500m for that
# competition, every one place = 0.
#
# THE GATE IS is.finite(mark), NOT place == 0. Measured on the corpus since
# 2020: 115,379 rows have place = 0 and only 18,238 (15.8%) carry a mark - the
# rest are DNF, DNS or a no-mark attempt. Pole vault alone has 5,867 such rows
# and 100 marks, because a failed vault records no height. Admitting place = 0
# wholesale would import abandonments as career bests.
#
# Where a mark does exist it is a genuine performance: place=0 1500m rows have a
# median of 3:45.41 against 3:56.71 for placed rows - faster, as splits inside
# elite Mile races should be - and long jump 6.85 against 6.89.
#
# What they are allowed to do is set out in docs/plans/split-times-2026-08-18.md:
# raise the best mark and refresh WHEN an athlete last showed that form. They do
# NOT count as evidence (one effort must not buy two races' worth of confidence)
# and are NOT scored as races (a full field of splits reproduces the parent
# race's ordering rather than adding to it).
# ADOPTED 2026-08-18, on the ground that excluding a real performance is wrong
# regardless of the metric. Measured anyway: Kerr's 1500m best 3:27.79 ->
# 3:27.62, his last 1500m 2025-09-17 -> 2026-07-18, and he goes from ABSENT to
# 14th because the recency filter can finally see him. Ratings are byte-identical
# (raw 2026 conc 71.541 both arms) since splits are not evidence, and agreement
# with World Athletics goes 70.2 -> 70.5. 259 bests raised, 214 dates refreshed,
# 39 extra rows passing the active filter - small and targeted.
USE_SPLITS <- Sys.getenv("SEQ_SPLITS", "1") != "0"
SPLITS <- NULL
if (USE_SPLITS) {
  SPLITS <- d[is.finite(perf) & is.finite(mark) & !is.na(date) & date >= FROM &
              (is.na(place) | place == 0),
              .(split_best = max(perf), split_last = max(date)),
              by = .(athlete_id, event_id)]
  cat(sprintf("[%s] splits: %s athlete-events carry a marked performance with no placing\n",
              TAG, format(nrow(SPLITS), big.mark = ",")))
}
d <- d[!is.na(perf) & !is.na(date) & !is.na(race_key) & !is.na(place) & place > 0 & date >= FROM]

# ORDER MATTERS. This guard tests RAW marks and must run BEFORE any correction.
# When the adjustment ran first it flagged 3 marks instead of 1: a headwind-
# corrected sprint can legitimately project past a world record, because the
# record itself was set in some wind. The guard exists to catch a result nobody
# ran, not a projection.
# IMPOSSIBLE MARKS. A performance better than its event's world record is either
# a new world record - in which case world_records.csv carries it, and there is a
# separate job to keep that current - or it is bad data. Found 2026-08-19 by Pete
# reading a PB column: Noah Lyles carried an 18.90 for 200m against a record of
# 19.19, dated 2020-07-09 with a 3.7 m/s HEADWIND, no competition and no venue.
# That is the annulled Inspiration Games run where he started roughly 15m early.
# It was `scoreable`, so the engine rated it, and at CEIL 0.30 it was setting
# 30% of his published rank - he was the number one men's 200m runner partly on a
# race that did not happen.
#
# Corpus-wide this catches almost nothing (3 marks in 5.2 million), which is
# exactly why it needs to be automatic: one bad row in a million is invisible to
# every aggregate check and lands squarely on a famous athlete at the top of a
# published table.
WRF <- file.path(OUT, "world_records.csv")
if (file.exists(WRF)) {
  .wr <- setDT(utils::read.csv(WRF, stringsAsFactors = FALSE))
  .wr[, wr_mark := vapply(strsplit(as.character(mark), ":", fixed = TRUE), function(q) {
        v <- suppressWarnings(as.numeric(q))
        if (anyNA(v)) NA_real_ else Reduce(function(a, b) a * 60 + b, v)
      }, numeric(1))]
  .wr <- .wr[is.finite(wr_mark), .(event_id, wr_mark)]
  # This whole guard is an inner join away from being vacuous: if the record
  # file's event_ids drift, .wr empties, `bad` is empty and the run reports
  # nothing dropped - exactly the state it is meant to make impossible.
  # orientation comes from the registry directly - `reg` here carries only
  # (event_id, family), so reusing it silently produced no orientation column
  .or <- as.data.table(citius::citius_events())[, .(event_id, orientation)]
  .n_wr <- nrow(.wr)
  .wr <- merge(.wr, .or, by = "event_id")
  # AFTER the join, not before. This check used to sit above the merge - so if
  # the record file's event_ids ever stopped matching the registry, .wr would
  # empty HERE, `bad` would always be empty, the bulk check below would pass at
  # 0 <= 200, and nothing would print. The comment above names precisely that
  # failure mode and the check was on the wrong side of it.
  stopifnot("world_records.csv parsed to no usable records" = .n_wr >= 20,
            "no world record matched the event registry - the impossible-mark
guard would silently check nothing" = nrow(.wr) >= 20)
  .wr[, wr_perf := fifelse(orientation == -1, -log(wr_mark), log(wr_mark))]
  n0 <- nrow(d)
  d <- merge(d, .wr[, .(event_id, wr_perf)], by = "event_id", all.x = TRUE)
  bad <- d[is.finite(wr_perf) & perf > wr_perf]
  if (nrow(bad)) {
    cat(sprintf("[%s] IMPOSSIBLE MARKS DROPPED: %d better than their world record\n",
                TAG, nrow(bad)))
    print(head(merge(bad[, .(event_id, date, athlete_id, perf)], .or, by = "event_id")[
      , .(event_id, date, athlete_id,
          mark = round(exp(fifelse(orientation == -1, -perf, perf)), 3))], 10))
    d <- d[!(is.finite(wr_perf) & perf > wr_perf)]
  }
  d[, wr_perf := NULL]
  # If this ever fires in bulk the world-record table is stale or mis-parsed,
  # which is a different problem from a bad result row - do not silently bin
  # thousands of real performances.
  stopifnot("more than 200 marks beat their world record - world_records.csv is
wrong or mis-parsed, not the corpus" = (n0 - nrow(d)) <= 200)
}

# ADJUSTED MARKS. Correct each performance for wind and venue before rating it,
# rather than leaving the engine to infer both from the field. Built and
# validated in build_adjusted_marks.R: within-athlete scatter falls 2.08% overall
# and in every family - sprint -4.53%, road -3.09%, distance -2.68% - with 58-74%
# of individual athletes becoming more consistent. The corrections know nothing
# about which athlete produced which mark, so a tighter athlete cannot be an
# artefact of fitting.
#
# NOT COMBINED WITH SEQ_WINDCS. Only one wind correction should ever be live;
# this one operates on the mark, that one inside the engine.
#
# The race shock still runs afterwards and estimates whatever conditions remain -
# that is the intended order, correct the mark then measure the day.
# ADOPTED 2026-08-19. Rating on corrected marks beats rating on raw ones.
#
# THE HONEST NUMBER IS THE SMALLER ONE. Fitted over the whole corpus the gain
# reads 71.740 -> 71.810 raw and 73.178 -> 73.282 weighted - but those
# corrections saw the sealed window. Refitted on <=2025 only: 71.803 raw
# (+0.063, so 93% of the raw gain survives) and 73.206 weighted (+0.028, about a
# quarter of it). The weighted metric leans on majors and T1 meets, which is
# exactly where a venue effect fitted partly on 2026 flatters itself.
#
# AND NEITHER FIGURE CLEARS ITS NOISE FLOOR (measured 2026-08-19). Kish
# effective sample on the sealed window: raw 644,735 pairs, floor 0.054 pp, so
# +0.063 is 1.17x - marginal. Tier-weighted 74,037 effective pairs, floor 0.159
# pp, so +0.028 is 0.18x - inside the noise. The 40/12/1 weighting costs 8.7x in
# effective sample; the note at W_MAJ below already said the floor rises to
# 0.118 pp and nobody joined it up to the gains reported against it.
#
# That is a limit of the referee, not a verdict on this correction. The evidence
# that carries it is independent of this metric: the within-athlete scatter test,
# where corrected marks made athletes measurably more self-consistent, and which
# is what caught the wind sign error when sprints got 6.7% WORSE. Quote the
# sealed figures WITH their floors, or quote the scatter test instead.
#
# The DEPLOYED corrections still use the full corpus: for predicting 2027 more
# data is better, and the restriction exists to make the EVALUATION honest, not
# to make the model worse. SEQ_ADJFILE selects either.
#
# 92.5% of performances
# corrected, median adjustment 0.2%. By family it lands exactly where wind and
# altitude live - road +0.378 (4 of 4 events up), sprint +0.214, distance +0.164,
# middle +0.074 (9 of 10 up) - with 100m M +0.637 over 72,838 pairs against a
# 0.160 noise floor, and the marathons +0.39 to +0.50. Walk is the one loss at
# -0.226, on small samples, and is the open question.
# SEQ_ADJ=0 rates on raw marks.
ADJ <- Sys.getenv("SEQ_ADJ", "1") != "0"
# The "only one wind correction" rule above was a comment and nothing else. This
# file guards far smaller landmines with stopifnot, so leaving a documented,
# known-dangerous combination unenforced is inconsistent: SEQ_WINDCS defaults off
# today, but a future experiment turning it on without knowing to set SEQ_ADJ=0
# would remove wind twice - once corpus-wide, once inside the cold-start prior.
stopifnot("SEQ_ADJ and SEQ_WINDCS are both on - wind would be removed twice, once
on the mark and once inside the engine. Pick one." = !(ADJ && WINDCS))
if (ADJ) {
  # overridable so a corrections file fitted on a restricted window can be tested
  # against the full-corpus one without swapping files under a running experiment
  af <- file.path(OUT, Sys.getenv("SEQ_ADJFILE", "adjusted_marks.parquet"))
  if (!file.exists(af))
    stop("SEQ_ADJ is on but adjusted_marks.parquet is missing.\n",
         "  Build it:  Rscript citiusdata/scripts/build_adjusted_marks.R")
  .adj <- setDT(read_parquet(af, col_select = c("race_key", "athlete_id", "event_id",
                                                "wind_adj", "venue_adj", "indoor_adj")))
  .adj[, athlete_id := as.character(athlete_id)]
  # SURFACE duplicates rather than absorbing them. This used to call unique()
  # straight away, which made the row-count assertion below UNFALSIFIABLE: with
  # .adj deduped first, the merge can never fan out, so nrow(d) == n0 always held
  # - including in the one case it claimed to guard, an athlete with two marks in
  # one race. Both rows would then take the same adj_total, from whichever mark
  # happened to survive the dedup.
  .key <- c("race_key", "athlete_id", "event_id")
  .dupe_adj <- nrow(.adj) - nrow(unique(.adj, by = .key))
  if (.dupe_adj > 0)
    cat(sprintf("[%s] adjusted marks: %s duplicate key(s) collapsed\n",
                TAG, format(.dupe_adj, big.mark = ",")))
  .adj <- unique(.adj, by = .key)
  # And check the side that actually matters: if `d` carries two performances
  # under one key, they cannot be told apart and both would take the same
  # correction. d is not deduplicated until ~400 lines below this point.
  .dupe_d <- nrow(d) - nrow(unique(d, by = .key))
  if (.dupe_d > 0) {
    # REAL, and found the moment this check was added: 41 of ~1.3M performances
    # share a key, so the pair cannot be told apart and both take the same
    # correction - taken from whichever mark survived the dedup above. At 0.003%
    # that is not worth blocking a pipeline over, but it must be VISIBLE rather
    # than absorbed, which is what the previous row-count assertion did.
    cat(sprintf("[%s] WARNING: %s performance(s) share a (race_key, athlete_id,\n",
                TAG, format(.dupe_d, big.mark = ",")))
    cat("        event_id) key. Each pair takes a single shared correction, which\n")
    cat("        may belong to the other mark. Logged as a data-quality item.\n")
  }
  stopifnot("more than 0.1% of performances share a key - corrections cannot be
matched to the right mark at that rate" = .dupe_d <= 0.001 * nrow(d))
  # indoor_adj joined the file on 2026-08-20 and was NOT summed here for its
  # first run, so it was written and never read - the A/B came back byte-
  # identical on both arms, which is the only reason it was caught. A column
  # nothing consumes is the same as a column that does not exist.
  # Tolerated as absent so older adjusted_marks files still load.
  if (!"indoor_adj" %chin% names(.adj)) .adj[, indoor_adj := 0]
  .adj[, adj_total := fifelse(is.finite(wind_adj), wind_adj, 0) +
                      fifelse(is.finite(venue_adj), venue_adj, 0) +
                      fifelse(is.finite(indoor_adj), indoor_adj, 0)]
  n0 <- nrow(d)
  d[, athlete_id := as.character(athlete_id)]
  d <- merge(d, .adj[, .(race_key, athlete_id, event_id, adj_total)],
             by = c("race_key", "athlete_id", "event_id"), all.x = TRUE)
  # a fan-out here would duplicate performances and inflate every rating
  stopifnot("joining adjusted marks changed the row count - the key is not unique" =
              nrow(d) == n0)
  hit <- is.finite(d$adj_total) & d$adj_total != 0
  cat(sprintf("[%s] adjusted marks: %s of %s performances corrected (%.1f%%), median |adj| %.4f\n",
              TAG, format(sum(hit), big.mark = ","), format(nrow(d), big.mark = ","),
              100 * mean(hit), stats::median(abs(d$adj_total[hit]))))
  # If almost nothing is corrected the join failed and the arm is really a
  # baseline wearing the wrong label - the most expensive kind of null result.
  # A SINGLE combined floor could not see either correction failing alone: wind
  # covers 27.6% of performances and venue 92.8%, so if venue matching broke
  # entirely `hit` would fall to 27.6% and still clear a 10% bar - silently
  # discarding the correction worth most of the gain.
  .cov <- mean(hit)
  cat(sprintf("[%s] adjustment coverage: %.1f%% of performances\n", TAG, 100 * .cov))
  stopifnot("fewer than 60% of performances got an adjustment - the join failed" =
              .cov > 0.60)
  d[is.finite(adj_total), perf := perf - adj_total]
  d[, adj_total := NULL]
}

d <- merge(d, reg, by = "event_id", all.x = TRUE)
d <- merge(d, wb, by = "event_id", all.x = TRUE)
d[, rc := fifelse(grepl("semi", round, ignore.case=TRUE), "semi",
        fifelse(grepl("heat|round 1|qual", round, ignore.case=TRUE), "heat", "final"))]
d[, hl := fifelse(!is.na(family) & family %chin% names(HFAM), HFAM[family], HDEF)]
setorder(d, date, race_key)
cat(sprintf("[%s] %s rows | %s races | %s athlete-events\n", TAG,
    format(nrow(d), big.mark=","), format(uniqueN(d$race_key), big.mark=","),
    format(uniqueN(paste(d$athlete_id, d$event_id)), big.mark=",")))

MU <- d[, .(mu = mean(perf)), by = event_id]; MUv <- setNames(MU$mu, MU$event_id)
# variance prior: within-race spread per event (median of race-level var), the
# broadest honest starting uncertainty -- narrows only with an athlete's own evidence
VP <- d[, .(v = var(perf)), by = .(event_id, race_key)][is.finite(v),
        .(vp = stats::median(v)), by = event_id]
VPv <- setNames(VP$vp, VP$event_id)
if (VPRIOR) {
  # computed on a COPY: d must stay in (date, race_key) order, since the
  # boundary scan that drives the whole sweep is built from its row order
  dd <- d[, .(athlete_id, event_id, date, race_key, perf)]
  setorder(dd, athlete_id, event_id, date, race_key)
  dv <- dd[, if (.N >= 8L) .(vd = stats::var(diff(perf))/2) else NULL,
           by = .(athlete_id, event_id)]
  est <- dv[is.finite(vd) & vd > 0, .(vp = stats::median(vd)/VPADJ, n_ath = .N),
            by = event_id][n_ath >= VPMINA]
  # events too thin for their own estimate keep a SHRUNK version of the old
  # prior rather than the wide one - the failure being fixed is worst exactly
  # where evidence is thinnest, so falling back to the old value would leave
  # combined events (the 9,126-point decathlon) untouched.
  cmp <- merge(data.table(event_id = names(VPv), old = as.numeric(VPv)), est, by = "event_id")
  shrink <- stats::median(cmp$old / cmp$vp)
  newv <- VPv
  newv[] <- as.numeric(VPv) / shrink
  newv[est$event_id] <- est$vp
  cat(sprintf("[%s] variance prior: %d of %d events from within-athlete data, %d shrunk by %.2fx
",
      TAG, nrow(est), length(VPv), length(VPv) - nrow(est), shrink))
  cat(sprintf("[%s]   median prior sd %.2f%% -> %.2f%% of a mark
", TAG,
      100*(exp(sqrt(stats::median(as.numeric(VPv))))-1),
      100*(exp(sqrt(stats::median(as.numeric(newv))))-1)))
  VPv <- newv
  rm(dd, dv, est, cmp); invisible(gc())
}
KFLOORv <- .ev_vec("kfloor", KFLOOR, names(MUv))
HUBERv  <- .ev_vec("huber",  HUBER,  names(MUv))
XBLENDv <- .ev_vec("xblend", XBLEND, names(MUv))
# Added 2026-08-18 so the family optimiser can actually SHIP what it finds.
# These three were swept per family and could not be applied, which made the
# diagnosis useless: kappa controls how fast the learning rate decays with
# evidence, cens discounts a bad heat or semi, kt1 scales learning from
# championship races. Globally kt1 1.4 is the largest weighted-sealed gain
# measured today (+0.054), and it is exactly the kind of thing that should
# differ between a family where a championship is the season's only real test
# and one where athletes meet each other fortnightly.
# ATTENUATION. The engine's surprise is perf - r_pre, which assumes the true
# slope of performance on rating is exactly 1. Measured over 902,925 athlete-
# races it is 0.828, and by family it runs from 0.629 (middle) to 0.871
# (throw). A slope below 1 means every race docks the favourite and credits the
# outsider BY CONSTRUCTION - which is the favourite penalty, monotone across
# five rating bands, and why a tactical championship winner loses rating.
#
# The correction adds back the part the engine over-attributes:
#     surprise <- surprise + (1 - b) * (r_pre - field mean)
# Centring on the field mean matters: a shock shared by the whole field cancels
# out of every within-race comparison, so only the athlete's rating RELATIVE to
# the field can reorder anything. Adding an uncentred term would just shift all
# ratings in the race, which is inert.
#
# 1 = off, and off is the default. Note a single GLOBAL b is close to
# indistinguishable from scaling k0 by b, and k0 is already tuned - so a global
# value is expected to do little. What k0 cannot express is the per-FAMILY
# variation, which is where the measured spread is.
ATTEN  <- .env_num("SEQ_ATTEN", 1)
ATTENv <- .ev_vec("atten", ATTEN, names(MUv))
KAPPAv <- .ev_vec("kappa", KAPPA, names(MUv))
CENSv  <- .ev_vec("cens",  CENS,  names(MUv))
KT1v   <- .ev_vec("kt1",   KT1,   names(MUv))

# per-event ceiling weight: baseline plus an adjustment by event character
CEILv <- .ev_vec("ceil", CEIL, names(MUv))
if (CEILADJ != 0) {
  rg <- as.data.table(citius::citius_events())[, .(event_id, tactical, technical)]
  tech <- rg[technical == TRUE, event_id]; tact <- rg[tactical == TRUE, event_id]
  CEILv[names(CEILv) %chin% tech] <- CEIL + CEILADJ
  CEILv[names(CEILv) %chin% tact] <- CEIL - CEILADJ
  # a weight outside [0,1] is not a blend, it is an extrapolation
  CEILv[] <- pmin(pmax(CEILv, 0), 1)
  cat(sprintf("[%s] ceiling blend: technical %.2f (%d events), tactical %.2f (%d), other %.2f
",
      TAG, CEIL + CEILADJ, sum(names(CEILv) %chin% tech),
      CEIL - CEILADJ, sum(names(CEILv) %chin% tact), CEIL))
}

# per-event k0, derived from that same variance
# PER-FAMILY LEARNING RATE, adopted 2026-08-18 from optimise_family_params.R.
#
# How fast a rating should chase a result is not one number for all of
# athletics. A sprint is contested often and its time is a precise, repeatable
# measurement, so a single result genuinely says a lot. A marathon is run twice
# a year on a different course in different weather, so a single result says
# much less. Measured over four values (0.75 / 0.95 / 1.15 / 1.30), kept only
# where the winning value beat the incumbent on BOTH the tune and the sealed
# window, then shrunk toward 0.95 by pairs/(pairs + 20,000):
#
#   sprint   214,482 pairs  tune +0.022  sealed +0.039  ->  1.133
#   hurdles   84,056 pairs  tune +0.002  sealed +0.101  ->  1.112
#   road       2,193 pairs  tune +0.410  sealed +0.981  ->  0.930
#
# The other six families keep 0.95. Road's raw optimum was 0.75 - a large move
# on very little evidence - and the shrinkage pulls it back to nearly the
# incumbent, which is the mechanism working rather than a compromise.
#
# Verified end to end, not assumed: the assembled configuration scored
# 71.712 raw 2026 and 73.066 weighted sealed against 71.680 / 73.036, and
# scored BY FAMILY the gain appears only where it was fitted - hurdles +0.045,
# road +0.031, sprint +0.022, and exactly 0.000 in all six untouched families.
# It also lands where the benchmark said the model was weakest: sprint was
# +0.28 over simply sorting by season best, and hurdles was -1.09.
# REVERTED 2026-08-18, hours after adopting it. A THIRD window refused it.
#
# Fitted on 2025 -> 2026 the keepers were sprint, hurdles and road. Refitted on
# 2022-23 -> 2024 they are jump, throw and hurdles. Only HURDLES appears in
# both, and sprint (-0.005 tune) and road (-0.046 tune) fail outright on the
# earlier window. Different windows, different families, and for `huber` even
# opposite values - distance wanted 4.5 on the later window and 99 on the
# earlier one. That is the signature of noise, not structure.
#
# The arithmetic I should have done before shipping: under a null of no family
# effect, a family is positive on two independent windows by chance 25% of the
# time. With 9 families that is 2.25 expected. k0 produced 3, ceil 1, kfloor 1,
# huber 4 - so two of the four parameters produced FEWER survivors than chance
# alone, and the two that beat it did so by about one family. "3 of 9
# replicated" reads like evidence and is very nearly the null.
#
# What survived every check was the machinery, not the values:
# optimise_family_params.R assembles per-family optima from N global runs, and
# the end-to-end verification plus by-family scoring (gains only where fitted,
# exactly 0.000 elsewhere) is a real test. It is the two-window filter that is
# too weak at 9 families, and the fix is more windows rather than more
# parameters.
#
# HURDLES is the one candidate with three-window support (1.15 chosen on both
# fits). Not reinstated alone: its measured effect is +0.002 to +0.101, which is
# inside the noise of a single family, and cherry-picking the one survivor from
# a filter that is barely better than chance is how a null result becomes a
# finding.
FAM_K0 <- c()
K0v <- setNames(rep(K0, length(VPv)), names(VPv))
if (K0 == 0.95) {   # only apply where the incumbent they were fitted against holds
  fam_of <- reg$family[match(names(K0v), reg$event_id)]
  for (f in names(FAM_K0)) K0v[!is.na(fam_of) & fam_of == f] <- FAM_K0[[f]]
}
# an explicit SEQ_EVPARAM override still wins over the family default
.k0_ov <- .ev_vec("k0", NA_real_, names(VPv))
K0v[is.finite(.k0_ov)] <- .k0_ov[is.finite(.k0_ov)]
if (KPOW != 0) {
  sd_ev <- sqrt(as.numeric(VPv)); ref <- stats::median(sd_ev)
  k0_raw <- K0 * (ref / sd_ev)^KPOW
  # bounded: k > 1 means the update overshoots PAST the race it just saw, and a
  # rate near zero freezes an event entirely. Neither is a rate, so clamp.
  K0v[] <- pmin(pmax(k0_raw, 0.25), 1.30)
  o <- order(K0v)
  cat(sprintf("[%s] per-event k0 (KPOW %.2f): range %.3f-%.3f, median %.3f
",
      TAG, KPOW, min(K0v), max(K0v), stats::median(K0v)))
  cat(sprintf("[%s]   slowest: %s | fastest: %s
", TAG,
      paste(sprintf("%s %.2f", sub("^AT-","",names(K0v)[utils::head(o,3)]), K0v[utils::head(o,3)]), collapse=", "),
      paste(sprintf("%s %.2f", sub("^AT-","",names(K0v)[utils::tail(o,3)]), K0v[utils::tail(o,3)]), collapse=", ")))
}
R <- new.env(parent=emptyenv()); NE <- new.env(parent=emptyenv())
V <- new.env(parent=emptyenv())   # EW variance of own surprises; prior = event pop
LD <- new.env(parent=emptyenv()); LE <- new.env(parent=emptyenv())
BYA <- new.env(parent=emptyenv())
# Running best perf per athlete-event: career, and within the current season.
# Updated AFTER a race is scored, so reads are always strictly lagged.
BC <- new.env(parent=emptyenv()); BS <- new.env(parent=emptyenv())
BSY <- new.env(parent=emptyenv())
# Top-K marks per athlete-event, for the ceiling blend. See SEQ_BEST_K.
BKV <- new.env(parent=emptyenv()); BKD <- new.env(parent=emptyenv())
key <- function(a, e) paste0(a, "|", e)

# --- SEQ_SEED: pre-populate state from held results -------------------------
# Done ONCE before the loop rather than per race: an athlete-event's seed is a
# function of its FIRST corpus date, which is known up front, so there is
# nothing to look up mid-sweep. Setting R/NE/LD/BC here means the athlete is
# simply `seen` when they first appear — no special case in the hot loop.
n_seeded <- 0L
if (SEEDON) {
  cf <- list.files(file.path(OUT, "athletics_careers_store"), pattern = "[.]parquet$",
                   recursive = TRUE, full.names = TRUE)
  ca <- rbindlist(lapply(cf, function(f) tryCatch(setDT(read_parquet(f,
          col_select = c("athlete_id","date","perf","discipline","sex"))),
          error = function(e) NULL)), fill = TRUE)
  ca <- ca[!is.na(perf) & is.finite(perf) & !is.na(date)]
  ca[, athlete_id := as.character(athlete_id)]
  ca[, event_id := paste0("AT-", gsub("[^A-Za-z0-9]", "", discipline), "-", sex)]
  ca <- ca[event_id %chin% names(MUv)]
  fd <- d[, .(first_date = min(date)), by = .(athlete_id, event_id)]
  sd0 <- ca[fd, on = .(athlete_id, event_id), allow.cartesian = TRUE, nomatch = NULL]
  # STRICTLY earlier than the first scored race, or the gain is leakage
  sd0 <- sd0[date < first_date]
  # per-event half-life, from the observed race frequency of the event itself
  hl_ev <- setNames(rep(SEEDHL, length(MUv)), names(MUv))
  if (SEEDHLPOW != 0) {
    g <- d[order(athlete_id, event_id, date)]
    g[, .gap := as.numeric(date - shift(date)), by = .(athlete_id, event_id)]
    fq <- g[is.finite(.gap) & .gap > 0, .(mg = stats::median(.gap)), by = event_id]
    ref <- stats::median(fq$mg)
    fq[, hl := SEEDHL * (mg / ref)^SEEDHLPOW]
    hl_ev[fq$event_id] <- pmin(pmax(fq$hl, 7), 1460)
    cat(sprintf("[%s] seed half-life by event (pow %.2f): %.0f-%.0f days, median %.0f
",
        TAG, SEEDHLPOW, min(hl_ev), max(hl_ev), stats::median(hl_ev)))
    sl <- sort(hl_ev)
    cat(sprintf("[%s]   shortest: %s | longest: %s
", TAG,
        paste(sprintf("%s %.0fd", sub("^AT-","",names(sl)[1:3]), sl[1:3]), collapse=", "),
        paste(sprintf("%s %.0fd", sub("^AT-","",names(sl)[(length(sl)-2):length(sl)]),
                      sl[(length(sl)-2):length(sl)]), collapse=", ")))
    rm(g, fq)
  }
  # An explicit per-event override wins over the frequency scaling above.
  #
  # WHY AN OVERRIDE RATHER THAN THE PARAMETRIC FORM. Decay genuinely does vary
  # by event - half-life 200 against 45, replicated on two independent windows:
  #   road     +0.263 (2025-26, 3 up 1 down)   +0.212 (2022-24, 4 up 0 down)
  #   distance +0.064                          +0.170
  #   sprint   -0.042                          -0.100 (1 up of 10)
  #   hurdles  -0.053                          -0.048
  # SEQ_SEEDHLPOW was the elegant way to get that from one parameter, and it
  # does not work: at 0.5 it pooled to -0.006 and -0.009, kept the sign for road
  # and distance at a fraction of the strength, lost the sprint effect entirely,
  # and damaged jumps (-0.049) and combined (-0.061). Race frequency alone is
  # the wrong functional form - it adjusts all 86 events including the field
  # events that want no adjustment, and hands walks a long memory on the strength
  # of their race frequency alone - when the direct test showed walks FLIP sign
  # between windows (-0.095 on 2025-26, +0.553 on 2022-24), i.e. unstable rather
  # than long-memory. An earlier version of this comment claimed the test showed
  # walks "do not want one", which is a stronger statement than the measurement
  # supports.
  # Override only the events the file names, so any frequency scaling above
  # survives everywhere else - replacing the whole vector would silently discard
  # it for every event the override file does not mention.
  if (!is.null(EVP) && "seedhl" %in% names(EVP)) {
    ov <- .ev_vec("seedhl", NA_real_, names(hl_ev))
    hl_ev[is.finite(ov)] <- ov[is.finite(ov)]
    cat(sprintf("[%s] seed half-life overridden on %d event(s): %.0f-%.0f days\n",
                TAG, sum(is.finite(ov)), min(hl_ev), max(hl_ev)))
  }
  sd0[, hl_e := hl_ev[event_id]]
  sd0[!is.finite(hl_e), hl_e := SEEDHL]
  sd0[, w := 2^(-as.numeric(first_date - date) / hl_e)]
  sg <- sd0[, .(r0 = sum(w * perf) / sum(w), ne0 = min(sum(w), SEEDNE),
                best0 = max(perf), last0 = max(date)), by = .(athlete_id, event_id)]
  sg <- sg[is.finite(r0) & is.finite(ne0)]
  # ANCHOR: a seed is a mark in the same event, so it must land near that
  # event's mean. A systematic offset would mean the two `perf` conventions
  # differ (orientation, units) and the seeds are nonsense dressed as numbers.
  sg[, dev := r0 - MUv[event_id]]
  cat(sprintf("[%s] seed: %s athlete-events | median dev from event mean %+.4f (|dev|>1 in %.2f%%)
",
      TAG, format(nrow(sg), big.mark = ","), stats::median(sg$dev),
      100 * mean(abs(sg$dev) > 1)))
  stopifnot(abs(stats::median(sg$dev)) < 0.5)
  kz <- key(sg$athlete_id, sg$event_id)
  for (i in seq_len(nrow(sg))) {
    K <- kz[i]
    R[[K]] <- sg$r0[i]; NE[[K]] <- sg$ne0[i]
    LD[[K]] <- as.numeric(sg$last0[i]); BC[[K]] <- sg$best0[i]
    # seed the top-K buffer too, or an athlete's pre-corpus history would be
    # invisible to the ceiling blend under BEST_K > 1 while being visible
    # under BEST_K = 1 - a difference between arms that is nothing to do with K
    if (BEST_K > 1) { BKV[[K]] <- sg$best0[i]; BKD[[K]] <- as.numeric(sg$last0[i]) }
  }
  n_seeded <- nrow(sg)
  rm(ca, sd0, sg); invisible(gc())
}
# All i<j index pairs where the two placings differ. Replaces
# CJ(i=,j=)[i<j][place[i]!=place[j]], whose cost is data.table dispatch overhead
# rather than the pair arithmetic.
# Order-sensitive 31-bit hash of a race key, for a reproducible per-race seed.
# Position-weighted so an anagram or a shared prefix does not collide, and it
# reads the whole key rather than a truncation.
.rk_seed <- function(k) {
  cp <- utf8ToInt(k)
  h <- 5381
  # multiplier kept small on purpose: h is < 2^31 and R does this in doubles,
  # so h * 16777619 overflows 2^53 and silently loses bits (measured: only 74.9%
  # of keys got a distinct seed). h * 131 stays under 2^39 and is exact.
  for (i in seq_along(cp)) h <- (h * 131 + cp[i]) %% 2147483647
  as.integer(h)
}
.pairs <- function(n, place) {
  if (n < 2L) return(list(i = integer(0), j = integer(0)))
  ii <- rep.int(seq_len(n - 1L), (n - 1L):1L)
  jj <- sequence((n - 1L):1L, 2:n)
  keep <- place[ii] != place[jj]
  list(i = ii[keep], j = jj[keep])
}

# Dedup ONCE rather than per race (measured: 27s over 165,133 races). It must
# come after MU/VP above, which are deliberately computed on the undeduped table.
# Keeping the first row per (race, athlete) matches the per-race unique(by=) it
# replaces, because d is sorted and split preserves within-group row order.
d <- unique(d, by = c("race_key", "athlete_id"))
# Group BOUNDARIES instead of split(). split() built 165,133 data.tables and held
# them all at once: measured 110MB from a 16.6MB source, a 6.6x blowup, which is
# what let two concurrent arms exhaust memory on 2026-08-14. Boundaries plus
# plain vectors allocate one small slice per race and nothing in between.
#
# CONTIGUITY. A boundary scan assumes every race's rows are adjacent; split()
# never required that, and the corpus race_key carries no date
# (`comp|event||round|section`), so a key spanning two dates lands in two blocks.
# This is NOT hypothetical: `7174333|10229522||11|4` (100mH W round 1, 2023) has
# five rows on 2023-08-03 and one athlete mis-dated 2023-08-01. A naive from:to
# range over it would have swallowed 186 races / 1,387 rows into one "race" --
# the same failure as the merged-heats corpus bug, silently.
#
# So force contiguity instead of assuming it: number the blocks under the
# (date, race_key) order, give every row its key's FIRST block number, then
# stable-sort on that. Each key becomes one block, keys stay in first-appearance
# order, and within a key the row order is preserved -- which is exactly what
# split(sorted = FALSE) produced, including dt0 taking the earliest date.
d[, .blk0 := rleid(race_key)]
d[, .first := .blk0[1L], by = race_key]
setorder(d, .first)                      # data.table's sort is stable
blk <- rleid(d$race_key)
starts <- which(!duplicated(blk))
ends   <- c(starts[-1L] - 1L, length(blk))
if (uniqueN(d$race_key) != length(starts))
  stop(sprintf(paste0("race_key still not contiguous after the stabilising sort: ",
                      "%s keys in %s blocks -- the grouping logic is wrong."),
               format(uniqueN(d$race_key), big.mark = ","),
               format(length(starts), big.mark = ",")))
# Columns as plain vectors, extracted once. Order of groups is (date, race_key),
# identical to what split(sorted = FALSE) produced on the sorted table; a
# sequential model changes its answer if the order changes, so the bit-identical
# regression run is what proves it.
Vath <- d$athlete_id; Vperf <- d$perf; Vplace <- d$place; Vrc <- d$rc
Vage <- d$age; Vwind <- d$wind; Vbeta <- d$beta; Vhl <- d$hl
Vev <- d$event_id; Vdate <- d$date; Vfam <- d$family
Vtier <- d$meet_tier; Vcls <- d$class; Vrk <- d$race_key
# Dates as plain numbers for the hot loop, and the year precomputed. Both were
# recomputed per athlete or per race from Date objects; year() in particular
# goes through an IDate conversion every time it is called.
Vdaten <- as.numeric(d$date); Vyr <- year(d$date)

# conc counts a TIE as 0.5 (standard concordance). Before 2026-08-16 the rule
# was `(r_pre[i] > r_pre[j]) == (place[i] < place[j])`, which is FALSE on a tie,
# so a tied pair scored correct only when row i finished BEHIND row j — i.e. it
# was decided by corpus row order, not by the model. Two cold-start athletes in
# one race carry the identical event mean, so this hit 53,582 pairs (6.9% of the
# 2026 metric) and they scored 41.37%, below chance. See check_coldstart_share.R.
#
# _bs/_mx/_bc split every pair by whether BOTH athletes carried a rating in, one
# did, or NEITHER did — so a cold-start change can be scored on the band it
# actually targets instead of diluted across a metric that is 71% established
# athletes. The ladder's "cross-event cold start is dead" verdict was measured
# on the undiluted metric and is not established.
.a0 <- c(conc=0,pairs=0,fav=0,nr=0,brier=0,brier_base=0,npred=0,
         conc_bs=0,pairs_bs=0, conc_mx=0,pairs_mx=0, conc_bc=0,pairs_bc=0,
         # weighted concordance, plus the sums needed for its EFFECTIVE sample
         # size: ESS = (sum w)^2 / sum(w^2). Heavy upweighting of a small
         # stratum crushes ESS, so the metric must report how much resolving
         # power it actually has rather than implying the full pair count.
         conc_w=0, w_sum=0, w_sq=0)
acc <- list(y25 = .a0, y26 = .a0)
# Per-race rating history (SEQ_HIST=1). r_pre is the rating an athlete CARRIED
# INTO the race — the only version that answers an out-of-sample question. The
# final state written below has already absorbed every race you would test it
# against, so measuring against that is circular (learned 2026-08-14).
# Preallocated vectors, not a growing list: the maj[[length+1]] pattern is fine
# for 757 majors finals but would add per-object overhead across 165,133 races.
NR <- if (HIST) nrow(d) else 0L
H <- list(race_key = character(NR), date = numeric(NR), event_id = character(NR),
          athlete_id = character(NR), r_pre = numeric(NR), r_use = numeric(NR),
          n_eff = numeric(NR),
          v_pre = numeric(NR), perf = numeric(NR), place = integer(NR),
          rc = character(NR), seen = logical(NR),
          # shock/surprise/k are what the engine ACTUALLY fed the update, and
          # none of them were stored. Every athlete trace therefore printed
          # perf - r_pre, which is the raw deviation BEFORE the shared race
          # shock, and reading that as the thing that moved a rating produced a
          # wrong diagnosis of Almgren's 10,000m on 2026-08-18. The shock cannot
          # be reconstructed afterwards: it is a trimmed mean over ESTABLISHED
          # athletes only, scaled by their share of the field, and the history
          # records neither. Three numeric columns is a cheap price for traces
          # that are exact.
          shock = rep(NA_real_, NR), surprise = rep(NA_real_, NR),
          k = rep(NA_real_, NR))
hi <- 0L
MAJ <- c("olympics","world_champs","european_champs","commonwealth")
# WEIGHTED CONCORDANCE. The unweighted metric is 98% ordinary meets, so it tunes
# for exactly the races the verse does not exist to predict — and the majors
# scorecard cannot substitute: 757 finals is 33,240 pairs, a noise floor of
# ~0.25pp optimistic and ~0.39pp with within-race correlation, against effects
# of 0.18-0.35pp. It literally cannot choose between arms.
#
# Championships are rare by construction (~120 major finals a year), so no
# harvesting fixes that. The only way to let majors DRIVE a decision while
# keeping an estimate stable enough to resolve the effect is to weight.
#
# These weights are a judgement call and are FIXED HERE, before any arm runs.
# Tuning them until a favoured arm wins would make the metric a formality.
# Raised from 20/8 to 40/12 (Pete, 2026-08-16). At 20/8/1 championships carried
# 16.3% of the metric; at 40/12/1 they carry 27.2% and T2 drops 77.1% -> 64.5%,
# for almost no precision cost (noise 0.079 -> 0.118 pp, still well under the
# 0.17-0.26 pp effects being measured). Chosen over 20/8/0.5, which reaches a
# similar share by suppressing everything else rather than lifting the races we
# care about, and lands at the same noise. Past ~40% majors the noise floor
# collides with the effects and the metric stops being able to choose at all.
W_MAJ <- .env_num("SEQ_W_MAJ", 40)   # olympics / worlds / euros / commonwealth
W_T1  <- .env_num("SEQ_W_T1",  12)   # other T1_elite: diamond league, world indoor
W_T2  <- .env_num("SEQ_W_T2",   1)   # T2_strong
W_RND <- .env_num("SEQ_W_RND", 0.5)  # multiplier for a non-final round
# --- metric weight per row, enumerated and asserted -------------------------
# Computed up front rather than inline so that EVERY combination present in the
# corpus is visible and checked. A fall-through that quietly assigns the T2
# weight to an uncatalogued major would bias the metric in the exact direction
# the weighting exists to correct, and nothing downstream would show it.
d[, w_tier := fifelse(!is.na(class) & class %chin% MAJ, W_MAJ,
              fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", W_T1, W_T2))]
d[, w_rnd := fifelse(rc == "final", 1, W_RND)]
d[, wt := w_tier * w_rnd]
wtab <- d[, .(races = uniqueN(race_key), rows = .N, weight = wt[1]),
          by = .(class = fifelse(is.na(class), "(uncatalogued)", class),
                 meet_tier, rc)][order(-weight, -races)]
cat(sprintf("[%s] METRIC WEIGHTS -- every race type present, %d combinations:
", TAG, nrow(wtab)))
print(wtab)
# rc is derived by regex and can only be final/semi/heat, but assert it rather
# than trust it: a new round label would silently become a "heat".
stopifnot("every row must carry a finite weight" = all(is.finite(d$wt)),
          "rc must be one of final/semi/heat"    = all(d$rc %chin% c("final","semi","heat")),
          "no weight may be zero"                = all(d$wt > 0))
cat(sprintf("[%s] weight check: all %s rows weighted, range %g to %g
",
            TAG, format(nrow(d), big.mark=","), min(d$wt), max(d$wt)))
Vwt <- d$wt
MAJ_FROM <- as.Date("2021-01-01")   # hoisted: was re-parsed on every race
maj <- list()
t0 <- Sys.time()
for (r_ in seq_along(starts)) {
  i1 <- starts[r_]; i2 <- ends[r_]
  # one check, on already-deduped rows; the original checked, deduped, rechecked
  if (i2 - i1 + 1L < 3L) next
  ii <- i1:i2
  # A plain list, not a data.table: `$` on a list is a pointer read, while every
  # data.table access pays class dispatch. The eight per-athlete columns are
  # sliced; the six read only at [1] keep a length-1 slice, which leaves every
  # z$col[1] in the body below working unchanged.
  z <- list(athlete_id = Vath[ii], perf = Vperf[ii], place = Vplace[ii],
            rc = Vrc[ii], age = Vage[ii], wind = Vwind[ii], beta = Vbeta[ii],
            hl = Vhl[ii],
            event_id = Vev[i1], date = Vdate[i1], family = Vfam[i1],
            meet_tier = Vtier[i1], class = Vcls[i1], race_key = Vrk[i1],
            wt = Vwt[i1])
  dt0n <- Vdaten[i1]; yr <- Vyr[i1]
  a <- z$athlete_id; ev <- z$event_id[1]; kk <- key(a, ev); dt0 <- z$date[1]
  mu <- MUv[[ev]]
  r_pre <- numeric(length(a)); n_eff <- numeric(length(a)); seen <- logical(length(a))
  fam1 <- z$family[1]
  agef <- if (AGEF && !is.na(fam1)) agefun[[fam1]] else NULL
  for (m in seq_along(a)) {
    v <- R[[kk[m]]]
    if (is.null(v)) { r_pre[m] <- mu; n_eff[m] <- 0; next }
    seen[m] <- TRUE
    gap <- dt0n - LD[[kk[m]]]
    if (!is.null(agef) && !is.na(z$age[m])) {
      {
        le <- LE[[kk[m]]]
        eff_now <- agef(z$age[m])
        if (!is.null(le) && !is.na(le)) v <- v + (eff_now - le)
        LE[[kk[m]]] <- eff_now
      }
    }
    ne <- NE[[kk[m]]]
    if (STALE) ne <- ne * 2^(-gap / z$hl[m])
    r_pre[m] <- v; n_eff[m] <- ne
  }
  # r_use is what ORDERS the field; r_pre is what the model learns from.
  r_use <- r_pre
  # cross-event: only for thin records, and only worth the lookup there
  xb_e <- XBLENDv[[ev]]; if (is.null(xb_e) || !is.finite(xb_e)) xb_e <- XBLEND
  if (xb_e > 0) for (m in seq_along(a)) {
    if (!seen[m] || n_eff[m] >= XB_MAXN) next
    sib <- BYA[[a[m]]]
    if (is.null(sib)) next
    sib <- sib[sib != ev]
    if (!length(sib)) next
    if (XB_MINCOR > 0) {
      # MEASURED similarity instead of the taxonomy. See the SEQ_XB_MINCOR note
      # above: family both blocks 1500m<->3000m at r=0.91 and permits
      # Hammer<->Shot at r=0.24.
      cs <- vapply(sib, function(sv) {
        q <- SIM[[if (ev < sv) paste0(ev, "|", sv) else paste0(sv, "|", ev)]]
        if (is.null(q)) NA_real_ else q
      }, numeric(1))
      keep <- is.finite(cs) & cs >= XB_MINCOR
      sib <- sib[keep]; cs <- cs[keep]
      if (!length(sib)) next
    } else {
      if (!(z$family[1] %chin% XB_FAM)) next
      fam <- reg$family[match(sib, reg$event_id)]
      sib <- sib[!is.na(fam) & fam == z$family[1]]
      if (!length(sib)) next
      cs <- rep(NA_real_, length(sib))
    }
    ne_s <- vapply(sib, function(sv) { q <- NE[[key(a[m], sv)]]
                                       if (is.null(q)) 0 else q }, numeric(1))
    okm <- ne_s >= XB_MINS
    sib <- sib[okm]; cs <- cs[okm]; ne_s <- ne_s[okm]
    if (!length(sib)) next
    # WHICH other events to borrow from, and HOW MANY.
    #
    # The original rule took the single other event with the most races. That
    # picks the relationship with the most EVIDENCE rather than the strongest
    # one, and the two disagree: for Kerr's 1500m it takes his 3000m (r 0.847,
    # 1.36 races) over his Mile (r 0.873, 1.27) - and the Mile implies 3:28.75
    # for him against the 3000m's 3:33.28. Ordering by correlation fixes that.
    #
    # Taking only one also discards the rest. Every event above the correlation
    # gate carries information; combining them weighted by how much they explain
    # (r^2) and how much evidence stands behind them uses all of it.
    ord <- if (XB_PICK == "cor" && any(is.finite(cs))) order(-cs) else order(-ne_s)
    take <- ord[seq_len(min(XB_NSIB, length(ord)))]
    tw <- 0; tv <- 0
    for (t in take) {
      rs <- R[[key(a[m], sib[t])]]; ms <- MUv[[sib[t]]]
      if (is.null(rs) || is.null(ms) || !is.finite(rs) || !is.finite(ms)) next
      cw <- if (is.finite(cs[t])) cs[t]^2 else 1
      tw <- tw + cw * ne_s[t]
      tv <- tv + cw * ne_s[t] * (rs - ms + mu)
    }
    if (tw <= 0) next
    w <- xb_e / (n_eff[m] + xb_e)
    r_use[m] <- (1 - w) * r_use[m] + w * (tv / tw)
  }
  ceil_e <- CEILv[[ev]]; if (is.null(ceil_e) || !is.finite(ceil_e)) ceil_e <- CEIL
  if (ceil_e > 0) for (m in seq_along(a)) {
    if (!seen[m]) next
    if (BEST_K > 1) {
      b <- .best_k(BEST_K, BKV[[kk[m]]], BKD[[kk[m]]], dt0n)
    } else {
      bsy <- BSY[[kk[m]]]
      b <- if (!is.null(bsy) && bsy == yr) BS[[kk[m]]] else BC[[kk[m]]]
    }
    # COMPOSES with whatever r_use already holds - it must not reset to r_pre.
    # It did, and that silently discarded the cross-event blend above for every
    # athlete carrying a best mark, which is nearly all of them: XBLEND 0, 1, 2
    # and 3 all returned byte-identical scores because the feature never
    # survived to be measured.
    if (!is.null(b)) r_use[m] <- (1 - ceil_e) * r_use[m] + ceil_e * b
  }
  vp0 <- VPv[[ev]]; if (is.null(vp0) || !is.finite(vp0)) vp0 <- stats::var(z$perf)
  v_pre <- numeric(length(a))
  for (m in seq_along(a)) { vv <- V[[kk[m]]]; v_pre[m] <- if (is.null(vv)) vp0 else vv }
  if (HIST) {
    ix <- hi + seq_along(a); hi <- hi + length(a)
    H$race_key[ix] <- z$race_key[1]; H$date[ix] <- as.numeric(dt0)
    H$event_id[ix] <- ev;            H$athlete_id[ix] <- a
    H$r_pre[ix] <- r_pre;            H$n_eff[ix] <- n_eff
    H$r_use[ix] <- r_use;
    H$v_pre[ix] <- v_pre;            H$perf[ix] <- z$perf
    H$place[ix] <- z$place;          H$rc[ix] <- z$rc
    H$seen[ix] <- seen
    hix <- ix          # filled again below, once the update is actually known
  }
  slot <- if (yr == 2025L) "y25" else if (yr == 2026L) "y26" else NA
  # DO NOT SCORE A MERGED RACE. race_key is competition|event|round|date and
  # carries no section identifier, so parallel sections of one round collapse
  # into a single race - one 60m "final" holds eight athletes all placed 1st.
  # A duplicated finishing position proves it: two athletes cannot both be third.
  #
  # .pairs() already drops pairs with EQUAL places, which is why this went
  # unnoticed - the damage is the pairs that look legitimate, first in section A
  # against second in section B, comparing athletes who never met. Measured on
  # the deployed history: 8,994 of 168,006 scored races, carrying 9.97% of all
  # concordance pairs.
  #
  # The race still updates ratings. A blended shock across sections is noisier
  # but not biased, which is the same argument the corpus builder makes for
  # partial fields - whereas scoring it is simply wrong. Set SEQ_SCORE_MERGED=1
  # to restore the old behaviour for comparison.
  if (!is.na(slot) && !SCORE_MERGED && anyDuplicated(z$place[is.finite(z$place)]))
    slot <- NA
  if (!is.na(slot)) {
    # All i<j pairs as plain integer vectors. CJ() cost ~0.4ms per call in fixed
    # data.table dispatch overhead regardless of field size (measured: 79x at
    # n=8), paid once per scored race.
    sel <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg <- .pairs(length(sel), z$place[sel])
    g <- list(i = sel[gg$i], j = sel[gg$j])   # map back to full-field indices
    if (length(g$i)) {
      di <- r_use[g$i] - r_use[g$j]
      pl <- z$place[g$i] < z$place[g$j]
      cw <- as.numeric((di > 0) == pl); cw[di == 0] <- 0.5    # tie = half credit
      acc[[slot]]["conc"] <- acc[[slot]]["conc"] + sum(cw)
      acc[[slot]]["pairs"] <- acc[[slot]]["pairs"] + length(g$i)
      si <- seen[g$i]; sj <- seen[g$j]
      bs <- si & sj; bc <- !si & !sj; mx <- !bs & !bc
      acc[[slot]]["conc_bs"] <- acc[[slot]]["conc_bs"] + sum(cw[bs])
      acc[[slot]]["pairs_bs"] <- acc[[slot]]["pairs_bs"] + sum(bs)
      acc[[slot]]["conc_mx"] <- acc[[slot]]["conc_mx"] + sum(cw[mx])
      acc[[slot]]["pairs_mx"] <- acc[[slot]]["pairs_mx"] + sum(mx)
      acc[[slot]]["conc_bc"] <- acc[[slot]]["conc_bc"] + sum(cw[bc])
      acc[[slot]]["pairs_bc"] <- acc[[slot]]["pairs_bc"] + sum(bc)
      wt <- z$wt
      acc[[slot]]["conc_w"] <- acc[[slot]]["conc_w"] + wt * sum(cw)
      acc[[slot]]["w_sum"]  <- acc[[slot]]["w_sum"]  + wt * length(cw)
      acc[[slot]]["w_sq"]   <- acc[[slot]]["w_sq"]   + wt * wt * length(cw)
      # favourite: ties at the top are broken at random, so credit the expected
      # hit rate rather than whichever athlete which.max happened to return
      rs <- r_use[sel]; ps <- z$place[sel]; tm <- which(rs == max(rs))
      acc[[slot]]["fav"] <- acc[[slot]]["fav"] + mean(ps[tm] == min(ps))
      acc[[slot]]["nr"] <- acc[[slot]]["nr"] + 1
      # WIN PROBABILITIES from rating + own-variance draws. The shared race
      # shock cancels from ordering, so it is deliberately absent.
      # OFF by default (SEQ_WINP=1): measured at ~60s of a ~360s run, and the
      # accumulators it feeds are never written or printed. Nothing else in the
      # loop draws randomness, so skipping it cannot move a scored metric.
      # NOTE the seed is badly collided (25,793 races -> 203 seeds on the 800m W;
      # see check_form_seed_collisions.R) and the first-race variance is
      # mis-initialised — fix both before trusting any Brier from this block.
      if (WINP) {
      # Order-sensitive hash of the WHOLE key. The old seed was
      # sum(utf8ToInt(substr(race_key, 1, 20))), which failed twice over:
      # 20 characters truncates at or before the round, so a meet's rounds and
      # sections hashed alike, and summing character codes discards order AND
      # compresses ~20 ASCII values into a ~300-wide band. Measured on
      # AT-800Metres-W: 25,793 distinct races produced 203 distinct seeds, the
      # largest collision group covering 554 races. set.seed() is global, so any
      # two races sharing a seed and a field size drew an IDENTICAL matrix --
      # their win probabilities were the same random numbers, not independent
      # draws. See check_form_seed_collisions.R.
      set.seed(.rk_seed(z$race_key[1]))
      nf <- length(a)
      dr <- matrix(rnorm(1000L * nf), 1000L, nf) * rep(sqrt(v_pre), each = 1000L) +
            rep(r_use, each = 1000L)
      wins <- tabulate(max.col(dr), nf)
      p_gold <- wins / 1000
      hit <- as.integer(z$place == min(z$place))
      acc[[slot]]["brier"] <- acc[[slot]]["brier"] + sum((p_gold - hit)^2)
      acc[[slot]]["brier_base"] <- acc[[slot]]["brier_base"] + sum((1/nf - hit)^2)
      acc[[slot]]["npred"] <- acc[[slot]]["npred"] + nf
      }
    }
  }
  if (!is.na(z$class[1]) && z$class[1] %chin% MAJ && z$rc[1] == "final" &&
      dt0 >= MAJ_FROM) {
    ms <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg2 <- .pairs(length(ms), z$place[ms])
    g2 <- list(i = ms[gg2$i], j = ms[gg2$j])
    if (length(g2$i)) maj[[length(maj)+1L]] <- data.table(
      class = z$class[1], yr = as.character(yr), event_id = ev,
      conc = { d2 <- r_use[g2$i] - r_use[g2$j]
               c2 <- as.numeric((d2 > 0) == (z$place[g2$i] < z$place[g2$j]))
               c2[d2 == 0] <- 0.5; sum(c2) },
      pairs = length(g2$i),
      fav = { t2 <- which(r_use == max(r_use)); mean(z$place[t2] == min(z$place)) },
      medal3 = sum(a[order(-r_use)][1:3] %chin% a[z$place <= 3]),
      winner_rank = which(order(-r_use) == which.min(z$place)))
  }
  est <- n_eff >= 2
  # SEQ_SHOCK  "mean" (the original) or "median". The shock is what the whole
  # field shared, and a MEAN is not robust to athletes who stop racing.
  #
  # The case that found it: London 1500m, 2025-07-19. Eleven athletes ran
  # 3:28.82 to 3:34.03 and every one beat their rating. Three more came in at
  # 4:18.37, 4:24.03 and 4:27.54 - pacemakers or drop-outs, 45 seconds down. The
  # mean residual is dragged to -0.0262, so the model reads a FAST race as a slow
  # one and hands every genuine finisher a 2.6% bonus as personal credit.
  # Selemon Barega finished 9th, was credited with a +0.0409 surprise on n_eff
  # 0.60 (learning rate 0.79), and came out of it rated 3:29.02 - four seconds
  # faster than the quickest 1500m he has ever run. He then topped the rankings.
  #
  # The Huber clip protects an athlete's own update from their own catastrophe.
  # NOTHING protected the shared shock from someone else's, and the shock is
  # applied to everybody. The median is the same statistic the display offset
  # already uses, and for the same stated reason.
  resid_est <- z$perf[est] - r_pre[est]
  # A TRIMMED mean is the shape this wants. The median is robust but discards the
  # information in every ordinary race, and measured it costs 0.08 pp of raw
  # concordance against the mean. Trimming the tails keeps the efficiency of a
  # mean while refusing to let three athletes who stopped define the race.
  m_est <- sum(est)
  S <- (if (m_est >= SHOCK_MINN)
          (if (SHOCK == "median") stats::median(resid_est)
           else if (SHOCK == "trim") mean(resid_est, trim = SHOCK_TRIM)
           else mean(resid_est))
        else 0) * (if (SHOCK_W == "kappa") m_est / (m_est + SHOCK_K)
                   else m_est / length(a))
  # Per-athlete correction. With slope off this is the constant S for everyone,
  # which is exactly the original behaviour.
  corr <- rep(S, length(a))
  if (SLOPE && sum(est) >= SLOPE_MINN) {
    xe <- r_pre[est]; ye <- z$perf[est]
    # fit on the trimmed set: a regression is even more sensitive to an athlete
    # who stopped than a mean is
    r0 <- ye - xe
    q <- stats::quantile(r0, c(SHOCK_TRIM, 1 - SHOCK_TRIM), names = FALSE)
    kp <- r0 >= q[1] & r0 <= q[2]
    if (sum(kp) >= SLOPE_MINN) {
      xk <- xe[kp]; yk <- ye[kp]
      vx <- stats::var(xk)
      if (is.finite(vx) && vx > 1e-8) {
        b_hat <- stats::cov(xk, yk) / vx
        if (is.finite(b_hat)) {
          # shrink toward 1: with few points, mostly trust shift-only
          w <- length(xk) / (length(xk) + SLOPE_K)
          b <- 1 + w * (b_hat - 1)
          aa <- mean(yk) - b * mean(xk)
          # Use the SAME weighting as S above. This line held sum(est)/length(a)
          # - the field-share dilution the 2026-08-19 shock fix replaced - so
          # turning SEQ_SLOPE on would silently reintroduce the bug it was
          # written to remove. Caught in review; SLOPE is off by default, so
          # nothing shipped was affected.
          corr <- ((aa + b * r_pre) - r_pre) *
                  (if (SHOCK_W == "kappa") m_est / (m_est + SHOCK_K)
                   else m_est / length(a))
        }
      }
    }
  }
  surprise <- (z$perf - r_pre) - corr
  att_e <- ATTENv[[ev]]; if (is.null(att_e) || !is.finite(att_e)) att_e <- ATTEN
  if (att_e != 1) {
    fin <- is.finite(r_pre)
    # a field of one rated athlete has no "relative to the field" to speak of
    if (sum(fin) >= 3) surprise <- surprise + (1 - att_e) * (r_pre - mean(r_pre[fin]))
  }
  k0e <- K0v[[ev]]; if (is.null(k0e) || !is.finite(k0e)) k0e <- K0
  kfl_e <- KFLOORv[[ev]]; if (is.null(kfl_e) || !is.finite(kfl_e)) kfl_e <- KFLOOR
  kap_e <- KAPPAv[[ev]]; if (is.null(kap_e) || !is.finite(kap_e)) kap_e <- KAPPA
  kv <- pmax(k0e * kap_e / (n_eff + kap_e), kfl_e)
  kt1_e <- KT1v[[ev]]; if (is.null(kt1_e) || !is.finite(kt1_e)) kt1_e <- KT1
  if (kt1_e != 1 && z$meet_tier[1] == "T1_elite") kv <- pmin(kv * kt1_e, 0.9)
  cen_e <- CENSv[[ev]]; if (is.null(cen_e) || !is.finite(cen_e)) cen_e <- CENS
  if (cen_e < 1 || CENSWIN < 1) {
    fac <- rep(1, length(a))
    if (cen_e < 1) fac[z$rc != "final" & surprise < 0] <- cen_e
    if (CENSWIN < 1) {
      # place 0 marks a split or an unplaced entry, so require a real placing
      neg_win <- is.finite(z$place) & z$place >= 1 & z$place <= CENSWIN_P &
                 surprise < 0
      fac[neg_win] <- pmin(fac[neg_win], CENSWIN)
    }
    kv <- kv * fac
  }
  hub_e <- HUBERv[[ev]]; if (is.null(hub_e) || !is.finite(hub_e)) hub_e <- HUBER
  if (hub_e > 0) {
    lim <- hub_e * sqrt(v_pre)
    ex <- is.finite(lim) & lim > 0 & abs(surprise) > lim
    if (any(ex)) kv[ex] <- kv[ex] * (lim[ex] / abs(surprise[ex]))
  }
  # after censoring and Huber clipping, so k is the rate that was really used
  if (HIST) { H$shock[hix] <- corr; H$surprise[hix] <- surprise; H$k[hix] <- kv }
  for (m in seq_along(a)) {
    if (!seen[m]) {
      p0 <- z$perf[m]
      if (WINDCS && !is.na(z$beta[m]) && !is.na(z$wind[m])) p0 <- p0 - z$beta[m] * z$wind[m]
      init <- p0 - S
      if (XEV) {
        sib <- BYA[[a[m]]]
        if (!is.null(sib)) {
          sib <- sib[sib != ev]
          if (length(sib)) {
            fams <- reg$family[match(sib, reg$event_id)]
            sib <- sib[!is.na(fams) & fams == z$family[1]]
            if (length(sib)) {
              depth <- vapply(sib, function(s) { n <- NE[[key(a[m], s)]]; if (is.null(n)) 0 else n }, numeric(1))
              best <- which.max(depth)
              if (depth[best] >= 5) {
                xr <- (R[[key(a[m], sib[best])]] - MUv[[sib[best]]]) + mu
                init <- 0.5 * init + 0.5 * xr
              }
            }
          }
        }
      }
      R[[kk[m]]] <- init
      if (!is.null(agef) && !is.na(z$age[m])) LE[[kk[m]]] <- agef(z$age[m])
      BYA[[a[m]]] <- unique(c(BYA[[a[m]]], ev))
    } else {
      R[[kk[m]]] <- r_pre[m] + kv[m] * surprise[m]
    }
    # Variance learns at the same rate; the floor stops a lucky streak
    # collapsing it to zero (the career model's thin-record sigma defect).
    #
    # ONLY for an athlete who carried a rating in. A cold start sets R to absorb
    # this very performance, so its true surprise is zero — but `surprise[m]` is
    # still measured against the population mean, and updating V with that made
    # a debutant's variance the squared distance of their debut from the mean.
    # Elite newcomers got an enormous variance and average ones almost none,
    # which is backwards, and it reached the page as a 10,558-point decathlon
    # and a sub-world-record 100m. Leaving V unset keeps the event prior until
    # there is a real surprise to learn from.
    # `V` IS A LEARNING VARIANCE, NOT A PREDICTIVE ONE. Read that before using it
    # to put an interval on anything. It is an EWMA of SURPRISE squared, and
    # surprise is the residual AFTER the shared race shock has been removed, so
    # it measures the athlete's own race-to-race variability. That is the right
    # quantity for the update below and the wrong one for predicting a mark:
    #
    #   perf - r_pre = surprise + shock          (verified exactly in the stored
    #                                             history, max gap 2.8e-17)
    #
    # so a predictive variance needs v PLUS the race-conditions variance, which
    # is stored per event in predictive_variance.json (pooled shock sd 0.926%,
    # ranging 0.55% in sprints to 1.78% in throws). Standardising a raw residual
    # by sqrt(v) alone gives sd 2.03 instead of 1; adding the shock term takes it
    # to 1.58, and the remainder is a genuinely fat tail rather than a scale
    # error - robustly measured the scale is 1.04-1.09 for any athlete with real
    # evidence, while |z| > 5 occurs 1.1% of the time against a normal's
    # 0.00006%. Do not rescale v to chase sd(z) = 1; quote intervals from
    # empirical quantiles, which is what the peak-mark column already does.
    #
    # NOTE the learning rate: kv here has already been reduced by the Huber clip
    # for large surprises, so an extreme race moves the variance less than its
    # size warrants. That is deliberate for the RATING and is a known
    # conservatism in the variance. See check_predictive_variance.R.
    if (seen[m])
      V[[kk[m]]] <- max(v_pre[m] + kv[m] * (surprise[m]^2 - v_pre[m]), 0.04 * vp0)
    NE[[kk[m]]] <- n_eff[m] + 1
    LD[[kk[m]]] <- dt0n
    # running bests, updated last so every read above stayed lagged
    bc <- BC[[kk[m]]]
    if (is.null(bc) || z$perf[m] > bc) BC[[kk[m]]] <- z$perf[m]
    bsy <- BSY[[kk[m]]]
    if (!is.null(bsy) && bsy == yr) {
      if (z$perf[m] > BS[[kk[m]]]) BS[[kk[m]]] <- z$perf[m]
    } else { BSY[[kk[m]]] <- yr; BS[[kk[m]]] <- z$perf[m] }
    # keep the K best marks and their dates, so the ceiling blend can average a
    # level rather than take a maximum. Updated here with the other running
    # bests, i.e. AFTER every read above, so it stays strictly lagged.
    if (BEST_K > 1) {
      vv <- c(BKV[[kk[m]]], z$perf[m]); dd <- c(BKD[[kk[m]]], dt0n)
      if (length(vv) > BEST_K) {
        o <- order(-vv)[seq_len(BEST_K)]; vv <- vv[o]; dd <- dd[o]
      }
      BKV[[kk[m]]] <- vv; BKD[[kk[m]]] <- dd
    }
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
res <- data.table(tag = TAG,
  conc25 = 100*acc$y25["conc"]/acc$y25["pairs"], fav25 = 100*acc$y25["fav"]/acc$y25["nr"],
  conc26 = 100*acc$y26["conc"]/acc$y26["pairs"], fav26 = 100*acc$y26["fav"]/acc$y26["nr"],
  wconc25 = 100*acc$y25["conc_w"]/acc$y25["w_sum"],
  wconc26 = 100*acc$y26["conc_w"]/acc$y26["w_sum"],
  ess25 = acc$y25["w_sum"]^2/acc$y25["w_sq"], ess26 = acc$y26["w_sum"]^2/acc$y26["w_sq"],
  conc26_bs = 100*acc$y26["conc_bs"]/acc$y26["pairs_bs"],
  conc26_mx = 100*acc$y26["conc_mx"]/acc$y26["pairs_mx"],
  conc26_bc = 100*acc$y26["conc_bc"]/acc$y26["pairs_bc"],
  share26_cold = 100*(acc$y26["pairs_mx"]+acc$y26["pairs_bc"])/acc$y26["pairs"],
  races25 = acc$y25["nr"], races26 = acc$y26["nr"], mins = round(el,1),
  # RAW Brier per prediction, written only when SEQ_WINP computed it (NA
  # otherwise, never a silent 0). These accumulators used to be computed on
  # every race and then discarded — the same dead-computation family as CSHRINK.
  #
  # Deliberately NOT reported as skill against `brier_base`: that baseline is a
  # uniform 1/field prior, and "report skill against a uniform prior" is on this
  # repo's Not-to-do list. Raw Brier is comparable BETWEEN ARMS on the same
  # race set, which is what it is for.
  brier25 = if (WINP && acc$y25["npred"] > 0) acc$y25["brier"]/acc$y25["npred"] else NA_real_,
  brier26 = if (WINP && acc$y26["npred"] > 0) acc$y26["brier"]/acc$y26["npred"] else NA_real_,
  maxplace = MAXPLACE, ceil = CEIL, seeded = n_seeded, huber = HUBER,
  seedhl = SEEDHL, seedhlpow = SEEDHLPOW, seedne = SEEDNE, k0 = K0, kappa = KAPPA, kfloor = KFLOOR,
  kpow = KPOW, ceiladj = CEILADJ, xblend = XBLEND,
  xb_fam = paste(XB_FAM, collapse = "+"),
  evparam = if (is.null(EVP)) "" else basename(EVPARAM),
  w_maj = W_MAJ, w_t1 = W_T1, w_t2 = W_T2, w_rnd = W_RND,
  cens=CENS, age=AGEF, stale=STALE, xev=XEV, kt1=KT1, windcs=WINDCS,
  k0=K0, kappa=KAPPA, kfloor=KFLOOR)
cat(sprintf("[%s] TUNE 2025: conc %.3f%% fav %.1f%% (%d races) | CONFIRM 2026: conc %.3f%% fav %.1f%% (%d races) | %.1f min\n",
    TAG, res$conc25, res$fav25, res$races25, res$conc26, res$fav26, res$races26, el))
cat(sprintf("[%s] 2026 by band: both-rated %.3f%% | one-cold %.3f%% | both-cold %.3f%% | cold pairs %.1f%% of metric
",
    TAG, res$conc26_bs, res$conc26_mx, res$conc26_bc, res$share26_cold))
cat(sprintf("[%s] WEIGHTED (maj %g / T1 %g / T2 %g, non-final x%g): tune %.3f%% (ess %s) | sealed %.3f%% (ess %s)
",
    TAG, W_MAJ, W_T1, W_T2, W_RND, res$wconc25, format(round(res$ess25), big.mark=","),
    res$wconc26, format(round(res$ess26), big.mark=",")))
mj <- rbindlist(maj)
if (nrow(mj)) {
  write_parquet(mj, file.path(SC, sprintf("seqv3_majors_%s.parquet", TAG)))
  cat("
== MAJORS FINALS (2021+), walk-forward ==
")
  print(mj[, .(finals = .N, conc = round(100*sum(conc)/sum(pairs),2),
               fav = round(100*mean(fav),1), medal_hits = round(100*sum(medal3)/(3*.N),1),
               med_winner_rank = as.double(median(winner_rank))), by = .(class, yr)][order(yr, class)])
  cat("
pooled:
")
  print(mj[, .(finals = .N, conc = round(100*sum(conc)/sum(pairs),2),
               fav = round(100*mean(fav),1), medal_hits = round(100*sum(medal3)/(3*.N),1))])
}
f <- file.path(SC, "seqv2_results.csv")
fwrite(res, f, append = file.exists(f))
ids <- ls(R)
st <- data.table(k = ids, R = vapply(ids, function(i) R[[i]], numeric(1)),
                 n_eff = vapply(ids, function(i) NE[[i]], numeric(1)),
                 # v carries the per-athlete performance variance, needed for a
                 # "on a good day" column. NOTE it is the variance of the
                 # SHOCK-ADJUSTED surprise, so it understates what an athlete
                 # actually varies by: the raw residual still contains the race
                 # shock S. Measured sd of (perf-r_pre)/sqrt(v) is 1.52, not 1.
                 # Use an EMPIRICAL quantile of that ratio, never a normal one.
                 v = vapply(ids, function(i) { vv <- V[[i]]
                                               if (is.null(vv)) NA_real_ else vv }, numeric(1)),
                 last = as.Date(vapply(ids, function(i) LD[[i]], numeric(1)),
                                origin = "1970-01-01"),
                 # the athlete's best mark so far, and the blend that ORDERS a
                 # field. R stays the pure rating so nothing downstream silently
                 # inherits the ceiling without asking for it.
                 # Must use the SAME rule the in-race blend used, or R_ceil in
                 # the state table would describe a different model than the one
                 # that was scored. Ages are measured against the athlete's own
                 # last race, so someone still racing whose bests are years old
                 # decays heavily, while a retired athlete does not - the latter
                 # is handled by the display's recency filter, not here.
                 best = vapply(ids, function(i) {
                   b <- if (BEST_K > 1)
                          .best_k(BEST_K, BKV[[i]], BKD[[i]], LD[[i]])
                        else BC[[i]]
                   if (is.null(b)) NA_real_ else b }, numeric(1)))
st[, c("athlete_id","event_id") := tstrsplit(k, "|", fixed = TRUE)]
# Fold split performances into the BEST and the LAST-SEEN date only - never into
# R or n_eff. See the note at the place > 0 filter above for why, and what
# place = 0 does and does not mean.
if (!is.null(SPLITS) && nrow(SPLITS)) {
  st <- merge(st, SPLITS, by = c("athlete_id", "event_id"), all.x = TRUE)
  nb <- sum(is.finite(st$split_best) & (is.na(st$best) | st$split_best > st$best))
  nl <- sum(is.finite(st$split_last) & (is.na(st$last) | st$split_last > st$last))
  st[is.finite(split_best) & (is.na(best) | split_best > best), best := split_best]
  st[is.finite(split_last) & (is.na(last)  | split_last > last),  last := split_last]
  st[, c("split_best", "split_last") := NULL]
  cat(sprintf("[%s] splits raised the best on %s athlete-events and refreshed the last-seen date on %s\n",
              TAG, format(nb, big.mark = ","), format(nl, big.mark = ",")))
}
st[, R_ceil := fifelse(is.na(best), R, (1 - CEIL) * R + CEIL * best)]
if (HIST) {
  hd <- as.data.table(lapply(H, function(v) v[seq_len(hi)]))
  hd[, date := as.Date(date, origin = "1970-01-01")]
  write_parquet(hd, file.path(SC, sprintf("seqv3_history_%s.parquet", TAG)))
  cat(sprintf("[%s] history: %s athlete-races (races with <3 athletes are skipped\n",
              TAG, format(nrow(hd), big.mark = ",")))
  cat("        by the loop entirely, so this is scored racing, not every result)\n")
  # PROVENANCE. Added 2026-08-18, the day it was needed. A per-family table was
  # built comparing arms run at 18:32 against a baseline run at 15:02, with this
  # script edited at 17:20 in between - so every delta in it was the parameter
  # PLUS the edit, and the three families that seemed to move without being
  # fitted were exactly the ones whose FAM_K0 override that edit removed. Two
  # parquets, same schema, same row count, silently incomparable, with nothing
  # in the output saying so. score_by_event.R reads this stamp and refuses to
  # difference arms built by different engines.
  meta <- list(tag = TAG,
               written = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               engine = basename(ENGINE_SRC),
               engine_sha = ENGINE_SHA,
               rows = nrow(hd),
               env = as.list(Sys.getenv(grep("^SEQ_", names(Sys.getenv()), value = TRUE))))
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
             file.path(SC, sprintf("seqv3_meta_%s.json", TAG)))
  cat(sprintf("[%s] engine sha %s\n", TAG, substr(ENGINE_SHA, 1, 12)))
}
write_parquet(st[, !"k"], file.path(SC, sprintf("seqv2_state_%s.parquet", TAG)))
