# THE FORM MODEL, SWIMMING -- sequential walk-forward ratings, v1. Sibling of
# form_ratings.R (athletics "Sequential v3"); see that file's header for the
# method this is derived from. Kept as a SEPARATE FILE per instruction -- no
# `sport ==` branch was added to form_ratings.R.
#
# WHAT IS THE SAME: parse SEQ_* env knobs, load the corpus + event registry,
# set priors, run ONE global chronological sweep computing a shock/surprise/
# learning-rate update per athlete, write a scorecard and a seqv2_state parquet
# with the SAME schema as the athletics engine.
#
# WHAT IS DIFFERENT, and why (see the task brief this was built from for the
# full reasoning; the short version is recorded inline at each knob):
#
#   * NO wind / venue / altitude / indoor-outdoor correction. Swimming has no
#     such concept -- course type (SCM/LCM) is already baked into `perf` via
#     `course_offset` upstream, in the corpus build.
#   * NO per-family half-life table. One flat SEQ_HL, default 180 days,
#     validated in backtest_swimming.R (180 beat 730 on every measure).
#   * NO world-record impossible-mark guard. data/world_records.csv exists
#     but holds 0 swimming rows (every row is an AT- event id) -- no swimming
#     WR data to guard against, dropped for v1.
#   * NO age-drift / aging-curve adjustment. The corpus carries no `age`
#     column and there is no swimming aging.rds -- the mechanism has nothing
#     to attach to.
#   * NO technical/tactical ceiling split. citius_events() has `tactical` and
#     `technical` always FALSE for every swim event, so the split is a no-op
#     by construction -- not ported (form_ratings.R found the equivalent
#     athletics knob was ALSO a no-op / actively harmful when tried).
#   * THE SCORING/PAIRING UNIT CHANGES. This is the single biggest structural
#     difference and is documented at length where it happens, below --
#     search "WHOLE-FIELD-PER-COMPETITION-EVENT".
#   * TIERING is a LEFT join, never an inner join. Only 64 of 24,524 swim
#     competitions carry a tier (T1_elite; no T2 exists), so an athletics-style
#     inner join would drop 99.7% of the data. Every race is still RATED
#     regardless of tier; tier only ever affects the METRIC WEIGHT.
#   * SEEDING resolves identity through a crosswalk first. The careers store
#     (swim_athlete_history.rds) carries raw World Aquatics athlete ids with
#     NO person_id column, unlike the main corpus (whose `athlete_id` was
#     confirmed, empirically, to already equal the resolved `person_id` for
#     every one of its 1,832,222 rows -- 0 mismatches). Seeding on the raw
#     careers id without resolving it first would silently refragment
#     identity for exactly the debutants the seed exists to help.
#   * NO cross-event similarity matrix (event_similarity_spec is athletics-
#     only, and building a swimming-scoped one from a first state file is
#     circular for a brand-new engine that has no state file yet). SEQ_XBLEND
#     therefore keeps only the plain FAMILY-GATE mode and defaults OFF.
#
# WHAT WAS DELIBERATELY NOT PORTED, to keep a v1 in scope (all easy to add
# later, none of them was asked for explicitly, all default off/absent in the
# athletics engine anyway):
#   SEQ_SLOPE (per-race slope fit), SEQ_HUBER_LO (asymmetric Huber -- the spec
#   explicitly says symmetric only), SEQ_XEV / SEQ_SEED_XEV (cold-start
#   cross-event blending, distinct from SEQ_XBLEND), SEQ_SEEDHLPOW,
#   SEQ_CEILADJ (explicitly excluded by the spec), SEQ_WINP / Brier scoring,
#   the SPLIT-PERFORMANCE mark-only-no-placing salvage (an athletics-specific
#   finding about mid-race split times; not established for swimming), and
#   the MAJORS FINALS scorecard (the swim competition catalogue carries no
#   `class` column distinguishing Olympics/Worlds/Commonwealth from other
#   T1_elite meets, so the machinery has nothing to key on).
#
# TUNE/CONFIRM WINDOW: athletics scores 2025 vs 2026 as a pre-registered
# tuning-then-sealed pair, built up over months of ladder experiments. No such
# convention exists yet for swimming, so this engine reports ONE overall
# metric rather than inventing a swim-specific split. Cutting the report by
# year is easy to add once a tuning methodology exists.
#
# SEVERAL NUMERIC DEFAULTS BELOW ARE INHERITED FROM ATHLETICS UNCHANGED AND
# ARE FLAGGED, NOT SILENTLY ASSUMED. KAPPA/KFLOOR, CENS, CENSWIN, HUBER, CEIL
# and CEIL_MODE all keep their athletics-fitted values because the task spec
# grouped them under "copy essentially unchanged" and no swap was noted --
# but none of them has been swept against swimming data (CENS/CENSWIN were
# tested as a possible fix for the calibration issue below and ruled OUT --
# tested, not just left alone). Treat the rest as placeholders pending a
# swimming-specific fit, not validated choices -- this is called out again at
# each knob. K0 and DEBUT_PRIOR are the two exceptions: each swept and
# promoted on its own knob's comment below (DEBUT_PRIOR on 2026-08-24 to
# "replacement", K0 on 2026-08-25 to 0.7) -- see each for the numbers.
suppressMessages(library(data.table)); suppressMessages(library(arrow))

.env_num <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) return(default)
  x <- suppressWarnings(as.numeric(v))
  if (!is.finite(x)) stop(sprintf("%s='%s' is not a finite number", name, v))
  x
}

OUT <- "C:/dev/citiusverse/citiusdata/data"
SC  <- Sys.getenv("FORM_OUT", here::here("citiusdata", "data"))

# --- learning-rate schedule ---------------------------------------------------
# K0 SWEPT 2026-08-25 (citiusdata#16 investigation): 0.95 (inherited from
# athletics) is too aggressive for swimming's post-debut updates. Swept
# {0.3,0.5,0.7,0.85,0.95}: K0=0.7 strictly dominates the athletics default on
# BOTH axes measured -- confirm-window concordance 79.514% -> 79.544%, and
# "typical" calibration on the 2026 out-of-sample window improves substantially
# at low/mid evidence depth (n_eff<=1 band: 47.4% -> 49.9% beaten, target 50%;
# n_eff 1-2: 38.1% -> 41.6%) with ZERO change at high n_eff (5-10 and >10 bands
# identical to baseline either way -- K0 does not explain the persistent
# high-n_eff miscalibration, see below). 0.3/0.5 improve low-n_eff calibration
# even further (up to 56.1%/50.0% at K0=0.3) but cost concordance (78.998%/
# 79.288% confirm) -- 0.7 was chosen as the point that does not trade one
# metric for the other. KAPPA/KFLOOR NOT swept alongside it -- still inherited
# placeholders.
#
# THE HIGH-n_eff MISCALIBRATION IS NOT A K0 PROBLEM AND IS STILL OPEN. Cutting
# "typical beaten %" by n_eff (not by month -- the original "worst in January"
# framing was a confound, see citiusdata#16) shows a monotonic decline from
# ~50% at low evidence to 0% at n_eff>10, UNCHANGED by K0 in the 5-10/>10
# bands across the whole swept range. Per-race `surprise` shows an
# overshoot-and-drift shape (positive right after debut, growing negative with
# n_eff) that K0 only partially addresses. SEQ_CENS/SEQ_CENSWIN tested and
# ruled out directly (no effect on any band). Candidate causes not yet tested:
# the debut-prior's own calibration (promoted on a concordance sweep, never
# calibration-checked), a feedback loop through the shared race-shock
# estimator (established athletes' already-biased surprise feeding into the
# shock estimate for their own races), or an aging effect this engine has no
# mechanism for. Needs a real experiment campaign, not a knob tweak.
K0 <- .env_num("SEQ_K0", 0.7); KAPPA <- .env_num("SEQ_KAPPA", 3)
KFLOOR <- .env_num("SEQ_KFLOOR", 0.32)
CENS <- .env_num("SEQ_CENS", 0.3); STALE <- Sys.getenv("SEQ_STALE","1") != "0"
CENSWIN   <- .env_num("SEQ_CENSWIN", 0.1)
CENSWIN_P <- .env_num("SEQ_CENSWIN_PLACE", 1)
KT1 <- .env_num("SEQ_KT1", 1)
# One flat half-life, not an athletics-style per-family table. VALIDATED for
# swimming specifically in backtest_swimming.R (180 beats 730 on every
# measure: gold skill 0.253 vs 0.234, mean reliability gap 0.035 vs 0.046,
# 70% vs 67% of races beating baseline). This is the one constant in this
# file that IS a real swimming-specific measurement, not a transplant.
HL <- .env_num("SEQ_HL", 180)

# --- ceiling / best-mark blend (INHERITED numeric default, unswept) ---------
CEIL <- .env_num("SEQ_CEIL", 0.30)
CEIL_MODE <- Sys.getenv("SEQ_CEIL_MODE", "best")
CEILC     <- .env_num("SEQ_CEIL_C", 0.684)
stopifnot("SEQ_CEIL_MODE must be 'best' or 'quantile'" =
            CEIL_MODE %chin% c("best", "quantile"))
BEST_K  <- max(1L, as.integer(.env_num("SEQ_BEST_K", 1)))
BEST_HL <- local({
  v <- Sys.getenv("SEQ_BEST_HL", "")
  if (!nzchar(v)) return(Inf)
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x) || x <= 0) stop(sprintf("SEQ_BEST_HL='%s' must be a positive number of days, or Inf", v))
  x
})
.best_k <- function(K, v, dts, now) {
  if (is.null(v) || !length(v)) return(NULL)
  w <- if (is.finite(BEST_HL)) 2^(-(now - dts) / BEST_HL) else rep(1, length(v))
  if (!sum(w) > 0) return(max(v))
  sum(w * v) / sum(w)
}

# --- debut prior --------------------------------------------------------------
# SEQ_DEBUT_PRIOR: mean | replacement.
#   mean         seed a debutant at MU, the population mean.
#   replacement  seed at the walk-forward expanding mean of DEBUT performances
#                in the SAME event, strictly before the debut being predicted.
# PROMOTED to the default 2026-08-24: a 4-arm sweep (ceil_mode x debut_prior)
# found replacement beats mean by +0.75pp tune / +0.72-0.76pp confirm,
# consistent in sign across both ceil_mode pairings -- a robust win, unlike
# ceil_mode itself (see SEQ_CEIL_MODE below, sign-flipped between windows and
# left at its inherited default). Full sweep numbers in DECISIONS.md's
# 2026-08-24 entry (no separate docs/reviews/ write-up exists yet).
# The athletics engine also has replacement_tier / replacement_field variants;
# NOT ported here. Porting all four athletics variants (two of which were
# refuted or marginal there) would be manufacturing false precision on a knob
# this sweep only tested in its two-way form.
DEBUT_PRIOR <- Sys.getenv("SEQ_DEBUT_PRIOR", "replacement")
stopifnot("SEQ_DEBUT_PRIOR must be mean or replacement" =
            DEBUT_PRIOR %chin% c("mean", "replacement"))

# --- Huber robust update, SYMMETRIC ONLY (per spec) --------------------------
HUBER <- .env_num("SEQ_HUBER", 3)

# --- race shock estimator (trimmed mean across established athletes) --------
SHOCK <- Sys.getenv("SEQ_SHOCK", "trim")
stopifnot("SEQ_SHOCK must be 'mean', 'median' or 'trim'" =
            SHOCK %in% c("mean", "median", "trim"))
SHOCK_TRIM <- .env_num("SEQ_SHOCK_TRIM", 0.20)
SHOCK_MINN <- .env_num("SEQ_SHOCK_MINN", 2)
SHOCK_W    <- Sys.getenv("SEQ_SHOCK_W", "kappa")
SHOCK_K    <- .env_num("SEQ_SHOCK_K", 2)
stopifnot("SEQ_SHOCK_W must be 'share' or 'kappa'" = SHOCK_W %chin% c("share", "kappa"))

# --- attenuation correction (off by default, as in athletics) ---------------
ATTEN <- .env_num("SEQ_ATTEN", 1)

# --- variance prior from within-athlete variation (generic, ported as-is) ---
VPRIOR <- Sys.getenv("SEQ_VPRIOR","1") != "0"
VPADJ  <- .env_num("SEQ_VPADJ", 0.5)
VPMINA <- .env_num("SEQ_VPMINA", 20)
KPOW   <- .env_num("SEQ_KPOW", 0)

# --- SEQ_SEED: seed a debut rating from the careers store -------------------
# See the crosswalk block below the loader for why this MUST resolve identity
# before seeding.
SEEDON <- Sys.getenv("SEQ_SEED","1") != "0"
SEEDHL <- .env_num("SEQ_SEEDHL", 45)
SEEDNE <- .env_num("SEQ_SEEDNE", 5)

# --- cross-event blend (family-gate only; DEFAULT OFF per spec) -------------
# No event_similarity_spec.parquet scoped to swimming exists -- building one
# needs a state file from a run of THIS engine, which does not exist yet on a
# clean checkout. The SEQ_XB_MINCOR / measured-similarity mode from
# form_ratings.R is therefore not ported; only the plain family gate is, and
# it stays inert until someone turns SEQ_XBLEND on.
XBLEND  <- .env_num("SEQ_XBLEND", 0)
XB_MAXN <- .env_num("SEQ_XB_MAXN", 1e9)
XB_MINS <- .env_num("SEQ_XB_MINS", 2)
# Generic default: all four swim families. Unmeasured (XBLEND is off by
# default so this is inert), unlike athletics' distance/middle/sprint/hurdles
# set, which was chosen from three windows of real measurement.
XB_FAM <- strsplit(Sys.getenv("SEQ_XB_FAM",
                    "swim_sprint,swim_middle,swim_distance,swim_im"), ",")[[1]]
XB_PICK <- Sys.getenv("SEQ_XB_PICK", "cor")
XB_NSIB <- max(1L, as.integer(.env_num("SEQ_XB_NSIB", 3)))
stopifnot("SEQ_XB_PICK must be 'evidence' or 'cor'" = XB_PICK %in% c("evidence", "cor"))

# --- per-event parameter overrides (same mechanism as athletics) ------------
TAG <- Sys.getenv("SEQ_TAG", "SW-baseline")   # "SW-" prefix by default so the
# output files never collide with the athletics engine's seqv2_state_baseline
# / seqv3_meta_baseline.json in the SAME output directory.
EVPARAM <- Sys.getenv("SEQ_EVPARAM", "")
EVP <- NULL
if (nzchar(EVPARAM) && !file.exists(EVPARAM))
  stop(sprintf("SEQ_EVPARAM is set to '%s' but that file does not exist", EVPARAM))
if (nzchar(EVPARAM)) {
  EVP <- setDT(read_parquet(EVPARAM))
  stopifnot("SEQ_EVPARAM file has no event_id column" = "event_id" %in% names(EVP))
  EVP[, event_id := as.character(event_id)]
  ovr <- setdiff(names(EVP), "event_id")
  known <- c("k0", "kfloor", "ceil", "huber", "xblend", "seedhl",
             "kappa", "cens", "kt1", "atten")
  if (length(setdiff(ovr, known)))
    stop(sprintf("SEQ_EVPARAM has unknown column(s): %s (known: %s)",
                 paste(setdiff(ovr, known), collapse = ", "),
                 paste(known, collapse = ", ")))
  stopifnot("SEQ_EVPARAM has no parameter columns" = length(ovr) > 0)
  cat(sprintf("[%s] per-event overrides from %s: %d events, columns %s\n",
      TAG, basename(EVPARAM), nrow(EVP), paste(ovr, collapse = ", ")))
}
.ev_vec <- function(nm, global, events) {
  v <- setNames(rep(global, length(events)), events)
  if (!is.null(EVP) && nm %in% names(EVP)) {
    want <- EVP[!is.na(get(nm))]
    e <- want[event_id %chin% events]
    if (nrow(e)) v[e$event_id] <- e[[nm]]
  }
  v
}

MAXPLACE <- as.integer(.env_num("SEQ_MAXPLACE", 0))   # 0 = score everyone.
# Athletics defaults to 12 after a specific judgement call about what the
# model should be scored on. No equivalent call has been made for swimming
# (a "full final" is typically 8 lanes, sometimes 10) -- rather than invent
# that number, this defaults to uncapped. Set SEQ_MAXPLACE explicitly to cap.
HIST <- Sys.getenv("SEQ_HIST","") != ""
FROM <- as.Date(Sys.getenv("SEQ_FROM", "2015-01-01"))
stopifnot("SEQ_FROM is not a readable date" = !is.na(FROM))
# UNVALIDATED. Athletics' 2020 cutoff came from a measured trade-off between
# elite sample and total sample (see form_ratings.R's own note on SEQ_FROM,
# itself left "unresolved, not rejected"). No such sweep has been run for
# swimming. 2015 is a generic choice (spans Rio 2016 through the present,
# ~2 Olympic cycles) -- flagged as a follow-up, not a finding.

# --- metric weights: T1_elite vs everything else, no T2 tier exists ---------
# UNVALIDATED transplant of athletics' T1 weight. No W_MAJ: the swim
# competition catalogue has no `class` column distinguishing Olympics/Worlds/
# Commonwealth from other T1_elite meets (see harvesting notes), so there is
# nothing to key an extra "majors" tier on for v1.
W_T1_ELITE <- .env_num("SEQ_W_T1_ELITE", 12)
W_DEFAULT  <- .env_num("SEQ_W_DEFAULT",   1)
W_RND      <- .env_num("SEQ_W_RND",     0.5)

ENGINE_SRC <- local({
  f <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
  if (is.na(f) || !nzchar(f)) f <- here::here("citiusdata", "scripts", "form_ratings_swimming.R")
  f
})
ENGINE_SHA <- if (file.exists(ENGINE_SRC))
  digest::digest(file = ENGINE_SRC, algo = "sha256") else "unknown"

# =============================================================================
# LOAD: registry, competition catalogue, corpus
# =============================================================================
reg <- as.data.table(citius::citius_events())[sport == "Swimming", .(event_id, family)]
stopifnot("swimming event registry is empty" = nrow(reg) > 0)

cat0 <- setDT(read_parquet(file.path(OUT, "swim_competition_catalogue.parquet")))
stopifnot("swim_competition_catalogue.parquet loaded 0 rows" = nrow(cat0) > 0)
cat0[, competition_id := as.character(competition_id)]
cat0 <- cat0[, .(competition_id, tier)]
stopifnot("competition_id must be unique in the catalogue" =
            !anyDuplicated(cat0$competition_id))

evs <- setdiff(sub("^event_id=", "", list.dirs(file.path(OUT, "swimming_corpus_store"),
               recursive = FALSE, full.names = FALSE)), "__unmatched__")
stopifnot("no swimming_corpus_store event partitions found" = length(evs) > 0)
dl <- list()
n_fail <- 0L
for (EV in evs) {
  f <- file.path(OUT, sprintf("swimming_corpus_store/event_id=%s/part-0.parquet", EV))
  x <- tryCatch(setDT(read_parquet(f)), error = function(e) e)
  if (inherits(x, "error")) {
    n_fail <- n_fail + 1L
    warning(sprintf("failed to read swimming_corpus_store partition event_id=%s: %s",
                    EV, conditionMessage(x)))
    next
  }
  x[, `:=`(event_id = EV, athlete_id = as.character(athlete_id),
           competition_id = as.character(competition_id))]
  dl[[EV]] <- x
}
d <- rbindlist(dl, fill = TRUE); rm(dl); invisible(gc())
cat(sprintf("[%s] loaded %s rows from %d of %d swim event partitions (%d failed)\n", TAG,
            format(nrow(d), big.mark = ","), length(evs) - n_fail, length(evs), n_fail))
# A partial partition-read failure would otherwise produce plausible-looking
# output built on silently-incomplete history -- fail loud rather than let a
# clean-looking scorecard hide missing data (this file's own SEQ_SEED coverage
# check below applies the same discipline; this is the one place it was
# missing before this fix).
stopifnot("one or more swimming_corpus_store partitions failed to read - see warnings above" =
            n_fail == 0L,
          "no rows loaded from swimming_corpus_store" = nrow(d) > 0)

# A small number of rows (1,722 of 1.83M in the full corpus) carry a garbage
# athlete_id like "worldaquatics|NA" -- an unresolved-identity sentinel that
# happens to contain the same "|" this file uses as the R/NE/... key
# separator (`key(a, e) <- paste0(a, "|", e)`). Left in, it broke the
# tstrsplit() that recovers (athlete_id, event_id) from that key at the end
# of the run. Drop them here: they are not a usable identity either way.
n0 <- nrow(d)
d <- d[!grepl("|", athlete_id, fixed = TRUE)]
if (nrow(d) < n0)
  cat(sprintf("[%s] dropped %s row(s) with a '|' in athlete_id (unresolved-identity sentinel)\n",
              TAG, format(n0 - nrow(d), big.mark = ",")))

# The corpus's OWN `tier` column is 100% NA (confirmed empirically) -- it is
# LEFT here from championship_results-style provenance and never populated for
# swimming. Drop it and LEFT JOIN the catalogue's tier instead, keeping every
# row regardless of match (only 64 of 24,524 competitions carry a tier; an
# inner join would drop 99.7% of the corpus, which is wrong -- tier affects
# the METRIC WEIGHT below, never inclusion in rating).
if ("tier" %chin% names(d)) d[, tier := NULL]
n0 <- nrow(d)
d <- merge(d, cat0, by = "competition_id", all.x = TRUE)
stopifnot("the tier left-join changed the row count" = nrow(d) == n0)
cat(sprintf("[%s] tier coverage: %.2f%% of rows carry T1_elite (left join, not filtered)\n",
            TAG, 100 * mean(!is.na(d$tier) & d$tier == "T1_elite")))

# `date` has a small NA rate (0.5% corpus-wide); fall back to the
# competition's start date rather than dropping those rows outright.
d[, date := fifelse(is.na(date), comp_start, date)]
d <- d[!is.na(perf) & !is.na(date) & !is.na(race_key) & !is.na(place) & place > 0 & date >= FROM]
# Guards below this point (row-count-preserved joins, "every row has a finite
# weight") are vacuously TRUE on an empty data.table -- stopifnot(nrow(d)==0)
# would pass, all(logical(0)) is TRUE. This is the one place that actually
# distinguishes "empty because nothing survived filtering" from "the filter
# worked as intended," so every check downstream can rely on nrow(d) > 0.
stopifnot("no rows survived corpus filtering (date/perf/place/race_key) - FROM cutoff or upstream corpus may be broken" =
            nrow(d) > 0)

n0 <- nrow(d)
d <- merge(d, reg, by = "event_id", all.x = TRUE)
stopifnot("event join changed row count" = nrow(d) == n0,
          "every partitioned event_id must resolve to a registry family" =
            sum(is.na(d$family)) == 0)

setorder(d, date, race_key)
cat(sprintf("[%s] %s rows | %s races (race_key) | %s athlete-events\n", TAG,
    format(nrow(d), big.mark=","), format(uniqueN(d$race_key), big.mark=","),
    format(uniqueN(paste(d$athlete_id, d$event_id)), big.mark=",")))

# =============================================================================
# WHOLE-FIELD-PER-COMPETITION-EVENT SCORING UNIT
# =============================================================================
# THE HIGHEST-RISK PIECE OF THIS FILE. Read this before touching it.
#
# form_ratings.R iterates one block per `race_key` (competition|event|round|
# date, with no section id) because in athletics 2,462 of 2,545 multi-section
# groups place PER SECTION -- each heat/section crowns its own winner, and
# comparing "1st in section A" against "1st in section B" would compare
# athletes who never raced each other.
#
# Swimming is the opposite in exactly the case that matters: backtest_swimming
# .R found 115 of 116 multi-section FINAL groups place GLOBALLY -- a distance
# "timed final" (800m/1500m free and similar) is swum in several heats purely
# because a pool has finite lanes, and the medals are decided on time across
# the WHOLE field, not per heat. Keying those by race_key would give 4.5%
# multi-winner races and 11.1% with no winner in the field; keying them by
# (competition_id, event_id) gives 0.3% / 0.0% on that validated subset.
#
# THIS ENGINE THEREFORE GROUPS DIFFERENTLY BY ROUND:
#   * round is EXACTLY "Final" or "Finals" (case-insensitive, anchored) -->
#     merge every race_key section sharing that (competition_id, event_id)
#     into ONE block. This is the "timed final split across heats" case.
#   * every other round (Heats, Semifinals, Preliminaries, Quarterfinals, and
#     -- importantly -- "A Final" / "B Final" / "C Final" / "Timed Finals")
#     stays keyed by race_key, exactly as in athletics.
# A/B/C finals are deliberately NOT merged with plain "Final": they are
# genuinely different competitive tiers (the B final is the next 8 fastest,
# not a parallel heat of the A final), so merging them would compare a B-final
# winner against an A-final winner as if they raced each other, which they did
# not. This is why the merge condition is an EXACT, ANCHORED match on the
# round label and not a substring "contains final" test.
#
# WHY NOT DERIVE PLACE FROM `perf` INSTEAD OF TRUSTING THE RAW FIELD. This was
# tried and rejected during development. Ranking every row of a merged group
# by `perf` looks appealing -- it would sidestep any question of whether the
# raw scraped `place` is section-local or field-global -- but empirically,
# even within SINGLE-section "Final" races, perf-rank agrees with the raw
# `place` field only 60.1% of the time (mean per-race agreement; only 40.8%
# of races agree exactly). Spot-checking disagreements shows contamination
# from the messier sources (age-group meets, mixed splits, occasional
# mis-parsed distances) rather than a place/perf orientation problem -- e.g. a
# 37.20s "time" placed 3rd in a field of 100m+ swims, almost certainly a
# different distance or a relay split misattributed to this race_key. Deriving
# place from perf would launder that contamination INTO the placings instead
# of flagging it. The raw `place` field, imperfect as it is, is closer to
# ground truth than a perf-based re-derivation over the full (not just the
# validated, cleaner) corpus -- so this engine trusts it, same as
# backtest_swimming.R does, and instead protects the SCORING step with the
# conflict guard below.
#
# THE CONFLICT GUARD. Measured on this corpus: 76.5% of multi-section exact-
# Final groups have MORE than one row at place==1 (i.e. `place` is often
# SECTION-LOCAL, not global, in the wider corpus outside backtest_swimming's
# validated finals-with-full-ability-history subset). `.pairs()` below already
# drops any pair with EQUAL place, so two section-local "place 1"s are simply
# never compared -- safe. What is NOT automatically safe is two DIFFERENT
# place numbers from different, incompatibly-numbered sections being compared
# as if one genuinely beat the other. The guard below (ported directly from
# form_ratings.R's SCORE_MERGED check, just applied at the new grouping
# granularity) detects the tell of an inconsistent group -- the SAME place
# held by rows with DIFFERENT marks -- and skips SCORING (never RATING) that
# block. Measured: 13.6% of exact-Final (competition_id, event_id) groups hit
# this guard. The rating update always proceeds regardless -- a blended shock
# across an inconsistently-numbered group is noisier but not biased, the same
# argument form_ratings.R already makes for its own merged-race case.
#
# THE CONDITIONS/SCORING ASYMMETRY THIS ENGINE DOES NOT IMPLEMENT.
# backtest_swimming.R notes race_key remains the right unit for the SHARED
# RACE SHOCK (decompose_races()), since sections of a timed final really were
# swum in separate heats with their own conditions, even though they are
# scored as one field. This engine computes ONE shock across the WHOLE merged
# block rather than one shock per original race_key section. That is a
# simplification, not an oversight: 89.6% of exact-Final groups are already
# single-section (the merge is a no-op for the large majority), and for the
# 10.4% that are not, form_ratings.R's own reasoning about its merged-race
# case applies directly -- "a blended shock across sections is noisier but
# not biased." A true per-section shock (computed within each race_key,
# applied only to that section's athletes, while pairing still spans the
# whole merged block) is the more precise version and is a reasonable follow-
# up, not a blocker for v1.
d[, round_norm := trimws(as.character(round))]
d[, rc := fifelse(grepl("semi|quarterfinal", round_norm, ignore.case = TRUE), "semi",
           fifelse(grepl("heat|preliminar", round_norm, ignore.case = TRUE), "heat",
                   "final"))]
d[, is_final_exact := grepl("^finals?$", round_norm, ignore.case = TRUE)]
d[, block_key := fifelse(is_final_exact, paste(competition_id, event_id, sep = "||"), race_key)]
n_merged <- uniqueN(d[(is_final_exact), .(competition_id, event_id)])
n_merged_multi <- d[(is_final_exact), .(nsec = uniqueN(race_key)), by = .(competition_id, event_id)][nsec > 1, .N]
cat(sprintf("[%s] whole-field scoring unit: %s exact-Final (competition,event) groups, %s of them span >1 race_key section\n",
            TAG, format(n_merged, big.mark=","), format(n_merged_multi, big.mark=",")))

# =============================================================================
# priors
# =============================================================================
MU <- d[, .(mu = mean(perf)), by = event_id]; MUv <- setNames(MU$mu, MU$event_id)

RLE <- new.env(hash = TRUE, parent = emptyenv())   # event|year, for DEBUT_PRIOR="replacement"
if (DEBUT_PRIOR == "replacement") {
  .fd <- d[, .(first_date = min(date)), by = .(athlete_id, event_id)]
  .db <- merge(d[, .(athlete_id, event_id, date, perf)], .fd, by = c("athlete_id", "event_id"))
  .db <- .db[date == first_date & is.finite(perf)]
  .db[, yr := year(date)]
  stopifnot("no debut rows found - the first_date join is wrong" = nrow(.db) > 0)
  g <- .db[, .(s = sum(perf), n = .N), by = .(event_id, yr)]
  setorder(g, event_id, yr)
  g[, `:=`(cs = cumsum(s) - s, cn = cumsum(n) - n), by = event_id]
  g <- g[cn > 0]; g[, rl := cs / cn]
  if (nrow(g)) {
    ks <- paste(g$event_id, g$yr, sep = "|")
    for (i in seq_len(nrow(g))) assign(ks[i], g$rl[i], envir = RLE)
  }
  cat(sprintf("[%s] debut prior 'replacement': %s debut rows, %d event-year cells\n",
              TAG, format(nrow(.db), big.mark = ","), nrow(g)))
  if (!nrow(g))
    cat(sprintf("[%s] WARNING: debut prior table is EMPTY - every seed falls back to MU\n", TAG))
  rm(.fd, .db, g)
}

VP <- d[, .(v = var(perf)), by = .(event_id, race_key)][is.finite(v),
        .(vp = stats::median(v)), by = event_id]
VPv <- setNames(VP$vp, VP$event_id)
if (VPRIOR) {
  dd <- d[, .(athlete_id, event_id, date, race_key, perf)]
  setorder(dd, athlete_id, event_id, date, race_key)
  dv <- dd[, if (.N >= 8L) .(vd = stats::var(diff(perf))/2) else NULL, by = .(athlete_id, event_id)]
  est <- dv[is.finite(vd) & vd > 0, .(vp = stats::median(vd)/VPADJ, n_ath = .N), by = event_id][n_ath >= VPMINA]
  cmp <- merge(data.table(event_id = names(VPv), old = as.numeric(VPv)), est, by = "event_id")
  shrink <- if (nrow(cmp)) stats::median(cmp$old / cmp$vp) else 1
  newv <- VPv; newv[] <- as.numeric(VPv) / shrink
  if (nrow(est)) newv[est$event_id] <- est$vp
  cat(sprintf("[%s] variance prior: %d of %d events from within-athlete data, %d shrunk by %.2fx\n",
      TAG, nrow(est), length(VPv), length(VPv) - nrow(est), shrink))
  VPv <- newv
  rm(dd, dv, est, cmp); invisible(gc())
}

K0v     <- .ev_vec("k0",     K0,     names(MUv))
KFLOORv <- .ev_vec("kfloor", KFLOOR, names(MUv))
HUBERv  <- .ev_vec("huber",  HUBER,  names(MUv))
XBLENDv <- .ev_vec("xblend", XBLEND, names(MUv))
ATTENv  <- .ev_vec("atten",  ATTEN,  names(MUv))
KAPPAv  <- .ev_vec("kappa",  KAPPA,  names(MUv))
CENSv   <- .ev_vec("cens",   CENS,   names(MUv))
KT1v    <- .ev_vec("kt1",    KT1,    names(MUv))
CEILv   <- .ev_vec("ceil",   CEIL,   names(MUv))
if (KPOW != 0) {
  sd_ev <- sqrt(as.numeric(VPv)); ref <- stats::median(sd_ev)
  K0v[] <- pmin(pmax(K0 * (ref / sd_ev)^KPOW, 0.25), 1.30)
}

R <- new.env(parent=emptyenv()); NE <- new.env(parent=emptyenv())
V <- new.env(parent=emptyenv())
LD <- new.env(parent=emptyenv())
BC <- new.env(parent=emptyenv()); BS <- new.env(parent=emptyenv()); BSY <- new.env(parent=emptyenv())
BKV <- new.env(parent=emptyenv()); BKD <- new.env(parent=emptyenv())
BYA <- new.env(parent=emptyenv())   # athlete -> vector of events raced, for XBLEND
key <- function(a, e) paste0(a, "|", e)

# =============================================================================
# SEQ_SEED: pre-populate state from the careers store, THROUGH THE CROSSWALK
# =============================================================================
# swim_athlete_history.rds (-> swimming_careers_store) carries raw World
# Aquatics athlete ids with NO person_id column. The main corpus's
# `athlete_id` was verified to already equal the resolved `person_id` for
# every one of its 1,832,222 rows (0 mismatches) -- it was resolved at
# corpus-build time. Seeding directly on the careers store's raw id would
# create a SEPARATE, unlinked identity for every seeded debutant: the seed
# would sit under key "0000ae2c-...|SW-..." while the corpus rates that same
# swimmer under "BYUNHYEYOUNG|SW-...", so the seed would silently never be
# read. Resolve through athlete_crosswalk_swimming.parquet first.
n_seeded <- 0L
if (SEEDON) {
  # Partitioned by event_id exactly like swimming_corpus_store -- event_id is
  # a directory name, not a column inside the parquet, so it must be read off
  # the partition path rather than requested via col_select (which silently
  # fails to match and, via the tryCatch below, would make the WHOLE seed
  # step a silent no-op if this were gotten wrong. It was, once, during
  # development, and surfaced exactly this way: a 0-column empty table).
  cev <- setdiff(sub("^event_id=", "", list.dirs(file.path(OUT, "swimming_careers_store"),
                 recursive = FALSE, full.names = FALSE)), "__unmatched__")
  cl <- list(); cev_fail <- 0L
  for (EV in cev) {
    f <- file.path(OUT, sprintf("swimming_careers_store/event_id=%s/part-0.parquet", EV))
    x <- tryCatch(setDT(read_parquet(f, col_select = c("athlete_id", "date", "perf"))),
                  error = function(e) e)
    if (inherits(x, "error")) {
      cev_fail <- cev_fail + 1L
      warning(sprintf("failed to read swimming_careers_store partition event_id=%s: %s",
                      EV, conditionMessage(x)))
      next
    }
    x[, event_id := EV]
    cl[[EV]] <- x
  }
  if (cev_fail > 0L)
    cat(sprintf("[%s] WARNING: %d of %d swimming_careers_store partitions failed to read -- seeding proceeds on the rest, coverage check below still applies\n",
                TAG, cev_fail, length(cev)))
  ca <- rbindlist(cl, fill = TRUE); rm(cl)
  ca <- ca[!is.na(perf) & is.finite(perf) & !is.na(date) & !is.na(event_id)]
  ca[, athlete_id := as.character(athlete_id)]
  n_careers <- nrow(ca)

  cw <- setDT(read_parquet(file.path(OUT, "athlete_crosswalk_swimming.parquet")))
  cw <- cw[sport == "Swimming" & source == "worldaquatics",
           .(athlete_id = as.character(athlete_id), person_id)]
  stopifnot("crosswalk has duplicate worldaquatics athlete_id" = !anyDuplicated(cw$athlete_id))
  ca <- merge(ca, cw, by = "athlete_id")
  cov <- if (n_careers > 0) nrow(ca) / n_careers else 0
  cat(sprintf("[%s] seed identity crosswalk: %.1f%% of %s careers-store rows resolved to a person_id\n",
              TAG, 100 * cov, format(n_careers, big.mark = ",")))
  # A collapsed join here is exactly the failure mode that silently
  # refragments identity -- fail loudly rather than seed on a near-empty join.
  stopifnot("fewer than 90% of careers-store rows resolved through the crosswalk - identity join is broken" =
              cov > 0.90)
  ca[, athlete_id := NULL]; setnames(ca, "person_id", "athlete_id")
  ca <- ca[event_id %chin% names(MUv)]

  fd <- d[, .(first_date = min(date)), by = .(athlete_id, event_id)]
  sd0 <- ca[fd, on = .(athlete_id, event_id), allow.cartesian = TRUE, nomatch = NULL]
  sd0 <- sd0[date < first_date]   # strictly earlier than the first scored race
  sd0[, w := 2^(-as.numeric(first_date - date) / SEEDHL)]
  sg <- sd0[, .(r0 = sum(w * perf) / sum(w), ne0 = min(sum(w), SEEDNE),
                best0 = max(perf), last0 = max(date)), by = .(athlete_id, event_id)]
  sg <- sg[is.finite(r0) & is.finite(ne0)]
  sg[, dev := r0 - MUv[event_id]]
  cat(sprintf("[%s] seed: %s athlete-events | median dev from event mean %+.4f (|dev|>1 in %.2f%%)\n",
      TAG, format(nrow(sg), big.mark = ","), stats::median(sg$dev), 100 * mean(abs(sg$dev) > 1)))
  # ANCHOR: a seed is a mark in the same event, so it must land near that
  # event's own mean, or the perf convention differs and the seeds are junk.
  # nrow(sg) == 0 is NOT silently treated as "check passed" -- SEEDON=TRUE and
  # a >90%-resolved crosswalk join already happened above, so an empty sg here
  # is itself suspicious (every seed candidate failed the date/event-match
  # filters) rather than an expected "nothing to seed" state. Warn loudly
  # rather than let a genuinely broken join hide behind a vacuous stopifnot.
  if (nrow(sg) == 0L) {
    warning("SEQ_SEED: zero seed rows survived after a >90%-resolved crosswalk join -- ",
            "the anchor check below has nothing to verify against. Investigate before ",
            "trusting SEQ_SEED on this run.")
  } else {
    stopifnot("seeds do not land near the event mean - identity or scale mapping is wrong" =
                abs(stats::median(sg$dev)) < 0.5)
  }
  kz <- key(sg$athlete_id, sg$event_id)
  for (i in seq_len(nrow(sg))) {
    K <- kz[i]
    R[[K]] <- sg$r0[i]; NE[[K]] <- sg$ne0[i]
    LD[[K]] <- as.numeric(sg$last0[i]); BC[[K]] <- sg$best0[i]
    if (BEST_K > 1) { BKV[[K]] <- sg$best0[i]; BKD[[K]] <- as.numeric(sg$last0[i]) }
  }
  n_seeded <- nrow(sg)
  rm(ca, sd0, sg); invisible(gc())
}

.pairs <- function(n, place) {
  if (n < 2L) return(list(i = integer(0), j = integer(0)))
  ii <- rep.int(seq_len(n - 1L), (n - 1L):1L)
  jj <- sequence((n - 1L):1L, 2:n)
  keep <- place[ii] != place[jj]
  list(i = ii[keep], j = jj[keep])
}

# =============================================================================
# block boundaries, forced contiguous -- same technique as form_ratings.R,
# just on block_key instead of race_key
# =============================================================================
d <- unique(d, by = c("block_key", "athlete_id"))
d[, .blk0 := rleid(block_key)]
d[, .first := .blk0[1L], by = block_key]
setorder(d, .first)
blk <- rleid(d$block_key)
starts <- which(!duplicated(blk))
ends   <- c(starts[-1L] - 1L, length(blk))
if (uniqueN(d$block_key) != length(starts))
  stop(sprintf("block_key still not contiguous after the stabilising sort: %s keys in %s blocks",
               format(uniqueN(d$block_key), big.mark = ","), format(length(starts), big.mark = ",")))

Vath <- d$athlete_id; Vperf <- d$perf; Vplace <- d$place; Vrc <- d$rc
Vev <- d$event_id; Vdate <- d$date; Vfam <- d$family
Vtier <- d$tier; Vbk <- d$block_key; Vrk <- d$race_key
Vdaten <- as.numeric(d$date); Vyr <- year(d$date)

# --- metric weight per row ---------------------------------------------------
d[, w_tier := fifelse(!is.na(tier) & tier == "T1_elite", W_T1_ELITE, W_DEFAULT)]
d[, w_rnd  := fifelse(rc == "final", 1, W_RND)]
d[, wt := w_tier * w_rnd]
wtab <- d[, .(races = uniqueN(block_key), rows = .N, weight = wt[1]),
          by = .(tier = fifelse(is.na(tier), "(untiered)", tier), rc)][order(-weight, -races)]
cat(sprintf("[%s] METRIC WEIGHTS -- every combination present, %d rows:\n", TAG, nrow(wtab)))
print(wtab)
stopifnot("every row must carry a finite, positive weight" = all(is.finite(d$wt) & d$wt > 0),
          "rc must be one of final/semi/heat" = all(d$rc %chin% c("final","semi","heat")))
Vwt <- d$wt

.a0 <- c(conc=0, pairs=0, fav=0, nr=0,
         conc_w=0, w_sum=0, w_sq=0)
acc <- .a0
# TUNE/CONFIRM split by year(date), same convention as form_ratings.R: years
# before 2025 are a baseline/warm-up bucket (not reported separately), 2025 is
# "tune", 2026+ is "confirm" (sealed). Additive to `acc` -- the overall-run
# accumulator above is untouched and still drives the existing printout.
acc_yr <- list(base = .a0, tune = .a0, confirm = .a0)
NR <- if (HIST) nrow(d) else 0L
H <- list(block_key = character(NR), date = numeric(NR), event_id = character(NR),
          athlete_id = character(NR), r_pre = numeric(NR), r_use = numeric(NR),
          n_eff = numeric(NR), v_pre = numeric(NR), perf = numeric(NR),
          place = integer(NR), rc = character(NR), seen = logical(NR),
          shock = rep(NA_real_, NR), surprise = rep(NA_real_, NR), k = rep(NA_real_, NR))
hi <- 0L
n_conflict_skipped <- 0L
t0 <- Sys.time()

for (r_ in seq_along(starts)) {
  i1 <- starts[r_]; i2 <- ends[r_]
  if (i2 - i1 + 1L < 3L) next
  ii <- i1:i2
  z <- list(athlete_id = Vath[ii], perf = Vperf[ii], place = Vplace[ii], rc = Vrc[ii],
            event_id = Vev[i1], family = Vfam[i1], tier = Vtier[i1],
            block_key = Vbk[i1], wt = Vwt[i1])
  dt0n <- min(Vdaten[ii]); yr <- Vyr[i1]
  a <- z$athlete_id; ev <- z$event_id; kk <- key(a, ev)
  mu <- MUv[[ev]]
  mu_debut <- mu
  if (DEBUT_PRIOR == "replacement") {
    .rl <- RLE[[paste(ev, yr, sep = "|")]]
    if (!is.null(.rl) && is.finite(.rl)) mu_debut <- .rl
  }
  r_pre <- numeric(length(a)); n_eff <- numeric(length(a)); seen <- logical(length(a))
  for (m in seq_along(a)) {
    v <- R[[kk[m]]]
    if (is.null(v)) { r_pre[m] <- mu_debut; n_eff[m] <- 0; next }
    seen[m] <- TRUE
    gap <- dt0n - LD[[kk[m]]]
    ne <- NE[[kk[m]]]
    if (STALE) ne <- ne * 2^(-gap / HL)
    r_pre[m] <- v; n_eff[m] <- ne
  }
  r_use <- r_pre

  # --- cross-event blend (family gate only; inert unless SEQ_XBLEND>0) ------
  xb_e <- XBLENDv[[ev]]; if (is.null(xb_e) || !is.finite(xb_e)) xb_e <- XBLEND
  if (xb_e > 0) for (m in seq_along(a)) {
    if (!seen[m] || n_eff[m] >= XB_MAXN) next
    sib <- BYA[[a[m]]]; if (is.null(sib)) next
    sib <- sib[sib != ev]; if (!length(sib)) next
    if (!(z$family %chin% XB_FAM)) next
    fam <- reg$family[match(sib, reg$event_id)]
    sib <- sib[!is.na(fam) & fam == z$family]
    if (!length(sib)) next
    ne_s <- vapply(sib, function(sv) { q <- NE[[key(a[m], sv)]]; if (is.null(q)) 0 else q }, numeric(1))
    okm <- ne_s >= XB_MINS; sib <- sib[okm]; ne_s <- ne_s[okm]
    if (!length(sib)) next
    ord <- order(-ne_s)
    take <- ord[seq_len(min(XB_NSIB, length(ord)))]
    tw <- 0; tv <- 0
    for (t in take) {
      rs <- R[[key(a[m], sib[t])]]; ms <- MUv[[sib[t]]]
      if (is.null(rs) || is.null(ms) || !is.finite(rs) || !is.finite(ms)) next
      tw <- tw + ne_s[t]; tv <- tv + ne_s[t] * (rs - ms + mu)
    }
    if (tw <= 0) next
    w <- xb_e / (n_eff[m] + xb_e)
    r_use[m] <- (1 - w) * r_use[m] + w * (tv / tw)
  }

  # --- ceiling / best-mark blend --------------------------------------------
  ceil_e <- CEILv[[ev]]; if (is.null(ceil_e) || !is.finite(ceil_e)) ceil_e <- CEIL
  if (ceil_e > 0) for (m in seq_along(a)) {
    if (!seen[m]) next
    if (BEST_K > 1) {
      b <- .best_k(BEST_K, BKV[[kk[m]]], BKD[[kk[m]]], dt0n)
    } else {
      bsy <- BSY[[kk[m]]]
      b <- if (!is.null(bsy) && bsy == yr) BS[[kk[m]]] else BC[[kk[m]]]
    }
    if (CEIL_MODE == "quantile") {
      vv <- V[[kk[m]]]
      if (is.null(vv) || !is.finite(vv)) vv <- stats::var(z$perf)
      if (is.finite(vv) && vv > 0) b <- r_use[m] + CEILC * sqrt(vv) else b <- NULL
    }
    if (!is.null(b)) r_use[m] <- (1 - ceil_e) * r_use[m] + ceil_e * b
  }

  vp0 <- VPv[[ev]]; if (is.null(vp0) || !is.finite(vp0)) vp0 <- stats::var(z$perf)
  v_pre <- numeric(length(a))
  for (m in seq_along(a)) { vv <- V[[kk[m]]]; v_pre[m] <- if (is.null(vv)) vp0 else vv }

  if (HIST) {
    ix <- hi + seq_along(a); hi <- hi + length(a)
    H$block_key[ix] <- z$block_key; H$date[ix] <- dt0n
    H$event_id[ix] <- ev; H$athlete_id[ix] <- a
    H$r_pre[ix] <- r_pre; H$n_eff[ix] <- n_eff; H$r_use[ix] <- r_use
    H$v_pre[ix] <- v_pre; H$perf[ix] <- z$perf; H$place[ix] <- z$place
    H$rc[ix] <- z$rc; H$seen[ix] <- seen
    hix <- ix
  }

  # --- CONFLICT GUARD: skip SCORING (never rating) an internally
  # contradictory block. See the design note above. ---------------------------
  score_this <- TRUE
  .ok <- is.finite(z$place) & is.finite(z$perf)
  if (sum(.ok) > 1L) {
    .o  <- order(z$place[.ok], z$perf[.ok])
    .pl <- z$place[.ok][.o]; .pf <- z$perf[.ok][.o]; .n <- length(.pl)
    if (any(.pl[-1L] == .pl[-.n] & .pf[-1L] != .pf[-.n])) {
      score_this <- FALSE; n_conflict_skipped <- n_conflict_skipped + 1L
    }
  }

  if (score_this) {
    sel <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg <- .pairs(length(sel), z$place[sel])
    g <- list(i = sel[gg$i], j = sel[gg$j])
    if (length(g$i)) {
      di <- r_use[g$i] - r_use[g$j]
      pl <- z$place[g$i] < z$place[g$j]
      cw <- as.numeric((di > 0) == pl); cw[di == 0] <- 0.5
      acc["conc"] <- acc["conc"] + sum(cw)
      acc["pairs"] <- acc["pairs"] + length(g$i)
      wt <- z$wt
      acc["conc_w"] <- acc["conc_w"] + wt * sum(cw)
      acc["w_sum"]  <- acc["w_sum"]  + wt * length(cw)
      acc["w_sq"]   <- acc["w_sq"]   + wt * wt * length(cw)
      rs <- r_use[sel]; ps <- z$place[sel]; tm <- which(rs == max(rs))
      acc["fav"] <- acc["fav"] + mean(ps[tm] == min(ps))
      acc["nr"] <- acc["nr"] + 1

      byr <- if (yr < 2025) "base" else if (yr == 2025) "tune" else "confirm"
      ay <- acc_yr[[byr]]
      ay["conc"] <- ay["conc"] + sum(cw)
      ay["pairs"] <- ay["pairs"] + length(g$i)
      ay["conc_w"] <- ay["conc_w"] + wt * sum(cw)
      ay["w_sum"]  <- ay["w_sum"]  + wt * length(cw)
      ay["w_sq"]   <- ay["w_sq"]   + wt * wt * length(cw)
      ay["fav"] <- ay["fav"] + mean(ps[tm] == min(ps))
      ay["nr"] <- ay["nr"] + 1
      acc_yr[[byr]] <- ay
    }
  }

  # --- shock / surprise / learning rate -------------------------------------
  est <- n_eff >= 2
  resid_est <- z$perf[est] - r_pre[est]
  m_est <- sum(est)
  S <- (if (m_est >= SHOCK_MINN)
          (if (SHOCK == "median") stats::median(resid_est)
           else if (SHOCK == "trim") mean(resid_est, trim = SHOCK_TRIM)
           else mean(resid_est))
        else 0) * (if (SHOCK_W == "kappa") m_est / (m_est + SHOCK_K) else m_est / length(a))
  corr <- rep(S, length(a))
  surprise <- (z$perf - r_pre) - corr
  att_e <- ATTENv[[ev]]; if (is.null(att_e) || !is.finite(att_e)) att_e <- ATTEN
  if (att_e != 1) {
    fin <- is.finite(r_pre)
    if (sum(fin) >= 3) surprise <- surprise + (1 - att_e) * (r_pre - mean(r_pre[fin]))
  }
  k0e <- K0v[[ev]]; if (is.null(k0e) || !is.finite(k0e)) k0e <- K0
  kfl_e <- KFLOORv[[ev]]; if (is.null(kfl_e) || !is.finite(kfl_e)) kfl_e <- KFLOOR
  kap_e <- KAPPAv[[ev]]; if (is.null(kap_e) || !is.finite(kap_e)) kap_e <- KAPPA
  kv <- pmax(k0e * kap_e / (n_eff + kap_e), kfl_e)
  kt1_e <- KT1v[[ev]]; if (is.null(kt1_e) || !is.finite(kt1_e)) kt1_e <- KT1
  if (kt1_e != 1 && !is.na(z$tier) && z$tier == "T1_elite") kv <- pmin(kv * kt1_e, 0.9)
  cen_e <- CENSv[[ev]]; if (is.null(cen_e) || !is.finite(cen_e)) cen_e <- CENS
  if (cen_e < 1 || CENSWIN < 1) {
    fac <- rep(1, length(a))
    if (cen_e < 1) fac[z$rc != "final" & surprise < 0] <- cen_e
    if (CENSWIN < 1) {
      neg_win <- is.finite(z$place) & z$place >= 1 & z$place <= CENSWIN_P & surprise < 0
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
  if (HIST) { H$shock[hix] <- corr; H$surprise[hix] <- surprise; H$k[hix] <- kv }

  for (m in seq_along(a)) {
    if (!seen[m]) {
      R[[kk[m]]] <- z$perf[m] - S
      BYA[[a[m]]] <- unique(c(BYA[[a[m]]], ev))
    } else {
      R[[kk[m]]] <- r_pre[m] + kv[m] * surprise[m]
    }
    if (seen[m])
      V[[kk[m]]] <- max(v_pre[m] + kv[m] * (surprise[m]^2 - v_pre[m]), 0.04 * vp0)
    NE[[kk[m]]] <- n_eff[m] + 1
    LD[[kk[m]]] <- dt0n
    bc <- BC[[kk[m]]]
    if (is.null(bc) || z$perf[m] > bc) BC[[kk[m]]] <- z$perf[m]
    bsy <- BSY[[kk[m]]]
    if (!is.null(bsy) && bsy == yr) { if (z$perf[m] > BS[[kk[m]]]) BS[[kk[m]]] <- z$perf[m] }
    else { BSY[[kk[m]]] <- yr; BS[[kk[m]]] <- z$perf[m] }
    if (BEST_K > 1) {
      vv <- c(BKV[[kk[m]]], z$perf[m]); dd2 <- c(BKD[[kk[m]]], dt0n)
      if (length(vv) > BEST_K) { o <- order(-vv)[seq_len(BEST_K)]; vv <- vv[o]; dd2 <- dd2[o] }
      BKV[[kk[m]]] <- vv; BKD[[kk[m]]] <- dd2
    }
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

# TUNE (2025) / CONFIRM (2026+) windows -- see acc_yr note above. 2025 is for
# picking a knob value; 2026 is the sealed window that should govern the
# decision (do not pick a knob by looking at 2026 and re-running).
.wr <- function(ay) list(wconc = 100 * ay["conc_w"] / ay["w_sum"],
                          ess = as.numeric(ay["w_sum"])^2 / ay["w_sq"],
                          races = as.integer(ay["nr"]))
tune_w <- .wr(acc_yr[["tune"]]); confirm_w <- .wr(acc_yr[["confirm"]])

res <- data.table(tag = TAG,
  conc = 100 * acc["conc"] / acc["pairs"], fav = 100 * acc["fav"] / acc["nr"],
  wconc = 100 * acc["conc_w"] / acc["w_sum"],
  ess = acc["w_sum"]^2 / acc["w_sq"], races = acc["nr"], mins = round(el, 1),
  wconc_2025 = tune_w$wconc, ess_2025 = tune_w$ess, races_2025 = tune_w$races,
  wconc_2026 = confirm_w$wconc, ess_2026 = confirm_w$ess, races_2026 = confirm_w$races,
  conflict_skipped = n_conflict_skipped,
  seeded = n_seeded, huber = HUBER, seedhl = SEEDHL, seedne = SEEDNE,
  k0 = K0, kappa = KAPPA, kfloor = KFLOOR, kpow = KPOW,
  hl = HL, xblend = XBLEND, xb_fam = paste(XB_FAM, collapse = "+"),
  ceil = CEIL, ceil_mode = CEIL_MODE, ceil_c = CEILC,
  debut_prior = DEBUT_PRIOR, maxplace = MAXPLACE,
  w_t1_elite = W_T1_ELITE, w_default = W_DEFAULT, w_rnd = W_RND,
  cens = CENS, censwin = CENSWIN, stale = STALE, kt1 = KT1, atten = ATTEN,
  from = as.character(FROM))
cat(sprintf("[%s] concordance %.3f%% | favourite %.1f%% | %d scored races (%d skipped by the conflict guard, %.1f%% of %s exact-Final groups) | %.1f min\n",
    TAG, res$conc, res$fav, res$races, n_conflict_skipped,
    if (n_merged > 0) 100 * n_conflict_skipped / n_merged else NA_real_,
    format(n_merged, big.mark = ","), el))
cat(sprintf("[%s] WEIGHTED (T1_elite %g / default %g, non-final x%g): %.3f%% (ess %s)\n",
    TAG, W_T1_ELITE, W_DEFAULT, W_RND, res$wconc, format(round(res$ess), big.mark = ",")))
cat(sprintf("[%s] TUNE    (2025) weighted %.3f%% (ess %s, %s races)\n",
    TAG, tune_w$wconc, format(round(tune_w$ess), big.mark = ","), format(tune_w$races, big.mark = ",")))
cat(sprintf("[%s] CONFIRM (2026) weighted %.3f%% (ess %s, %s races) -- sealed window, governs the decision\n",
    TAG, confirm_w$wconc, format(round(confirm_w$ess), big.mark = ","), format(confirm_w$races, big.mark = ",")))

f <- file.path(SC, "seqv2_results_swimming.csv")
fwrite(res, f, append = file.exists(f))

ids <- ls(R)
st <- data.table(k = ids, R = vapply(ids, function(i) R[[i]], numeric(1)),
                 n_eff = vapply(ids, function(i) NE[[i]], numeric(1)),
                 v = vapply(ids, function(i) { vv <- V[[i]]; if (is.null(vv)) NA_real_ else vv }, numeric(1)),
                 last = as.Date(vapply(ids, function(i) LD[[i]], numeric(1)), origin = "1970-01-01"),
                 best = vapply(ids, function(i) {
                   b <- if (BEST_K > 1) .best_k(BEST_K, BKV[[i]], BKD[[i]], LD[[i]]) else BC[[i]]
                   if (is.null(b)) NA_real_ else b }, numeric(1)))
# sub(), not tstrsplit(): event_id never contains "|" but a stray athlete_id
# could in principle, so split at the LAST "|" rather than every one.
st[, event_id := sub(".*\\|", "", k)]
st[, athlete_id := sub("\\|[^|]*$", "", k)]
st[, R_ceil := fifelse(is.na(best), R, (1 - CEIL) * R + CEIL * best)]

if (HIST) {
  hd <- as.data.table(lapply(H, function(v) v[seq_len(hi)]))
  hd[, date := as.Date(date, origin = "1970-01-01")]
  write_parquet(hd, file.path(SC, sprintf("seqv3_history_%s.parquet", TAG)))
  cat(sprintf("[%s] history: %s athlete-races\n", TAG, format(nrow(hd), big.mark = ",")))
}
# SECURITY: Sys.getenv(character(0)) does NOT return an empty result -- it
# returns THE ENTIRE ENVIRONMENT (confirmed empirically: 117 vars, including
# API tokens and secrets, on this machine). form_ratings.R's identical-looking
# `Sys.getenv(grep("^SEQ_", names(Sys.getenv()), value = TRUE))` inherits this
# exact trap: on a bare run with no SEQ_* variables actually set in the shell
# (every knob above has an in-script default, so this is the common case, not
# an edge case), grep() returns character(0) and the "env" field silently
# becomes a dump of secrets into a git-adjacent data file. Guarded here
# explicitly; flagged in the task report as worth fixing upstream too.
.seq_env_names <- grep("^SEQ_", names(Sys.getenv()), value = TRUE)
meta <- list(tag = TAG,
             written = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             engine = basename(ENGINE_SRC),
             engine_sha = ENGINE_SHA,
             rows = nrow(st),
             env = if (length(.seq_env_names)) as.list(Sys.getenv(.seq_env_names)) else list())
writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
           file.path(SC, sprintf("seqv3_meta_%s.json", TAG)))
cat(sprintf("[%s] engine sha %s\n", TAG, substr(ENGINE_SHA, 1, 12)))

write_parquet(st[, !"k"], file.path(SC, sprintf("seqv2_state_%s.parquet", TAG)))
cat(sprintf("[%s] wrote seqv2_state_%s.parquet: %s athlete-events\n", TAG, TAG, format(nrow(st), big.mark = ",")))
