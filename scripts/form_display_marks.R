# Turn form ratings into DISPLAYABLE MARKS.
#
# Two things stand between a rating and a mark on a page:
#
# 1. ORIENTATION. perf = orientation * log(mark) exactly (verified: residual 0
#    to machine precision on every event tested). orientation is -1 for
#    time-based families and +1 for jump/throw/combined, and it lives in the
#    event registry. So mark = exp(orientation * perf). Getting this wrong is
#    how field-event marks came out inverted.
#
# 2. LEVEL. A rating converges to the athlete's typical race, which includes
#    jogged heats; a ratings page implies a final. The offset is fitted PER
#    EVENT because it varies and changes sign (ShotPut W +0.92%, 100m M
#    -0.17%), on the 2020-2024 lead-in ONLY so the scoring windows stay clean.
#
# The offset is a LEVEL correction and cannot reorder anyone within an event --
# every athlete in an event gets the same shift. That is deliberate: the
# ordering question was closed separately (two depth corrections REJECTED, see
# refuted-hypotheses.md), so this file changes what is shown, never the rank.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- Sys.getenv("FORM_OUT", "C:/dev/citiusverse/citiusdata/data")
TAG <- Sys.getenv("FORM_TAG", "final")
FIT_BEFORE <- as.Date("2025-01-01")
# An env var set to "" is NOT unset: Sys.getenv returns "" and as.integer("")
# is NA, which would silently disable the thin-event fallback. Treat empty as
# unset (learned the hard way on SEQ_MAXPLACE, 2026-08-15).
# Shared with every other script now, rather than a private copy here. The
# lesson below was learned in this file and then not applied to the nine other
# knobs in it, including both recency windows - an empty FORM_ACT_EVENT_D would
# have made the window NA, and `last >= ASOF - NA` is NA, which matches no rows
# and empties the page.
source(here::here("citiusdata", "scripts", "_env.R"))
MIN_N <- .env_int("FORM_OFFSET_MIN_N", 200L)
# Minimum evidence before a "good day" mark is shown at all. Also the population
# the spread is FITTED on, so the column is calibrated for the readers who see
# it rather than for an average over athletes it is hidden from.
PEAK_MIN_N <- .env_int("FORM_PEAK_MIN_N", 8L)

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
st <- setDT(read_parquet(file.path(OUT, sprintf("seqv2_state_%s.parquet", TAG))))
reg <- as.data.table(citius::citius_events())[, .(event_id, family, orientation, unit)]

# --- 1. per-event finals offset, fitted on the lead-in only ------------------
fit <- h[seen == TRUE & rc == "final" & date < FIT_BEFORE & is.finite(perf) & is.finite(r_pre)]
fit[, resid := perf - r_pre]
# MEDIAN, not mean. The residual is left-skewed (-0.47 measured): a fall or a
# blow-up has no mirror image, because nobody runs 18% FAST. On a left-skewed
# distribution the mean sits BELOW the median, so a mean offset put the
# "typical" line slower than a typical race and it was beaten 53.9% of the time
# instead of 50%. Median centring takes that to 51.8% out of sample.
off <- fit[, .(offset = stats::median(resid), n_fit = .N), by = event_id]
pooled <- fit[, stats::median(resid)]
cat(sprintf("offset fitted on %s finals rows before %s; pooled %+.4f%%\n",
            format(nrow(fit), big.mark = ","), FIT_BEFORE, 100 * pooled))
# A thin event cannot support its own offset; fall back to pooled rather than
# to a number estimated from a handful of races.
off[n_fit < MIN_N, offset := pooled]
# NOTE the offset is deliberately NOT refitted per evidence depth, even though
# deep records beat 'typical' only ~44.5% of the time against 50.26% overall.
# A depth-dependent offset would give two athletes in the same event different
# shifts and could therefore REORDER them, which this file guarantees it never
# does (see the header). That is the known evidence-depth bias; it is reported
# by the calibration block below and belongs upstream, not here.
cat(sprintf("%d of %d events use their own offset; %d fall back to pooled (n < %d)\n",
            off[n_fit >= MIN_N, .N], nrow(off), off[n_fit < MIN_N, .N], MIN_N))

# --- 2. apply --------------------------------------------------------------
st[, athlete_id := as.character(athlete_id)]
st <- merge(st, reg, by = "event_id", all.x = TRUE)
st <- merge(st, off[, .(event_id, offset, n_fit)], by = "event_id", all.x = TRUE)
st[is.na(offset), offset := pooled]
if (any(is.na(st$orientation)))
  stop(sprintf("%d rows have no orientation in the registry -- refusing to guess a mark",
               sum(is.na(st$orientation))))
# --- 2a. DEPTH CORRECTION on the displayed centre -----------------------------
# The pooled per-event offset is right on average and wrong for everybody. It is
# beaten 50.3% of the time overall - which looks perfect - while being beaten
# 53.6% by records with under one effective race and 43.9% by records with 15+.
# The offset carries the average depth of its fit population and fits nobody at
# the extremes.
#
# Corrected with one median per evidence BAND, shared across events, on top of
# the per-event offset. Per event AND band would be 86 x 7 cells and far too
# thin; this is the smallest change that can work, and it keeps the per-event
# level, which is real and varies in sign.
#
# Fitted before 2025, then checked on 2025 and 2026 SEPARATELY. Worst band
# deviation from 50%: 2025 6.1 pp -> 2.2 pp, 2026 5.3 pp -> 2.5 pp. It has to
# improve the window that is neither fitted nor sealed, or it was fitted to the
# sealed data by accident.
#
# APPLIED TO THE DISPLAYED CENTRE ONLY, NEVER TO rank_mark. Two athletes on the
# same rating with different evidence get different depth corrections, so putting
# this in the sort key WOULD reorder them - and this file guarantees in its header
# that it changes what is shown and never the rank. What an athlete typically runs
# and how they are ranked are different questions that never had to share an
# offset; separating them satisfies the guarantee instead of breaking it.
DEPTH_ADJ <- Sys.getenv("FORM_DEPTH_ADJ", "1") != "0"
.band_of <- function(n) cut(n, c(-Inf, 1, 2, 3, 5, 8, 15, Inf),
                            labels = c("<1", "1-2", "2-3", "3-5", "5-8", "8-15", "15+"))
if (DEPTH_ADJ) {
  fitb <- copy(fit)
  fitb[, band := .band_of(n_eff)]
  dadj <- fitb[, .(band_adj = stats::median(resid - pooled), n_band = .N), by = band]
  dadj[n_band < 500, band_adj := 0]      # a band too thin to estimate gets nothing
  st[, band := .band_of(n_eff)]
  st <- merge(st, dadj[, .(band, band_adj)], by = "band", all.x = TRUE)
  st[is.na(band_adj), band_adj := 0]
  cat(sprintf("depth correction: %d bands, range %+.4f to %+.4f log-perf\n",
              dadj[band_adj != 0, .N], min(dadj$band_adj), max(dadj$band_adj)))
  stopifnot("the depth correction is inert - every band came out zero" =
              any(dadj$band_adj != 0))
} else {
  st[, band_adj := 0]
}
# the centre every DISPLAYED mark is built from
# Rebuild the SAME centre on any validation frame. Checking a new spread against
# an old centre measures neither, which is exactly what happened on the first
# attempt at this - the good-day column read 7.80% purely because the check line
# had no depth correction while the spread had been refitted with one.
# Uses r_pre (the rating carried into that race) rather than R (the end state),
# because a validation row is a race and not an athlete.
.add_centre <- function(x) {
  if (DEPTH_ADJ) {
    x[, band := .band_of(n_eff)]
    x <- merge(x, dadj[, .(band, band_adj)], by = "band", all.x = TRUE)
    x[is.na(band_adj), band_adj := 0]
  } else x[, band_adj := 0]
  x[, centre := r_pre + offset + band_adj][]
}
st[, centre := R + offset + band_adj]
st[, pred_mark := exp(orientation * centre)]
st[, raw_mark  := exp(orientation * R)]
# THE MARK THE TABLE IS ACTUALLY SORTED BY. Ranking runs on R_ceil (see the
# setorder below) while `pred_mark` comes from R, so without this the visible
# column is not the sort key and the order reads as arbitrary: in the men's 200m
# Bednarek's 19.67 sat below Lyles' 19.80. Pete could not tell what the table was
# ranked by, which is a fair complaint about a ranking table.
# NOT COMPUTED HERE ANY MORE. This ran from R_ceil, before the cross-event and
# combined-event blends modify R_rank further down, so it was stale the moment
# those shipped: the published 1500m read Wanyonyi 3:32.2 first and
# Ingebrigtsen 3:31.3 third, and rank_mark could not explain it either. It is
# now derived from R_rank itself, immediately before the sort - see below.

# --- 2b. PEAK: what the athlete runs on a good day --------------------------
# A quantile of the athlete's own distribution, using an EMPIRICAL quantile of
# z = (perf - r_pre)/sqrt(v). A normal quantile would be badly wrong here for
# two measured reasons: sd(z) is 1.52, not 1 (v learns from the shock-adjusted
# surprise while the raw residual still carries the race shock), and the
# good-side tail is fat (q99 3.50 vs a normal 2.33).
#
# Only the SPREAD above typical is taken from the quantile -- (q90 - q50) --
# because the level is already handled by the per-event offset above. Taking
# the whole quantile would count the level twice.
# FITTED ON THE DISPLAY POPULATION. The column is only shown at n_eff >=
# PEAK_MIN_N, and deep records vary less and beat their typical mark less often
# than thin ones do. A spread fitted across all depths and applied to deep
# records alone made the column a 1-in-17 event when it claims 1-in-10 - fitting
# one population and displaying to another.
zf <- h[seen == TRUE & rc == "final" & date < FIT_BEFORE & n_eff >= PEAK_MIN_N &
        is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0]
# z is measured AFTER the same centring the mark uses. Mixing a mean-centred
# offset with a spread taken around zero double-counted the skew.
zf <- merge(zf, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
zf[is.na(offset), offset := pooled]
# z is measured around the SAME centre the peak mark is built on, depth
# correction included. Fitting the spread around one centre and applying it
# around another would reintroduce exactly the bias just removed.
if (DEPTH_ADJ) {
  zf[, band := .band_of(n_eff)]
  zf <- merge(zf, dadj[, .(band, band_adj)], by = "band", all.x = TRUE)
  zf[is.na(band_adj), band_adj := 0]
} else zf[, band_adj := 0]
zf[, z := (perf - r_pre - offset - band_adj) / sqrt(v_pre)]
q50 <- stats::quantile(zf$z, 0.50); q90 <- stats::quantile(zf$z, 0.90)
# ZSPREAD IS q90, NOT q90 - q50. The column promises P(beat it) = 10%, the rule
# applied is `perf > r_pre + offset + ZSPREAD * sqrt(v)`, and z here is ALREADY
# (perf - r_pre - offset)/sqrt(v_pre) - so the value delivering that promise is
# the 90th percentile of z, full stop.
#
# Subtracting q50 was double-centring. The comment above argued that only the
# spread above typical should be taken "because the level is already handled by
# the per-event offset" - which is exactly why q50 must NOT be subtracted again:
# the offset is already inside z. It reads as harmless because q50 looks like
# zero, and it is not: on the deep population it is -0.131, because deep records
# run slightly below their rating (the known evidence-depth bias). Subtracting a
# negative made ZSPREAD 1.544 instead of 1.413, pushed the good-day mark further
# out, and left it beaten 8.05% of the time against the 10% it advertises - a
# 1-in-12.4 event labelled 1 in 10.
#
# This needed no tuning on the sealed window, which is why the earlier decision
# not to "refit the spread to force 10%" was right and still left a bug: it is
# arithmetic, not calibration. 2025 sits unused between the fit (< 2025) and the
# sealed check (2026), and is reported below as an independent validation.
ZSPREAD <- unname(q90)
cat(sprintf("peak spread: q90 of z = %.3f sd (normal would be %.3f); q50 %.3f\n",
            ZSPREAD, stats::qnorm(0.9), q50))
st[, peak_mark := exp(orientation * (centre + ZSPREAD * sqrt(v)))]
st[!is.finite(v) | v <= 0, peak_mark := NA_real_]
# SUPPRESS the good-day mark on a thin record rather than capping it.
#
# The column claims a 90th percentile of an athlete's own distribution, and on
# 3-5 races we do not know that distribution: 42.6% of that band get a mark more
# than 2% past their own best and 8.4% more than 5% past it, against 27.7%/3.9%
# at 5-8 races and 7.7%/0.0% at 20+. The extremes are indefensible - a 42.15m
# discus thrower shown 58.08m.
#
# The cause is a CONFLATION, not a scale error, so clipping would hide it rather
# than fix it: for a thin record, uncertainty about the athlete's LEVEL is large,
# and that is not the same quantity as their race-to-race UPSIDE. `v` currently
# carries both, so a good-day mark reads "level + our ignorance of the level +
# upside". Until those are separated (see the over-confident-sigma item in
# NEXT-STEPS), declining to make the claim is the honest option; a capped number
# would still be a claim, just a quieter wrong one.
n_thin <- st[is.finite(peak_mark) & n_eff < PEAK_MIN_N, .N]
st[n_eff < PEAK_MIN_N, peak_mark := NA_real_]
cat(sprintf("good day suppressed on %s rows with n_eff < %d (%.1f%% of those with a peak)\n",
            format(n_thin, big.mark = ","), PEAK_MIN_N,
            100 * n_thin / max(1L, n_thin + st[is.finite(peak_mark), .N])))

# Bound the peak by the best mark ever recorded for the event. A "good day"
# better than anything anyone has ever done is not a good day, it is an error,
# and one implausible number on a page costs more than the column earns.
#
# 37 of 37,141 rows (0.10%) needed this, concentrated in combined events and
# walks. The cause is upstream and known: the engine mis-initialises variance on
# an athlete's first race (see FORM-MODEL.md), so a debutant's v becomes roughly
# the squared distance of their debut from the population mean. The worst
# offenders all have n_eff under 4. Fix that and most of this cap goes unused.
bestp <- rbindlist(lapply(unique(st$event_id), function(EV) {
  f <- file.path(OUT, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(NULL)
  x <- setDT(read_parquet(f, col_select = "perf"))
  if (!nrow(x) || all(!is.finite(x$perf))) return(NULL)
  data.table(event_id = EV, best_perf = max(x$perf, na.rm = TRUE))
}))
st <- merge(st, bestp, by = "event_id", all.x = TRUE)
st[, peak_perf := orientation * log(peak_mark)]
st[, capped := is.finite(peak_perf) & is.finite(best_perf) & peak_perf > best_perf]
n_cap <- st[capped == TRUE, .N]
st[capped == TRUE, peak_mark := exp(orientation * best_perf)]
cat(sprintf("peak capped at the all-time event best on %s of %s rows (%.2f%%)\n",
            format(n_cap, big.mark = ","), format(nrow(st), big.mark = ","),
            100 * n_cap / nrow(st)))

# --- 3. names + activity ----------------------------------------------------
nm <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/blog/athlete-ratings.parquet"))
nm <- unique(nm[, .(athlete_id = as.character(athlete_id), athlete_name)])
st <- merge(st, nm, by = "athlete_id", all.x = TRUE)
# WHO IS SHOWN. The old rule was `n_eff >= 3 & last >= 2026-01-01`, applied per
# athlete-EVENT, and it hid people who were plainly active: Josh Kerr had no
# 1500m ranking despite 35 races and a 3:27.79 Olympic final, because his last
# 1500m was 2025-09-17 - while he raced a mile that July. The same rule emptied
# the 20km race walk, where walkers contest the distance twice a year.
#
# The profile harvest gave the first external referee for this: World Athletics
# publish their own ranking per event, built by a different method. Judged on
# precision@10 against it (check_active_filter.R, 18 variants):
#
#   old rule                                   62.5%   WA #1 shown 82.5%
#   athlete active + n_eff>=1 + event 11mo     65.2%   WA #1 shown 93.2%
#
# Three separate conditions, each doing a distinct job:
#   ATHLETE recency  - drives precision. Without it, stale athletes crowd the
#                      top ten and precision falls to 59.1%.
#   evidence bar     - drives coverage. Dropping it from 3 to 1 is what lifts
#                      "is the world number one even on the page" from 82.5% to
#                      93.2%, because elite 10,000m runners race it twice a year.
#   EVENT recency    - stops the low bar admitting ratings built on one race two
#                      years ago. Bracketed: 17mo 64.8, 14mo 65.2, 11mo 65.2,
#                      8mo 60.2 - a clean peak and a sharp cliff.
#
# Expressed in months back from the DATA date, not as fixed dates: hardcoding
# them would silently tighten the window every time the corpus is extended.
ASOF        <- max(st$last, na.rm = TRUE)
# WIDENED 210 -> 730 on 2026-08-19 (Pete). The 210-day athlete window was
# calibrated on track athletes and quietly deleted the people who race least
# often, which in athletics means the marathoners and the walkers.
#
# It cost us the Olympic champion. Sifan Hassan won the Paris 2024 marathon and
# had NO ranking in any event - not unnamed on the race page, absent from the
# rankings entirely - despite 129 scoreable marks, 40 rated races and n_eff 6.41
# in the 5000m. Her last race was 2025-11-02, which is 287 days before the data
# date. A marathoner runs two a year by design; the rule asked her to be a track
# athlete. Across Paris 2024, 152 of 991 medal-round performances (15.3%)
# belonged to athletes the rankings did not contain.
#
# The measurement that settles it - share of athlete-events passing the EVENT
# test that the 210-day ATHLETE test then killed, against how long that family
# actually goes between races:
#
#   family     killed by 210d    median gap    p90 gap
#   road            31.5%           196 d       567 d
#   walk            22.6%           175 d       455 d
#   jump             9.5%            23 d       265 d
#   sprint           8.1%            16 d       265 d
#
# Road athletes have a p90 racing gap of 567 days. A 210-day window asks them to
# race nearly three times more often than the sport does, so it removes a third
# of them at any moment - and a filtered-out athlete leaves no trace, which is
# why this ran for months without anyone seeing it.
#
# At 730 the ATHLETE test stops binding at all: the EVENT test (400 d) is
# strictly tighter, so the composite rule becomes "contested THIS event within
# 400 days", which is the condition that was doing the work anyway. That is the
# honest description of what this now is - not a wider athlete window, but the
# retirement of a rule that was fighting the calendar of half the sport.
# RAISED 730 -> 800 with the event window, and the two must move together.
# Setting the event window to 800 alone got medallist coverage to 98.4%, not
# 100%: Brian Pintado and Alvaro Martin both last raced on 2024-08-01, which is
# 745 days back, so the EVENT test admitted them and the 730-day ATHLETE test
# then silently excluded them again. The composite rule is the tighter of the
# two, so an athlete window below the event window is a hidden second gate that
# looks like nothing at all when you read the event window on its own.
# Keep ACT_ATHLETE >= ACT_EVENT unless there is a reason to want that gate.
ACT_ATHLETE <- .env_int("FORM_ACT_ATHLETE_D", "800")  # see above
# WIDENED 330 -> 365 on 2026-08-18. Pete asked whether 330 was too strict. It
# was, though not for the reason offered: staleness decays `n_eff`, never `R`,
# so a rating sits at full strength indefinitely and the window is the ONLY
# thing holding a stale athlete down. Widening is a real risk, not a free option.
#
# Swept against World Athletics on the current corpus, athlete-recency held at
# 210 days (precision@10 / WA #1 shown):
#   330  70.2 / 92.9     390  70.0 / 97.7     550  69.3 / 97.7
#   345  70.0 / 97.7     400  70.0 / 97.7     730  69.3 / 97.7
#   360  70.0 / 97.7     420  69.5 / 97.7     900  68.2 / 97.7
# Having the world number one on the page at all jumps 92.9 -> 97.7 at 345 days
# and precision holds flat to 400 before a cliff at 420. Missing the actual #1
# is a worse failure than one extra wrong name in a top ten, and 0.2 pp is about
# one slot. Two years costs 0.9 pp for nothing further.
#
# 400, not 365. Pete's reason, and it is the better one: an athlete who contests
# an event ANNUALLY will often exceed a year between runnings, because the
# calendar moves - a championship a few weeks later than last time, a meeting
# shifted in the schedule. A 365-day window drops exactly those athletes in the
# weeks before they next contest the event. 400 gives five weeks of slack for
# calendar drift, still sits on the 70.0 / 97.7 plateau, and stays clear of the
# cliff at 420.
# WIDENED 400 -> 800 on 2026-08-19 (Pete's call, after seeing the medallist
# audit). This is the change that takes Paris 2024 medallist coverage from
# 89.0% to 100%: fourteen medallists had no rated result in their own event
# inside 400 days, seven of them because their last run of it WAS the Olympic
# final, which sits 735-745 days back. 730 recovers seven of the fourteen; 760
# recovers all fourteen; 800 clears them with a margin.
#
# The audit is the thing to trust here rather than the concordance metric, and
# it took four attempts to define a medallist correctly - see
# check_paris_medallists.R, which found that `place <= 3` counts DNFs (0 and -1
# are sentinels), that top three in a heat is not a medal, and that decathlon
# component placings were awarding nine extra 100m medals.
#
# Every one of the fourteen was checked individually and none is a data fault.
# Sydney McLaughlin-Levrone did not stop racing, she moved to the flat 400m and
# won the world title in it. Cheptegei did not run Tokyo 2025 at all. Sifan
# Hassan has not contested a 10,000m since Paris. Fred Kerley entered ten races
# in 2025 and produced a mark in none of them, and in every one of those races
# he was the only athlete in a field of 7-9 without one. So no amount of
# harvesting would have recovered these; a window was the only lever.
#
# WHAT THIS COSTS, measured on referees that can see it rather than p@10 (440
# slots, one athlete moves it 0.2pp). A recency window is a pure FILTER: it
# never re-rates anybody, which check_window_common.R demonstrates by scoring
# every window on the population common to all of them and getting identical
# numbers to four decimals. So widening cannot degrade an existing athlete's
# rating - it only adds staler names beside the medallists. Overall Spearman is
# flat across the whole range (0.9250-0.9299 on ~4,500 matched athletes).
#
# THE ONE THING TO WATCH: 800 is measured from the DATA date, not a fixed date,
# so it does not silently tighten as the corpus grows. But the 735-745 day
# figure it was chosen against is the distance to Paris 2024, and that distance
# grows daily - this reaches the Games until roughly late 2026 and then stops.
# It is not a permanent answer to "keep championship medallists visible"; a
# medallist-persistence rule is. Re-check this against
# check_paris_medallists.R before Tokyo 2027, not after.
ACT_EVENT   <- .env_int("FORM_ACT_EVENT_D", "800")  # two years and two months; see above
ACT_MIN_N   <- .env_num("FORM_ACT_MIN_NEFF", "1")
la <- h[, .(last_any = max(date)), by = athlete_id]
la[, athlete_id := as.character(athlete_id)]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
act <- st[n_eff >= ACT_MIN_N &
          !is.na(last_any) & last_any >= ASOF - ACT_ATHLETE &
          !is.na(last)     & last     >= ASOF - ACT_EVENT]
cat(sprintf("active: %s of %s athlete-events (as at %s; athlete within %dd, event within %dd, n_eff >= %.1f)
",
            format(nrow(act), big.mark = ","), format(nrow(st), big.mark = ","),
            ASOF, ACT_ATHLETE, ACT_EVENT, ACT_MIN_N))
# RANK ON THE CEILING-BLENDED RATING, not the raw one.
#
# R tracks an athlete's AVERAGE form, and for a championship ranking that is the
# wrong quantity. Josh Kerr's carried R is 3:36.55 - SLOWER than his own median
# 1500m of 3:34.93 and nine seconds off his 3:27.79 best - because the average
# includes a fall and several tactical rounds. He ranked 21st.
#
# The engine already computes the fix and nobody read it. form_ratings.R writes
# R_ceil, the same (1-CEIL)*R + CEIL*best blend it uses to ORDER a field inside
# a race, adopted 2026-08-15 on +0.28 sealed. Until now NOTHING in the repo read
# that column, so the ceiling blend had never once changed a published ranking.
#
# Measured against World Athletics as an outside referee:
#   precision@10      67.7% -> 68.2%   (interior peak at CEIL 0.30; 0.50 worse)
#   middle distance   57.5% -> 67.5%
#   Kerr              21st -> 9th
# 75 athlete-events across 35 events have a best mark that would rank top-10
# while their raw rank sits outside the top 20 - a systematic bias against
# athletes who peak in championships, not one anecdote.
#
# KNOWN COST: 55 rows newly enter a top ten, 91% on marks under a year old,
# about 3 on marks over two years.
#
# The obvious fix - blend toward a top-k mean rather than the single best - was
# BUILT AND MEASURED, and does not work. Against World Athletics at display
# time, precision@10 falls monotonically: 70.2% single-best, 69.8% top-3, 69.0%
# top-5, 68.8% top-3-with-decay, and it is worst in the families it was meant to
# help (middle 65.0 -> 62.5, distance 60.0 -> 56.7). Of 37 athletes it removes
# from a top ten, a sample split 2 clean catches, 1 borderline, 1 unrelated
# filter confound and 1 wrongful demotion of a genuinely consistent thrower. See
# SEQ_BEST_K in form_ratings.R, which stays at 1.
#
# An earlier version of this comment cited Jack Rayner going 35th -> 9th as the
# bad case. That was measured on the pre-harvest corpus and is now stale: his
# 28:15.73 from 2025-12-13 is in the data and he sits 19th under this rule with
# no change needed.
#
# DISPLAY HONESTY, unresolved: the mark columns are still derived from R, so a
# reader can see an athlete ranked above someone showing a faster typical time.
if (!"R_ceil" %in% names(act))
  stop("form_display_marks.R expects R_ceil from the state table.\n",
       "  Re-run form_ratings.R - a state file written before 2026-08-15 lacks it.")
act[, R_rank := fifelse(is.finite(R_ceil), R_ceil, R)]
cat(sprintf("ranking on R_ceil; %s of %s rows fall back to raw R (no best mark)\n",
            format(sum(!is.finite(act$R_ceil)), big.mark = ","),
            format(nrow(act), big.mark = ",")))
# --- COMBINED EVENTS: rank on the SIMULATED score ------------------------------
#
# A decathlon score is a deterministic function of ten marks, and rating the
# points total as if it were one event discards that. Simulating the total from
# ten separately-rated component marks orders the field better on the properly
# powered referee: Spearman against the World Athletics order over 74 ranked
# athletes runs 0.873 for the points total and 0.879 for the simulation, and on
# the decathlon alone 0.871 against 0.899.
#
# WHY A BLEND RATHER THAN THE SIMULATION OUTRIGHT. Two reasons, both measured.
# Coverage: even after filling missing slots from the athlete own combined-event
# marks, 15-23% of active athletes cannot be simulated, and they would vanish
# from the table. And an athlete who has actually scored 8,500 twice needs no
# prior - the measured total is evidence. So the simulation is the PRIOR and the
# points-total rating updates it, weighted by how many combined events the
# athlete has contested: w = PW / (perfs + PW).
#
# WHY PW = 8. Sweeping it, Spearman rises monotonically with trust in the
# simulation - 0.871, 0.873, 0.875, 0.878, 0.879, 0.880 - while precision@10
# holds at 75 up to PW 8 and falls to 70 at PW 20. Eight is two-thirds weight on
# the simulation at the median, with no measured loss on either metric. The
# monotone shape is the robust finding; the exact 8 was chosen on this referee.
CE_ON <- Sys.getenv("FORM_CE_BLEND", "1") != "0"
CE_PW <- .env_num("FORM_CE_PRIOR_W", "8")
CE_EV <- c("AT-Decathlon-M", "AT-Heptathlon-M", "AT-Heptathlon-W", "AT-Pentathlon-W")
if (CE_ON && any(act$event_id %chin% CE_EV)) {
  fsim <- file.path(OUT, "combined_simulated.parquet")
  fcmp <- file.path(OUT, "combined_components.parquet")
  # a silently skipped blend would publish the old ranking while every log line
  # said the feature was on - the same failure the cross-event blend guards
  if (!file.exists(fsim) || !file.exists(fcmp))
    stop("FORM_CE_BLEND is on but the combined-event artefacts are missing.\n",
         "  Build them:  Rscript citiusdata/scripts/build_combined_components.R\n",
         "               Rscript citiusdata/scripts/build_combined_simulation.R\n",
         "  Or turn it off explicitly:  FORM_CE_BLEND=0")
  # imputed_slots travels too: a first-time decathlete can have up to 3 of 10
  # component slots filled rather than observed, and without this the published
  # rank is partly a guess that looks identical to a fully observed one.
  .cs <- setDT(read_parquet(fsim))
  if (!"imputed_slots" %chin% names(.cs)) .cs[, imputed_slots := NA_integer_]
  csim <- .cs[, .(event_id = ce, athlete_id = as.character(athlete_id),
                  sim_mean, sim_sd, imputed_slots)]
  ccmp <- setDT(read_parquet(fcmp))
  cper <- unique(ccmp[complete == TRUE, .(tid, ce, athlete_id = as.character(athlete_id))])[
    , .(ce_perfs = .N), by = .(event_id = ce, athlete_id)]
  act <- merge(act, csim, by = c("event_id", "athlete_id"), all.x = TRUE)
  act <- merge(act, cper, by = c("event_id", "athlete_id"), all.x = TRUE)
  act[is.na(ce_perfs), ce_perfs := 0]
  # blend in z space within event, then map back so R_rank keeps its units
  act[event_id %chin% CE_EV,
      `:=`(.mu = mean(R_rank), .sd = stats::sd(R_rank)), by = event_id]
  act[event_id %chin% CE_EV & is.finite(.sd) & .sd > 0,
      .zt := (R_rank - .mu) / .sd]
  act[event_id %chin% CE_EV & is.finite(sim_mean),
      .zs := (sim_mean - mean(sim_mean)) / stats::sd(sim_mean), by = event_id]
  act[, .wc := fifelse(event_id %chin% CE_EV & is.finite(.zs) & is.finite(.zt),
                       CE_PW / (ce_perfs + CE_PW), 0)]
  moved_ce <- act[.wc > 0]
  act[.wc > 0, R_rank := .mu + .sd * ((1 - .wc) * .zt + .wc * .zs)]
  cat(sprintf("combined-event blend: %s of %s combined-event rows re-ranked on the\n",
              format(nrow(moved_ce), big.mark = ","),
              format(sum(act$event_id %chin% CE_EV), big.mark = ",")),
      sprintf("  simulated score (prior weight %.0f, median weight %.2f on the simulation)\n",
              CE_PW, if (nrow(moved_ce)) stats::median(moved_ce$.wc) else NA_real_), sep = "")
  stopifnot("the combined-event blend moved no rows at all" = nrow(moved_ce) > 0)
  act[, ce_share := fifelse(is.finite(.wc), .wc, 0)]   # weight on the simulation
  act[, c(".mu", ".sd", ".zt", ".zs", ".wc") := NULL]
}

# --- BORROW FROM CORRELATED EVENTS, for thin records only --------------------
#
# WHY HERE AND NOT IN THE ENGINE. form_ratings.R has a cross-event blend, and it
# CANNOT reach a published ranking: it modifies r_use, which is loop-local and
# used only to order a field while scoring, while the state table computes
# R_ceil from R and the best mark alone. Verified 2026-08-18 - Barega's 10,000m
# rating came out byte-identical at similarity gates 0.80, 0.50 and 0.30. No
# engine-side setting can move a rank, so the blend has to happen here.
#
# WHAT IT FIXES. Almgren won the European 10,000m in 27:23 off slow tactical
# wins and ranked 16th; Barega sat 2nd on 2.77 races and an 11-month-old result.
# Both are thin records where a correlated event knows more than the event
# itself does.
#
# WHY THE EVIDENCE CAP MATTERS MOST. Blending everyone costs precision@10
# (69.1 -> 68.4 at xb 1.0). Blending only athletes with n_eff <= MAXN recovers
# all of it - every capped configuration beat its uncapped sibling across a
# 24-point sweep. A deep record has nothing to learn from a sibling event.
#
# MEASURED against World Athletics as an outside referee (check_export_blend.R):
#   precision@10   69.1 -> 69.1     WA #1 shown   97.7 -> 97.7
#   Barega 10,000m    2nd -> 4th    Almgren 10,000m   16th -> 9th
# HONEST CAVEAT: the specific settings below tie the baseline and were CHOSEN on
# that metric across 24 configurations, so 69.1 is an in-sample tie. What is not
# selected is the shape - capping helped everywhere, and both named athletes
# move the right way at every setting tested.
#
# THE MATRIX MATTERS AS MUCH AS THE SETTINGS. With the old 200-pair matrix, which
# contained no road or walk events at all, Barega does not move at ALL: his
# 5000m rating sits outside the 400-day window, so he had no usable sibling. It
# is the road events in event_similarity_all.parquet that let his half marathon
# and 10km reach his 10,000m.
#
# Ratings are z-scored WITHIN event before blending - a 10,000m rating and a
# marathon rating are not comparable numbers - then mapped back to the event's
# own scale, so everything downstream is unchanged in units.
XB_ON     <- Sys.getenv("FORM_XBLEND", "1") != "0"
# SPECIALISTS ONLY. Measured 2026-08-18: a pair's correlation is systematically
# inflated by athletes a combined event forced into both events, and the effect
# is concentrated in exactly the cross-family transfers this blend exists to use
# (-0.064 cross-family against -0.020 within). 800m/Heptathlon read 0.631 over
# 1,569 shared athletes and collapses to 0.002 over 13 specialists; Javelin/Long
# Jump 0.411 -> 0.000; even Discus/Shot Put runs 0.869 against 0.486 over 1,147
# actual throwers. 79 pairs disappear entirely - 800m/Shot Put at 0.362 rested on
# 2,006 shared athletes, every one a heptathlete.
# Precision@10 is identical on either matrix (69.1), so this buys correctness at
# no measured cost. event_similarity_all.parquet is kept for comparison.
XB_SIMF   <- Sys.getenv("FORM_XB_SIMFILE", "event_similarity_spec.parquet")
XB_MINCOR <- .env_num("FORM_XB_MINCOR", "0.30")
# TIGHTENED 2026-08-19 from xb 1.0 / maxn 8, after LOOKING AT THE PAGE. At the
# old settings precision@10 was unchanged (69.1 either way) and the published
# 1500m read: 1. Wanyonyi (n_eff 1.5, an 800m runner), 2. El Bakkali (n_eff 1.1,
# a steeplechaser), 3. Ingebrigtsen (n_eff 12.5, the fastest typical mark in the
# field), with Josh Kerr 18th. At n_eff 1.5 the old weight put 40% of an
# athlete's rank on another event. The referee could not see it; the page could.
#
# At 0.25/4 the same list reads Ingebrigtsen, Myers, Wanyonyi - and the cases the
# blend exists for are unaffected, because the ENGINE changes (winner censoring,
# the shock fix) are what actually fixed them: Almgren's 10,000m rank is 8th at
# either setting, and Kerr recovers from 18th to 13th.
XB_STR    <- .env_num("FORM_XB", "0.25")
XB_NSIB   <- .env_int("FORM_XB_NSIB", "6")
XB_MAXN   <- .env_num("FORM_XB_MAXN", "4")
if (XB_ON) {
  simf <- file.path(OUT, XB_SIMF)   # OUT is this script's data dir, not D
  # Deliberately a hard stop. A silently skipped blend would publish the old
  # ranking while every log line said the feature was on, which is how a knob
  # that changes nothing gets read as a null result.
  if (!file.exists(simf))
    stop("FORM_XBLEND is on but ", XB_SIMF, " is missing.\n",
         "  Build it:  Rscript citiusdata/scripts/build_event_similarity.R\n",
         "  Or turn the blend off explicitly:  FORM_XBLEND=0")
  sim <- setDT(read_parquet(simf))
  scol <- if ("cor_use" %chin% names(sim)) "cor_use" else "cor"
  sim[, corv := as.numeric(get(scol))]
  sim <- sim[is.finite(corv) & corv >= XB_MINCOR]
  if (!nrow(sim))
    stop("no event pair reaches FORM_XB_MINCOR=", XB_MINCOR, " in ", XB_SIMF)
  sim2 <- rbindlist(list(sim[, .(ev = e1, sv = e2, corv)],
                         sim[, .(ev = e2, sv = e1, corv)]))
  act[, .z := (R_rank - mean(R_rank)) / stats::sd(R_rank), by = event_id]
  act[, `:=`(.mu = mean(R_rank), .sd = stats::sd(R_rank)), by = event_id]
  jj <- merge(act[is.finite(.z), .(athlete_id, ev = event_id, n_eff, .z)],
              sim2, by = "ev", allow.cartesian = TRUE)
  jj <- merge(jj, act[is.finite(.z), .(athlete_id, sv = event_id,
                                       z_sib = .z, ne_sib = n_eff)],
              by = c("athlete_id", "sv"))
  setorder(jj, athlete_id, ev, -corv)
  jj <- jj[, head(.SD, XB_NSIB), by = .(athlete_id, ev)]
  jj[, wt := corv^2 * ne_sib]
  agg <- jj[, .(z_borrow = sum(wt * z_sib) / sum(wt), sibs = .N), by = .(athlete_id, ev)]
  act <- merge(act, agg, by.x = c("athlete_id", "event_id"),
               by.y = c("athlete_id", "ev"), all.x = TRUE)
  act[, .w := fifelse(is.finite(z_borrow) & n_eff <= XB_MAXN & is.finite(.sd) & .sd > 0,
                      XB_STR / (n_eff + XB_STR), 0)]
  moved <- act[.w > 0]
  act[.w > 0, R_rank := .mu + .sd * ((1 - .w) * .z + .w * z_borrow)]
  cat(sprintf("cross-event blend: %s of %s rows borrowed (n_eff <= %.0f, mincor %.2f,\n",
              format(nrow(moved), big.mark = ","), format(nrow(act), big.mark = ","),
              XB_MAXN, XB_MINCOR),
      sprintf("  xb %.1f, up to %d siblings from %s); median %.1f siblings used\n",
              XB_STR, XB_NSIB, XB_SIMF,
              if (nrow(moved)) stats::median(moved$sibs) else NA_real_), sep = "")
  # A blend that moves nothing is a broken configuration, not a null result.
  stopifnot("the cross-event blend moved no rows at all" = nrow(moved) > 0)
  # KEEP the provenance of the rating. n_eff alone cannot tell a reader whether a
  # rank rests on the athlete's own racing or on what a correlated event implies
  # about them - and for a thin record those are very different claims. xb_share
  # is the fraction of the ranking key taken from other events; xb_sibs is how
  # many events it was taken from.
  act[, xb_share := .w]
  act[!is.finite(xb_share), xb_share := 0]
  act[, xb_sibs := fifelse(xb_share > 0 & is.finite(sibs), as.integer(sibs), 0L)]
  act[, c(".z", ".mu", ".sd", ".w") := NULL]
}

# THE MARK THE TABLE IS ACTUALLY SORTED BY, derived from the sort key itself
# and computed AFTER every adjustment to it. Any column built earlier is stale
# by construction, which is how a ranking table came to show a slower athlete
# above a faster one with nothing on the page to explain it.
# --- SHRINK THE RANKING KEY BY EVIDENCE ---------------------------------------
#
# A rank resting on one race is not the same claim as one resting on twelve, and
# until now the table presented them identically: 352 of 819 published top-ten
# rows had fewer than three effective races, the men's 10,000m top ten contained
# three athletes with exactly 1.0, and Barega led the 1500m on 1.60 races.
#
# A hard cutoff would discard a genuinely fast athlete who has raced twice and
# pick a threshold with nothing behind it. Shrinking toward the event mean in
# proportion to evidence is the same empirical-Bayes shape the ratings already
# use: w = n_eff / (n_eff + k). A deep record barely moves, a single race is
# pulled most of the way back.
#
# k = 0.5, chosen on FIVE referees rather than one, because they disagree and the
# disagreement is the finding. Against the World Athletics order:
#
#   k     p@10   p@16   p@20   spearman  sp_top30   thin top-ten rows
#   none  70.2   69.0   68.1   0.9256    0.7754     352
#   0.5   71.6   71.9   70.8   0.9337    0.7851     311
#   1.0   71.6   72.7   71.9   0.9342    0.7722     284
#   2.0   70.9   72.9   71.5   0.9304    0.7438     257
#
# RETIRED 2026-08-19: DEFAULT 0.5 -> 0. Everything above is agreement with the
# World Athletics ranking, and that is a REFERENCE, not the referee. The metric
# this model is judged on is out-of-sample tier-weighted concordance - given the
# athletes who actually lined up, did the rating order them the way the race did.
# Measured on that (check_shrinkage_concordance.R), shrinkage is not a small win,
# it is a large loss:
#
#   k      sealed 2026 weighted   vs none      tune 2025 vs none
#   0      76.429                  -           -
#   0.25   75.838                 -0.591       -0.531
#   0.5    75.399                 -1.030       -0.929
#   1.0    74.688                 -1.741       -1.609
#   4.0    72.776                 -3.653       -3.805
#
# The noise floor on that window is 0.159 pp, so -1.030 is 6.5x the floor. It is
# monotone in k, and the independent 2025 window agrees in sign at every step.
# There is no reading of this on which shrinkage helps.
#
# WHY THE TWO REFEREES DISAGREED, which is the lesson worth keeping. Pulling thin
# athletes toward the event mean makes our list resemble WA's more closely -
# their ranking has its own recency and quality rules that penalise exactly those
# athletes - while making it WORSE at ordering actual races. Agreeing with another
# system is not the same as being right, and when the reference and the ground
# truth point opposite ways, the ground truth decides.
#
# WHAT IS LOST. Thin evidence returns to the published top tens: 312 rows with
# under three effective races, against 276 with shrinkage at 0.5. That problem is
# real and remains open - but the fix cannot be a transformation that measurably
# worsens the ordering, and the honest position is that this attempt was refuted
# rather than that the problem is solved. Set FORM_EVID_K to re-enable.
EVID_K <- .env_num("FORM_EVID_K", "0")
if (EVID_K > 0) {
  act[, .mu_r := mean(R_rank), by = event_id]
  act[, .w_ev := n_eff / (n_eff + EVID_K)]
  .thin_before <- act[, {setorder(.SD, -R_rank); sum(n_eff[seq_len(min(10, .N))] < 3)},
                      by = event_id][, sum(V1)]
  act[, R_rank := .mu_r + .w_ev * (R_rank - .mu_r)]
  .thin_after <- act[, {setorder(.SD, -R_rank); sum(n_eff[seq_len(min(10, .N))] < 3)},
                     by = event_id][, sum(V1)]
  cat(sprintf("evidence shrinkage k=%.1f: median weight %.2f | thin top-ten rows %d -> %d\n",
              EVID_K, stats::median(act$.w_ev), .thin_before, .thin_after))
  # If it removes nothing it is not doing its job, and a silently inert transform
  # is exactly what this repo keeps producing.
  stopifnot("evidence shrinkage removed no thin rows at all - it is inert" =
              .thin_after < .thin_before)
  act[, c(".mu_r", ".w_ev") := NULL]
}

act[, rank_mark := exp(orientation * (R_rank + offset))]
setorder(act, event_id, -R_rank)
act[, rk := seq_len(.N), by = event_id]

# A ranking table whose displayed key does not move with the rank is unreadable,
# and no aggregate metric will ever catch it. Assert it instead: within an event,
# rank_mark must improve monotonically as rk improves. orientation -1 means a
# lower mark is better (track), +1 means higher is better (field).
.mono <- act[is.finite(rank_mark), {
  o <- orientation[1]
  d <- diff(rank_mark[order(rk)])
  .(bad = sum(if (o == -1) d < -1e-9 else d > 1e-9))
}, by = event_id]
if (sum(.mono$bad) > 0) {
  print(.mono[bad > 0][order(-bad)])
  stop("rank_mark is not monotone with rank in ", nrow(.mono[bad > 0]), " event(s). ",
       "The displayed mark is not derived from the key the table is sorted by.")
}
cat(sprintf("rank_mark monotone with rank in all %d events\n", nrow(.mono)))

fmt <- function(m, unit) {
  ifelse(is.na(m), "  -  ",
  # `unit` is "seconds"/"metres"/"points" from the registry. This tested
  # `unit != "s"` and so never formatted a time: the 10,000m printed 1625.07
  # instead of 27:05.07 and the 1500m 209.63 instead of 3:29.63.
  ifelse(!(unit %chin% c("s", "seconds")), sprintf("%.2f", m),
  ifelse(m < 60, sprintf("%.2f", m),
  ifelse(m < 3600, sprintf("%d:%05.2f", floor(m/60), m %% 60),
         sprintf("%d:%02d:%02.0f", floor(m/3600), floor((m %% 3600)/60), m %% 60)))))
}
show <- function(ev, k = 5) {
  e <- act[event_id == ev][rk <= k]
  if (!nrow(e)) return(invisible())
  cat(sprintf("== %s  (offset %+.3f%%, n_fit %s) ==\n", sub("^AT-","",ev),
              100*e$offset[1], format(e$n_fit[1], big.mark=",")))
  for (i in seq_len(nrow(e)))
    cat(sprintf("  %d. %-24s typical %9s   good day %9s   n_eff %.1f\n", e$rk[i],
                substr(e$athlete_name[i],1,24), fmt(e$pred_mark[i], e$unit[i]),
                fmt(e$peak_mark[i], e$unit[i]), e$n_eff[i]))
  cat("\n")
}
cat("\n")
for (ev in c("AT-100Metres-M","AT-800Metres-W","AT-800Metres-M","AT-1500Metres-M",
             "AT-Marathon-M","AT-PoleVault-M","AT-LongJump-M","AT-ShotPut-W",
             "AT-HighJump-W")) show(ev)

# --- 4. ANCHORS: a displayed mark is exactly where a wrong transform looks ---
# plausible. Assert against marks known independently of this pipeline.
anchor <- function(ev, who, lo, hi) {
  e <- act[event_id == ev][grep(who, athlete_name, ignore.case = TRUE)][1]
  ok <- nrow(e) > 0 && is.finite(e$pred_mark) && e$pred_mark >= lo && e$pred_mark <= hi
  cat(sprintf("%-16s %-14s %10s in [%g, %g]  %s\n", sub("^AT-","",ev), who,
              if (nrow(e)) sprintf("%.2f", e$pred_mark) else "absent", lo, hi,
              if (isTRUE(ok)) "OK" else "*** FAIL ***"))
  isTRUE(ok)
}
cat("ANCHORS (plausible-range checks on the DISPLAYED mark)\n")
res <- c(anchor("AT-100Metres-M", "Lyles",      9.6,  10.1),
         anchor("AT-800Metres-W", "Hodgkinson", 113,  121),
         anchor("AT-PoleVault-M", "Duplantis",  5.7,  6.5),
         anchor("AT-LongJump-M",  "Tentoglou",  7.8,  8.8))
# The peak column needs its own assertion: the anchors above only test the
# TYPICAL mark, and the first version of this script shipped a 9.55 100m and a
# 3:23 1500m because nothing checked the other column.
bad_peak <- act[is.finite(peak_mark) & is.finite(best_perf) &
                orientation * log(peak_mark) > best_perf + 1e-9, .N]
cat(sprintf("peaks still beating the all-time event best after the cap: %d\n", bad_peak))
res <- c(res, bad_peak == 0L)
cat(sprintf("\n%d of %d mark anchors hold\n", sum(res), length(res)))
# A wrong transform and an uncovered event both produce "no sensible mark", and
# they need opposite responses: the first is a bug, the second is missing data.
# So assert marks only where there IS coverage, and report coverage separately.
if (!all(res)) stop("a displayed mark is outside its plausible range - check the transform")

# --- 4b. Is the "good day" column honest? -----------------------------------
# It claims a 90th percentile, so ~10% of actual finals should beat it. Measured
# on 2026, which the quantile was NOT fitted on. A column that says "good day"
# and is beaten 40% of the time is worse than no column at all.
# EACH COLUMN IS SCORED ON THE POPULATION IT IS SHOWN TO. 'typical' is displayed
# for every athlete; only 'good day' is suppressed below PEAK_MIN_N. An earlier
# version filtered BOTH to deep records, which measured 'typical' on a
# population the page does not restrict, and duly reported it as broken (44.54%)
# when the column readers actually see is fine.
val <- h[seen == TRUE & rc == "final" & year(date) == 2026 &
         is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0]
val <- merge(val, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
val[is.na(offset), offset := pooled]
val <- .add_centre(val)
val[, peak_perf := centre + ZSPREAD * sqrt(v_pre)]
val_pk <- val[n_eff >= PEAK_MIN_N]        # the good-day column's own population
hit <- val_pk[, mean(perf > peak_perf)]
typ_hit <- val[, mean(perf > centre)]
# 2025 IS AN INDEPENDENT WINDOW, and it was going spare. The spread is fitted on
# dates before 2025 and the honest check is 2026, so 2025 belongs to neither -
# which makes it the right place to confirm a change to this column WITHOUT
# reading the sealed window first. If 2025 and 2026 disagree the fix is
# overfitted to one of them and should not ship.
v25 <- h[seen == TRUE & rc == "final" & year(date) == 2025 &
         is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0]
v25 <- merge(v25, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
v25[is.na(offset), offset := pooled]
v25 <- .add_centre(v25)
v25_pk <- v25[n_eff >= PEAK_MIN_N]
stopifnot("the 2025 validation window is empty" = nrow(v25_pk) > 1000)
cat(sprintf("\nVALIDATION (2025, neither fitted nor sealed):\n"))
cat(sprintf("  'good day' beaten %.2f%% over %s finals with n_eff >= %d (target 10%%)\n",
            100 * v25_pk[, mean(perf > centre + ZSPREAD * sqrt(v_pre))],
            format(nrow(v25_pk), big.mark = ","), PEAK_MIN_N))

cat(sprintf("\nCALIBRATION (2026, out of sample):\n"))
cat(sprintf("  'typical'  beaten %.2f%% over %s finals (target 50%%)\n",
            100*typ_hit, format(nrow(val), big.mark=",")))
cat(sprintf("  'good day' beaten %.2f%% over %s finals with n_eff >= %d (target 10%%) -> about 1 in %.1f\n",
            100*hit, format(nrow(val_pk), big.mark=","), PEAK_MIN_N, 1/hit))
# Depth-dependent, and worth watching: the offset is fitted pooled, but deep
# records beat 'typical' LESS often than thin ones do. Reported rather than
# corrected - it is the known evidence-depth bias, not a fault in this file.
cat(sprintf("  ...'typical' among those same deep records: %.2f%%\n",
            100*val_pk[, mean(perf > centre)]))
# The page must state the MEASURED frequency, not the nominal one. Median
# centring fixed the skew half of this; the rest is structural. ZSPREAD is ONE
# pooled quantile of z applied to athletes whose variances differ, so it cannot
# be a 90th percentile for any of them individually — the mixture is
# over-dispersed and it under-covers. Refitting the spread to force 10% would
# have to be tuned on 2026, and spending the sealed window on a display label
# is a bad trade.
# 1/hit is Inf when no out-of-sample final beat its peak mark, and this string
# is published in the calibration JSON. Refuse to publish a number that means
# "never" dressed as a frequency.
if (!is.finite(hit) || hit <= 0)
  stop("no out-of-sample final beat its peak mark, so the good-day label would
publish as 'about 1 race in Inf'. Investigate the peak calibration rather than
shipping it.")
PEAK_LABEL <- sprintf("about 1 race in %.0f", 1/hit)
cat(sprintf("  -> page label: \"%s\"\n", PEAK_LABEL))
# Tolerance was +/-5pp, which passed a 14.2% rate in silence while the page
# still said "90th percentile". The label was wrong and no guard fired, which is
# the failure this check existed to prevent.
if (abs(hit - 0.10) > 0.02)
  cat("  NOTE: nominal 90th percentile does not hold; use PEAK_LABEL on the page.\n")
if (abs(typ_hit - 0.50) > 0.05)
  cat("  *** TYPICAL MISCALIBRATED — the central mark is not central ***\n")

cov <- merge(reg[, .(event_id, family)],
             act[, .(active = .N), by = event_id], by = "event_id", all.x = TRUE)
cov[is.na(active), active := 0L]
empty <- cov[active == 0L]
cat(sprintf("\nCOVERAGE: %d of %d registry events have no displayable athlete\n",
            nrow(empty), nrow(cov)))
if (nrow(empty)) {
  for (fm in empty[, unique(family)]) {
    ids <- empty[family == fm, sub("^(AT|SW)-", "", event_id)]
    cat(sprintf("  %-14s %2d  %s\n", fm, length(ids),
                paste(utils::head(ids, 4), collapse = ", ")),
        if (length(ids) > 4) sprintf("  %-14s     ... and %d more\n", "", length(ids) - 4) else "",
        sep = "")
  }
  cat("\n  swim_* is scope, not a gap: this engine reads the athletics corpus only.\n")
  cat("\nROAD IS THE STRUCTURAL ONE, and it is upstream of this script.\n",
      "AT-Marathon-M holds 1,698 corpus rows dated 2026, but of 2024+ marathon\n",
      "rows only 171 are T1_elite and 84 T2_strong; 5,386 are absent from the\n",
      "competition catalogue and 2,739 are T3_development. The engine keeps\n",
      "T1/T2 only, so the majors are invisible to it. Fix the catalogue tiering,\n",
      "not the filter here.\n", sep = "")
}
stopifnot("rank_mark missing - the table would sort by a column it does not show" =
            "rank_mark" %in% names(act))
# xb_share / xb_sibs / ce_share travel with the table so a reader can see WHERE a
# rating came from: how much of the ranking key is the athlete's own racing in
# this event, and how much is inferred from correlated events or, for combined
# events, from a simulation of their components. Defaulted so the columns exist
# even when a blend is switched off.
if (!"xb_share" %in% names(act)) act[, xb_share := 0]
if (!"xb_sibs"  %in% names(act)) act[, xb_sibs  := 0L]
if (!"ce_share" %in% names(act)) act[, ce_share := 0]
if (!"imputed_slots" %in% names(act)) act[, imputed_slots := NA_integer_]
act[, ce_imputed := fifelse(is.finite(imputed_slots), as.integer(imputed_slots), 0L)]
# band_adj IS PUBLISHED, not just used. Without it pred_mark stopped being
# reconstructible from the published columns the moment the depth correction
# landed - exp(R + offset) no longer reproduces it - and check_panel_marks.R
# caught exactly that by failing its reconstruction assertion. A displayed number
# that cannot be rebuilt from the columns beside it is one nobody can check.
if (!"band_adj" %chin% names(act)) act[, band_adj := 0]
write_parquet(act[, .(event_id, athlete_id, athlete_name, rk, R, R_ceil, offset,
                      band_adj,
                      pred_mark, rank_mark, peak_mark, raw_mark, n_eff, v, last, unit,
                      xb_share, xb_sibs, ce_share, ce_imputed)],
              file.path(OUT, sprintf("form_display_%s.parquet", TAG)))
cat(sprintf("wrote form_display_%s.parquet (%s rows)\n", TAG,
            format(nrow(act), big.mark = ",")))

# Calibration travels as DATA, not as a sentence typed into the page. The
# "good day" column is beaten about 1 race in 7, not the 1 in 10 its
# construction implies, and a hard-coded label drifts the moment the spread is
# refitted. The blog exporter reads this file and builds its caveat from the
# measured number, so the claim is fixed in one place.
jsonlite::write_json(list(
  window             = "2026 finals, out of sample",
  n_finals           = nrow(val),
  typical_beaten_pct = round(100 * typ_hit, 2),
  goodday_beaten_pct = round(100 * hit, 2),
  goodday_min_n_eff  = PEAK_MIN_N,
  goodday_one_in     = round(1 / hit, 1),
  peak_label         = PEAK_LABEL,
  zspread            = round(ZSPREAD, 4),
  centring           = "median"),
  file.path(OUT, sprintf("form_display_%s_calib.json", TAG)),
  auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("wrote form_display_%s_calib.json (peak label: \"%s\")\n", TAG, PEAK_LABEL))
