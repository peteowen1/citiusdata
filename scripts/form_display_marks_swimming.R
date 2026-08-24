# Turn SWIMMING form ratings into DISPLAYABLE MARKS. Sibling of
# form_display_marks.R (athletics), same relationship form_ratings_swimming.R
# already has to form_ratings.R -- no `sport ==` branch was added to the
# athletics file. See that file's header for the two-thing framing (ORIENTATION
# + LEVEL) this is derived from; not repeated here.
#
# WHY A SEPARATE FILE, NOT AN RBIND INTO form_display_marks.R (confirmed by
# research the same day this file was written, do not re-litigate). Several of
# the athletics file's statistics are fitted POOLED ACROSS EVERY EVENT IN ITS
# INPUT TABLE: the offset's pooled fallback, the depth-band correction, ZSPREAD,
# the single-scalar ASOF, and the 2025/2026 calibration windows that get
# quoted verbatim on the published page. Rbinding swimming rows into that
# pipeline before those computations run would silently shift every one of
# those numbers for ATHLETICS too, using unvalidated swimming statistics
# nobody asked for. So this file re-runs the SAME KIND of computation once,
# independently, on swimming's own seqv2_state_SW-baseline.parquet /
# seqv3_history_SW-baseline.parquet, and writes a SEPARATE output,
# form_display_SW-baseline.parquet.
#
# WHAT IS DIFFERENT FROM ATHLETICS, and why -- the short version; each is also
# flagged inline where it happens:
#   * PEAK-MARK ("good day") IS NOT COMPUTED. Gated at n_eff >= PEAK_MIN_N (8)
#     the same as athletics, but only 169 of 83,494 swimming state rows
#     (0.20%) ever reach n_eff 8 -- median n_eff in the state table is 1.35,
#     against athletics' much deeper history. A column that would render for
#     0.2% of rows is not a v1 feature, it is a near-empty column with a
#     confusing near-total dash rate. See the section below for the measured
#     numbers. peak_mark is still WRITTEN, as NA throughout, to keep the
#     output schema identical to athletics' so a FORM_TAG-parametrized
#     downstream reader needs no code change.
#   * DEPTH-BAND CORRECTION IS KEPT. Unlike the peak column, the evidence here
#     supports it: the fit population (263,477 finals rows before 2025) has
#     >500 rows in 6 of the 7 depth bands (the "<1" band alone has 93,754),
#     which is the same n_band<500-gets-zero mechanism athletics already
#     guards with, ported unchanged. Only the deepest band ("15+", 171 rows)
#     falls back to a zero adjustment through that existing guard -- nothing
#     swim-specific had to be added to make that safe.
#   * PER-EVENT OFFSET NEEDS NO POOLED FALLBACK IN PRACTICE. All 36 registry
#     events clear FORM_OFFSET_MIN_N (200) in the pre-2025 fit population --
#     the thinnest event (SW-100mIndividualMedley-W) still has 1,182 finals
#     rows. The pooled-fallback CODE PATH is kept (same mechanism, same env
#     knob, shared with athletics) in case a future event addition or a
#     tighter FROM cutoff thins an event below the bar -- it simply does not
#     fire on this corpus, and the run reports how many events use it.
#   * NO COMBINED-EVENTS BLEND, NO CROSS-EVENT BLEND, NO EVIDENCE SHRINKAGE.
#     Swimming has no combined events. event_similarity_spec.parquet is
#     athletics-only (form_ratings_swimming.R's own SEQ_XBLEND already stays
#     family-gate-only and off by default for the identical reason -- no
#     swimming-scoped similarity matrix exists yet, and building one from a
#     first state file would be circular). Evidence shrinkage (FORM_EVID_K) is
#     EVID_K=0 in athletics anyway, i.e. already off there; not ported here
#     either. act$R_rank is therefore just act$R_ceil, unmodified, throughout.
#   * ACTIVITY-WINDOW CONSTANTS ARE UNVALIDATED FOR SWIMMING. The mechanism
#     (n_eff >= ACT_MIN_N & last_any >= ASOF - ACT_ATHLETE & last >= ASOF -
#     ACT_EVENT) is family-agnostic and ported as-is, sharing the SAME env var
#     names as athletics (FORM_ACT_ATHLETE_D / FORM_ACT_EVENT_D /
#     FORM_ACT_MIN_NEFF) -- the same "one shared knob, two engines read it"
#     convention form_ratings_swimming.R already established for its SEQ_*
#     knobs. The 800-day windows were swept against World Athletics for TRACK
#     racing cadence; nothing swim-specific has validated them against a major
#     taper-meet / domestic short-course calendar. Treat as a starting default,
#     not a fitted choice, same as every constant form_ratings_swimming.R
#     inherited and flagged.
#   * TWO DATA-INTEGRITY GUARDS NOT PRESENT IN ATHLETICS, both found while
#     building this file. (1) CROSS-SEX GUARD: 996 athlete_ids carry a rated
#     result under both a -M and a -W event, which is structurally impossible
#     (sex is fixed per event); 1,174 minority-evidence rows dropped, 95%
#     concentrated in the five Individual Medley events, traced to a
#     competition_id collision in the swimming corpus build (see the guard's
#     own comment for the worked example). (2) IMPOSSIBLE-MARK GUARD: rows
#     whose typical mark beats the all-time best ever recorded in that event's
#     own corpus partition are dropped (mirrors athletics' peak-mark cap
#     principle, applied to the central mark instead). NEITHER GUARD FIXES THE
#     ROOT CAUSE -- the contaminated corpus rows were already fed into
#     form_ratings_swimming.R's sequential engine, so ratings for legitimate
#     athletes who shared a corrupted race block may be mistrained in a way no
#     display-layer filter can undo. This needs a fix in the swimming corpus
#     harvest (swimming_corpus_store), reported separately, not attempted here.
#   * CALIBRATION REPORT IS SIMPLER. No ZSPREAD/peak calibration (nothing to
#     calibrate -- the column isn't computed). Reports whether the median-
#     centred "typical" mark lands near 50% beaten on an out-of-sample year,
#     the same sanity check athletics runs, without the fuller good-day
#     machinery.
#   * NAMES/NATIONALITY join from athlete_crosswalk_swimming.parquet, not
#     athlete-ratings.parquet (confirmed same day: athlete-ratings.parquet has
#     ZERO swimming coverage). The crosswalk carries 3,875 duplicate person_id
#     rows across sources and needs deduplication before the join -- see below.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- Sys.getenv("FORM_OUT", "C:/dev/citiusverse/citiusdata/data")
TAG <- Sys.getenv("FORM_TAG", "SW-baseline")   # shares the env var name with
# form_display_marks.R (default "final" there); this file's own default points
# at swimming's engine output, same "one knob name, two files, different
# defaults" convention as SEQ_TAG in form_ratings.R / form_ratings_swimming.R.
FIT_BEFORE <- as.Date("2025-01-01")
source(here::here("citiusdata", "scripts", "_env.R"))
MIN_N <- .env_int("FORM_OFFSET_MIN_N", 200L)   # shared knob; see header note --
# does not fire on this corpus but the fallback path is kept live.

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
st <- setDT(read_parquet(file.path(OUT, sprintf("seqv2_state_%s.parquet", TAG))))
reg <- as.data.table(citius::citius_events())[sport == "Swimming",
                                               .(event_id, family, orientation, unit)]
stopifnot("swimming event registry is empty" = nrow(reg) > 0,
          "swimming history table is empty" = nrow(h) > 0,
          "swimming state table is empty" = nrow(st) > 0,
          "swimming registry has duplicate event_id - a merge below would fan out" =
            !anyDuplicated(reg$event_id))
n_state0 <- nrow(st)   # retention floor reference; compared against what's finally written

# --- 1. per-event finals offset, fitted on the lead-in only ------------------
# Same MEDIAN-not-mean centring as athletics, for the same reason: a fall or a
# blow-up has no mirror image, so a left-skewed residual pulls a mean offset
# below the true typical race. Not independently re-measured for swimming's
# own skew here (no equivalent 53.9%->51.8% sweep has been run); the mechanism
# is generic and median centring is the more defensible default absent that
# sweep, not a swim-specific finding.
fit <- h[seen == TRUE & rc == "final" & date < FIT_BEFORE & is.finite(perf) & is.finite(r_pre)]
fit[, resid := perf - r_pre]
off <- fit[, .(offset = stats::median(resid), n_fit = .N), by = event_id]
pooled <- fit[, stats::median(resid)]
cat(sprintf("[SW] offset fitted on %s finals rows before %s; pooled %+.4f%%\n",
            format(nrow(fit), big.mark = ","), FIT_BEFORE, 100 * pooled))
off[n_fit < MIN_N, offset := pooled]
cat(sprintf("[SW] %d of %d events use their own offset; %d fall back to pooled (n < %d)\n",
            off[n_fit >= MIN_N, .N], nrow(off), off[n_fit < MIN_N, .N], MIN_N))

# --- 2. apply ----------------------------------------------------------------
st[, athlete_id := as.character(athlete_id)]
n_before <- nrow(st)
st <- merge(st, reg, by = "event_id", all.x = TRUE)
stopifnot("registry merge changed the row count - reg had a duplicate event_id after all" =
            nrow(st) == n_before)
st <- merge(st, off[, .(event_id, offset, n_fit)], by = "event_id", all.x = TRUE)
stopifnot("offset merge changed the row count" = nrow(st) == n_before)
# Two DIFFERENT reasons a row can land here with no fitted offset -- thin (in
# `off` but below MIN_N, already counted above) vs ABSENT (no rows in the
# pre-2025 fit population for that event_id at all, e.g. an event with zero
# finals before FIT_BEFORE). Collapsing both into one silent fallback hid
# which case was occurring; count the absent case separately.
n_no_fit <- sum(is.na(st$offset))
st[is.na(offset), offset := pooled]
if (n_no_fit > 0)
  cat(sprintf("[SW]   additionally, %d row(s) belong to an event_id with ZERO rows in the fit population (not merely thin) -- pooled offset used\n",
              n_no_fit))
if (any(is.na(st$orientation)))
  stop(sprintf("%d rows have no orientation in the registry -- refusing to guess a mark",
               sum(is.na(st$orientation))))

# --- 1b. CROSS-SEX IDENTITY GUARD ---------------------------------------------
# FOUND WHILE BUILDING THIS FILE, NOT PRESENT IN ATHLETICS: a swimmer cannot
# legitimately have a rated result in both a "-M" and a "-W" event, since sex
# is a fixed attribute of who is eligible to swim which race. 996 of 13,537
# state athlete_ids do -- traced one concretely: DENYSKESIL (a real male 200m
# IM swimmer with 11 genuine -M history rows spanning 2015-2026, confirmed
# against swimming_corpus_store) also carries a single SW-200mIndividualMedley
# -W row, sourced (per seqv3_history's block_key) from competition_id "4725",
# round "Heat 2", place 31 -- a genuine women's 200 IM heat at that same
# competition_id has a DIFFERENT athlete (KAMONCHANOKKWANMUANG) at place 31.
# Three male swimmers with similarly deep -M careers (MAHMOUDMOHAMED,
# GROTERSPATRICK, GIANSANTOS) show the identical pattern at the identical
# competition_id, each a single stray -W row. This points to a competition_id
# COLLISION or cross-event join-key bug in the swimming corpus build
# (swimming_corpus_store) -- NOT something wrong with the crosswalk (each of
# these athletes' identity resolves cleanly and consistently) and NOT
# something this display file can fix at the source.
#
# THE GUARD: for every athlete_id present under both sexes, keep only the
# MAJORITY-EVIDENCE sex (by summed n_eff across all their events) and drop the
# minority-sex row(s) entirely. This is deliberately NOT a per-athlete
# hardcoded exclusion list -- the four named swimmers above are illustrations,
# not the rule. WHY THIS DOES NOT FULLY FIX THE PROBLEM, and must be reported
# rather than treated as resolved: this only cleans up which athlete-event row
# is DISPLAYED. The contaminated corpus rows were already fed into
# form_ratings_swimming.R's sequential engine, so the shock/surprise updates
# for whichever OTHER, legitimate athletes shared that race block may have
# been computed against a corrupted field composition -- a display-layer
# guard cannot undo mistraining that already happened upstream. Concentrated
# almost entirely (95%) in the five Individual Medley events (589+265+110+86+
# 65 = 1,115 of 1,174 affected rows across SW-100/200/400mIndividualMedley-M/
# W) -- worth investigating as an IM-specific harvest bug, not a general one.
st[, .sex := sub(".*-", "", event_id)]
.sexev <- st[, .(neff_M = sum(n_eff[.sex == "M"]), neff_W = sum(n_eff[.sex == "W"])),
             by = athlete_id]
.sexev <- .sexev[neff_M > 0 & neff_W > 0]
cat(sprintf("[SW] *** CROSS-SEX GUARD: %d of %s athlete_ids carry both a -M and a -W rated result ***\n",
            nrow(.sexev), format(uniqueN(st$athlete_id), big.mark = ",")))
st <- merge(st, .sexev, by = "athlete_id", all.x = TRUE)
# TIE CASE: neff_M == neff_W gives neither side "< the other", so a strict-less-
# than comparison lets BOTH rows survive -- exactly backwards, since a tie is
# most likely for the thin, single-race contaminated rows this guard exists to
# catch (a debutant wrongly rated in both sexes at n_eff ~= 1 each). There is
# no principled way to pick a side on a genuine tie, so drop BOTH rather than
# keep either -- losing a contaminated athlete from the display is safer than
# showing them under a coin-flip sex.
st[, x_sex_tie := !is.na(neff_M) & neff_M == neff_W]
st[, x_sex_minority := !is.na(neff_M) &
     ((.sex == "M" & neff_M < neff_W) | (.sex == "W" & neff_W < neff_M))]
n_xsex <- st[x_sex_minority == TRUE, .N]
n_xtie <- st[x_sex_tie == TRUE, .N]
if (n_xsex > 0) {
  cat(sprintf("[SW]   dropping %d minority-sex state rows (kept the majority-evidence sex per athlete)\n", n_xsex))
  print(st[x_sex_minority == TRUE, .N, by = event_id][order(-N)])
}
if (n_xtie > 0)
  cat(sprintf("[SW]   dropping %d state row(s) from a TIED cross-sex pair (no majority side to keep)\n", n_xtie))
st <- st[(x_sex_minority == FALSE | is.na(x_sex_minority)) & x_sex_tie == FALSE]
# Post-guard verification, not trusted on the pre-guard count alone: confirm
# zero athlete_ids remain rated under both sexes after the drop above.
.sex_check <- st[, .(sexes = uniqueN(sub(".*-", "", event_id))), by = athlete_id]
stopifnot("cross-sex contamination survived the guard" = nrow(.sex_check[sexes > 1]) == 0)
st[, c(".sex", "neff_M", "neff_W", "x_sex_minority", "x_sex_tie") := NULL]
rm(.sex_check)

# --- 2a. DEPTH CORRECTION on the displayed centre -----------------------------
# Same mechanism as athletics: one median per evidence band, shared across
# events, on top of the per-event offset -- applied to the DISPLAYED CENTRE
# ONLY, never to the ranking key (rank_mark derives from R_rank + offset, no
# band_adj), for the identical reason athletics documents: two athletes on the
# same rating with different evidence would otherwise get reordered by a
# correction that is about what they typically run, not who is better.
#
# KEPT for swimming because the evidence supports it (see header): the fit
# population has >500 rows in 6 of 7 bands. NOT independently validated
# against an out-of-sample window the way athletics' bands were (2025 6.1pp ->
# 2.2pp, 2026 5.3pp -> 2.5pp) -- that sweep has not been run for swimming.
# Ported as the same mechanism on the reasoning that a band correction fit on
# a real, non-thin population is very unlikely to be worse than no correction
# at all, not as a re-confirmed finding.
DEPTH_ADJ <- Sys.getenv("FORM_DEPTH_ADJ", "1") != "0"
.band_of <- function(n) cut(n, c(-Inf, 1, 2, 3, 5, 8, 15, Inf),
                            labels = c("<1", "1-2", "2-3", "3-5", "5-8", "8-15", "15+"))
if (DEPTH_ADJ) {
  fitb <- copy(fit)
  fitb[, band := .band_of(n_eff)]
  dadj <- fitb[, .(band_adj = stats::median(resid - pooled), n_band = .N), by = band]
  dadj[n_band < 500, band_adj := 0]
  st[, band := .band_of(n_eff)]
  st <- merge(st, dadj[, .(band, band_adj)], by = "band", all.x = TRUE)
  st[is.na(band_adj), band_adj := 0]
  cat(sprintf("[SW] depth correction: %d bands, range %+.4f to %+.4f log-perf\n",
              dadj[band_adj != 0, .N], min(dadj$band_adj), max(dadj$band_adj)))
  stopifnot("the depth correction is inert - every band came out zero" =
              any(dadj$band_adj != 0))
} else {
  st[, band_adj := 0]
}
st[, centre := R + offset + band_adj]
st[, pred_mark := exp(orientation * centre)]
st[, raw_mark  := exp(orientation * R)]

# --- 2a-2. IMPOSSIBLE-MARK GUARD -----------------------------------------------
# FOUND WHILE BUILDING THIS FILE, NOT PRESENT IN ATHLETICS: 4 of 36 events had
# their rk=1 (headline, top-of-table) athlete showing a TYPICAL mark faster
# than the fastest time ever recorded anywhere in that event's own corpus
# partition -- e.g. a 41.17s "typical" 100m Individual Medley (world best is
# ~50s) and a 60.94s "typical" 200m IM (world best ~114s). All four are
# Individual Medley events, and two carry a cross-sex name mismatch on top of
# the impossible mark: JANKOVICS Tristan (a men's name) ranked #1 in the
# WOMEN'S 200 IM, and WALSHE Ellen (a women's name) ranked #1 in the MEN'S
# 400 IM. Every one of the affected rows carries n_eff close to 1 -- thin,
# consistent with a SEQ_SEED-only entry rather than real corpus race history.
#
# THIS IS AN UPSTREAM DATA BUG, NOT SOMETHING THIS FILE COMPUTES. The likely
# mechanism is form_ratings_swimming.R's SEQ_SEED step (which resolves
# identity through the crosswalk and reads swimming_careers_store,
# event_id-partitioned like swimming_corpus_store) picking up a seed value
# keyed to the wrong event_id/sex partition -- most plausibly a distance/event
# mislabel concentrated in the IM events specifically, since all four hits are
# IM and nothing else. NOT fixed here: that would mean editing the seeding
# mechanism or the careers-store harvest, both out of scope for a display-
# layer file and owned by form_ratings_swimming.R / the harvest scripts.
#
# WHAT THIS FILE DOES INSTEAD, mirroring the principle athletics already uses
# for its peak-mark cap ("one implausible number on a page costs more than the
# column earns", form_display_marks.R ~line 221) -- applied here to the
# CENTRAL mark rather than a peak, which is the more clear-cut case: a
# "typical" race beating the fastest swim ever recorded in the entire corpus
# for that event is not a rare-but-real outlier the way an exceptional good
# day occasionally can be, it is definitionally not typical. Rows that fail
# this check are DROPPED from the display table entirely (not merely capped),
# because a capped number here would still be a false claim about a specific
# athlete's identity/event, not just an exaggerated one.
.evids <- unique(st$event_id)
bestp <- rbindlist(lapply(.evids, function(EV) {
  f <- file.path(OUT, sprintf("swimming_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(data.table(event_id = EV, best_perf = NA_real_, read_ok = FALSE))
  x <- tryCatch(setDT(read_parquet(f, col_select = "perf")), error = function(e) NULL)
  ok <- !is.null(x) && nrow(x) > 0 && any(is.finite(x$perf))
  data.table(event_id = EV, best_perf = if (ok) max(x$perf, na.rm = TRUE) else NA_real_, read_ok = ok)
}))
# A missing/unreadable/empty partition makes best_perf NA, which makes
# `impossible` FALSE for every row of that event below -- i.e. the guard goes
# silently INACTIVE for that event, not merely conservative. Surface this
# explicitly rather than let "0 impossible rows found" mean either "genuinely
# clean" or "couldn't check" indistinguishably.
n_unread <- bestp[read_ok == FALSE, .N]
cat(sprintf("[SW]   impossible-mark guard: %d of %d event partitions readable\n",
            nrow(bestp) - n_unread, nrow(bestp)))
if (n_unread > 0) {
  cat(sprintf("[SW]   *** %d event(s) had NO readable corpus partition -- guard INACTIVE for these, not verified clean ***\n", n_unread))
  print(bestp[read_ok == FALSE, event_id])
}
st <- merge(st, bestp[, .(event_id, best_perf)], by = "event_id", all.x = TRUE)
st[, impossible := is.finite(centre) & is.finite(best_perf) & centre > best_perf + 1e-9]
n_imp <- st[impossible == TRUE, .N]
if (n_imp > 0) {
  cat(sprintf("[SW] *** IMPOSSIBLE-MARK GUARD: dropping %d of %s state rows whose typical mark beats the all-time corpus best for their event ***\n",
              n_imp, format(nrow(st), big.mark = ",")))
  print(st[impossible == TRUE, .(event_id, athlete_id, n_eff, pred_mark)][order(event_id)])
  cat("[SW]   root cause looks upstream (SEQ_SEED / swimming_careers_store, concentrated in IM events) -- see comment above, not fixed in this file\n")
}
st <- st[impossible == FALSE | is.na(impossible)]
st[, c("best_perf", "impossible") := NULL]

# --- 2b. PEAK MARK: NOT COMPUTED for v1 ---------------------------------------
# See header. Gated the same way athletics gates it (n_eff >= PEAK_MIN_N = 8),
# but only 169 of 83,494 state rows (0.20%) clear that bar -- median n_eff in
# the state table is 1.35 and the 90th percentile of n_eff on FINALS rows is
# 4.48, never mind 8. A "good day" column that renders for 1 in 500 rows is
# not a usable v1 feature (median swimming meet cadence, especially outside
# the taper-meet circuit, does not give this engine the same repeated-final
# history athletics has). peak_mark is still written as a column, all NA, so
# the output schema matches athletics' exactly.
PEAK_MIN_N <- 8L
n_would_qualify <- st[n_eff >= PEAK_MIN_N, .N]
cat(sprintf("[SW] peak-mark SKIPPED for v1: only %d of %s state rows (%.2f%%) reach n_eff >= %d;\n",
            n_would_qualify, format(nrow(st), big.mark = ","),
            100 * n_would_qualify / nrow(st), PEAK_MIN_N))
cat(sprintf("[SW]   median n_eff %.2f, p90 %.2f -- swimming's meet cadence does not support this column yet\n",
            stats::median(st$n_eff), stats::quantile(st$n_eff, 0.9, names = FALSE)))
st[, peak_mark := NA_real_]

# --- 3. names + activity ----------------------------------------------------
# athlete-ratings.parquet (athletics' name source) has ZERO swimming coverage
# -- confirmed empirically the same day this was written. Names come from
# athlete_crosswalk_swimming.parquet's person_id instead, which is the SAME
# identity space as this engine's athlete_id (form_ratings_swimming.R already
# established this: the corpus's athlete_id is resolved through this
# crosswalk at seed time, and 13,536 of 13,537 SW state athlete_ids match a
# person_id here).
#
# The crosswalk carries 3,875 duplicate person_id rows across sources (a
# swimmer harvested from worldaquatics AND swimcloud AND swimengland, etc.),
# so it must be DEDUPED before the join or the merge would fan out rows.
# Preference rule, arbitrary-but-documented past the first tiebreak (same
# discipline form_ratings_swimming.R applies to its own crosswalk joins):
#   1. a row with a non-NA/non-empty athlete_name over one without
#   2. worldaquatics as source over any other (it is the primary source: 64 of
#      the tracked competitions are its own T1_elite catalogue, and it is the
#      source this engine's own SEQ_SEED step resolves through)
#   3. first remaining row, arbitrary
cw <- setDT(read_parquet(file.path(OUT, "athlete_crosswalk_swimming.parquet")))
cw <- cw[sport == "Swimming"]
n_cw0 <- nrow(cw)
# A sport-label mismatch here is a demonstrated real risk in this exact
# codebase (athlete-ratings.parquet was found to have ZERO swimming coverage
# because it's actually athletics-only) -- if it happened here too, cw would
# be empty and !anyDuplicated(character(0)) passes VACUOUSLY, silently
# converting a "no rows" defect into an "every name is NA" outcome with only
# a percentage in a log line as the tell. Fail loud instead.
stopifnot("athlete_crosswalk_swimming.parquet has 0 rows with sport == 'Swimming' - check the source label" =
            n_cw0 > 0)
cw[, has_name := !is.na(athlete_name) & nzchar(athlete_name)]
cw[, src_pref := fifelse(source == "worldaquatics", 1L, 2L)]
setorder(cw, person_id, -has_name, src_pref)
nm <- unique(cw, by = "person_id")[, .(athlete_id = as.character(person_id), athlete_name)]
stopifnot("crosswalk still has duplicate person_id after dedup" = !anyDuplicated(nm$athlete_id))
cat(sprintf("[SW] crosswalk dedup: %s rows -> %s unique person_id (%d duplicates resolved)\n",
            format(n_cw0, big.mark = ","), format(nrow(nm), big.mark = ","),
            n_cw0 - nrow(nm)))
n_before <- nrow(st)
st <- merge(st, nm, by = "athlete_id", all.x = TRUE)
stopifnot("the name join changed the row count" = nrow(st) == n_before)
.name_cov <- 100 * mean(!is.na(st$athlete_name))
cat(sprintf("[SW] name coverage: %.1f%% of state rows matched a crosswalk person_id\n", .name_cov))
if (.name_cov < 95)
  stop(sprintf("name coverage dropped to %.1f%% (expected ~100%%, state athlete_id IS the crosswalk person_id by construction) - check the crosswalk sport filter or an upstream id-format change",
               .name_cov))

# WHO IS SHOWN. Same three-condition mechanism athletics settled on (athlete
# recency + evidence bar + event recency), same env var names -- see header:
# these constants are athletics-fitted and UNVALIDATED for swimming's meet
# calendar (major taper meets vs a domestic short-course circuit that races
# far more often than distance track running does). Copied as a starting
# default, not presented as a swim-specific finding.
ASOF <- max(st$last, na.rm = TRUE)
ACT_ATHLETE <- .env_int("FORM_ACT_ATHLETE_D", "800")
ACT_EVENT   <- .env_int("FORM_ACT_EVENT_D", "800")
ACT_MIN_N   <- .env_num("FORM_ACT_MIN_NEFF", "1")
la <- h[, .(last_any = max(date)), by = athlete_id]
la[, athlete_id := as.character(athlete_id)]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
act <- st[n_eff >= ACT_MIN_N &
          !is.na(last_any) & last_any >= ASOF - ACT_ATHLETE &
          !is.na(last)     & last     >= ASOF - ACT_EVENT]
cat(sprintf("[SW] active: %s of %s athlete-events (as at %s; athlete within %dd, event within %dd, n_eff >= %.1f)\n",
            format(nrow(act), big.mark = ","), format(nrow(st), big.mark = ","),
            ASOF, ACT_ATHLETE, ACT_EVENT, ACT_MIN_N))
stopifnot("no rows survived the activity filter - check ACT_* knobs or upstream state" =
            nrow(act) > 0)
# The activity filter is EXPECTED to remove most rows (that's its job) so it
# gets no retention floor of its own -- but the DATA-QUALITY guards above it
# (cross-sex, impossible-mark) are meant to remove a small, targeted number
# of genuinely-contaminated rows. If a bug in either guard's logic (rather
# than genuine contamination) started flagging a large fraction of the table,
# this is where it would first be visible, before the activity filter's own
# much larger intentional narrowing makes an inflated drop hard to spot.
.retained_pre_activity <- 100 * nrow(st) / n_state0
cat(sprintf("[SW] pre-activity-filter retention: %.1f%% of loaded state rows (%s of %s) survived the data-quality guards\n",
            .retained_pre_activity, format(nrow(st), big.mark = ","), format(n_state0, big.mark = ",")))
if (.retained_pre_activity < 90)
  stop(sprintf("only %.1f%% of state rows survived the cross-sex/impossible-mark guards (expected >90%% -- these guards remove a small targeted fraction, not most of the table) - a guard may be over-firing",
               .retained_pre_activity))

# --- RANK ON R_ceil, no blends applied on top ---------------------------------
# form_ratings_swimming.R already writes R_ceil (same (1-CEIL)*R + CEIL*best
# blend as athletics). No cross-event blend, no combined-event blend, no
# evidence shrinkage exist for swimming v1 (see header) -- R_rank is R_ceil,
# unmodified.
if (!"R_ceil" %in% names(act))
  stop("form_display_marks_swimming.R expects R_ceil from the state table.\n",
       "  Re-run form_ratings_swimming.R against a current engine build.")
act[, R_rank := fifelse(is.finite(R_ceil), R_ceil, R)]
cat(sprintf("[SW] ranking on R_ceil; %s of %s rows fall back to raw R (no best mark)\n",
            format(sum(!is.finite(act$R_ceil)), big.mark = ","),
            format(nrow(act), big.mark = ",")))

act[, rank_mark := exp(orientation * (R_rank + offset))]
setorder(act, event_id, -R_rank)
act[, rk := seq_len(.N), by = event_id]

# Same monotonicity guard as athletics: a ranking table whose displayed mark
# does not move with the rank is unreadable and no aggregate metric catches
# it, so assert it directly.
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
cat(sprintf("[SW] rank_mark monotone with rank in all %d events\n", nrow(.mono)))

fmt <- function(m, unit) {
  ifelse(is.na(m), "  -  ",
  ifelse(!(unit %chin% c("s", "seconds")), sprintf("%.2f", m),
  ifelse(m < 60, sprintf("%.2f", m),
  ifelse(m < 3600, sprintf("%d:%05.2f", floor(m/60), m %% 60),
         sprintf("%d:%02d:%02.0f", floor(m/3600), floor((m %% 3600)/60), m %% 60)))))
}
show <- function(ev, k = 5) {
  e <- act[event_id == ev][rk <= k]
  if (!nrow(e)) return(invisible())
  cat(sprintf("== %s  (offset %+.3f%%, n_fit %s) ==\n", sub("^SW-","",ev),
              100*e$offset[1], format(e$n_fit[1], big.mark=",")))
  for (i in seq_len(nrow(e)))
    cat(sprintf("  %d. %-24s typical %9s   n_eff %.1f\n", e$rk[i],
                substr(ifelse(is.na(e$athlete_name[i]), e$athlete_id[i], e$athlete_name[i]), 1, 24),
                fmt(e$pred_mark[i], e$unit[i]), e$n_eff[i]))
  cat("\n")
}
cat("\n")
for (ev in c("SW-100mFreestyle-M","SW-200mFreestyle-M","SW-400mIndividualMedley-M",
             "SW-800mFreestyle-W","SW-100mButterfly-W")) show(ev)

# --- 4. ANCHORS: a displayed mark is exactly where a wrong transform looks ---
# plausible. Three recent Olympic/Worlds-calibre swimmers in well-covered
# events, spanning sprint/im/distance families -- the same kind of check
# form_ratings_swimming.R's own build already ran successfully (it
# anchor-checked Popovici). Ranges are set from independently known
# career-calibre times (PB plus realistic slack for a "typical", not best,
# rating-implied mark), not from this run's own output.
anchor <- function(ev, who, lo, hi) {
  e <- act[event_id == ev][grep(who, athlete_name, ignore.case = TRUE)][1]
  ok <- nrow(e) > 0 && is.finite(e$pred_mark) && e$pred_mark >= lo && e$pred_mark <= hi
  cat(sprintf("%-28s %-10s %10s in [%g, %g]  %s\n", sub("^SW-","",ev), who,
              if (nrow(e)) sprintf("%.2f", e$pred_mark) else "absent", lo, hi,
              if (isTRUE(ok)) "OK" else "*** FAIL ***"))
  isTRUE(ok)
}
cat("ANCHORS (plausible-range checks on the DISPLAYED mark, swimming)\n")
res <- c(
  # David Popovici, PB 200m Free 1:43.21 (103.21s, 2022 Worlds); a rating-
  # implied typical race is expected slower than a career-best final.
  anchor("SW-200mFreestyle-M", "Popovici", 100, 118),
  # Leon Marchand, PB 400m IM 4:02.50 (242.50s, 2024 Olympics, WR).
  anchor("SW-400mIndividualMedley-M", "Marchand", 235, 275),
  # Katie Ledecky, PB 800m Free 8:04.79 (484.79s, WR).
  anchor("SW-800mFreestyle-W", "Ledecky", 470, 550))
cat(sprintf("\n%d of %d mark anchors hold\n", sum(res), length(res)))
if (!all(res)) stop("a displayed mark is outside its plausible range - check the transform")

# --- 4b. Is 'typical' honest? Simplified sanity check, per the task brief ----
# No good-day column exists to calibrate (see 2b), so this is the simpler
# check the brief allows in that case: does the median-centred typical mark
# land near 50% beaten on an out-of-sample year, the same sanity check
# athletics runs as its "typical" line, without the fuller ZSPREAD/goodday
# machinery this file deliberately does not build.
val <- h[seen == TRUE & rc == "final" & year(date) == 2026 &
         is.finite(perf) & is.finite(r_pre)]
val <- merge(val, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
val[is.na(offset), offset := pooled]
if (DEPTH_ADJ) {
  val[, band := .band_of(n_eff)]
  val <- merge(val, dadj[, .(band, band_adj)], by = "band", all.x = TRUE)
  val[is.na(band_adj), band_adj := 0]
} else val[, band_adj := 0]
val[, centre := r_pre + offset + band_adj]
stopifnot("the 2026 validation window is empty" = nrow(val) > 0)
typ_hit <- val[, mean(perf > centre)]
cat(sprintf("\nCALIBRATION (2026, out of sample):\n"))
cat(sprintf("  'typical'  beaten %.2f%% over %s finals (target 50%%)\n",
            100 * typ_hit, format(nrow(val), big.mark = ",")))
if (abs(typ_hit - 0.50) > 0.05)
  cat("  *** TYPICAL MISCALIBRATED - the central mark is not central ***\n")

cov <- merge(reg[, .(event_id, family)],
             act[, .(active = .N), by = event_id], by = "event_id", all.x = TRUE)
cov[is.na(active), active := 0L]
empty <- cov[active == 0L]
cat(sprintf("\nCOVERAGE: %d of %d swimming registry events have no displayable athlete\n",
            nrow(empty), nrow(cov)))
if (nrow(empty))
  cat(sprintf("  %s\n", paste(sub("^SW-", "", empty$event_id), collapse = ", ")))

stopifnot("rank_mark missing - the table would sort by a column it does not show" =
            "rank_mark" %in% names(act))
# xb_share / xb_sibs / ce_share / ce_imputed carried as constants (no blend
# exists for swimming v1) purely so the output schema matches athletics'
# form_display_<TAG>.parquet exactly -- a downstream reader that already
# tolerates a FORM_TAG-parametrized input needs no code change to read this.
act[, `:=`(xb_share = 0, xb_sibs = 0L, ce_share = 0, ce_imputed = 0L)]
if (!"band_adj" %chin% names(act)) act[, band_adj := 0]
write_parquet(act[, .(event_id, athlete_id, athlete_name, rk, R, R_ceil, offset,
                      band_adj,
                      pred_mark, rank_mark, peak_mark, raw_mark, n_eff, v, last, unit,
                      xb_share, xb_sibs, ce_share, ce_imputed)],
              file.path(OUT, sprintf("form_display_%s.parquet", TAG)))
cat(sprintf("wrote form_display_%s.parquet (%s rows)\n", TAG,
            format(nrow(act), big.mark = ",")))

# Calibration travels as DATA, not as a sentence typed into the page -- same
# discipline as athletics. Simplified per the header: no ZSPREAD/goodday
# fields, since that column is not computed. peak_mark_computed: false is the
# field a downstream reader (or a future re-enablement) should check before
# assuming this file's shape is identical in substance to athletics', even
# though the column names match.
jsonlite::write_json(list(
  window             = "2026 finals, out of sample",
  n_finals           = nrow(val),
  typical_beaten_pct = round(100 * typ_hit, 2),
  centring           = "median",
  peak_mark_computed = FALSE,
  peak_mark_skip_reason = sprintf(
    "only %.2f%% of state rows reach n_eff >= %d (median n_eff %.2f)",
    100 * n_would_qualify / nrow(st), PEAK_MIN_N, stats::median(st$n_eff)),
  depth_adj_applied   = DEPTH_ADJ,
  activity_window     = list(act_athlete_d = ACT_ATHLETE, act_event_d = ACT_EVENT,
                              act_min_neff = ACT_MIN_N, validated_for_swimming = FALSE)),
  file.path(OUT, sprintf("form_display_%s_calib.json", TAG)),
  auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("wrote form_display_%s_calib.json\n", TAG))
