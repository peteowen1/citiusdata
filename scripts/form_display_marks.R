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
.env_int <- function(name, default) {
  v <- Sys.getenv(name, ""); if (!nzchar(v)) return(default)
  x <- suppressWarnings(as.integer(v))
  if (is.na(x)) stop(sprintf("%s='%s' is not an integer", name, v)); x
}
MIN_N <- .env_int("FORM_OFFSET_MIN_N", 200L)

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
st[, pred_mark := exp(orientation * (R + offset))]
st[, raw_mark  := exp(orientation * R)]

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
zf <- h[seen == TRUE & rc == "final" & date < FIT_BEFORE &
        is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0]
# z is measured AFTER the same centring the mark uses. Mixing a mean-centred
# offset with a spread taken around zero double-counted the skew.
zf <- merge(zf, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
zf[is.na(offset), offset := pooled]
zf[, z := (perf - r_pre - offset) / sqrt(v_pre)]
q50 <- stats::quantile(zf$z, 0.50); q90 <- stats::quantile(zf$z, 0.90)
ZSPREAD <- unname(q90 - q50)
cat(sprintf("peak spread: empirical q90 %.3f - q50 %.3f = %.3f sd (normal would be %.3f)\n",
            q90, q50, ZSPREAD, stats::qnorm(0.9)))
st[, peak_mark := exp(orientation * (R + offset + ZSPREAD * sqrt(v)))]
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
PEAK_MIN_N <- .env_int("FORM_PEAK_MIN_N", 8L)
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
act <- st[n_eff >= 3 & last >= as.Date("2026-01-01")]
setorder(act, event_id, -R)
act[, rk := seq_len(.N), by = event_id]

fmt <- function(m, unit) {
  ifelse(is.na(m), "  -  ",
  ifelse(unit != "s", sprintf("%.2f", m),
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
val <- h[seen == TRUE & rc == "final" & year(date) == 2026 &
         is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) & v_pre > 0 &
         n_eff >= PEAK_MIN_N]   # score only what the page actually displays
val <- merge(val, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
val[is.na(offset), offset := pooled]
val[, peak_perf := r_pre + offset + ZSPREAD * sqrt(v_pre)]
hit <- val[, mean(perf > peak_perf)]
typ_hit <- val[, mean(perf > r_pre + offset)]
cat(sprintf("\nCALIBRATION (2026, out of sample), over %s finals:\n", format(nrow(val), big.mark=",")))
cat(sprintf("  'typical'  beaten %.2f%% of the time (target 50%%)\n", 100*typ_hit))
cat(sprintf("  'good day' beaten %.2f%% of the time (target 10%%) -> about 1 in %.1f\n",
            100*hit, 1/hit))
# The page must state the MEASURED frequency, not the nominal one. Median
# centring fixed the skew half of this; the rest is structural. ZSPREAD is ONE
# pooled quantile of z applied to athletes whose variances differ, so it cannot
# be a 90th percentile for any of them individually — the mixture is
# over-dispersed and it under-covers. Refitting the spread to force 10% would
# have to be tuned on 2026, and spending the sealed window on a display label
# is a bad trade.
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
write_parquet(act[, .(event_id, athlete_id, athlete_name, rk, R, offset,
                      pred_mark, peak_mark, raw_mark, n_eff, v, last, unit)],
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
  goodday_one_in     = round(1 / hit, 1),
  peak_label         = PEAK_LABEL,
  zspread            = round(ZSPREAD, 4),
  centring           = "median"),
  file.path(OUT, sprintf("form_display_%s_calib.json", TAG)),
  auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("wrote form_display_%s_calib.json (peak label: \"%s\")\n", TAG, PEAK_LABEL))
