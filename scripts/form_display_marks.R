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
MIN_N <- as.integer(Sys.getenv("FORM_OFFSET_MIN_N", "200"))

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
st <- setDT(read_parquet(file.path(OUT, sprintf("seqv2_state_%s.parquet", TAG))))
reg <- as.data.table(citius::citius_events())[, .(event_id, family, orientation, unit)]

# --- 1. per-event finals offset, fitted on the lead-in only ------------------
fit <- h[seen == TRUE & rc == "final" & date < FIT_BEFORE & is.finite(perf) & is.finite(r_pre)]
fit[, resid := perf - r_pre]
off <- fit[, .(offset = mean(resid), n_fit = .N), by = event_id]
pooled <- fit[, mean(resid)]
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
    cat(sprintf("  %d. %-24s %9s   (raw %9s)  n_eff %.1f\n", e$rk[i],
                substr(e$athlete_name[i],1,24), fmt(e$pred_mark[i], e$unit[i]),
                fmt(e$raw_mark[i], e$unit[i]), e$n_eff[i]))
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
cat(sprintf("\n%d of %d mark anchors hold\n", sum(res), length(res)))
# A wrong transform and an uncovered event both produce "no sensible mark", and
# they need opposite responses: the first is a bug, the second is missing data.
# So assert marks only where there IS coverage, and report coverage separately.
if (!all(res)) stop("a displayed mark is outside its plausible range - check the transform")

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
                      pred_mark, raw_mark, n_eff, last, unit)],
              file.path(OUT, sprintf("form_display_%s.parquet", TAG)))
cat(sprintf("wrote form_display_%s.parquet (%s rows)\n", TAG,
            format(nrow(act), big.mark = ",")))
