# Season best sits within ~1pp of the full model. It carries something the
# rating does not: an athlete's CEILING, where the rating tracks their average.
# If that is real and not redundant, blending should beat either alone.
#
# w is chosen on 2025 (tuning) ONLY. 2026 is the sealed window and confirms
# direction; it never picks w.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
# SCORE THE ORDERING VALUE. r_pre is the bare rating; r_use is that rating after
# the ceiling and cross-event blends, and it is what the engine actually orders
# a field with. Any concordance, win-rate or accuracy number here must use
# r_use; r_pre understates the model. On 2026-08-21/22 this same confusion
# inverted four separate conclusions - the hurdles "losing" to season best, the
# model "losing" on thin records, a pooled margin of 1.15 that is 1.79, and a
# "semi-final deficit" that does not exist. Set BASELINE_PRED=r_pre to score
# the bare rating deliberately.
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
MODEL_COL <- Sys.getenv("BASELINE_PRED", "r_use")
stopifnot("BASELINE_PRED names a column that does not exist" = MODEL_COL %chin% names(h))
cat(sprintf("scoring the model as `%s`\n", MODEL_COL))

h[, yr := year(date)]
setorder(h, athlete_id, event_id, date, race_key)
h[, p_seasbest := shift(cummax(perf), 1L), by = .(athlete_id, event_id, yr)]
# fall back to a 365-day rolling best so coverage is not restricted to in-season
h[, p_bestrec := shift(cummax(perf), 1L), by = .(athlete_id, event_id)]
setorder(h, date, race_key)

run <- function(y) {
  s <- h[seen == TRUE & place <= 12 & yr == y]
  s[, rid := .GRP, by = race_key]
  s[, sb := fifelse(is.na(p_seasbest), p_bestrec, p_seasbest)]   # full coverage
  a <- s[, .(rid, i = seq_len(.N), place, model = get(MODEL_COL), sb)]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x",".y"))
  m <- m[i.x < i.y & place.x != place.y]
  # blend only where sb exists for both; elsewhere the blend IS the model
  ok <- !is.na(m$sb.x) & !is.na(m$sb.y)
  cat(sprintf("\n%d: %s pairs, season/career best available for both in %.1f%%\n",
              y, format(nrow(m), big.mark=","), 100*mean(ok)))
  rbindlist(lapply(c(0, .1, .2, .3, .4, .5, .65, .8, 1), function(w) {
    bx <- fifelse(ok, (1-w)*m$model.x + w*m$sb.x, m$model.x)
    by <- fifelse(ok, (1-w)*m$model.y + w*m$sb.y, m$model.y)
    g <- bx != by
    data.table(w = w, concordance = round(100*mean(
      sign(bx[g]-by[g]) == sign(m$place.y[g]-m$place.x[g])), 3))
  }))
}
t25 <- run(2025); t26 <- run(2026)
res <- merge(t25[, .(w, `2025 tuning` = concordance)],
             t26[, .(w, `2026 sealed` = concordance)], by = "w")
cat("\nw = 0 is the model as deployed; w = 1 is pure best-mark.\n\n")
print(res)
b <- t25[which.max(concordance)]
cat(sprintf("\n2025 picks w = %.2f (+%.3f pp over deployed); 2026 at that w: %+.3f pp\n",
            b$w, b$concordance - t25[w==0, concordance],
            t26[w == b$w, concordance] - t26[w==0, concordance]))
