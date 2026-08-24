# Does a MOMENTUM term add anything on top of rating + ceiling?
#
# This is the gap the Werro case pointed at: a recency-weighted filter always
# LAGS a genuine step change, because it can only ever move a fraction of the
# way toward the latest result. An explicit trend can extrapolate. Whether that
# helps or just amplifies noise is an empirical question, so measure it.
#
# All features strictly lagged. g chosen on 2025 only; 2026 confirms.
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
AE <- c("athlete_id","event_id")
h[, p_best  := shift(cummax(perf), 1L), by = AE]
h[, p_sb    := shift(cummax(perf), 1L), by = .(athlete_id, event_id, yr)]
# momentum: recent form minus the rating it was measured against, lagged.
# positive = has been beating their own rating lately = improving.
h[, sup     := perf - r_pre]
h[, mom2    := shift(frollmean(sup, 2, na.rm = TRUE), 1L), by = AE]
h[, mom4    := shift(frollmean(sup, 4, na.rm = TRUE), 1L), by = AE]
h[, npr     := seq_len(.N) - 1L, by = AE]
setorder(h, date, race_key)

run <- function(y, gs) {
  s <- h[seen == TRUE & place <= 12 & yr == y]
  s[, rid := .GRP, by = race_key]
  s[, sb := fifelse(is.na(p_sb), p_best, p_sb)]
  # THE BASELINE MUST BE THE DEPLOYED ORDERING VALUE, not a reconstruction of
  # it. This was 0.7*r_pre + 0.3*sb, which approximates the ceiling blend but
  # is not it: the engine blends the athlete's BEST mark, not their season
  # best, and then applies the cross-event blend on top. So momentum was being
  # judged against a model that is not the one deployed, and "w = 0 is the
  # model as deployed" - which the header claims - was not true.
  s[, base := get(MODEL_COL)]
  s[, m2 := fifelse(is.na(mom2), 0, mom2)]
  s[, m4 := fifelse(is.na(mom4), 0, mom4)]
  a <- s[, .(rid, i = seq_len(.N), place, base, m2, m4)]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x",".y"))
  m <- m[i.x < i.y & place.x != place.y]
  rbindlist(lapply(gs, function(w) rbindlist(lapply(c("m2","m4"), function(k) {
    bx <- m$base.x + w*m[[paste0(k,".x")]]; by <- m$base.y + w*m[[paste0(k,".y")]]
    ok <- bx != by
    data.table(mom = k, g = w, concordance = round(100*mean(
      sign(bx[ok]-by[ok]) == sign(m$place.y[ok]-m$place.x[ok])), 3))
  }))))
}
GS <- c(0, .15, .3, .5, .75, 1)
r <- merge(run(2025, GS)[, .(mom, g, `2025 tuning` = concordance)],
           run(2026, GS)[, .(mom, g, `2026 sealed` = concordance)], by = c("mom","g"))
setorder(r, mom, g)
cat("base = 0.7*rating + 0.3*best mark. g = weight on momentum (g=0 is that base).\n")
cat("mom2 = mean surprise over last 2 races; mom4 = over last 4.\n\n")
print(r)
