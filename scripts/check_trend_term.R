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
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
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
  s[, base := 0.7*r_pre + 0.3*sb]                    # the blend just confirmed
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
