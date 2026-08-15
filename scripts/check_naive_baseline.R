# What is 68% concordance WORTH? Score the model against naive predictors on the
# IDENTICAL race set and the IDENTICAL pair set.
#
# The recorded ladder compares the form model to itself with flags off, which
# says how much the adjustments earned but not whether the model beats simply
# ordering athletes by their last result. That is the question a reader asks.
#
# Every predictor is strictly walk-forward: built from races BEFORE the one
# being scored, per athlete-event. Pairs are restricted to those where EVERY
# predictor is available, or the comparison is between different populations.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
h[, yr := year(date)]
setorder(h, date, race_key)
h[, seq := .I]

# --- walk-forward naive predictors, per athlete-event, strictly lagged ---
setorder(h, athlete_id, event_id, date, race_key)
h[, n_prior   := seq_len(.N) - 1L,                         by = .(athlete_id, event_id)]
h[, p_last    := shift(perf, 1L),                          by = .(athlete_id, event_id)]
h[, p_best    := shift(cummax(perf), 1L),                  by = .(athlete_id, event_id)]
h[, p_mean3   := shift(frollmean(perf, 3, na.rm = TRUE), 1L), by = .(athlete_id, event_id)]
h[, p_seasbest:= shift(cummax(perf), 1L),                  by = .(athlete_id, event_id, yr)]
setorder(h, date, race_key)

# deployed metric: only finishers placing <= 12 are scored (SEQ_MAXPLACE)
s <- h[seen == TRUE & place <= 12]
PRED <- c(model = "r_pre", last = "p_last", career_best = "p_best",
          mean_last3 = "p_mean3", season_best = "p_seasbest")
s <- s[complete.cases(s[, ..PRED])]          # common availability, all predictors
s[, nf := .N, by = race_key]; s <- s[nf >= 2]
cat(sprintf("scored races %s | athlete-races %s (pairs need every predictor present)\n",
            format(uniqueN(s$race_key), big.mark=","), format(nrow(s), big.mark=",")))

score <- function(dt, lab) {
  dt <- copy(dt)
  dt[, rid := .GRP, by = race_key]
  a <- dt[, c(list(rid = rid, i = seq_len(.N), place = place),
              setNames(lapply(PRED, function(cn) dt[[cn]]), names(PRED)))]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  out <- rbindlist(lapply(names(PRED), function(k) {
    px <- m[[paste0(k, ".x")]]; py <- m[[paste0(k, ".y")]]
    ok <- px != py                            # ties in the predictor cannot order
    conc <- mean(sign(px[ok] - py[ok]) == sign(m$place.y[ok] - m$place.x[ok]))
    data.table(predictor = k, pairs = sum(ok), concordance = round(100 * conc, 2))
  }))
  fav <- rbindlist(lapply(names(PRED), function(k) {
    cn <- PRED[[k]]
    w <- dt[, .(hit = place[which.max(.SD[[1L]])] == min(place)), by = rid, .SDcols = cn]
    data.table(predictor = k, fav_wins = round(100 * mean(w$hit), 1))
  }))
  cbind(window = lab, merge(out, fav, by = "predictor", sort = FALSE))
}
res <- rbind(score(s[yr == 2025], "2025 tuning"), score(s[yr == 2026], "2026 sealed"))
cat("\n"); print(res)
cat("\n50% concordance = coin flip. fav_wins is how often the top-rated athlete wins.\n")
