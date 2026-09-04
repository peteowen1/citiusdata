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
# FOLLOW FORM_TAG. Hardcoded, this scored the DEPLOYED arm no matter what it was
# asked about: run against harvest1 it returned 78.52/78.58 and pair counts
# identical to the previous night's `final` run, to the digit, on an arm holding
# 28,370 more races. Identical-to-the-digit is the tell; the sibling
# check_naive_baseline_by_family.R already did this correctly.
TAG <- Sys.getenv("FORM_TAG", "final")
.hf <- file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))
stopifnot("no history for that FORM_TAG - run the engine with SEQ_TAG first" =
            file.exists(.hf))
cat(sprintf("scoring %s
", basename(.hf)))
h <- setDT(read_parquet(.hf))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
# SCORE THE ORDERING VALUE. r_pre is the bare rating; r_use is that rating plus
# the ceiling and cross-event blends, and it is what the engine actually orders
# a field with. A benchmark asking "is the model better than sorting by season
# best" is asking about ordering, so scoring r_pre understates it by whatever
# those blends are worth. On 2026-08-21 that was enough to invert the hurdles
# result outright, from -0.90 (a loss to season best, which prompted a whole
# evening of investigation) to +0.68. Set BASELINE_PRED=r_pre to score the bare
# rating deliberately.
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
MODEL_COL <- Sys.getenv("BASELINE_PRED", "r_use")
stopifnot("BASELINE_PRED names a column that does not exist" = MODEL_COL %chin% names(h))
cat(sprintf("scoring the model as `%s`\n", MODEL_COL))

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
PRED <- c(model = MODEL_COL, last = "p_last", career_best = "p_best",
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
