# The benchmark table, BY FAMILY.
#
# The pooled version says the model beats season-best by 0.90 points. That is a
# single number over a corpus that is ~98% track, and this session has shown
# three times over that such a number hides opposite effects. Where the model is
# weakest by family is also where per-family parameters would earn their keep,
# so this is the input to that work rather than a decoration.
#
# Same construction as check_naive_baseline.R: every predictor strictly
# walk-forward per athlete-event, and pairs restricted to those where EVERY
# predictor is available, or the comparison is between different populations.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
# WHICH COLUMN IS "THE MODEL". r_pre is the pure rating; r_use is what the
# engine ORDERS A FIELD WITH, after the ceiling and cross-event blends. The
# question this table answers - "is the model better than sorting by season
# best" - is a question about ordering, so r_use is the honest answer and
# r_pre understates the model by whatever those blends are worth. Set
# BASELINE_PRED=r_pre to score the bare rating instead.
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
MODEL_COL <- Sys.getenv("BASELINE_PRED", "r_use")
stopifnot("BASELINE_PRED must name a column that exists" = MODEL_COL %chin% names(h))
cat(sprintf("scoring the model as `%s`
", MODEL_COL))
h[, yr := year(date)]

setorder(h, athlete_id, event_id, date, race_key)
h[, p_last     := shift(perf, 1L),                            by = .(athlete_id, event_id)]
h[, p_best     := shift(cummax(perf), 1L),                    by = .(athlete_id, event_id)]
h[, p_mean3    := shift(frollmean(perf, 3, na.rm = TRUE), 1L), by = .(athlete_id, event_id)]
h[, p_seasbest := shift(cummax(perf), 1L),                    by = .(athlete_id, event_id, yr)]
setorder(h, date, race_key)

# A pure head-to-head rating, from build_h2h_rating.R. It knows only who beat
# whom - no times at all - which is exactly the comparison worth having, because
# the form model is fitted on times and scored on ordering.
hf <- file.path(OUT, sprintf("h2h_history_%s.parquet", TAG))
if (file.exists(hf)) {
  e <- setDT(read_parquet(hf, col_select = c("race_key","athlete_id","event_id",
                                             "elo_pre","elo_n")))
  e[, athlete_id := as.character(athlete_id)]
  # only where a prior rating exists, matching how every other predictor is
  # treated - a debut carries no information for any of them
  e[elo_n < 1, elo_pre := NA_real_]
  h <- merge(h, e[, .(race_key, athlete_id, event_id, p_h2h = elo_pre)],
             by = c("race_key", "athlete_id", "event_id"), all.x = TRUE)
  cat(sprintf("head-to-head rating joined on %.0f%% of rows
",
              100 * mean(!is.na(h$p_h2h))))
} else {
  h[, p_h2h := NA_real_]
  cat("no head-to-head file - run build_h2h_rating.R
")
}

PRED <- c(model = MODEL_COL, head_to_head = "p_h2h", season_best = "p_seasbest",
          mean_last3 = "p_mean3", last = "p_last", career_best = "p_best")
s <- h[seen == TRUE & place <= 12]
s <- s[complete.cases(s[, ..PRED])]
s[, nf := .N, by = race_key]; s <- s[nf >= 2]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
s <- merge(s, reg, by = "event_id", all.x = TRUE)
cat(sprintf("scored races %s | athlete-races %s\n",
            format(uniqueN(s$race_key), big.mark = ","),
            format(nrow(s), big.mark = ",")))
# FAIL LOUDLY ON AN EMPTY JOIN. Run against an arm with no h2h file for that
# tag, the head-to-head merge drops every row, this prints "scored races 0"
# and the script carries on to die several steps later with an unrelated-
# looking error about a missing column. Zero scored races is never a valid
# state, and the obscure downstream error is what makes it expensive.
stopifnot("no scored races - a join dropped everything, most likely the h2h file for this FORM_TAG" =
            nrow(s) > 1000)

score <- function(dt) {
  if (!nrow(dt)) return(NULL)
  dt <- copy(dt); dt[, rid := .GRP, by = race_key]
  a <- dt[, c(list(rid = rid, i = seq_len(.N), place = place),
              setNames(lapply(PRED, function(cn) dt[[cn]]), names(PRED)))]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  rbindlist(lapply(names(PRED), function(k) {
    px <- m[[paste0(k, ".x")]]; py <- m[[paste0(k, ".y")]]
    ok <- px != py
    if (!sum(ok)) return(NULL)
    data.table(predictor = k, pairs = sum(ok),
               conc = round(100 * mean(sign(px[ok] - py[ok]) ==
                                       sign(m$place.y[ok] - m$place.x[ok])), 2))
  }))
}

cat("\n=== correct pairs by family, 2026 sealed window ===\n")
sealed <- s[yr == 2026]
byfam <- rbindlist(lapply(sort(unique(sealed$family)), function(f) {
  r <- score(sealed[family == f])
  if (is.null(r)) return(NULL)
  r[, family := f][]
}))
# pairs differs slightly per predictor (ties cannot order), so it must NOT be
# part of the cast key or every predictor lands on its own row
np <- byfam[predictor == "model", .(family, pairs)]
w <- dcast(byfam, family ~ predictor, value.var = "conc")
w <- merge(np, w, by = "family")
w <- w[, .(family, pairs, model, head_to_head, season_best, mean_last3,
           last, career_best)]
w[, h2h_vs_model := round(head_to_head - model, 2)]
w[, edge_over_SB := round(model - season_best, 2)]
setorder(w, -edge_over_SB)
print(w)

cat("\n=== the same, pooled, for reference ===\n")
p <- score(sealed)
print(p)
cat("\nedge_over_SB is where the model earns its keep. A family where it is\n")
cat("negative is one where sorting athletes by their season best is better,\n")
cat("and is the first place per-family parameters should be aimed.\n")

f <- file.path(OUT, "benchmark_by_family.json")
jsonlite::write_json(list(sealed_window = 2026, by_family = w, pooled = p),
                     f, auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nwrote %s\n", basename(f)))
