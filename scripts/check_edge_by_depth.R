# WHERE does the form model beat "just use their season best"?
#
# The headline comparison restricts to pairs where every naive predictor exists,
# which forces both athletes to have >=3 prior races IN SEASON — the population
# where naive predictors are strongest. The model exists for the opposite case.
# So measure the edge as a function of evidence depth.
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
h[, yr := year(date)]
setorder(h, athlete_id, event_id, date, race_key)
h[, n_prior    := seq_len(.N) - 1L,        by = .(athlete_id, event_id)]
h[, p_seasbest := shift(cummax(perf), 1L), by = .(athlete_id, event_id, yr)]
h[, p_last     := shift(perf, 1L),         by = .(athlete_id, event_id)]
setorder(h, date, race_key)

s <- h[seen == TRUE & place <= 12 & yr == 2026]
s[, rid := .GRP, by = race_key]
a <- s[, .(rid, i = seq_len(.N), place, n_prior, model = r_pre,
           seas = p_seasbest, last = p_last)]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x",".y"))
m <- m[i.x < i.y & place.x != place.y]
m[, thin := pmin(n_prior.x, n_prior.y)]                 # evidence of the THINNER athlete
m[, band := cut(thin, c(-1,0,1,3,7,15,Inf),
                labels = c("0 (debut)","1","2-3","4-7","8-15","16+"))]
m[, correct_model := sign(model.x - model.y) == sign(place.y - place.x)]
conc <- function(px, py, ok) mean(sign(px[ok]-py[ok]) == sign(m$place.y[ok]-m$place.x[ok]))

r <- m[, {
  okm <- model.x != model.y
  oks <- !is.na(seas.x) & !is.na(seas.y) & seas.x != seas.y
  okl <- !is.na(last.x) & !is.na(last.y) & last.x != last.y
  # score model on the SAME pairs the naive predictor can score, or it is unfair
  both_s <- okm & oks; both_l <- okm & okl
  .(pairs = .N,
    model_all      = round(100*mean(sign(model.x[okm]-model.y[okm]) == sign(place.y[okm]-place.x[okm])), 2),
    seas_cov       = round(100*mean(oks), 1),
    model_on_seas  = round(100*mean(sign(model.x[both_s]-model.y[both_s]) == sign(place.y[both_s]-place.x[both_s])), 2),
    season_best    = round(100*mean(sign(seas.x[both_s]-seas.y[both_s])   == sign(place.y[both_s]-place.x[both_s])), 2),
    model_on_last  = round(100*mean(sign(model.x[both_l]-model.y[both_l]) == sign(place.y[both_l]-place.x[both_l])), 2),
    last_race      = round(100*mean(sign(last.x[both_l]-last.y[both_l])   == sign(place.y[both_l]-place.x[both_l])), 2))
}, by = band][order(band)]
r[, edge_vs_seasbest := round(model_on_seas - season_best, 2)]
r[, edge_vs_last     := round(model_on_last - last_race, 2)]
cat("2026 SEALED WINDOW, by evidence depth of the THINNER athlete in each pair\n")
cat("seas_cov = % of pairs where season-best is even available for both\n\n")
print(r[, .(band, pairs, seas_cov, model_on_seas, season_best, edge_vs_seasbest,
            model_on_last, last_race, edge_vs_last)])
cat(sprintf("\nALL 2026 pairs the model can score: %s, concordance %.2f%%\n",
            format(m[model.x != model.y, .N], big.mark=","),
            100*mean(m[model.x != model.y, sign(model.x-model.y) == sign(place.y-place.x)])))
