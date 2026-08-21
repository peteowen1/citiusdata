# The naive-baseline table, split by MEET TIER - and an estimate of how much
# room is left above the model at all.
#
# Two questions:
#   1. Pete cares about T1. Does the model's edge over "just use their season
#      best" hold up on elite fields, or does it come from ordinary meets?
#   2. Is 80% reachable? Concordance is bounded by the FIELD, not by the model:
#      two athletes a hair apart in true ability split their meetings roughly
#      50/50 no matter how good the rating is. So score pairs by how far apart
#      the model rates them - if closely-matched pairs are near a coin flip and
#      they are a big share of the metric, the ceiling is low and the remaining
#      gap is noise rather than headroom.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h[, competition_id := tstrsplit(race_key, "[|]", keep = 1L)[[1]]]
h <- merge(h, cat0[, .(competition_id, meet_tier, class)], by = "competition_id", all.x = TRUE)
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
MAJ <- c("olympics", "world_champs", "european_champs", "commonwealth")

# --- walk-forward naive predictors, identical to check_naive_baseline.R -------
setorder(h, athlete_id, event_id, date, race_key)
h[, p_last     := shift(perf, 1L),                            by = .(athlete_id, event_id)]
h[, p_best     := shift(cummax(perf), 1L),                    by = .(athlete_id, event_id)]
h[, p_mean3    := shift(frollmean(perf, 3, na.rm = TRUE), 1L), by = .(athlete_id, event_id)]
h[, p_seasbest := shift(cummax(perf), 1L),                    by = .(athlete_id, event_id, yr)]
setorder(h, date, race_key)

PRED <- c(model = MODEL_COL, season_best = "p_seasbest", mean_last3 = "p_mean3",
          last = "p_last", career_best = "p_best")
s <- h[seen == TRUE & place <= 12 & yr %in% c(2025, 2026)]
s <- s[complete.cases(s[, ..PRED])]
s[, nf := .N, by = race_key]; s <- s[nf >= 2]
s[, stratum := fifelse(!is.na(class) & class %chin% MAJ, "majors",
                fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", "T1 other", "T2"))]

pairs_of <- function(dt) {
  dt <- copy(dt)[, rid := .GRP, by = race_key]
  a <- dt[, c(list(rid = rid, i = seq_len(.N), place = place),
              setNames(lapply(PRED, function(cn) dt[[cn]]), names(PRED)))]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m[i.x < i.y & place.x != place.y]
}
score1 <- function(m, k) {
  px <- m[[paste0(k, ".x")]]; py <- m[[paste0(k, ".y")]]
  ok <- px != py
  round(100 * mean(sign(px[ok] - py[ok]) == sign(m$place.y[ok] - m$place.x[ok])), 2)
}
cat("=== ORDERING A FIELD, by meet tier (2025-26, top-12 finishers) ===\n")
out <- rbindlist(lapply(c("majors", "T1 other", "T2"), function(k) {
  m <- pairs_of(s[stratum == k])
  if (!nrow(m)) return(NULL)
  cbind(data.table(tier = k, pairs = nrow(m)),
        as.data.table(setNames(lapply(names(PRED), function(p) score1(m, p)), names(PRED))))
}))
# elite = majors + T1 other, since that is the population Pete cares about
me <- pairs_of(s[stratum != "T2"])
out <- rbind(out, cbind(data.table(tier = "ALL T1 (elite)", pairs = nrow(me)),
             as.data.table(setNames(lapply(names(PRED), function(p) score1(me, p)), names(PRED)))))
print(out)

cat("\n=== IS 80% REACHABLE? concordance by how far apart the model rates them ===\n")
m <- pairs_of(s)
m[, gap := abs(model.x - model.y)]
m[, correct := sign(model.x - model.y) == sign(place.y - place.x)]
m[, band := cut(100 * gap, c(-1, 0.25, 0.5, 1, 2, 4, Inf),
                labels = c("<0.25%", "0.25-0.5%", "0.5-1%", "1-2%", "2-4%", "4%+"))]
r <- m[, .(pairs = .N, share = round(100 * .N / nrow(m), 1),
           concordance = round(100 * mean(correct), 2)), by = band][order(band)]
print(r)
cat(sprintf("\noverall %.2f%%\n", 100 * mean(m$correct)))
cat("\nA pair the model rates within 0.25% apart is nearly a coin flip BY NATURE:\n")
cat("no rating can order two athletes who are genuinely that close. The share of\n")
cat("such pairs sets a hard ceiling that has nothing to do with model quality.\n")
# crude ceiling: assume every band could reach the concordance of the widest one
best <- r[band == "4%+", concordance]
r[, headroom := round((best - concordance) * share / 100, 2)]
cat(sprintf("\nif every band scored like the 4%%+ band (%.2f%%), overall would be %.2f%%\n",
            best, 100 * mean(m$correct) + sum(r$headroom)))
cat("That is an upper bound nothing can reach - close pairs cannot be ordered\n")
cat("as reliably as far-apart ones however good the model gets.\n")
