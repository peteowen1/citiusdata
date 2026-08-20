# Evidence shrinkage, judged on the metric that actually matters.
#
# WHY THIS FILE EXISTS. `k` was chosen against agreement with the World
# Athletics ranking - precision@N and rank correlation. That is a reference, not
# the referee: WA is another system's opinion, built by a different method, and
# agreeing with it is not the same as being right. The main metric here is
# OUT-OF-SAMPLE WEIGHTED CONCORDANCE - given the athletes who actually lined up,
# did the rating order them the way the race did - and it is ground truth.
#
# THE OBSTACLE, AND WHY IT IS NOT A REASON TO SETTLE FOR WA AGREEMENT. Shrinkage
# is applied in form_display_marks.R to `R_rank`, a current-state value computed
# from all data including the sealed window. Scoring that against sealed races
# would be scoring a number that has already seen them. So the parameter cannot
# be judged by simply re-running the engine's own concordance, which is why the
# WA referee got used in the first place.
#
# It can be judged properly, because the history stores what is needed. `r_pre`
# is the rating an athlete CARRIED INTO a race and `n_eff` is the evidence they
# had at that moment - both strictly pre-race. Applying the same shrinkage to
# r_pre using that n_eff reproduces exactly the transformation the display
# performs, at a point in time where the outcome is genuinely unseen. That is a
# real out-of-sample test of the ranking rule rather than a proxy for one.
#
# THE WEIGHTS ARE THE ENGINE'S, not re-chosen here: majors 40, other T1 12, T2 1,
# non-final x0.5. Copied deliberately rather than tuned, because a metric whose
# weights move with the thing being measured is a formality.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("SHRINK_SEALED_YEAR", "2026")
TUNE <- .env_int("SHRINK_TUNE_YEAR", "2025")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h[, athlete_id := as.character(athlete_id)]
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & place <= 12 &
       is.finite(n_eff)]
h[, yr := year(date)]
stopifnot("history is empty after filtering" = nrow(h) > 10000,
          "the sealed year has no races" = h[yr == SEAL, .N] > 1000)

# --- tier weights, rebuilt from the catalogue exactly as the engine does ------
cp <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("race_key", "competition_id")))
cp[, competition_id := as.character(competition_id)]
cp <- unique(cp, by = "race_key")
cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet"),
                         col_select = c("competition_id", "class", "meet_tier")))
cg[, competition_id := as.character(competition_id)]
cp <- merge(cp, cg, by = "competition_id", all.x = TRUE)
h  <- merge(h, cp[, .(race_key, class, meet_tier)], by = "race_key", all.x = TRUE)
MAJ <- c("olympics", "world_champs", "european_champs", "commonwealth")
h[, w_tier := fifelse(!is.na(class) & class %chin% MAJ, 40,
              fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", 12, 1))]
h[, wt := w_tier * fifelse(rc == "final", 1, 0.5)]
stopifnot("every row must carry a finite weight" = all(is.finite(h$wt)),
          "no major-weighted rows - the catalogue join failed" = any(h$w_tier == 40))
cat(sprintf("%s: %s scored rows | %s major-weighted | sealed %d, tune %d\n", TAG,
            format(nrow(h), big.mark = ","), format(sum(h$w_tier == 40), big.mark = ","),
            SEAL, TUNE))

# --- the shrinkage, applied to the PRE-RACE rating ----------------------------
# Toward the event mean, the same target the display uses. The mean is taken over
# the scored population of that event, and it does not matter that it is a
# constant per event: shrinkage still REORDERS a race, because the weight
# n_eff/(n_eff + k) differs between athletes in the same field. That is the whole
# mechanism - a thin record is pulled further back than a deep one.
h[, mu := mean(r_use), by = event_id]

# ARGUMENT NAMED kv, NOT k. The history carries its own `k` column - the
# per-race learning rate - and inside a data.table `[` that column wins over a
# function argument of the same name. The first version of this shrank by the
# learning rate instead of the parameter and failed with a length error; had the
# column been length-1 compatible it would have run and returned nonsense.
score_k <- function(kv, yr_keep) {
  x <- h[yr %in% yr_keep]
  x[, r_k := if (kv <= 0) r_use else mu + (n_eff / (n_eff + kv)) * (r_use - mu)]
  # EXCLUDE MERGED RACES, for the same reason the engine now does. race_key
  # carries no section identifier, so parallel sections collapse into one race
  # and a duplicated finishing place proves it. Pairs across sections compare
  # athletes who never met - about a tenth of them - and every figure this file
  # produced before 2026-08-20 was computed with them in.
  # A shared place with DIFFERENT marks proves a merge; a shared place with the
# SAME mark is an ordinary tie and the athletes really did compete.
.dup <- x[, .(ath = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
          ath > 1 & marks > 1, unique(race_key)]
  x <- x[!race_key %chin% .dup]
  x[, rid := .GRP, by = race_key]
  a <- x[, .(rid, event_id, i = seq_len(.N), place, r = r_k, wt)]
  m <- merge(a, a, by = c("rid", "event_id"), allow.cartesian = TRUE,
             suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  d <- m$r.x - m$r.y
  m[, cw := as.numeric((d > 0) == (place.x < place.y))]
  m[d == 0, cw := 0.5]
  m[, w := wt.x]                       # both rows of a pair share a race weight
  # Kish effective n, because a weighted percentage on 40x weights does not have
  # the precision its raw pair count suggests - see the noise-floor work.
  ess <- sum(m$w)^2 / sum(m$w^2)
  data.table(k = kv, pairs = nrow(m), ess = round(ess),
             raw = round(100 * mean(m$cw), 3),
             weighted = round(100 * stats::weighted.mean(m$cw, m$w), 3),
             floor = round(100 * sqrt(0.75 * 0.25 / ess), 3))
}

KS <- c(0, 0.25, 0.5, 1, 2, 4)
cat(sprintf("\n=== SEALED %d — the metric that decides ===\n", SEAL))
sealed <- rbindlist(lapply(KS, score_k, yr_keep = SEAL))
sealed[, vs_none := round(weighted - sealed[k == 0, weighted], 3)]
print(sealed)
cat(sprintf("\n=== TUNE %d — a second window, for sign agreement ===\n", TUNE))
tune <- rbindlist(lapply(KS, score_k, yr_keep = TUNE))
tune[, vs_none := round(weighted - tune[k == 0, weighted], 3)]
print(tune)

best <- sealed[which.max(weighted)]
cat(sprintf("\nbest sealed weighted concordance at k = %.2f (%.3f%%)\n",
            best$k, best$weighted))
cat(sprintf("deployed k = 0.5 is %+.3f pp against no shrinkage, on a floor of %.3f pp\n",
            sealed[k == 0.5, vs_none], sealed[k == 0.5, floor]))
cat("\nk = 0 must reproduce the unshrunk ranking exactly. If it does not, the\n")
cat("transformation is wrong and nothing below it means anything.\n")
stopifnot("k = 0 changed the ranking - the shrinkage code is wrong" =
            abs(sealed[k == 0, weighted] - score_k(0, SEAL)$weighted) < 1e-9)
cat("control passes.\n")
cat("\nJudge on SEALED weighted, with the floor beside it, and require the tune\n")
cat("window to agree in sign. A gain smaller than its floor is not a gain.\n")

f <- file.path(D, "shrinkage_concordance.json")
writeLines(jsonlite::toJSON(list(tag = TAG, sealed_year = SEAL, tune_year = TUNE,
                                 sealed = sealed, tune = tune),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
