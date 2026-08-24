# Score the asymmetric-cap arms against the identity arm, on both windows.
#
# THE IDENTITY ARM IS THE FIRST TEST, NOT A FORMALITY. `asym_id` runs with
# SEQ_HUBER_LO unset, which should make the split clip arithmetically identical
# to the symmetric one it replaces. If it is not identical to the deployed arm,
# the refactor changed behaviour and every other number here is measuring that
# instead of the hypothesis. This repo has already lost a week to six arms that
# agreed suspiciously closely because they shared something the reference did
# not - so identity is asserted, not inspected.
#
# BOTH WINDOWS MUST AGREE IN SIGN. The per-family huber this replaces was
# adopted on two windows and refused by a third. One window can move for reasons
# unrelated to the change.
#
# AND THE SCORE CANNOT SEE THE FAILURE THAT MATTERS. Clipping bad days harder
# blunts genuine decline exactly as it blunts falls, and the two are identical in
# the data. check_huber_decline.R is the referee for that; concordance is not.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT   <- here::here("citiusdata", "data")
BASE  <- Sys.getenv("ASYM_BASE", "asym_id")
ARMS  <- strsplit(Sys.getenv("ASYM_ARMS", "asym_id,asym_25,asym_20,asym_15"), ",")[[1]]
SEAL  <- .env_int("ASYM_SEALED", "2026")
TUNE  <- .env_int("ASYM_TUNE",   "2025")

.load <- function(tag) {
  f <- file.path(OUT, sprintf("seqv3_history_%s.parquet", tag))
  stopifnot("missing history for that arm" = file.exists(f))
  d <- setDT(read_parquet(f, col_select = c("race_key","event_id","athlete_id","date",
                                            "r_pre","r_use","place","perf","seen","rc")))
  # SCORE r_use, NOT r_pre. r_pre is the pure rating; r_use is the value the
  # engine actually ORDERS a field with, after the ceiling blend and the
  # cross-event blend. Anything that only touches ordering - the ceiling
  # statistic above all - is invisible in r_pre, and scoring r_pre returned four
  # ceiling arms identical to three decimals including C=0.4 against C=1.0.
  # Identical-to-the-digit across arms that should differ is never a null result;
  # it means the thing under test never reached the metric. The engine records
  # the same failure once before, when XBLEND 0/1/2/3 all scored byte-identically
  # because the blend was being reset before it could be measured.
  if (!"r_use" %chin% names(d)) d[, r_use := r_pre]
  d[!is.finite(r_use), r_use := r_pre]
  d[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
}

# --- IDENTITY: the base arm must match the deployed model -------------------
b <- .load(BASE)
fin <- file.path(OUT, "seqv3_history_final.parquet")
if (file.exists(fin)) {
  d <- setDT(read_parquet(fin, col_select = c("race_key","athlete_id","r_pre","seen")))
  d <- d[seen == TRUE & is.finite(r_pre)]
  k <- c("race_key","athlete_id")
  m <- merge(b[, ..k][, .b := b$r_pre], d[, ..k][, .d := d$r_pre], by = k)
  worst <- if (nrow(m)) max(abs(m$.b - m$.d)) else NA_real_
  cat(sprintf("IDENTITY: %s rows matched with deployed final | largest r_pre difference %.3e\n",
              format(nrow(m), big.mark = ","), worst))
  stopifnot("the identity arm does not reproduce the deployed model - the refactor changed behaviour" =
              is.finite(worst) && worst < 1e-9)
  cat("IDENTITY PASSED: the split clip is arithmetically the symmetric one when unset\n\n")
} else cat("NOTE: no deployed final to compare identity against\n\n")

# --- tier weights, as the engine builds them --------------------------------
cp <- unique(setDT(read_parquet(file.path(OUT, "athletics_corpus.parquet"),
                                col_select = c("race_key","competition_id"))), by = "race_key")
cp[, competition_id := as.character(competition_id)]
cg <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet"),
                         col_select = c("competition_id","class","meet_tier")))
cg[, competition_id := as.character(competition_id)]
wt_of <- merge(cp, cg, by = "competition_id", all.x = TRUE)[, .(race_key, class, meet_tier)]
MAJ <- c("olympics","world_champs","european_champs","commonwealth")

score <- function(d, yr) {
  d <- merge(d[year(date) == yr], wt_of, by = "race_key", all.x = TRUE)
  d[, wt := fifelse(!is.na(class) & class %chin% MAJ, 40,
            fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", 12, 1)) *
            fifelse(grepl("final", rc, ignore.case = TRUE) &
                    !grepl("semi", rc, ignore.case = TRUE), 1, 0.5)]
  # merged races out, as everywhere: a shared place with different marks means
  # parallel sections under one key, and those pairs never met.
  dup <- d[, .(n = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
           n > 1 & marks > 1, unique(race_key)]
  d <- d[!race_key %chin% dup]
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, wt), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  dd <- m$r_use.x - m$r_use.y
  cw <- as.numeric((dd > 0) == (m$place.x < m$place.y)); cw[dd == 0] <- 0.5
  ess <- sum(m$wt.x)^2 / sum(m$wt.x^2)          # Kish, because the weights cost a lot
  data.table(pairs = nrow(m), ess = round(ess),
             weighted = round(100 * stats::weighted.mean(cw, m$wt.x), 3),
             floor = round(100 * sqrt(0.25 / ess), 3))
}

res <- rbindlist(lapply(ARMS, function(tg) {
  d <- .load(tg)
  rbind(cbind(arm = tg, window = sprintf("%d tune", TUNE), score(d, TUNE)),
        cbind(arm = tg, window = sprintf("%d sealed", SEAL), score(d, SEAL)))
}), fill = TRUE)

for (w in unique(res$window)) {
  r <- res[window == w]
  r[, vs_base := round(weighted - r[arm == BASE, weighted], 3)]
  cat(sprintf("=== %s ===\n", w)); print(r[, .(arm, pairs, ess, weighted, vs_base, floor)])
  cat("\n")
}

# --- BY FAMILY, on the arms already built -----------------------------------
# The mechanism is concentrated in the hurdles - fattest left tail, and the only
# family where season best wins - and hurdles are ~10% of pairs, so a global knob
# is diluted by construction. If tighter clipping helps HURDLES on both windows
# while hurting elsewhere, that is a case for a per-family value despite the
# 2026-08-18 reversion, because this one is pre-registered and has a mechanism.
# If it does not help hurdles either, the hypothesis is simply wrong.
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
scoref <- function(d, yr, fam) {
  d <- merge(d[year(date) == yr], reg, by = "event_id", all.x = TRUE)[family == fam]
  if (nrow(d) < 100) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  dd <- m$r_use.x - m$r_use.y
  cw <- as.numeric((dd > 0) == (m$place.x < m$place.y)); cw[dd == 0] <- 0.5
  data.table(pairs = nrow(m), conc = round(100 * mean(cw), 3),
             floor = round(100 * sqrt(0.25 / nrow(m)), 3))
}
cat("=== hurdles only, unweighted, both windows ===
")
for (yr in c(TUNE, SEAL)) {
  rr <- rbindlist(lapply(ARMS, function(tg) cbind(arm = tg, scoref(.load(tg), yr, "hurdles"))), fill = TRUE)
  if (!nrow(rr)) next
  rr[, vs_base := round(conc - rr[arm == BASE, conc], 3)]
  cat(sprintf("-- %d --
", yr)); print(rr[, .(arm, pairs, conc, vs_base, floor)])
}
cat("
=== sprint only, as the control (thinner tail, should benefit less) ===
")
for (yr in c(TUNE, SEAL)) {
  rr <- rbindlist(lapply(ARMS, function(tg) cbind(arm = tg, scoref(.load(tg), yr, "sprint"))), fill = TRUE)
  if (!nrow(rr)) next
  rr[, vs_base := round(conc - rr[arm == BASE, conc], 3)]
  cat(sprintf("-- %d --
", yr)); print(rr[, .(arm, pairs, conc, vs_base, floor)])
}

f <- file.path(OUT, "asym_huber_scores.json")
writeLines(jsonlite::toJSON(res, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("wrote %s\n", basename(f)))
cat("\nA gain counts only if it beats `floor` on BOTH windows and agrees in sign.\n")
cat("Anything inside the floor is the answer too: the knob does not matter.\n")
