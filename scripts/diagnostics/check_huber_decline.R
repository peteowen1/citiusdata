# Does the robust update blunt GENUINE decline?
#
# SEQ_HUBER caps the step taken from a catastrophic race. That is right when the
# athlete fell and wrong when they are actually finished, and those two are
# IDENTICAL in the data — a fall and a collapse both look like one very slow
# result. The concordance metric will happily reward over-clipping, because
# falls are common and noisy while genuine declines are rare, so the swept
# winner has to be checked against decline specifically before it ships.
#
# The test, pre-specified so it cannot be tuned into agreement:
#   - find every "catastrophe": a seen athlete-race with surprise < -0.10
#     (0.81% of the corpus, ~11% slower than their rating)
#   - score the NEXT 5 races for that athlete-event in each arm
#   - a fall predicts RECOVERY, so if the clipping is right the Huber arm should
#     do better on that window; if it is wrong, it is holding a rating up for
#     athletes who really have dropped and it should do worse
#
# Run after round 3 picks a Huber value, with both arms writing history:
#   SEQ_TAG=hist_ref  SEQ_HUBER=0  SEQ_HIST=1
#   SEQ_TAG=hist_hub  SEQ_HUBER=<winner> SEQ_HIST=1
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D <- "C:/dev/citiusverse/citiusdata/data"
REF <- Sys.getenv("HUB_REF", "seqv3_history_hist_ref.parquet")
HUB <- Sys.getenv("HUB_ARM", "seqv3_history_hist_hub.parquet")
WIN <- .env_int("HUB_WIN", "5")     # races after the catastrophe

load1 <- function(f, lab) {
  x <- setDT(read_parquet(file.path(D, f)))
  x <- x[is.finite(perf) & is.finite(r_pre) & is.finite(place)]
  # SCORE THE ORDERING VALUE for the concordance below. r_pre is right for the
  # residual and bias columns - the engine updates on r_pre - but wrong for
  # deciding whether clipping helped, which is an ORDERING question.
  if (!"r_use" %chin% names(x)) x[, r_use := r_pre]
  x[!is.finite(r_use), r_use := r_pre]
  setorder(x, athlete_id, event_id, date, race_key)
  x[, idx := seq_len(.N), by = .(athlete_id, event_id)]
  x[, arm := lab][]
}
a <- load1(REF, "no huber"); b <- load1(HUB, "huber")

# catastrophes are located in the REFERENCE arm only, so both arms are scored on
# the SAME set of races — locating them per-arm would compare different subsets
a[, resid := perf - r_pre]
cat0 <- a[seen == TRUE & resid < -0.10, .(athlete_id, event_id, hit = idx)]
cat(sprintf("catastrophes (surprise < -0.10) in the reference arm: %s\n",
            format(nrow(cat0), big.mark = ",")))

win <- cat0[, .(athlete_id, event_id, lo = hit + 1L, hi = hit + WIN)]
mark <- function(x) {
  m <- x[win, on = .(athlete_id, event_id), allow.cartesian = TRUE, nomatch = NULL]
  unique(m[idx >= lo & idx <= hi, .(athlete_id, event_id, idx)])
}
keys <- unique(rbind(mark(a), mark(b)))
cat(sprintf("post-catastrophe athlete-races scored: %s\n", format(nrow(keys), big.mark = ",")))

score <- function(x, lab) {
  s <- x[keys, on = .(athlete_id, event_id, idx), nomatch = NULL]
  s <- x[race_key %chin% unique(s$race_key)]        # whole race, for pairs
  s <- s[place <= 12]
  s[, rid := .GRP, by = race_key]
  p <- s[, .(rid, i = seq_len(.N), place, r = r_use)]
  m <- merge(p, p, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  d <- m$r.x - m$r.y
  cw <- as.numeric((d > 0) == (m$place.x < m$place.y)); cw[d == 0] <- 0.5
  # bias on the catastrophe victims themselves: negative = rating still too high
  v <- x[keys, on = .(athlete_id, event_id, idx), nomatch = NULL]
  data.table(arm = lab, pairs = nrow(m), conc = round(100 * mean(cw), 3),
             victim_races = nrow(v),
             victim_bias_pct = round(100 * mean(v$perf - v$r_pre), 3))
}
r <- rbind(score(a, "no huber"), score(b, "huber"))
print(r)
cat("\nconc higher for huber  => clipping the fall was RIGHT (they recovered)\n")
cat("conc higher for no-huber => it is holding ratings up for real decline\n")
cat("victim_bias near 0 is the honest rating; strongly NEGATIVE means the arm\n")
cat("keeps expecting more than the athlete now delivers.\n")
