# Score any set of engine arms PER EVENT, not in aggregate.
#
# WHY THIS EXISTS. The engine's headline number is a single weighted concordance
# over the whole corpus, and that corpus is ~98% T2 track. An effect concentrated
# in one family is invisible in it: cross-event blending measured +0.029 pp
# globally while being +1.35 pp on the women's 10,000m and -0.27 pp across the
# throws. A global metric reported those two opposite truths as "nothing".
#
# So: run the arms, then run this. The global number says whether a change is
# safe overall; this says where it actually acts, and whether the sign holds on
# a window that took no part in choosing it.
#
# NOTE ON COMPARABILITY. This is NOT the engine's headline metric computed per
# event. It is unweighted pairwise concordance within an event (finishers in the
# top 12 of a race, both with a rating), which is what makes arms comparable to
# each other inside one event. Do not compare its level to the engine's 71.x -
# compare DELTAS between arms.
#
# Usage:
#   ARMS="xbh_0,xbh_1,f_core" Rscript score_by_event.R
#   ARMS="..." YEARS="2022,2023,2024" Rscript score_by_event.R   # untouched window
# The FIRST arm listed is the base every other arm is differenced against.
# Each tag needs seqv3_history_<tag>.parquet, i.e. that arm ran with SEQ_HIST=1.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D     <- here::here("citiusdata", "data")
tags  <- trimws(strsplit(Sys.getenv("ARMS", ""), ",")[[1]])
YEARS <- as.integer(trimws(strsplit(Sys.getenv("YEARS", "2025,2026"), ",")[[1]]))
MINP  <- .env_int("MIN_PAIRS", "300")
stopifnot("ARMS needs at least two tags" = length(tags) >= 2,
          "YEARS parsed to nothing" = length(YEARS) > 0 && all(is.finite(YEARS)))
SCORE_OUT <- Sys.getenv("SCORE_OUT", "")   # optional JSON of the by-family tables
.score_json <- list()
base <- tags[1]
cat(sprintf("base arm: %s | comparing: %s | years: %s\n",
            base, paste(tags[-1], collapse = ", "), paste(YEARS, collapse = ", ")))

# --- are these arms even comparable? -----------------------------------------
# 2026-08-18: a per-family table was built from arms run at 18:32 against a
# baseline run at 15:02, with form_ratings.R edited at 17:20 in between. Every
# delta in it was the parameter PLUS the edit, and three families appeared to
# move without being fitted because their FAM_K0 override had been removed by
# that edit. Two parquets, same schema, same row count, silently incomparable -
# the mismatch was only visible in file mtimes, which nothing checks.
#
# Arms written by an engine that stamps seqv3_meta_<tag>.json can be checked
# properly. Arms older than that stamp cannot, so fall back to mtime, which is
# weak evidence but is exactly the evidence that would have caught this one.
meta_of <- function(tag) {
  f <- file.path(D, sprintf("seqv3_meta_%s.json", tag))
  if (file.exists(f)) jsonlite::fromJSON(f) else NULL
}
mt <- vapply(tags, function(tg)
  as.numeric(file.mtime(file.path(D, sprintf("seqv3_history_%s.parquet", tg)))),
  numeric(1))
ms <- lapply(tags, meta_of); names(ms) <- tags
shas <- vapply(ms, function(m) if (is.null(m$engine_sha)) NA_character_ else m$engine_sha, "")
if (all(!is.na(shas))) {
  if (uniqueN(shas) > 1) {
    print(data.table(arm = tags, engine_sha = substr(shas, 1, 12),
                     written = vapply(ms, function(m) m$written, "")))
    stop("these arms were built by DIFFERENT versions of form_ratings.R - the ",
         "deltas would be the parameter plus the code change. Rebuild the base.")
  }
  cat(sprintf("provenance: all %d arms built by engine sha %s\n",
              length(tags), substr(shas[1], 1, 12)))
} else {
  eng <- file.path(here::here("citiusdata", "scripts"), "form_ratings.R")
  spread_h <- (max(mt) - min(mt)) / 3600
  crossed <- file.exists(eng) &&
    as.numeric(file.mtime(eng)) > min(mt) && as.numeric(file.mtime(eng)) < max(mt)
  cat(sprintf("provenance: %d of %d arms carry no engine stamp; histories span %.1f h\n",
              sum(is.na(shas)), length(tags), spread_h))
  if (crossed)
    cat("  *** WARNING: form_ratings.R was modified BETWEEN the oldest and newest\n",
        "     arm here. The deltas below include that edit. Rebuild the base arm\n",
        "     on current code before drawing any conclusion from them.\n", sep = "")
}

score_arm <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f))
    stop(sprintf("no history for arm '%s' - was it run with SEQ_HIST=1?", tag))
  h <- setDT(read_parquet(f))
  if (!"r_use" %in% names(h)) h[, r_use := r_pre]
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
         year(date) %in% YEARS]
  stopifnot("no rows survived the year filter" = nrow(h) > 0)
  h[, rid := .GRP, by = race_key]
  a <- h[, .(rid, event_id, yr = year(date), i = seq_len(.N), place, r = r_use)]
  m <- merge(a, a, by = c("rid", "event_id", "yr"), allow.cartesian = TRUE,
             suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  d <- m$r.x - m$r.y
  m[, cw := as.numeric((d > 0) == (place.x < place.y))]
  m[d == 0, cw := 0.5]
  m[, .(tag = tag, pairs = .N, conc = 100 * mean(cw)), by = .(event_id, yr)]
}

sc <- rbindlist(lapply(tags, score_arm))
# collapse the requested years into one figure per event per arm
ev <- sc[, .(pairs = sum(pairs), conc = weighted.mean(conc, pairs)),
         by = .(tag, event_id)]
b  <- ev[tag == base, .(event_id, base_conc = conc, pairs)]
x  <- merge(ev[tag != base, .(tag, event_id, conc)], b, by = "event_id")
stopifnot("no shared events between arms" = nrow(x) > 0)
x[, delta := conc - base_conc]
# a per-event noise floor, reported but NOT used as a gate: it answers "could
# this event alone show this?", which is the wrong question when the effect is
# predicted by mechanism and checkable on another window. Judge on sign
# agreement across windows and on the family pattern instead.
x[, noise := 100 * sqrt(0.75 * 0.25 / pairs)]

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
x <- merge(x, reg, by = "event_id", all.x = TRUE)

for (t in tags[-1]) {
  a <- x[tag == t & pairs >= MINP]
  setorder(a, -delta)
  cat(sprintf("\n================ %s vs %s ================\n", t, base))
  cat(sprintf("events scored (>= %d pairs): %d | up %d, down %d\n",
              MINP, nrow(a), sum(a$delta > 0), sum(a$delta < 0)))
  cat(sprintf("pair-weighted mean delta: %+.3f pp\n",
              weighted.mean(a$delta, a$pairs)))

  cat("\n-- by family (where the effect actually lives) --\n")
  fam <- a[, .(events = .N, up = sum(delta > 0), down = sum(delta < 0),
               pooled = round(weighted.mean(delta, pairs), 3)), by = family]
  setorder(fam, -pooled)
  # Keep the measured table so a report can quote it, rather than someone
  # retyping numbers out of a console days later.
  # Store the PER-EVENT deltas too, not only the family totals. Storing the
  # summary alone made "do the same walk events lose on both changes, or
  # different ones?" unanswerable without re-running every arm - which is the
  # difference between a family property and noise. pairs and noise travel with
  # each row so the sample behind a delta is never lost.
  .score_json[[length(.score_json) + 1L]] <- list(
    arm = t, base = base, by_family = fam,
    by_event = a[, .(event_id, discipline, sex, family, pairs,
                     delta = round(delta, 4), noise = round(noise, 4))])
  print(fam)

  cat("\n-- 10 biggest gains --\n")
  print(head(a[, .(discipline, sex, family, pairs,
                   delta = round(delta, 3), noise = round(noise, 3))], 10))
  cat("\n-- 10 biggest losses --\n")
  print(head(a[order(delta), .(discipline, sex, family, pairs,
                               delta = round(delta, 3), noise = round(noise, 3))], 10))
}

if (nzchar(SCORE_OUT) && length(.score_json)) {
  writeLines(jsonlite::toJSON(.score_json, dataframe = "rows", auto_unbox = TRUE,
                              na = "null"), file.path(D, SCORE_OUT))
  cat(sprintf("wrote %s (%d comparison(s))\n", SCORE_OUT, length(.score_json)))
}
