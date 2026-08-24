# The cross-event blend cap, judged on the metric that decides.
#
# `FORM_XB_MAXN = 4` says an athlete only borrows from correlated events while
# they have four or fewer effective races of their own. What the repo can support
# today is that a cap HELPS - every capped configuration beat its uncapped
# sibling across a 24-point sweep. What it cannot support is the value 4, which
# was chosen on precision@10 across 24 configurations, and form_display_marks.R
# honestly records the result as an in-sample tie.
#
# precision@10 is 440 slots over 44 events; one athlete moves it 0.2 pp. It is
# also the referee that said evidence shrinkage was fine when out-of-sample
# weighted concordance said it cost 6.5x the noise floor. So the cap deserves the
# same treatment shrinkage got.
#
# THE OBSTACLE, AND THE SAME ANSWER AS SHRINKAGE. The blend runs in the display
# layer on R_rank, a current-state value that has already seen the sealed window,
# so re-running the engine cannot judge it. But the history stores what is needed:
# `r_pre` is the rating an athlete CARRIED INTO a race and `n_eff` the evidence
# they had at that moment. Rebuilding the blend from those, with each sibling
# rating taken as of the race date and never after it, reproduces the display's
# rule at a point where the outcome is genuinely unseen.
#
# MERGED RACES ARE EXCLUDED, as everywhere else: a place shared by different
# marks means parallel sections or age divisions under one key, and pairs across
# them compare athletes who never met.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("BLEND_SEALED_YEAR", "2026")
TUNE <- .env_int("BLEND_TUNE_YEAR",   "2025")
XB_STR    <- .env_num("FORM_XB",        "0.25")
XB_MINCOR <- .env_num("FORM_XB_MINCOR", "0.30")
XB_NSIB   <- .env_int("FORM_XB_NSIB",   "6")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","event_id","athlete_id","date",
                                       "r_pre","r_use","n_eff","place","perf","rc","seen")))
h[, athlete_id := as.character(athlete_id)]
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & place <= 12 &
       is.finite(n_eff) & is.finite(perf)]
h[, yr := year(date)]
stopifnot("history is empty" = nrow(h) > 10000)

# --- tier weights, as the engine builds them ---------------------------------
cp <- unique(setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                                col_select = c("race_key","competition_id"))), by = "race_key")
cp[, competition_id := as.character(competition_id)]
cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet"),
                         col_select = c("competition_id","class","meet_tier")))
cg[, competition_id := as.character(competition_id)]
h <- merge(h, merge(cp, cg, by = "competition_id", all.x = TRUE)[, .(race_key, class, meet_tier)],
           by = "race_key", all.x = TRUE)
MAJ <- c("olympics","world_champs","european_champs","commonwealth")
h[, wt := fifelse(!is.na(class) & class %chin% MAJ, 40,
          fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", 12, 1)) *
          fifelse(rc == "final", 1, 0.5)]

# merged races out
.dup <- h[, .(ath = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
          ath > 1 & marks > 1, unique(race_key)]
h <- h[!race_key %chin% .dup]
cat(sprintf("%s: %s scored rows after removing %s merged races\n", TAG,
            format(nrow(h), big.mark = ","), format(length(.dup), big.mark = ",")))

# --- the sibling ratings, as of the race date and never after ----------------
sim0 <- setDT(read_parquet(file.path(D, "event_similarity_spec.parquet")))
simcol <- intersect(c("cor_use","cor_shrunk","cor"), names(sim0))[1]
stopifnot("similarity table has no correlation column" = !is.na(simcol))
# Stored one row per unordered pair (e1, e2), so mirror it - an event needs to
# find its siblings whichever side of the pair it happens to sit on.
sim <- rbindlist(list(
  sim0[, .(event_id = e1, sib_event = e2, corv = get(simcol))],
  sim0[, .(event_id = e2, sib_event = e1, corv = get(simcol))]))
sim <- sim[is.finite(corv) & corv >= XB_MINCOR & event_id != sib_event]
setorder(sim, event_id, -corv)
sim <- sim[, head(.SD, XB_NSIB), by = event_id]
stopifnot("no event pair clears the correlation gate" = nrow(sim) > 0)

# every rating an athlete carried into a race, keyed for a rolling lookup
# ONE rating per athlete-event-DAY. An athlete contesting heats and a final on
# the same day carries a different r_pre into each, which leaves duplicate join
# keys and fans the rolling join out - caught by the row-count guard below rather
# than silently inflating the sibling set. Keep the last of the day, which is the
# most recent rating actually available.
rt <- h[order(athlete_id, event_id, date), .(athlete_id, event_id, date, r_pre)]
rt <- rt[, .(r_pre = data.table::last(r_pre)), by = .(athlete_id, event_id, date)]
setkey(rt, athlete_id, event_id, date)
# ratings are not comparable between events, so z-score within event first
mu <- rt[, .(m = mean(r_pre), s = stats::sd(r_pre)), by = event_id][is.finite(s) & s > 0]

tg <- h[yr %in% c(SEAL, TUNE)]
ts <- merge(tg[, .(race_key, athlete_id, event_id, date, r_use, n_eff, place, wt, yr)],
            sim[, .(event_id, sib_event, corv)], by = "event_id", allow.cartesian = TRUE)
# roll = Inf takes the most recent sibling rating at or before this race's date,
# so nothing after the race can leak into it
# AN EXPLICIT ROLLING JOIN, not rt[.(athlete_id, sib_event, date), ...]. In that
# form data.table evaluates the i-expression in rt's scope first, so
# `athlete_id` and `date` resolve to RT's columns rather than ts's - the lengths
# then differ (1,005,982 against 2,552,585) and it recycles with remainder,
# silently producing a join against the wrong rows. It warns, and the numbers
# still print. This repo's own rules file records the same trap.
q <- ts[, .(athlete_id, event_id = sib_event, date, .row = .I)]
setkey(q, athlete_id, event_id, date)
rr <- rt[q, roll = Inf, mult = "last"]                     # most recent rating at or before date
stopifnot("the rolling join changed row count" = nrow(rr) == nrow(q))
ts[rr$.row, sib_r := rr$r_pre]
ts <- ts[is.finite(sib_r)]
ts <- merge(ts, mu[, .(sib_event = event_id, sm = m, ss = s)], by = "sib_event")
ts[, sib_z := (sib_r - sm) / ss]
bor <- ts[, .(z_borrow = stats::weighted.mean(sib_z, corv), nsib = .N),
          by = .(race_key, athlete_id, event_id)]
cat(sprintf("rows with at least one sibling rating available: %s of %s\n",
            format(nrow(bor), big.mark = ","), format(nrow(tg), big.mark = ",")))

x <- merge(tg, bor, by = c("race_key","athlete_id","event_id"), all.x = TRUE)
x <- merge(x, mu[, .(event_id, m, s)], by = "event_id", all.x = TRUE)
x[, r_bor := m + z_borrow * s]          # sibling estimate, on this event's scale

score <- function(maxn, yrs) {
  d <- x[yr %in% yrs]
  # the display's weight: XB_STR / (n_eff + XB_STR), only while thin
  d[, w := fifelse(is.finite(r_bor) & is.finite(m) & n_eff <= maxn,
                   XB_STR / (n_eff + XB_STR), 0)]
  d[, r_k := fifelse(w > 0, (1 - w) * r_use + w * r_bor, r_use)]
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r = r_k, wt), by = race_key]
  m2 <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m2 <- m2[i.x < i.y & place.x != place.y]
  if (!nrow(m2)) return(NULL)
  dd <- m2$r.x - m2$r.y
  m2[, cw := as.numeric((dd > 0) == (place.x < place.y))]
  m2[dd == 0, cw := 0.5]
  ess <- sum(m2$wt.x)^2 / sum(m2$wt.x^2)
  data.table(maxn = maxn, moved = d[w > 0, .N], ess = round(ess),
             weighted = round(100 * stats::weighted.mean(m2$cw, m2$wt.x), 3),
             floor = round(100 * sqrt(0.75 * 0.25 / ess), 3))
}
KS <- c(0, 2, 4, 6, 10, 1e9)   # 0 = blend off, 1e9 = blend everyone
cat(sprintf("\n=== SEALED %d — the metric that decides ===\n", SEAL))
s1 <- rbindlist(lapply(KS, score, yrs = SEAL))
s1[, vs_off := round(weighted - s1[maxn == 0, weighted], 3)]
print(s1)
cat(sprintf("\n=== TUNE %d — must agree in sign ===\n", TUNE))
s2 <- rbindlist(lapply(KS, score, yrs = TUNE))
s2[, vs_off := round(weighted - s2[maxn == 0, weighted], 3)]
print(s2)
cat("\nmaxn = 0 is the blend switched off; 1e9 blends every athlete regardless of\n")
cat("evidence. If an interior value beats BOTH ends on both windows, the cap is\n")
cat("real and its location is measured rather than inherited.\n")
f <- file.path(D, "blend_concordance.json")
writeLines(jsonlite::toJSON(list(tag = TAG, sealed = s1, tune = s2),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
