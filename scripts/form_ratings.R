# THE FORM MODEL -- sequential walk-forward ratings. See docs/plans/FORM-MODEL.md
# for the method, its validation, and the adjustment ladder. This answers "how
# good are you RIGHT NOW"; the career model (estimate_ability) answers "how good
# are you over a full record" and keeps the forecasts.
#
# Sequential walk-forward engine v3. All athletics events, T1+T2, 2020->now,
# ONE global chronological sweep so cross-event information is available at the
# moment it is needed. Every adjustment is a flag; 2025 races are the TUNING
# window, 2026 the CONFIRMATION window (score both, tune only ever on 2025).
# v3 over v2: per-athlete variance learned at the same rate as the mean (with a
# floor so a lucky streak cannot collapse it), and a walk-forward majors-finals
# scorecard (concordance, favourite, medal hits) written per run.
#
# Flags (env): SEQ_CENS   censor weight for negative surprise in heats/semis/qual
#                         (1 = off; 0.3 = a cruise counts 30% on the way down)
#              SEQ_AGE    1 = drift ratings along the family aging curve between
#                         appearances (exact curve difference; NA age = no drift)
#              SEQ_STALE  1 = evidence decays with time away (n_eff, family
#                         half-life), so k recovers after a layoff
#              SEQ_XEV    1 = cold-start from a same-family sibling event rating
#                         (mean-shift mapping, blended 50/50 with the first race)
#              SEQ_KT1    k multiplier at T1 meets (1 = off)
#              SEQ_WINDCS 1 = wind-adjust the first race at cold start
suppressMessages(library(data.table)); suppressMessages(library(arrow))
# Numeric knobs from the environment, safely. An env var set to the EMPTY string
# is not unset: Sys.getenv returns "" rather than the default, and as.numeric("")
# is NA — so `SEQ_MAXPLACE=""` silently gave MAXPLACE = NA and the run died deep
# in the loop (2026-08-15). Worse, an empty SEQ_K0 would have run the whole model
# with an NA learning rate. Treat empty as unset, and refuse garbage loudly.
.env_num <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) return(default)
  x <- suppressWarnings(as.numeric(v))
  if (!is.finite(x)) stop(sprintf("%s='%s' is not a finite number", name, v))
  x
}
OUT <- "C:/dev/citiusverse/citiusdata/data"
SC  <- Sys.getenv("FORM_OUT", here::here("citiusdata", "data"))
# Defaults are the 2026-08-14 swept optimum (see docs/plans/FORM-MODEL.md):
# k0 0.95 and floor 0.32 both moved; kappa 3 was already optimal. The old
# eye-chosen 0.55 / 3 / 0.18 scored 68.028 on the 2025 tuning window; these
# score 68.564, and 67.353 -> 68.018 on the sealed 2026 window.
K0 <- .env_num("SEQ_K0", 0.95); KAPPA <- .env_num("SEQ_KAPPA", 3)
KFLOOR <- .env_num("SEQ_KFLOOR", 0.32); CSHRINK <- .env_num("SEQ_C", 4)
# The ladder winners are ON by default, so a bare run IS the chosen model rather
# than the model minus its adjustments. Set SEQ_AGE=0 / SEQ_STALE=0 / SEQ_CENS=1
# to turn them off. (Leaving them opt-in is how 350,401 fitted race effects sat
# inert on every shipped number — dormant by flag, which no wiring guard sees.)
CENS <- .env_num("SEQ_CENS", 0.3); AGEF <- Sys.getenv("SEQ_AGE","1") != "0"
STALE <- Sys.getenv("SEQ_STALE","1") != "0"; XEV <- Sys.getenv("SEQ_XEV","") != ""
KT1 <- .env_num("SEQ_KT1", 1); WINDCS <- Sys.getenv("SEQ_WINDCS","") != ""
# SEQ_CEIL  weight on an athlete's BEST MARK SO FAR, blended into the value used
# to ORDER a field: r_use = (1-CEIL)*r_pre + CEIL*best. Season best where the
# athlete has raced this year, career best otherwise, both strictly lagged.
#
# The rating tracks an athlete's AVERAGE; the best mark tracks their CEILING,
# and the two are not redundant — ordering by best mark alone scores 77.22% on
# the 2026 sealed window against the model's 78.05%, i.e. it is nearly as good
# on its own while being wrong in different places. Offline sweep on 2025 gave a
# clean interior peak at 0.30 (+0.332 pp), confirmed at +0.278 pp on 2026.
#
# PREDICTION ONLY. The update below deliberately still runs on r_pre: feeding a
# blended value back into R would make the rating chase its own ceiling, and the
# two would co-drift with nothing anchoring the level.
# ADOPTED 2026-08-15 at 0.30 after an end-to-end A/B: 69.127 -> 69.427 tuning,
# 69.387 -> 69.669 sealed, favourite 52.7% -> 53.2%. SEQ_CEIL=0 is bit-identical
# to the pre-blend engine (verified: it reproduced 69.127 / 69.387 exactly).
CEIL <- .env_num("SEQ_CEIL", 0.30)
# SEQ_SEED  1 = initialise a debut rating from results already held in the
# careers store (4,978,201 rows against the corpus's 1,225,339 — the corpus is
# roughly a quarter of what is on disk). 27.9% of 2026 cold-start athlete-events
# have a prior SAME-EVENT result, 92.2% of those within two years.
#
# Cold starts are 28.7% of the scored metric at 52.94% while every other depth
# band sits at 74–77%, so this is the only large lever left. See
# check_cold_coverage.R / check_cold_recency.R.
#
# The seeded races are NOT added to the corpus — they only set what an athlete
# carries INTO their first scored race. The scored set is unchanged, so the
# metric stays comparable and any corpus-quality reason for their exclusion
# (tier filter is the likely one) stays contained.
# ADOPTED 2026-08-16: on by default. Sealed window 70.267 -> 71.214 (+0.947 pp)
# and MAJORS FINALS 70.84 -> 73.89 (+3.05 pp), favourite 47.1% -> 51.9%, medal
# hits 60.5% -> 64.9%. Set SEQ_SEED=0 to turn it off.
SEEDON <- Sys.getenv("SEQ_SEED","1") != "0"
# 45, not 365 (2026-08-16). Bracketed on the WEIGHTED metric: 365 -> 71.847,
# 180 -> 71.323 (at 20/8), 45 -> 72.019, 21 -> 72.000, 10 -> 71.357 — worse on
# both sides. +0.172 over 365 against a 0.118 pp noise floor. An earlier reading
# that majors preferred 365 was noise: that metric has a ~0.39 pp floor and the
# spread being read off it was 0.35 pp.
SEEDHL <- .env_num("SEQ_SEEDHL", 45)    # half-life in days for the weighted mean
SEEDNE <- .env_num("SEQ_SEEDNE", 5)     # cap on seeded n_eff, so it still learns fast
# SEQ_HUBER  robust update. 0 = off. Otherwise a surprise larger than
# HUBER x the athlete's OWN sd has its step capped there, so a catastrophe moves
# the rating by a bounded amount instead of a proportional one.
#
# Motivating case: Werro, European Indoors final 2025-03-09, ran 2:27.37 off a
# 2:01.39 rating — a 4.9-sigma miss — while the other five finished within 1.3%
# of theirs. She fell. Her rating went 2:01.4 -> 2:07.6 in one afternoon and took
# four races to recover. Results >11% off a rating are 0.81% of the corpus and
# the residual distribution is left-skewed (-0.47): nobody runs 18% FAST.
#
# The tension worth remembering before tuning this: a fall and a genuine
# collapse are IDENTICAL in the data. Clipping the tail also blunts real
# decline, so a lower HUBER is not automatically better even if it scores
# better — check what it does to athletes who really did fall off.
#
# Heat censoring is a crude special case of this (bad qualifiers only); Huber is
# the general form and applies in finals too, which is where falls hurt most.
# 3. Huber 2 scores 0.047 pp higher on the tuning window and 3 scores 0.039 pp
# higher on the sealed one, both inside a 0.118 pp noise floor — so the pair is
# not separable and the score cannot choose. 3 is taken because it is the value
# check_huber_decline.R validated, and because it clips LESS aggressively, which
# is the conservative side of the one failure this knob has that the metric
# cannot see: blunting a genuine collapse. Both clearly beat off (+0.202/+0.155).
HUBER <- .env_num("SEQ_HUBER", 3)
# SEQ_VPRIOR  1 = derive the thin-record variance prior from WITHIN-ATHLETE
# variation instead of within-race spread. Default off until A/B'd.
#
# The old prior was the median within-race variance of `perf` — how spread out
# DIFFERENT athletes are in one race. `v_pre` is supposed to be ONE athlete's
# race-to-race variation. Those are different quantities, and the first is
# **5.19x larger** than the second at the median event, up to 12.9x for the
# throws (shot put M: prior sd 8.93% of a mark against a learned 2.44%) — for
# the obvious reason that a shot put final spans 15m to 22m while any one
# athlete varies by ~2.4%.
#
# Estimated as median over athletes with >= 8 races of var(diff(perf))/2.
# Differencing removes the athlete's level AND any slow improvement trend, which
# a plain var(perf) would wrongly bank as race-to-race noise. Correlates 0.968
# (log scale) with what deep records actually learn, and unlike the learned
# value it is computable from the corpus, so it is not circular.
#
# VPADJ: the estimator runs 1.63x larger than the learned variance because
# var(diff) retains the race shock while v_pre is the variance of the
# SHOCK-ADJUSTED surprise. Dividing puts the prior on the scale the model
# actually learns on, so a thin record starts where a deep one ends.
# ON by default (2026-08-16). Costs 0.034 pp on the weighted metric — inside its
# 0.118 pp noise floor, so effectively free — and takes the "good day" column
# from being beaten 12.19% of the time to 10.06%, i.e. it becomes a genuine 90th
# percentile rather than one in name. Set SEQ_VPRIOR=0 to revert.
VPRIOR <- Sys.getenv("SEQ_VPRIOR","1") != "0"
VPADJ  <- .env_num("SEQ_VPADJ", 1.63)
VPMINA <- .env_num("SEQ_VPMINA", 20)   # min athletes before an event is trusted
# SEQ_KPOW  scale the initial learning rate by how NOISY the event is:
#   k0_event = k0 * (median_sd / event_sd) ^ KPOW
#
# One knob, not one per family, because the mechanism says what the shape should
# be rather than leaving it to be fitted. A filter should learn SLOWLY from a
# noisy measurement and FAST from a precise one, and athletics events differ by
# 2.7x in exactly that: measured within-athlete sd is 2.44% of a mark in the
# shot put against 0.90% in the 60m hurdles. One global k0 cannot be right for
# both, and until now every event has used the same one.
#
# KPOW = 0 is the current behaviour exactly (identity check). 1 is full inverse
# scaling. The per-event sd comes from the same within-athlete estimate the
# variance prior uses, so this needs SEQ_VPRIOR on - which it is by default.
KPOW <- .env_num("SEQ_KPOW", 0)
TAG <- Sys.getenv("SEQ_TAG","baseline")
# SEQ_WINP  1 = compute win probabilities and Brier. Default OFF: the draws cost
#           ~60s of a ~360s run (measured) and nothing reads the accumulators.
# SEQ_HIST  1 = write the per-race r_pre history (see below).
WINP <- Sys.getenv("SEQ_WINP","") != ""
# SEQ_MAXPLACE  score only finishers placing <= this (0 = off, score everyone).
# A METRIC change, not a model change: updates still use the whole field, so an
# athlete's own rating still learns from their race whatever they placed. The
# question it answers is whether the form model can carry road racing once we
# stop grading it on ordering the back of a 500-runner field.
#
# ON BY DEFAULT AT 12 since 2026-08-15 (Pete's call). The model exists to say who
# wins and who medals; scoring whether it ranked 40th against 41st in a big field
# measures something nobody wants. 12 = a full track final plus a couple of
# places. Set SEQ_MAXPLACE=0 to score the whole field.
#
# CAPPING MAKES THE METRIC EASIER, so a capped number is NOT comparable to an
# uncapped one. Only capped-vs-capped on the same cap is a fair read, and every
# figure recorded before 2026-08-15 is UNCAPPED. The cap value cannot be chosen
# by maximising the metric — a smaller cap scores higher mechanically.
#
# Verified not to move the model: the full knob grid re-run at cap 12 puts every
# optimum where it was uncapped (k0 0.95, kappa 3, floor 0.32), largest deviation
# 0.02, so adopting it required no re-tuning.
MAXPLACE <- as.integer(.env_num("SEQ_MAXPLACE", 12))
HIST <- Sys.getenv("SEQ_HIST","") != ""
FROM <- as.Date("2020-01-01")

cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
cat0 <- cat0[meet_tier %in% c("T1_elite","T2_strong"), .(competition_id, meet_tier, class)]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
ag <- readRDS(file.path(OUT, "aging.rds"))
curves <- as.data.table(ag$curves)
agefun <- lapply(split(curves, curves$family), function(cv) approxfun(cv$age, cv$effect, rule = 2))
cal <- readRDS(file.path(OUT, "calibration_corpus_csigma_coast_keyfix.rds"))
wb <- as.data.table(cal$wind)[, .(event_id, beta)]
HFAM <- c(road = 1095, walk = 730); HDEF <- 365

evs <- setdiff(sub("^event_id=","",list.dirs(file.path(OUT,"athletics_corpus_store"),recursive=FALSE,full.names=FALSE)), "__unmatched__")
dl <- list()
for (EV in evs) {
  x <- tryCatch(setDT(read_parquet(file.path(OUT, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV)),
        col_select = c("athlete_id","competition_id","date","perf","mark","place","race_key","round","age","wind"))),
        error = function(e) NULL)
  if (is.null(x)) next
  x[, `:=`(event_id = EV, athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
  dl[[EV]] <- x
}
d <- rbindlist(dl, fill = TRUE); rm(dl); invisible(gc())
d <- merge(d, cat0, by = "competition_id")
d <- d[!is.na(perf) & !is.na(date) & !is.na(race_key) & !is.na(place) & place > 0 & date >= FROM]
d <- merge(d, reg, by = "event_id", all.x = TRUE)
d <- merge(d, wb, by = "event_id", all.x = TRUE)
d[, rc := fifelse(grepl("semi", round, ignore.case=TRUE), "semi",
        fifelse(grepl("heat|round 1|qual", round, ignore.case=TRUE), "heat", "final"))]
d[, hl := fifelse(!is.na(family) & family %chin% names(HFAM), HFAM[family], HDEF)]
setorder(d, date, race_key)
cat(sprintf("[%s] %s rows | %s races | %s athlete-events\n", TAG,
    format(nrow(d), big.mark=","), format(uniqueN(d$race_key), big.mark=","),
    format(uniqueN(paste(d$athlete_id, d$event_id)), big.mark=",")))

MU <- d[, .(mu = mean(perf)), by = event_id]; MUv <- setNames(MU$mu, MU$event_id)
# variance prior: within-race spread per event (median of race-level var), the
# broadest honest starting uncertainty -- narrows only with an athlete's own evidence
VP <- d[, .(v = var(perf)), by = .(event_id, race_key)][is.finite(v),
        .(vp = stats::median(v)), by = event_id]
VPv <- setNames(VP$vp, VP$event_id)
if (VPRIOR) {
  # computed on a COPY: d must stay in (date, race_key) order, since the
  # boundary scan that drives the whole sweep is built from its row order
  dd <- d[, .(athlete_id, event_id, date, race_key, perf)]
  setorder(dd, athlete_id, event_id, date, race_key)
  dv <- dd[, if (.N >= 8L) .(vd = stats::var(diff(perf))/2) else NULL,
           by = .(athlete_id, event_id)]
  est <- dv[is.finite(vd) & vd > 0, .(vp = stats::median(vd)/VPADJ, n_ath = .N),
            by = event_id][n_ath >= VPMINA]
  # events too thin for their own estimate keep a SHRUNK version of the old
  # prior rather than the wide one - the failure being fixed is worst exactly
  # where evidence is thinnest, so falling back to the old value would leave
  # combined events (the 9,126-point decathlon) untouched.
  cmp <- merge(data.table(event_id = names(VPv), old = as.numeric(VPv)), est, by = "event_id")
  shrink <- stats::median(cmp$old / cmp$vp)
  newv <- VPv
  newv[] <- as.numeric(VPv) / shrink
  newv[est$event_id] <- est$vp
  cat(sprintf("[%s] variance prior: %d of %d events from within-athlete data, %d shrunk by %.2fx
",
      TAG, nrow(est), length(VPv), length(VPv) - nrow(est), shrink))
  cat(sprintf("[%s]   median prior sd %.2f%% -> %.2f%% of a mark
", TAG,
      100*(exp(sqrt(stats::median(as.numeric(VPv))))-1),
      100*(exp(sqrt(stats::median(as.numeric(newv))))-1)))
  VPv <- newv
  rm(dd, dv, est, cmp); invisible(gc())
}
# per-event k0, derived from that same variance
K0v <- setNames(rep(K0, length(VPv)), names(VPv))
if (KPOW != 0) {
  sd_ev <- sqrt(as.numeric(VPv)); ref <- stats::median(sd_ev)
  k0_raw <- K0 * (ref / sd_ev)^KPOW
  # bounded: k > 1 means the update overshoots PAST the race it just saw, and a
  # rate near zero freezes an event entirely. Neither is a rate, so clamp.
  K0v[] <- pmin(pmax(k0_raw, 0.25), 1.30)
  o <- order(K0v)
  cat(sprintf("[%s] per-event k0 (KPOW %.2f): range %.3f-%.3f, median %.3f
",
      TAG, KPOW, min(K0v), max(K0v), stats::median(K0v)))
  cat(sprintf("[%s]   slowest: %s | fastest: %s
", TAG,
      paste(sprintf("%s %.2f", sub("^AT-","",names(K0v)[utils::head(o,3)]), K0v[utils::head(o,3)]), collapse=", "),
      paste(sprintf("%s %.2f", sub("^AT-","",names(K0v)[utils::tail(o,3)]), K0v[utils::tail(o,3)]), collapse=", ")))
}
R <- new.env(parent=emptyenv()); NE <- new.env(parent=emptyenv())
V <- new.env(parent=emptyenv())   # EW variance of own surprises; prior = event pop
LD <- new.env(parent=emptyenv()); LE <- new.env(parent=emptyenv())
BYA <- new.env(parent=emptyenv())
# Running best perf per athlete-event: career, and within the current season.
# Updated AFTER a race is scored, so reads are always strictly lagged.
BC <- new.env(parent=emptyenv()); BS <- new.env(parent=emptyenv())
BSY <- new.env(parent=emptyenv())
key <- function(a, e) paste0(a, "|", e)

# --- SEQ_SEED: pre-populate state from held results -------------------------
# Done ONCE before the loop rather than per race: an athlete-event's seed is a
# function of its FIRST corpus date, which is known up front, so there is
# nothing to look up mid-sweep. Setting R/NE/LD/BC here means the athlete is
# simply `seen` when they first appear — no special case in the hot loop.
n_seeded <- 0L
if (SEEDON) {
  cf <- list.files(file.path(OUT, "athletics_careers_store"), pattern = "[.]parquet$",
                   recursive = TRUE, full.names = TRUE)
  ca <- rbindlist(lapply(cf, function(f) tryCatch(setDT(read_parquet(f,
          col_select = c("athlete_id","date","perf","discipline","sex"))),
          error = function(e) NULL)), fill = TRUE)
  ca <- ca[!is.na(perf) & is.finite(perf) & !is.na(date)]
  ca[, athlete_id := as.character(athlete_id)]
  ca[, event_id := paste0("AT-", gsub("[^A-Za-z0-9]", "", discipline), "-", sex)]
  ca <- ca[event_id %chin% names(MUv)]
  fd <- d[, .(first_date = min(date)), by = .(athlete_id, event_id)]
  sd0 <- ca[fd, on = .(athlete_id, event_id), allow.cartesian = TRUE, nomatch = NULL]
  # STRICTLY earlier than the first scored race, or the gain is leakage
  sd0 <- sd0[date < first_date]
  sd0[, w := 2^(-as.numeric(first_date - date) / SEEDHL)]
  sg <- sd0[, .(r0 = sum(w * perf) / sum(w), ne0 = min(sum(w), SEEDNE),
                best0 = max(perf), last0 = max(date)), by = .(athlete_id, event_id)]
  sg <- sg[is.finite(r0) & is.finite(ne0)]
  # ANCHOR: a seed is a mark in the same event, so it must land near that
  # event's mean. A systematic offset would mean the two `perf` conventions
  # differ (orientation, units) and the seeds are nonsense dressed as numbers.
  sg[, dev := r0 - MUv[event_id]]
  cat(sprintf("[%s] seed: %s athlete-events | median dev from event mean %+.4f (|dev|>1 in %.2f%%)
",
      TAG, format(nrow(sg), big.mark = ","), stats::median(sg$dev),
      100 * mean(abs(sg$dev) > 1)))
  stopifnot(abs(stats::median(sg$dev)) < 0.5)
  kz <- key(sg$athlete_id, sg$event_id)
  for (i in seq_len(nrow(sg))) {
    K <- kz[i]
    R[[K]] <- sg$r0[i]; NE[[K]] <- sg$ne0[i]
    LD[[K]] <- as.numeric(sg$last0[i]); BC[[K]] <- sg$best0[i]
  }
  n_seeded <- nrow(sg)
  rm(ca, sd0, sg); invisible(gc())
}
# All i<j index pairs where the two placings differ. Replaces
# CJ(i=,j=)[i<j][place[i]!=place[j]], whose cost is data.table dispatch overhead
# rather than the pair arithmetic.
# Order-sensitive 31-bit hash of a race key, for a reproducible per-race seed.
# Position-weighted so an anagram or a shared prefix does not collide, and it
# reads the whole key rather than a truncation.
.rk_seed <- function(k) {
  cp <- utf8ToInt(k)
  h <- 5381
  # multiplier kept small on purpose: h is < 2^31 and R does this in doubles,
  # so h * 16777619 overflows 2^53 and silently loses bits (measured: only 74.9%
  # of keys got a distinct seed). h * 131 stays under 2^39 and is exact.
  for (i in seq_along(cp)) h <- (h * 131 + cp[i]) %% 2147483647
  as.integer(h)
}
.pairs <- function(n, place) {
  if (n < 2L) return(list(i = integer(0), j = integer(0)))
  ii <- rep.int(seq_len(n - 1L), (n - 1L):1L)
  jj <- sequence((n - 1L):1L, 2:n)
  keep <- place[ii] != place[jj]
  list(i = ii[keep], j = jj[keep])
}

# Dedup ONCE rather than per race (measured: 27s over 165,133 races). It must
# come after MU/VP above, which are deliberately computed on the undeduped table.
# Keeping the first row per (race, athlete) matches the per-race unique(by=) it
# replaces, because d is sorted and split preserves within-group row order.
d <- unique(d, by = c("race_key", "athlete_id"))
# Group BOUNDARIES instead of split(). split() built 165,133 data.tables and held
# them all at once: measured 110MB from a 16.6MB source, a 6.6x blowup, which is
# what let two concurrent arms exhaust memory on 2026-08-14. Boundaries plus
# plain vectors allocate one small slice per race and nothing in between.
#
# CONTIGUITY. A boundary scan assumes every race's rows are adjacent; split()
# never required that, and the corpus race_key carries no date
# (`comp|event||round|section`), so a key spanning two dates lands in two blocks.
# This is NOT hypothetical: `7174333|10229522||11|4` (100mH W round 1, 2023) has
# five rows on 2023-08-03 and one athlete mis-dated 2023-08-01. A naive from:to
# range over it would have swallowed 186 races / 1,387 rows into one "race" --
# the same failure as the merged-heats corpus bug, silently.
#
# So force contiguity instead of assuming it: number the blocks under the
# (date, race_key) order, give every row its key's FIRST block number, then
# stable-sort on that. Each key becomes one block, keys stay in first-appearance
# order, and within a key the row order is preserved -- which is exactly what
# split(sorted = FALSE) produced, including dt0 taking the earliest date.
d[, .blk0 := rleid(race_key)]
d[, .first := .blk0[1L], by = race_key]
setorder(d, .first)                      # data.table's sort is stable
blk <- rleid(d$race_key)
starts <- which(!duplicated(blk))
ends   <- c(starts[-1L] - 1L, length(blk))
if (uniqueN(d$race_key) != length(starts))
  stop(sprintf(paste0("race_key still not contiguous after the stabilising sort: ",
                      "%s keys in %s blocks -- the grouping logic is wrong."),
               format(uniqueN(d$race_key), big.mark = ","),
               format(length(starts), big.mark = ",")))
# Columns as plain vectors, extracted once. Order of groups is (date, race_key),
# identical to what split(sorted = FALSE) produced on the sorted table; a
# sequential model changes its answer if the order changes, so the bit-identical
# regression run is what proves it.
Vath <- d$athlete_id; Vperf <- d$perf; Vplace <- d$place; Vrc <- d$rc
Vage <- d$age; Vwind <- d$wind; Vbeta <- d$beta; Vhl <- d$hl
Vev <- d$event_id; Vdate <- d$date; Vfam <- d$family
Vtier <- d$meet_tier; Vcls <- d$class; Vrk <- d$race_key
# Dates as plain numbers for the hot loop, and the year precomputed. Both were
# recomputed per athlete or per race from Date objects; year() in particular
# goes through an IDate conversion every time it is called.
Vdaten <- as.numeric(d$date); Vyr <- year(d$date)

# conc counts a TIE as 0.5 (standard concordance). Before 2026-08-16 the rule
# was `(r_pre[i] > r_pre[j]) == (place[i] < place[j])`, which is FALSE on a tie,
# so a tied pair scored correct only when row i finished BEHIND row j — i.e. it
# was decided by corpus row order, not by the model. Two cold-start athletes in
# one race carry the identical event mean, so this hit 53,582 pairs (6.9% of the
# 2026 metric) and they scored 41.37%, below chance. See check_coldstart_share.R.
#
# _bs/_mx/_bc split every pair by whether BOTH athletes carried a rating in, one
# did, or NEITHER did — so a cold-start change can be scored on the band it
# actually targets instead of diluted across a metric that is 71% established
# athletes. The ladder's "cross-event cold start is dead" verdict was measured
# on the undiluted metric and is not established.
.a0 <- c(conc=0,pairs=0,fav=0,nr=0,brier=0,brier_base=0,npred=0,
         conc_bs=0,pairs_bs=0, conc_mx=0,pairs_mx=0, conc_bc=0,pairs_bc=0,
         # weighted concordance, plus the sums needed for its EFFECTIVE sample
         # size: ESS = (sum w)^2 / sum(w^2). Heavy upweighting of a small
         # stratum crushes ESS, so the metric must report how much resolving
         # power it actually has rather than implying the full pair count.
         conc_w=0, w_sum=0, w_sq=0)
acc <- list(y25 = .a0, y26 = .a0)
# Per-race rating history (SEQ_HIST=1). r_pre is the rating an athlete CARRIED
# INTO the race — the only version that answers an out-of-sample question. The
# final state written below has already absorbed every race you would test it
# against, so measuring against that is circular (learned 2026-08-14).
# Preallocated vectors, not a growing list: the maj[[length+1]] pattern is fine
# for 757 majors finals but would add per-object overhead across 165,133 races.
NR <- if (HIST) nrow(d) else 0L
H <- list(race_key = character(NR), date = numeric(NR), event_id = character(NR),
          athlete_id = character(NR), r_pre = numeric(NR), r_use = numeric(NR),
          n_eff = numeric(NR),
          v_pre = numeric(NR), perf = numeric(NR), place = integer(NR),
          rc = character(NR), seen = logical(NR))
hi <- 0L
MAJ <- c("olympics","world_champs","european_champs","commonwealth")
# WEIGHTED CONCORDANCE. The unweighted metric is 98% ordinary meets, so it tunes
# for exactly the races the verse does not exist to predict — and the majors
# scorecard cannot substitute: 757 finals is 33,240 pairs, a noise floor of
# ~0.25pp optimistic and ~0.39pp with within-race correlation, against effects
# of 0.18-0.35pp. It literally cannot choose between arms.
#
# Championships are rare by construction (~120 major finals a year), so no
# harvesting fixes that. The only way to let majors DRIVE a decision while
# keeping an estimate stable enough to resolve the effect is to weight.
#
# These weights are a judgement call and are FIXED HERE, before any arm runs.
# Tuning them until a favoured arm wins would make the metric a formality.
# Raised from 20/8 to 40/12 (Pete, 2026-08-16). At 20/8/1 championships carried
# 16.3% of the metric; at 40/12/1 they carry 27.2% and T2 drops 77.1% -> 64.5%,
# for almost no precision cost (noise 0.079 -> 0.118 pp, still well under the
# 0.17-0.26 pp effects being measured). Chosen over 20/8/0.5, which reaches a
# similar share by suppressing everything else rather than lifting the races we
# care about, and lands at the same noise. Past ~40% majors the noise floor
# collides with the effects and the metric stops being able to choose at all.
W_MAJ <- .env_num("SEQ_W_MAJ", 40)   # olympics / worlds / euros / commonwealth
W_T1  <- .env_num("SEQ_W_T1",  12)   # other T1_elite: diamond league, world indoor
W_T2  <- .env_num("SEQ_W_T2",   1)   # T2_strong
W_RND <- .env_num("SEQ_W_RND", 0.5)  # multiplier for a non-final round
# --- metric weight per row, enumerated and asserted -------------------------
# Computed up front rather than inline so that EVERY combination present in the
# corpus is visible and checked. A fall-through that quietly assigns the T2
# weight to an uncatalogued major would bias the metric in the exact direction
# the weighting exists to correct, and nothing downstream would show it.
d[, w_tier := fifelse(!is.na(class) & class %chin% MAJ, W_MAJ,
              fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", W_T1, W_T2))]
d[, w_rnd := fifelse(rc == "final", 1, W_RND)]
d[, wt := w_tier * w_rnd]
wtab <- d[, .(races = uniqueN(race_key), rows = .N, weight = wt[1]),
          by = .(class = fifelse(is.na(class), "(uncatalogued)", class),
                 meet_tier, rc)][order(-weight, -races)]
cat(sprintf("[%s] METRIC WEIGHTS -- every race type present, %d combinations:
", TAG, nrow(wtab)))
print(wtab)
# rc is derived by regex and can only be final/semi/heat, but assert it rather
# than trust it: a new round label would silently become a "heat".
stopifnot("every row must carry a finite weight" = all(is.finite(d$wt)),
          "rc must be one of final/semi/heat"    = all(d$rc %chin% c("final","semi","heat")),
          "no weight may be zero"                = all(d$wt > 0))
cat(sprintf("[%s] weight check: all %s rows weighted, range %g to %g
",
            TAG, format(nrow(d), big.mark=","), min(d$wt), max(d$wt)))
Vwt <- d$wt
MAJ_FROM <- as.Date("2021-01-01")   # hoisted: was re-parsed on every race
maj <- list()
t0 <- Sys.time()
for (r_ in seq_along(starts)) {
  i1 <- starts[r_]; i2 <- ends[r_]
  # one check, on already-deduped rows; the original checked, deduped, rechecked
  if (i2 - i1 + 1L < 3L) next
  ii <- i1:i2
  # A plain list, not a data.table: `$` on a list is a pointer read, while every
  # data.table access pays class dispatch. The eight per-athlete columns are
  # sliced; the six read only at [1] keep a length-1 slice, which leaves every
  # z$col[1] in the body below working unchanged.
  z <- list(athlete_id = Vath[ii], perf = Vperf[ii], place = Vplace[ii],
            rc = Vrc[ii], age = Vage[ii], wind = Vwind[ii], beta = Vbeta[ii],
            hl = Vhl[ii],
            event_id = Vev[i1], date = Vdate[i1], family = Vfam[i1],
            meet_tier = Vtier[i1], class = Vcls[i1], race_key = Vrk[i1],
            wt = Vwt[i1])
  dt0n <- Vdaten[i1]; yr <- Vyr[i1]
  a <- z$athlete_id; ev <- z$event_id[1]; kk <- key(a, ev); dt0 <- z$date[1]
  mu <- MUv[[ev]]
  r_pre <- numeric(length(a)); n_eff <- numeric(length(a)); seen <- logical(length(a))
  fam1 <- z$family[1]
  agef <- if (AGEF && !is.na(fam1)) agefun[[fam1]] else NULL
  for (m in seq_along(a)) {
    v <- R[[kk[m]]]
    if (is.null(v)) { r_pre[m] <- mu; n_eff[m] <- 0; next }
    seen[m] <- TRUE
    gap <- dt0n - LD[[kk[m]]]
    if (!is.null(agef) && !is.na(z$age[m])) {
      {
        le <- LE[[kk[m]]]
        eff_now <- agef(z$age[m])
        if (!is.null(le) && !is.na(le)) v <- v + (eff_now - le)
        LE[[kk[m]]] <- eff_now
      }
    }
    ne <- NE[[kk[m]]]
    if (STALE) ne <- ne * 2^(-gap / z$hl[m])
    r_pre[m] <- v; n_eff[m] <- ne
  }
  # r_use is what ORDERS the field; r_pre is what the model learns from.
  r_use <- r_pre
  if (CEIL > 0) for (m in seq_along(a)) {
    if (!seen[m]) next
    bsy <- BSY[[kk[m]]]
    b <- if (!is.null(bsy) && bsy == yr) BS[[kk[m]]] else BC[[kk[m]]]
    if (!is.null(b)) r_use[m] <- (1 - CEIL) * r_pre[m] + CEIL * b
  }
  vp0 <- VPv[[ev]]; if (is.null(vp0) || !is.finite(vp0)) vp0 <- stats::var(z$perf)
  v_pre <- numeric(length(a))
  for (m in seq_along(a)) { vv <- V[[kk[m]]]; v_pre[m] <- if (is.null(vv)) vp0 else vv }
  if (HIST) {
    ix <- hi + seq_along(a); hi <- hi + length(a)
    H$race_key[ix] <- z$race_key[1]; H$date[ix] <- as.numeric(dt0)
    H$event_id[ix] <- ev;            H$athlete_id[ix] <- a
    H$r_pre[ix] <- r_pre;            H$n_eff[ix] <- n_eff
    H$r_use[ix] <- r_use;
    H$v_pre[ix] <- v_pre;            H$perf[ix] <- z$perf
    H$place[ix] <- z$place;          H$rc[ix] <- z$rc
    H$seen[ix] <- seen
  }
  slot <- if (yr == 2025L) "y25" else if (yr == 2026L) "y26" else NA
  if (!is.na(slot)) {
    # All i<j pairs as plain integer vectors. CJ() cost ~0.4ms per call in fixed
    # data.table dispatch overhead regardless of field size (measured: 79x at
    # n=8), paid once per scored race.
    sel <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg <- .pairs(length(sel), z$place[sel])
    g <- list(i = sel[gg$i], j = sel[gg$j])   # map back to full-field indices
    if (length(g$i)) {
      di <- r_use[g$i] - r_use[g$j]
      pl <- z$place[g$i] < z$place[g$j]
      cw <- as.numeric((di > 0) == pl); cw[di == 0] <- 0.5    # tie = half credit
      acc[[slot]]["conc"] <- acc[[slot]]["conc"] + sum(cw)
      acc[[slot]]["pairs"] <- acc[[slot]]["pairs"] + length(g$i)
      si <- seen[g$i]; sj <- seen[g$j]
      bs <- si & sj; bc <- !si & !sj; mx <- !bs & !bc
      acc[[slot]]["conc_bs"] <- acc[[slot]]["conc_bs"] + sum(cw[bs])
      acc[[slot]]["pairs_bs"] <- acc[[slot]]["pairs_bs"] + sum(bs)
      acc[[slot]]["conc_mx"] <- acc[[slot]]["conc_mx"] + sum(cw[mx])
      acc[[slot]]["pairs_mx"] <- acc[[slot]]["pairs_mx"] + sum(mx)
      acc[[slot]]["conc_bc"] <- acc[[slot]]["conc_bc"] + sum(cw[bc])
      acc[[slot]]["pairs_bc"] <- acc[[slot]]["pairs_bc"] + sum(bc)
      wt <- z$wt
      acc[[slot]]["conc_w"] <- acc[[slot]]["conc_w"] + wt * sum(cw)
      acc[[slot]]["w_sum"]  <- acc[[slot]]["w_sum"]  + wt * length(cw)
      acc[[slot]]["w_sq"]   <- acc[[slot]]["w_sq"]   + wt * wt * length(cw)
      # favourite: ties at the top are broken at random, so credit the expected
      # hit rate rather than whichever athlete which.max happened to return
      rs <- r_use[sel]; ps <- z$place[sel]; tm <- which(rs == max(rs))
      acc[[slot]]["fav"] <- acc[[slot]]["fav"] + mean(ps[tm] == min(ps))
      acc[[slot]]["nr"] <- acc[[slot]]["nr"] + 1
      # WIN PROBABILITIES from rating + own-variance draws. The shared race
      # shock cancels from ordering, so it is deliberately absent.
      # OFF by default (SEQ_WINP=1): measured at ~60s of a ~360s run, and the
      # accumulators it feeds are never written or printed. Nothing else in the
      # loop draws randomness, so skipping it cannot move a scored metric.
      # NOTE the seed is badly collided (25,793 races -> 203 seeds on the 800m W;
      # see check_form_seed_collisions.R) and the first-race variance is
      # mis-initialised — fix both before trusting any Brier from this block.
      if (WINP) {
      # Order-sensitive hash of the WHOLE key. The old seed was
      # sum(utf8ToInt(substr(race_key, 1, 20))), which failed twice over:
      # 20 characters truncates at or before the round, so a meet's rounds and
      # sections hashed alike, and summing character codes discards order AND
      # compresses ~20 ASCII values into a ~300-wide band. Measured on
      # AT-800Metres-W: 25,793 distinct races produced 203 distinct seeds, the
      # largest collision group covering 554 races. set.seed() is global, so any
      # two races sharing a seed and a field size drew an IDENTICAL matrix --
      # their win probabilities were the same random numbers, not independent
      # draws. See check_form_seed_collisions.R.
      set.seed(.rk_seed(z$race_key[1]))
      nf <- length(a)
      dr <- matrix(rnorm(1000L * nf), 1000L, nf) * rep(sqrt(v_pre), each = 1000L) +
            rep(r_use, each = 1000L)
      wins <- tabulate(max.col(dr), nf)
      p_gold <- wins / 1000
      hit <- as.integer(z$place == min(z$place))
      acc[[slot]]["brier"] <- acc[[slot]]["brier"] + sum((p_gold - hit)^2)
      acc[[slot]]["brier_base"] <- acc[[slot]]["brier_base"] + sum((1/nf - hit)^2)
      acc[[slot]]["npred"] <- acc[[slot]]["npred"] + nf
      }
    }
  }
  if (!is.na(z$class[1]) && z$class[1] %chin% MAJ && z$rc[1] == "final" &&
      dt0 >= MAJ_FROM) {
    ms <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg2 <- .pairs(length(ms), z$place[ms])
    g2 <- list(i = ms[gg2$i], j = ms[gg2$j])
    if (length(g2$i)) maj[[length(maj)+1L]] <- data.table(
      class = z$class[1], yr = as.character(yr), event_id = ev,
      conc = { d2 <- r_use[g2$i] - r_use[g2$j]
               c2 <- as.numeric((d2 > 0) == (z$place[g2$i] < z$place[g2$j]))
               c2[d2 == 0] <- 0.5; sum(c2) },
      pairs = length(g2$i),
      fav = { t2 <- which(r_use == max(r_use)); mean(z$place[t2] == min(z$place)) },
      medal3 = sum(a[order(-r_use)][1:3] %chin% a[z$place <= 3]),
      winner_rank = which(order(-r_use) == which.min(z$place)))
  }
  est <- n_eff >= 2
  S <- (if (sum(est) >= 3L) mean(z$perf[est] - r_pre[est]) else 0) * (sum(est)/length(a))
  surprise <- (z$perf - r_pre) - S
  k0e <- K0v[[ev]]; if (is.null(k0e) || !is.finite(k0e)) k0e <- K0
  kv <- pmax(k0e * KAPPA / (n_eff + KAPPA), KFLOOR)
  if (KT1 != 1 && z$meet_tier[1] == "T1_elite") kv <- pmin(kv * KT1, 0.9)
  if (CENS < 1) {
    neg_heat <- z$rc != "final" & surprise < 0
    kv[neg_heat] <- kv[neg_heat] * CENS
  }
  if (HUBER > 0) {
    lim <- HUBER * sqrt(v_pre)
    ex <- is.finite(lim) & lim > 0 & abs(surprise) > lim
    if (any(ex)) kv[ex] <- kv[ex] * (lim[ex] / abs(surprise[ex]))
  }
  for (m in seq_along(a)) {
    if (!seen[m]) {
      p0 <- z$perf[m]
      if (WINDCS && !is.na(z$beta[m]) && !is.na(z$wind[m])) p0 <- p0 - z$beta[m] * z$wind[m]
      init <- p0 - S
      if (XEV) {
        sib <- BYA[[a[m]]]
        if (!is.null(sib)) {
          sib <- sib[sib != ev]
          if (length(sib)) {
            fams <- reg$family[match(sib, reg$event_id)]
            sib <- sib[!is.na(fams) & fams == z$family[1]]
            if (length(sib)) {
              depth <- vapply(sib, function(s) { n <- NE[[key(a[m], s)]]; if (is.null(n)) 0 else n }, numeric(1))
              best <- which.max(depth)
              if (depth[best] >= 5) {
                xr <- (R[[key(a[m], sib[best])]] - MUv[[sib[best]]]) + mu
                init <- 0.5 * init + 0.5 * xr
              }
            }
          }
        }
      }
      R[[kk[m]]] <- init
      if (!is.null(agef) && !is.na(z$age[m])) LE[[kk[m]]] <- agef(z$age[m])
      BYA[[a[m]]] <- unique(c(BYA[[a[m]]], ev))
    } else {
      R[[kk[m]]] <- r_pre[m] + kv[m] * surprise[m]
    }
    # Variance learns at the same rate; the floor stops a lucky streak
    # collapsing it to zero (the career model's thin-record sigma defect).
    #
    # ONLY for an athlete who carried a rating in. A cold start sets R to absorb
    # this very performance, so its true surprise is zero — but `surprise[m]` is
    # still measured against the population mean, and updating V with that made
    # a debutant's variance the squared distance of their debut from the mean.
    # Elite newcomers got an enormous variance and average ones almost none,
    # which is backwards, and it reached the page as a 10,558-point decathlon
    # and a sub-world-record 100m. Leaving V unset keeps the event prior until
    # there is a real surprise to learn from.
    if (seen[m])
      V[[kk[m]]] <- max(v_pre[m] + kv[m] * (surprise[m]^2 - v_pre[m]), 0.04 * vp0)
    NE[[kk[m]]] <- n_eff[m] + 1
    LD[[kk[m]]] <- dt0n
    # running bests, updated last so every read above stayed lagged
    bc <- BC[[kk[m]]]
    if (is.null(bc) || z$perf[m] > bc) BC[[kk[m]]] <- z$perf[m]
    bsy <- BSY[[kk[m]]]
    if (!is.null(bsy) && bsy == yr) {
      if (z$perf[m] > BS[[kk[m]]]) BS[[kk[m]]] <- z$perf[m]
    } else { BSY[[kk[m]]] <- yr; BS[[kk[m]]] <- z$perf[m] }
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
res <- data.table(tag = TAG,
  conc25 = 100*acc$y25["conc"]/acc$y25["pairs"], fav25 = 100*acc$y25["fav"]/acc$y25["nr"],
  conc26 = 100*acc$y26["conc"]/acc$y26["pairs"], fav26 = 100*acc$y26["fav"]/acc$y26["nr"],
  wconc25 = 100*acc$y25["conc_w"]/acc$y25["w_sum"],
  wconc26 = 100*acc$y26["conc_w"]/acc$y26["w_sum"],
  ess25 = acc$y25["w_sum"]^2/acc$y25["w_sq"], ess26 = acc$y26["w_sum"]^2/acc$y26["w_sq"],
  conc26_bs = 100*acc$y26["conc_bs"]/acc$y26["pairs_bs"],
  conc26_mx = 100*acc$y26["conc_mx"]/acc$y26["pairs_mx"],
  conc26_bc = 100*acc$y26["conc_bc"]/acc$y26["pairs_bc"],
  share26_cold = 100*(acc$y26["pairs_mx"]+acc$y26["pairs_bc"])/acc$y26["pairs"],
  races25 = acc$y25["nr"], races26 = acc$y26["nr"], mins = round(el,1),
  # RAW Brier per prediction, written only when SEQ_WINP computed it (NA
  # otherwise, never a silent 0). These accumulators used to be computed on
  # every race and then discarded — the same dead-computation family as CSHRINK.
  #
  # Deliberately NOT reported as skill against `brier_base`: that baseline is a
  # uniform 1/field prior, and "report skill against a uniform prior" is on this
  # repo's Not-to-do list. Raw Brier is comparable BETWEEN ARMS on the same
  # race set, which is what it is for.
  brier25 = if (WINP && acc$y25["npred"] > 0) acc$y25["brier"]/acc$y25["npred"] else NA_real_,
  brier26 = if (WINP && acc$y26["npred"] > 0) acc$y26["brier"]/acc$y26["npred"] else NA_real_,
  maxplace = MAXPLACE, ceil = CEIL, seeded = n_seeded, huber = HUBER,
  seedhl = SEEDHL, seedne = SEEDNE, k0 = K0, kappa = KAPPA, kfloor = KFLOOR,
  kpow = KPOW,
  w_maj = W_MAJ, w_t1 = W_T1, w_t2 = W_T2, w_rnd = W_RND,
  cens=CENS, age=AGEF, stale=STALE, xev=XEV, kt1=KT1, windcs=WINDCS,
  k0=K0, kappa=KAPPA, kfloor=KFLOOR)
cat(sprintf("[%s] TUNE 2025: conc %.3f%% fav %.1f%% (%d races) | CONFIRM 2026: conc %.3f%% fav %.1f%% (%d races) | %.1f min\n",
    TAG, res$conc25, res$fav25, res$races25, res$conc26, res$fav26, res$races26, el))
cat(sprintf("[%s] 2026 by band: both-rated %.3f%% | one-cold %.3f%% | both-cold %.3f%% | cold pairs %.1f%% of metric
",
    TAG, res$conc26_bs, res$conc26_mx, res$conc26_bc, res$share26_cold))
cat(sprintf("[%s] WEIGHTED (maj %g / T1 %g / T2 %g, non-final x%g): tune %.3f%% (ess %s) | sealed %.3f%% (ess %s)
",
    TAG, W_MAJ, W_T1, W_T2, W_RND, res$wconc25, format(round(res$ess25), big.mark=","),
    res$wconc26, format(round(res$ess26), big.mark=",")))
mj <- rbindlist(maj)
if (nrow(mj)) {
  write_parquet(mj, file.path(SC, sprintf("seqv3_majors_%s.parquet", TAG)))
  cat("
== MAJORS FINALS (2021+), walk-forward ==
")
  print(mj[, .(finals = .N, conc = round(100*sum(conc)/sum(pairs),2),
               fav = round(100*mean(fav),1), medal_hits = round(100*sum(medal3)/(3*.N),1),
               med_winner_rank = as.double(median(winner_rank))), by = .(class, yr)][order(yr, class)])
  cat("
pooled:
")
  print(mj[, .(finals = .N, conc = round(100*sum(conc)/sum(pairs),2),
               fav = round(100*mean(fav),1), medal_hits = round(100*sum(medal3)/(3*.N),1))])
}
f <- file.path(SC, "seqv2_results.csv")
fwrite(res, f, append = file.exists(f))
ids <- ls(R)
st <- data.table(k = ids, R = vapply(ids, function(i) R[[i]], numeric(1)),
                 n_eff = vapply(ids, function(i) NE[[i]], numeric(1)),
                 # v carries the per-athlete performance variance, needed for a
                 # "on a good day" column. NOTE it is the variance of the
                 # SHOCK-ADJUSTED surprise, so it understates what an athlete
                 # actually varies by: the raw residual still contains the race
                 # shock S. Measured sd of (perf-r_pre)/sqrt(v) is 1.52, not 1.
                 # Use an EMPIRICAL quantile of that ratio, never a normal one.
                 v = vapply(ids, function(i) { vv <- V[[i]]
                                               if (is.null(vv)) NA_real_ else vv }, numeric(1)),
                 last = as.Date(vapply(ids, function(i) LD[[i]], numeric(1)),
                                origin = "1970-01-01"),
                 # the athlete's best mark so far, and the blend that ORDERS a
                 # field. R stays the pure rating so nothing downstream silently
                 # inherits the ceiling without asking for it.
                 best = vapply(ids, function(i) { b <- BC[[i]]
                                                  if (is.null(b)) NA_real_ else b }, numeric(1)))
st[, R_ceil := fifelse(is.na(best), R, (1 - CEIL) * R + CEIL * best)]
st[, c("athlete_id","event_id") := tstrsplit(k, "|", fixed = TRUE)]
if (HIST) {
  hd <- as.data.table(lapply(H, function(v) v[seq_len(hi)]))
  hd[, date := as.Date(date, origin = "1970-01-01")]
  write_parquet(hd, file.path(SC, sprintf("seqv3_history_%s.parquet", TAG)))
  cat(sprintf("[%s] history: %s athlete-races (races with <3 athletes are skipped\n",
              TAG, format(nrow(hd), big.mark = ",")))
  cat("        by the loop entirely, so this is scored racing, not every result)\n")
}
write_parquet(st[, !"k"], file.path(SC, sprintf("seqv2_state_%s.parquet", TAG)))
