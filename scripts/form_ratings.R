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
OUT <- "C:/dev/citiusverse/citiusdata/data"
SC  <- Sys.getenv("FORM_OUT", here::here("citiusdata", "data"))
# Defaults are the 2026-08-14 swept optimum (see docs/plans/FORM-MODEL.md):
# k0 0.95 and floor 0.32 both moved; kappa 3 was already optimal. The old
# eye-chosen 0.55 / 3 / 0.18 scored 68.028 on the 2025 tuning window; these
# score 68.564, and 67.353 -> 68.018 on the sealed 2026 window.
K0 <- as.numeric(Sys.getenv("SEQ_K0","0.95")); KAPPA <- as.numeric(Sys.getenv("SEQ_KAPPA","3"))
KFLOOR <- as.numeric(Sys.getenv("SEQ_KFLOOR","0.32")); CSHRINK <- as.numeric(Sys.getenv("SEQ_C","4"))
# The ladder winners are ON by default, so a bare run IS the chosen model rather
# than the model minus its adjustments. Set SEQ_AGE=0 / SEQ_STALE=0 / SEQ_CENS=1
# to turn them off. (Leaving them opt-in is how 350,401 fitted race effects sat
# inert on every shipped number — dormant by flag, which no wiring guard sees.)
CENS <- as.numeric(Sys.getenv("SEQ_CENS","0.3")); AGEF <- Sys.getenv("SEQ_AGE","1") != "0"
STALE <- Sys.getenv("SEQ_STALE","1") != "0"; XEV <- Sys.getenv("SEQ_XEV","") != ""
KT1 <- as.numeric(Sys.getenv("SEQ_KT1","1")); WINDCS <- Sys.getenv("SEQ_WINDCS","") != ""
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
# CAPPING MAKES THE METRIC EASIER, so a capped number is NOT comparable to an
# uncapped one. Only capped-vs-capped on the same cap is a fair read.
MAXPLACE <- as.integer(Sys.getenv("SEQ_MAXPLACE","0"))
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
R <- new.env(parent=emptyenv()); NE <- new.env(parent=emptyenv())
V <- new.env(parent=emptyenv())   # EW variance of own surprises; prior = event pop
LD <- new.env(parent=emptyenv()); LE <- new.env(parent=emptyenv())
BYA <- new.env(parent=emptyenv())
key <- function(a, e) paste0(a, "|", e)
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
acc <- list(y25 = c(conc=0,pairs=0,fav=0,nr=0,brier=0,brier_base=0,npred=0), y26 = c(conc=0,pairs=0,fav=0,nr=0,brier=0,brier_base=0,npred=0))
# Per-race rating history (SEQ_HIST=1). r_pre is the rating an athlete CARRIED
# INTO the race — the only version that answers an out-of-sample question. The
# final state written below has already absorbed every race you would test it
# against, so measuring against that is circular (learned 2026-08-14).
# Preallocated vectors, not a growing list: the maj[[length+1]] pattern is fine
# for 757 majors finals but would add per-object overhead across 165,133 races.
NR <- if (HIST) nrow(d) else 0L
H <- list(race_key = character(NR), date = numeric(NR), event_id = character(NR),
          athlete_id = character(NR), r_pre = numeric(NR), n_eff = numeric(NR),
          v_pre = numeric(NR), perf = numeric(NR), place = integer(NR),
          rc = character(NR), seen = logical(NR))
hi <- 0L
MAJ <- c("olympics","world_champs","european_champs","commonwealth")
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
            meet_tier = Vtier[i1], class = Vcls[i1], race_key = Vrk[i1])
  a <- z$athlete_id; ev <- z$event_id[1]; kk <- key(a, ev); dt0 <- z$date[1]
  mu <- MUv[[ev]]
  r_pre <- numeric(length(a)); n_eff <- numeric(length(a)); seen <- logical(length(a))
  for (m in seq_along(a)) {
    v <- R[[kk[m]]]
    if (is.null(v)) { r_pre[m] <- mu; n_eff[m] <- 0; next }
    seen[m] <- TRUE
    gap <- as.numeric(dt0 - LD[[kk[m]]])
    if (AGEF && !is.na(z$age[m]) && !is.na(z$family[1])) {
      f <- agefun[[z$family[1]]]
      if (!is.null(f)) {
        le <- LE[[kk[m]]]
        eff_now <- f(z$age[m])
        if (!is.null(le) && !is.na(le)) v <- v + (eff_now - le)
        LE[[kk[m]]] <- eff_now
      }
    }
    ne <- NE[[kk[m]]]
    if (STALE) ne <- ne * 2^(-gap / z$hl[m])
    r_pre[m] <- v; n_eff[m] <- ne
  }
  vp0 <- VPv[[ev]]; if (is.null(vp0) || !is.finite(vp0)) vp0 <- stats::var(z$perf)
  v_pre <- vapply(kk, function(K) { vv <- V[[K]]; if (is.null(vv)) vp0 else vv }, numeric(1))
  if (HIST) {
    ix <- hi + seq_along(a); hi <- hi + length(a)
    H$race_key[ix] <- z$race_key[1]; H$date[ix] <- as.numeric(dt0)
    H$event_id[ix] <- ev;            H$athlete_id[ix] <- a
    H$r_pre[ix] <- r_pre;            H$n_eff[ix] <- n_eff
    H$v_pre[ix] <- v_pre;            H$perf[ix] <- z$perf
    H$place[ix] <- z$place;          H$rc[ix] <- z$rc
    H$seen[ix] <- seen
  }
  yr <- year(dt0)
  slot <- if (yr == 2025L) "y25" else if (yr == 2026L) "y26" else NA
  if (!is.na(slot)) {
    # All i<j pairs as plain integer vectors. CJ() cost ~0.4ms per call in fixed
    # data.table dispatch overhead regardless of field size (measured: 79x at
    # n=8), paid once per scored race.
    sel <- if (MAXPLACE > 0L) which(z$place <= MAXPLACE) else seq_along(a)
    gg <- .pairs(length(sel), z$place[sel])
    g <- list(i = sel[gg$i], j = sel[gg$j])   # map back to full-field indices
    if (length(g$i)) {
      acc[[slot]]["conc"] <- acc[[slot]]["conc"] + sum((r_pre[g$i] > r_pre[g$j]) == (z$place[g$i] < z$place[g$j]))
      acc[[slot]]["pairs"] <- acc[[slot]]["pairs"] + length(g$i)
      acc[[slot]]["fav"] <- acc[[slot]]["fav"] +
        (z$place[sel][which.max(r_pre[sel])] == min(z$place[sel]))
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
            rep(r_pre, each = 1000L)
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
    g2 <- .pairs(length(a), z$place)
    if (length(g2$i)) maj[[length(maj)+1L]] <- data.table(
      class = z$class[1], yr = as.character(yr), event_id = ev,
      conc = sum((r_pre[g2$i] > r_pre[g2$j]) == (z$place[g2$i] < z$place[g2$j])),
      pairs = length(g2$i),
      fav = z$place[which.max(r_pre)] == min(z$place),
      medal3 = sum(a[order(-r_pre)][1:3] %chin% a[z$place <= 3]),
      winner_rank = which(order(-r_pre) == which.min(z$place)))
  }
  est <- n_eff >= 2
  S <- (if (sum(est) >= 3L) mean(z$perf[est] - r_pre[est]) else 0) * (sum(est)/length(a))
  surprise <- (z$perf - r_pre) - S
  kv <- pmax(K0 * KAPPA / (n_eff + KAPPA), KFLOOR)
  if (KT1 != 1 && z$meet_tier[1] == "T1_elite") kv <- pmin(kv * KT1, 0.9)
  if (CENS < 1) {
    neg_heat <- z$rc != "final" & surprise < 0
    kv[neg_heat] <- kv[neg_heat] * CENS
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
      if (AGEF && !is.na(z$age[m]) && !is.na(z$family[1])) {
        f <- agefun[[z$family[1]]]; if (!is.null(f)) LE[[kk[m]]] <- f(z$age[m])
      }
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
    LD[[kk[m]]] <- dt0
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
res <- data.table(tag = TAG,
  conc25 = 100*acc$y25["conc"]/acc$y25["pairs"], fav25 = 100*acc$y25["fav"]/acc$y25["nr"],
  conc26 = 100*acc$y26["conc"]/acc$y26["pairs"], fav26 = 100*acc$y26["fav"]/acc$y26["nr"],
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
  maxplace = MAXPLACE,
  cens=CENS, age=AGEF, stale=STALE, xev=XEV, kt1=KT1, windcs=WINDCS,
  k0=K0, kappa=KAPPA, kfloor=KFLOOR)
cat(sprintf("[%s] TUNE 2025: conc %.3f%% fav %.1f%% (%d races) | CONFIRM 2026: conc %.3f%% fav %.1f%% (%d races) | %.1f min\n",
    TAG, res$conc25, res$fav25, res$races25, res$conc26, res$fav26, res$races26, el))
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
                 last = as.Date(vapply(ids, function(i) as.character(LD[[i]]), character(1))))
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
