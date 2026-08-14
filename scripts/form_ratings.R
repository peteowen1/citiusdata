# THE FORM MODEL -- sequential walk-forward ratings. See docs/plans/FORM-MODEL.md
# for the method, its validation, and the adjustment ladder. This answers "how
# good are you RIGHT NOW"; the career model (estimate_ability) answers "how good
# are you over a full record" and keeps the forecasts.
#
# Sequential walk-forward engine v2. All athletics events, T1+T2, 2020->now,
# ONE global chronological sweep so cross-event information is available at the
# moment it is needed. Every adjustment is a flag; 2025 races are the TUNING
# window, 2026 the CONFIRMATION window (score both, tune only ever on 2025).
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
K0 <- as.numeric(Sys.getenv("SEQ_K0","0.55")); KAPPA <- as.numeric(Sys.getenv("SEQ_KAPPA","3"))
KFLOOR <- as.numeric(Sys.getenv("SEQ_KFLOOR","0.18")); CSHRINK <- as.numeric(Sys.getenv("SEQ_C","4"))
CENS <- as.numeric(Sys.getenv("SEQ_CENS","1")); AGEF <- Sys.getenv("SEQ_AGE","") != ""
STALE <- Sys.getenv("SEQ_STALE","") != ""; XEV <- Sys.getenv("SEQ_XEV","") != ""
KT1 <- as.numeric(Sys.getenv("SEQ_KT1","1")); WINDCS <- Sys.getenv("SEQ_WINDCS","") != ""
TAG <- Sys.getenv("SEQ_TAG","baseline")
FROM <- as.Date("2020-01-01")

cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
cat0 <- cat0[meet_tier %in% c("T1_elite","T2_strong"), .(competition_id, meet_tier)]
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
R <- new.env(parent=emptyenv()); NE <- new.env(parent=emptyenv())
LD <- new.env(parent=emptyenv()); LE <- new.env(parent=emptyenv())
BYA <- new.env(parent=emptyenv())
key <- function(a, e) paste0(a, "|", e)

races <- split(d, by = "race_key", sorted = FALSE)
ord <- order(vapply(races, function(z) as.numeric(z$date[1]), numeric(1)))
races <- races[ord]
acc <- list(y25 = c(conc=0,pairs=0,fav=0,nr=0), y26 = c(conc=0,pairs=0,fav=0,nr=0))
t0 <- Sys.time()
for (z in races) {
  if (nrow(z) < 3L) next
  z <- unique(z, by = "athlete_id")
  if (nrow(z) < 3L) next
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
  yr <- format(dt0, "%Y")
  slot <- if (yr == "2025") "y25" else if (yr == "2026") "y26" else NA
  if (!is.na(slot)) {
    g <- CJ(i=seq_along(a), j=seq_along(a))[i < j][z$place[i] != z$place[j]]
    if (nrow(g)) {
      acc[[slot]]["conc"] <- acc[[slot]]["conc"] + sum((r_pre[g$i] > r_pre[g$j]) == (z$place[g$i] < z$place[g$j]))
      acc[[slot]]["pairs"] <- acc[[slot]]["pairs"] + nrow(g)
      acc[[slot]]["fav"] <- acc[[slot]]["fav"] + (z$place[which.max(r_pre)] == min(z$place))
      acc[[slot]]["nr"] <- acc[[slot]]["nr"] + 1
    }
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
    NE[[kk[m]]] <- n_eff[m] + 1
    LD[[kk[m]]] <- dt0
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
res <- data.table(tag = TAG,
  conc25 = 100*acc$y25["conc"]/acc$y25["pairs"], fav25 = 100*acc$y25["fav"]/acc$y25["nr"],
  conc26 = 100*acc$y26["conc"]/acc$y26["pairs"], fav26 = 100*acc$y26["fav"]/acc$y26["nr"],
  races25 = acc$y25["nr"], races26 = acc$y26["nr"], mins = round(el,1),
  cens=CENS, age=AGEF, stale=STALE, xev=XEV, kt1=KT1, windcs=WINDCS,
  k0=K0, kappa=KAPPA, kfloor=KFLOOR)
cat(sprintf("[%s] TUNE 2025: conc %.3f%% fav %.1f%% (%d races) | CONFIRM 2026: conc %.3f%% fav %.1f%% (%d races) | %.1f min\n",
    TAG, res$conc25, res$fav25, res$races25, res$conc26, res$fav26, res$races26, el))
f <- file.path(SC, "seqv2_results.csv")
fwrite(res, f, append = file.exists(f))
ids <- ls(R)
st <- data.table(k = ids, R = vapply(ids, function(i) R[[i]], numeric(1)),
                 n_eff = vapply(ids, function(i) NE[[i]], numeric(1)),
                 last = as.Date(vapply(ids, function(i) as.character(LD[[i]]), character(1))))
st[, c("athlete_id","event_id") := tstrsplit(k, "|", fixed = TRUE)]
write_parquet(st[, !"k"], file.path(SC, sprintf("seqv2_state_%s.parquet", TAG)))
