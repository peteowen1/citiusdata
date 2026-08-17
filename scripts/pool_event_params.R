# Per-event parameters by PARTIAL POOLING, not by 86 independent fits.
#
# THE PROBLEM WITH INDEPENDENT PER-EVENT FITS. Turning cross-event blending on
# measured +1.657 pp on the women's 600m and -0.447 pp on the men's 600m; +0.341
# on the women's 1000m and -1.156 on the men's. Same discipline, opposite sign by
# sex, on a few hundred pairs each. No physiology changes between the men's and
# women's 1000m - those are estimates made of noise, and an optimiser that takes
# a winner per event will write them into the configuration as findings.
#
# THE PROBLEM WITH ONE GLOBAL VALUE. It is the average of opposite truths:
# distance +0.271, throws -0.215, netting to +0.047.
#
# PARTIAL POOLING IS THE THIRD OPTION. Each event's estimate is shrunk toward
# its neighbours in proportion to the evidence behind it:
#
#     d_e = w_e * d_hat_e + (1 - w_e) * d_group,     w_e = n_e / (n_e + kappa)
#
# The 1000m M (3,200 pairs) is pulled hard toward the middle-distance mean; the
# 1500m M (70,000 pairs) barely moves. This is the SAME shrinkage the engine
# already applies to ratings - w = xb/(n_eff + xb) for the blend, k0*KAPPA/
# (n_eff + KAPPA) for the learning rate - lifted one level up, from ratings
# across races to parameters across events.
#
# WHY THIS IS NOT 86 FREE PARAMETERS. kappa is not fitted per event; it is
# estimated ONCE by empirical Bayes from the between-event spread against the
# within-event noise. Eighty-six numbers come out, but they cost about one
# degree of freedom, which is the whole reason this is safe where independent
# per-event fitting is not.
#
# Usage:
#   ARMS="xbh_0,xbh_1" Rscript pool_event_params.R
#   ARMS="xbh_0,xbh_1" GROUP=family Rscript pool_event_params.R   # or GROUP=neighbour
# The FIRST arm is the base (the value in force today); the second is the
# candidate. Output is a parquet of per-event values for the events whose SHRUNK
# estimate favours the candidate.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D     <- here::here("citiusdata", "data")
tags  <- trimws(strsplit(Sys.getenv("ARMS", ""), ",")[[1]])
PARAM <- Sys.getenv("PARAM", "xblend")
GROUP <- Sys.getenv("GROUP", "family")     # family | neighbour
MINP  <- as.integer(Sys.getenv("MIN_PAIRS", "200"))
TUNE  <- as.integer(trimws(strsplit(Sys.getenv("TUNE_YEARS", "2025"), ",")[[1]]))
SEAL  <- as.integer(trimws(strsplit(Sys.getenv("SEAL_YEARS", "2026"), ",")[[1]]))
PRIOR <- as.integer(trimws(strsplit(Sys.getenv("PRIOR_YEARS", "2022,2023,2024"), ",")[[1]]))
OUT   <- Sys.getenv("EVPARAM_OUT",
                    file.path(D, sprintf("event_params_%s_pooled.parquet", PARAM)))
stopifnot("ARMS needs exactly two tags: base,candidate" = length(tags) == 2)
base_tag <- tags[1]; cand_tag <- tags[2]
CAND_VAL <- as.numeric(Sys.getenv("CAND_VALUE", "1"))

# Concordance outcome for every comparable PAIR, keyed so the same pair can be
# matched across arms. Keyed on the two athlete ids, not on row position, so a
# difference in row order between arms cannot silently misalign the join.
pair_cw <- function(tag, yrs) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) stop(sprintf("no history for '%s' - run it with SEQ_HIST=1", tag))
  h <- setDT(read_parquet(f))
  if (!"r_use" %in% names(h)) h[, r_use := r_pre]
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
         year(date) %in% yrs]
  stopifnot("no rows survived the year filter" = nrow(h) > 0)
  a <- h[, .(race_key, event_id, athlete_id, place, r = r_use)]
  m <- merge(a, a, by = c("race_key", "event_id"), allow.cartesian = TRUE,
             suffixes = c(".x", ".y"))
  m <- m[athlete_id.x < athlete_id.y & place.x != place.y]
  d <- m$r.x - m$r.y
  m[, cw := as.numeric((d > 0) == (place.x < place.y))]
  m[d == 0, cw := 0.5]
  m[, .(race_key, event_id, athlete_id.x, athlete_id.y, cw)]
}

# THE PAIRED DIFFERENCE, which is the whole point. The two arms share races,
# finishers and base ratings; only thin-record athletes' orderings change, so on
# the large majority of pairs both arms return the IDENTICAL outcome and
# contribute exactly zero to the difference. Treating the arms as independent
# (var = 2*p*q/n) overstates the noise in the delta by a large factor, and
# overstated noise drives the between-event variance to zero and forces complete
# pooling - i.e. it manufactures the conclusion "events are indistinguishable".
# So measure the difference pair by pair and take its actual variance.
delta_over <- function(yrs) {
  b <- pair_cw(base_tag, yrs); setnames(b, "cw", "cw_b")
  cc <- pair_cw(cand_tag, yrs); setnames(cc, "cw", "cw_c")
  j <- merge(b, cc, by = c("race_key", "event_id", "athlete_id.x", "athlete_id.y"))
  stopifnot("the two arms share no pairs - are they the same corpus?" = nrow(j) > 0)
  j[, dif := 100 * (cw_c - cw_b)]
  r <- j[, .(n = .N, d = mean(dif), disagree = mean(dif != 0),
             v_within = stats::var(dif) / .N), by = event_id]
  rm(b, cc, j); invisible(gc())
  # var() is NA on a single observation, and an event resting on a handful of
  # pairs carries no information about a parameter either way. Drop them here
  # rather than let an NA propagate into tau^2 and silently void the pooling.
  n0 <- nrow(r)
  r <- r[n >= MINP & is.finite(v_within)]
  if (nrow(r) < n0)
    cat(sprintf("dropped %d event(s) under %d pairs or with undefined variance\n",
                n0 - nrow(r), MINP))
  stopifnot("no events left after the pair-count filter" = nrow(r) > 0)
  r
}

tune <- delta_over(TUNE)
reg <- as.data.table(citius::citius_events())[
  , .(event_id, discipline, sex, family, tactical, technical)]
x <- merge(tune, reg, by = "event_id")
stopifnot("no events to pool" = nrow(x) > 0)

# --- the group each event is shrunk toward ------------------------------------
# `family` is the crude version. `neighbour` is closer to the real claim: the
# 5000m should learn more from the 10,000m than from the 800m, because the
# events are near each other in distance, not merely in the same bucket. For
# running events that is proximity in log(distance); for field events distance
# is meaningless, so they fall back to family.
metres <- function(disc) {
  m <- suppressWarnings(as.numeric(gsub(",", "", sub("^([0-9,\\.]+)\\s*(Metres|Kilometres).*$", "\\1", disc))))
  km <- grepl("Kilometres", disc); m[km] <- m[km] * 1000
  m[grepl("^Mile", disc)] <- 1609.34
  m[grepl("Half Marathon", disc)] <- 21097.5
  m[grepl("^Marathon", disc)] <- 42195
  m
}
x[, dist_m := metres(discipline)]
if (GROUP == "neighbour") {
  # a Gaussian kernel on log-distance, within sex-agnostic running events only
  x[, ld := log(dist_m)]
  run <- x[is.finite(ld)]
  BW <- as.numeric(Sys.getenv("KERNEL_BW", "0.5"))   # ~1.6x in distance
  grp_mean <- vapply(seq_len(nrow(x)), function(i) {
    if (!is.finite(x$ld[i])) return(x[family == x$family[i], weighted.mean(d, n)])
    k <- exp(-0.5 * ((run$ld - x$ld[i]) / BW)^2)
    k[run$event_id == x$event_id[i]] <- 0        # leave-one-out: never its own
    if (sum(k * run$n) <= 0) return(x[family == x$family[i], weighted.mean(d, n)])
    weighted.mean(run$d, k * run$n)
  }, numeric(1))
  x[, d_group := grp_mean]
} else {
  # leave-one-out family mean, so an event is never shrunk toward itself
  x[, d_group := (sum(d * n) - d * n) / pmax(sum(n) - n, 1), by = family]
}

# --- empirical-Bayes shrinkage constant ---------------------------------------
# v_within now arrives MEASURED from the paired differences, not assumed.
stopifnot("v_within must come from delta_over(), measured not assumed" =
            "v_within" %in% names(x), "v_within must be finite" = all(is.finite(x$v_within)))
cat(sprintf("pairs where the two arms actually disagree: %.2f%% (median over events)\n",
            100 * median(x$disagree)))
# between-event variance is what is LEFT of the observed spread after taking out
# sampling noise. If that is <= 0 the events are indistinguishable and every
# estimate collapses to its group mean, which is the correct answer, not a bug.
tau2 <- max(0, x[, weighted.mean((d - d_group)^2, n)] - x[, weighted.mean(v_within, n)])
kappa <- if (tau2 <= 0) Inf else x[, weighted.mean(v_within * n, n)] / tau2
cat(sprintf("empirical Bayes: tau^2 (between events) = %.4f, kappa = %s\n",
            tau2, if (is.infinite(kappa)) "Inf (pool completely)" else sprintf("%.0f pairs", kappa)))
x[, w := if (is.infinite(kappa)) 0 else n / (n + kappa)]
x[, d_shrunk := w * d + (1 - w) * d_group]

cat(sprintf("\nevents: %d | grouping: %s | shrinkage weight w: median %.2f, range %.2f-%.2f\n",
            nrow(x), GROUP, median(x$w), min(x$w), max(x$w)))
cat("\n=== how far each estimate moved when pooled (10 most shrunk) ===\n")
setorder(x, w)
print(head(x[, .(discipline, sex, family, n, raw = round(d, 3),
                 group = round(d_group, 3), shrunk = round(d_shrunk, 3),
                 w = round(w, 2))], 10))

# --- selection on the SHRUNK estimate, then an honest out-of-sample test -------
sel <- x[d_shrunk > 0]
cat(sprintf("\nevents whose SHRUNK estimate favours the candidate: %d of %d\n",
            nrow(sel), nrow(x)))
raw_sel <- x[d > 0]
cat(sprintf("(selecting on the RAW estimate would have taken %d)\n", nrow(raw_sel)))

test_set <- function(ids, yrs, label) {
  dd <- merge(delta_over(yrs), reg, by = "event_id")
  dd[, chosen := event_id %chin% ids]
  r <- dd[, .(events = .N, pooled = round(weighted.mean(d, n), 3),
              up = sum(d > 0), down = sum(d < 0)), by = chosen]
  cat(sprintf("\n-- %s --\n", label)); print(r)
  invisible(r)
}
cat("\n================ OUT-OF-SAMPLE TESTS ================")
cat("\nSelection used the TUNE window only. These two did not take part.\n")
test_set(sel$event_id, SEAL,  sprintf("sealed window (%s), shrunk selection", paste(SEAL, collapse = ",")))
test_set(raw_sel$event_id, SEAL, sprintf("sealed window (%s), RAW selection - the comparison", paste(SEAL, collapse = ",")))
test_set(sel$event_id, PRIOR, sprintf("earlier window (%s), shrunk selection", paste(PRIOR, collapse = ",")))

o <- sel[, .(event_id)]; o[[PARAM]] <- CAND_VAL
write_parquet(o, OUT)
cat(sprintf("\nwrote %s (%d events at %s = %g)\n",
            basename(OUT), nrow(o), PARAM, CAND_VAL))
cat("VERIFY IT: re-run the engine with SEQ_EVPARAM pointing at that file.\n")
