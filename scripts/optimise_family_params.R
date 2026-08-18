# Per-FAMILY parameter optimisation, with shrinkage toward the global value.
#
# WHY FAMILY. The benchmark by family says the model's edge over simply sorting
# by season best is not evenly earned: middle +2.03, distance +1.76, throw
# +1.64, jump +1.04, sprint +0.28, hurdles -1.09. A single global parameter set
# is being asked to serve events where form moves week to week AND events where
# a season best is already the best predictor there is. That is the same shape
# as cross-event blending, which was +1.35 on the women's 10,000m and -0.27
# across the throws while reading +0.029 globally.
#
# WHY NOT PER EVENT. Measured 2026-08-17: an assembled per-event configuration
# scored 72.061/71.753 against the family gate's 72.053/71.755 - a tie. The
# family is the resolution this data supports, and pool_event_params.R showed
# why: shrink per-event estimates toward their neighbours and they collapse
# onto the family mean anyway.
#
# THE TRICK, same as optimise_event_params.R: ratings are built sequentially
# across all events at once, so a per-family value cannot be tested by running
# one family. But ONE global run scores every family, so N global runs at N
# candidate values give each family its concordance at each value, and the
# per-family optimum assembles from runs that already happened. N runs, not
# N x 9.
#
# THE GUARD, and it is deliberately NOT a noise floor. Gating on each family's
# own noise floor is what reduced cross-event blending from 27 events to 1 and
# produced "not supported" for a real effect. Instead:
#   1. the winning value must beat the global on the TUNE window, and
#   2. the SAME value must also beat it on the SEALED window.
# Sign agreement across two independent windows is the replication test; the
# noise floor is reported as context, not applied as a gate.
#
# Usage:
#   ARMS="k0_085=0.85,k0_095=0.95,k0_105=1.05" PARAM=k0 Rscript optimise_family_params.R
# Each tag needs seqv3_history_<tag>.parquet, i.e. that arm ran with SEQ_HIST=1.
# The FIRST arm listed is the incumbent - the value deployed today.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D     <- here::here("citiusdata", "data")
PARAM <- Sys.getenv("PARAM", "")
TUNE  <- as.integer(trimws(strsplit(Sys.getenv("TUNE_YEARS", "2025"), ",")[[1]]))
SEAL  <- as.integer(trimws(strsplit(Sys.getenv("SEAL_YEARS", "2026"), ",")[[1]]))
OUT   <- Sys.getenv("FAMPARAM_OUT",
                    file.path(D, sprintf("family_params_%s.parquet", PARAM)))
spec  <- strsplit(trimws(strsplit(Sys.getenv("ARMS", ""), ",")[[1]]), "=")
stopifnot("PARAM must be set" = nzchar(PARAM),
          "ARMS must be tag=value pairs, at least two" = length(spec) >= 2)
arms <- data.table(tag = vapply(spec, `[`, "", 1L),
                   value = as.numeric(vapply(spec, `[`, "", 2L)))
KNOWN <- c("k0", "kfloor", "ceil", "huber", "xblend", "seedhl")
stopifnot("PARAM is not one the engine can override per event" = PARAM %chin% KNOWN)
cat(sprintf("optimising %s over %d values: %s\n  incumbent: %s = %g\n",
            PARAM, nrow(arms), paste(arms$value, collapse = ", "),
            arms$tag[1], arms$value[1]))

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]

# concordance per FAMILY for one arm and one window
score_arm <- function(tag, yrs) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) stop(sprintf("no history for '%s' - run it with SEQ_HIST=1", tag))
  h <- setDT(read_parquet(f, col_select = c("race_key","date","event_id",
                                            "athlete_id","r_use","r_pre","place","seen")))
  if (!"r_use" %in% names(h)) h[, r_use := r_pre]
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
         year(date) %in% yrs]
  h <- merge(h, reg, by = "event_id", all.x = TRUE)
  h <- h[!is.na(family)]
  a <- h[, .(race_key, family, i = seq_len(.N), place, r = r_use), by = race_key][, -1]
  m <- merge(a, a, by = c("race_key", "family"), allow.cartesian = TRUE,
             suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  d <- m$r.x - m$r.y
  m[, cw := as.numeric((d > 0) == (place.x < place.y))]
  m[d == 0, cw := 0.5]
  m[, .(tag = tag, pairs = .N, conc = 100 * mean(cw)), by = family]
}
grab <- function(yrs, lab) {
  x <- rbindlist(lapply(arms$tag, score_arm, yrs = yrs))
  x <- merge(x, arms, by = "tag")
  b <- x[tag == arms$tag[1], .(family, base = conc, pairs)]
  y <- merge(x[tag != arms$tag[1], .(tag, value, family, conc)], b, by = "family")
  y[, `:=`(delta = conc - base, window = lab)]
  y[]
}
tu <- grab(TUNE, "tune")
se <- grab(SEAL, "sealed")
cat(sprintf("\nfamilies scored: %d | tune pairs %s | sealed pairs %s\n",
            uniqueN(tu$family), format(sum(unique(tu[, .(family, pairs)])$pairs), big.mark = ","),
            format(sum(unique(se[, .(family, pairs)])$pairs), big.mark = ",")))

# best value per family on the tune window
setorder(tu, family, -delta)
best <- tu[, .SD[1], by = family][, .(family, value, d_tune = delta, pairs)]
best[, noise := 100 * sqrt(0.75 * 0.25 / pairs)]   # context, NOT a gate
chk <- merge(best, se[, .(family, value, d_seal = delta)], by = c("family", "value"))
chk[, keep := d_tune > 0 & d_seal > 0]

cat("\n=== best value per family, and whether it replicates ===\n")
setorder(chk, -d_tune)
print(chk[, .(family, value, pairs, noise = round(noise, 3),
              tune = round(d_tune, 3), sealed = round(d_seal, 3), keep)])
cat(sprintf("\nfamilies whose best beats the incumbent on BOTH windows: %d of %d\n",
            sum(chk$keep), nrow(chk)))
cat("The noise column is context. A family is kept on sign agreement across two\n")
cat("independent windows, not on clearing its own floor - gating on the floor is\n")
cat("what turned a real cross-event effect into \"not supported\" on 2026-08-17.\n")

# --- shrink toward the incumbent ---------------------------------------------
# A family with little evidence should barely move. Same shape as the engine's
# own shrinkage and as pool_event_params.R: w = pairs / (pairs + kappa).
KAP <- .env_num <- as.numeric(Sys.getenv("FAM_KAPPA", "20000"))
inc <- arms$value[1]
chk[, w := pairs / (pairs + KAP)]
chk[, value_shrunk := inc + w * (value - inc)]
cat(sprintf("\nshrinkage kappa %s pairs: weights %.2f-%.2f\n",
            format(KAP, big.mark = ","), min(chk$w), max(chk$w)))
print(chk[keep == TRUE, .(family, incumbent = inc, best = value,
                          shrunk = round(value_shrunk, 4), w = round(w, 2))])

keepf <- chk[keep == TRUE]
if (!nrow(keepf)) {
  cat("\nNOTHING replicates. The honest conclusion is that this parameter does not\n")
  cat("vary by family and the global value should stand.\n")
} else {
  o <- merge(reg, keepf[, .(family, v = value_shrunk)], by = "family")
  o <- o[, .(event_id)][, (PARAM) := merge(reg, keepf[, .(family, v = value_shrunk)],
                                           by = "family")$v]
  write_parquet(o, OUT)
  cat(sprintf("\nwrote %s: %d events across %d families\n",
              basename(OUT), nrow(o), nrow(keepf)))
  cat("VERIFY IT: re-run the engine with SEQ_EVPARAM pointing at that file. The\n")
  cat("assembled configuration is not what any single arm measured, so if the\n")
  cat("gain does not reproduce it was arithmetic rather than a model.\n")
}
