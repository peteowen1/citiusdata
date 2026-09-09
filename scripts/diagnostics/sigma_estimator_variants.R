# Can any available sigma estimator beat a plain standard deviation?
#
# THE PROBLEM. Measured out of sample (test_sigma_estimator.R), the deployed
# sigma predicts an athlete's FUTURE scatter at pearson 0.057 while a plain SD
# of their own past marks manages 0.097. It also runs at about half the true
# level (median 0.00749 against a median future scatter of 0.01376). Four causes
# were eliminated: race-effect over-iteration, round contamination, shrinkage
# toward the event value, and the context adjustments. What is left is the
# estimator itself.
#
# WHAT THIS TESTS. estimate_ability() already exposes the switches, so the
# variants need no code change:
#
#   robust_sigma = TRUE  + parts estimator,weight   the deployed path
#   robust_sigma = TRUE  + parts weight             no robust estimator
#   robust_sigma = TRUE  + parts estimator          no weighted blend
#   robust_sigma = FALSE                            plain sigma_raw
#
# plus the naive baseline the model has to beat: sd(log(mark)) over the
# athlete's own first-half marks, computed outside the model entirely.
#
# THE YARDSTICK IS OUT OF SAMPLE. Estimate on races before a fixed cut, score
# against the athlete's ACTUAL scatter after it. Correlating an estimator with
# the same races it was fitted on is circular and is what made the deployed
# number look like 0.30 rather than 0.057.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT  <- here::here("citiusdata", "data")
EV   <- Sys.getenv("CITIUS_SV_EVENT", "AT-100Metres-M")
CUT  <- as.Date(Sys.getenv("CITIUS_SV_CUT", "2024-01-01"))
MINH <- as.integer(Sys.getenv("CITIUS_SV_MINHALF", "6"))
cal  <- deployed_calibration(OUT)

h <- setDT(deployed_history(OUT, events = EV, from = CUT - 2920, to = Sys.Date()))
h[, athlete_id := as.character(athlete_id)]
h <- h[is.finite(mark) & !is.na(date)]
h[, half := fifelse(date < CUT, 1L, 2L)]
keep <- h[, .(n1 = sum(half == 1L), n2 = sum(half == 2L)), by = athlete_id][n1 >= MINH & n2 >= MINH]
h <- h[athlete_id %chin% keep$athlete_id]
stopifnot("no athletes either side of the cut" = nrow(h) > 0)
cat(sprintf("%s | cut %s | %s athletes with >= %d results each side\n\n",
            EV, CUT, format(uniqueN(h$athlete_id), big.mark = ","), MINH))

truth <- h[half == 2L, .(future_sd = sd(log(mark))), by = athlete_id]
naive <- h[half == 1L, .(plain_sd = sd(log(mark))), by = athlete_id]

variants <- list(
  "deployed (robust+weight)" = list(rs = TRUE,  parts = c("estimator", "weight")),
  "weight only (no robust)"  = list(rs = TRUE,  parts = "weight"),
  "robust only (no weight)"  = list(rs = TRUE,  parts = "estimator"),
  "robust_sigma = FALSE"     = list(rs = FALSE, parts = c("estimator", "weight")),
  # The blend target matters more than any of the above. The registry cv_prior
  # for the 100m is 0.008 against a MEASURED sigma_within of 0.01249, and at a
  # median w_total of 4.30 that placeholder supplies 32% of the answer -- 53% at
  # the 25th percentile, 79% at the 10th. A constant carries no between-athlete
  # information, so whatever share it takes is subtracted from sigma's ability
  # to rank anyone.
  "target (measured, both paths)" = list(rs = TRUE, parts = c("estimator", "weight", "target")),
  "target_shrink (shrinkage only)" = list(rs = TRUE, parts = c("estimator", "weight", "target_shrink"))
)
res <- rbindlist(lapply(names(variants), function(nm) {
  v <- variants[[nm]]
  ab <- tryCatch(as.data.table(
    estimate_ability(h[half == 1L], as_of = CUT, half_life = 365, calibration = cal,
                     adjust_context = TRUE, adjust_race = FALSE,
                     robust_sigma = v$rs, sigma_parts = v$parts)),
    error = function(e) { cat(sprintf("  %s FAILED: %s\n", nm, conditionMessage(e))); NULL })
  if (is.null(ab)) return(NULL)
  a <- ab[event_id == EV, .(athlete_id = as.character(athlete_id), sigma)]
  d <- Reduce(function(x, y) merge(x, y, by = "athlete_id"), list(a, naive, truth))
  d <- d[is.finite(sigma) & is.finite(future_sd)]
  data.table(variant = nm, n = nrow(d),
             pearson = round(cor(d$sigma, d$future_sd), 4),
             spearman = round(cor(d$sigma, d$future_sd, method = "spearman"), 4),
             median_sigma = round(median(d$sigma), 5))
}))

d0 <- merge(naive, truth, by = "athlete_id")
res <- rbind(res, data.table(variant = "PLAIN SD (the baseline)", n = nrow(d0),
                             pearson = round(cor(d0$plain_sd, d0$future_sd), 4),
                             spearman = round(cor(d0$plain_sd, d0$future_sd, method = "spearman"), 4),
                             median_sigma = round(median(d0$plain_sd), 5)))
res <- rbind(res, data.table(variant = "-- truth (median future) --", n = nrow(d0),
                             pearson = NA_real_, spearman = NA_real_,
                             median_sigma = round(median(d0$future_sd), 5)))
cat("PREDICTING THE SAME ATHLETE'S FUTURE SCATTER (higher = better)\n\n")
print(res)
cat("\nA variant only earns its complexity if it beats PLAIN SD.\n")
