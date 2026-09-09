# How much should the event-level constant contribute to an athlete's sigma?
#
# THE BLEND (ability.R:1418, then :1437):
#   sigma_emitted = ratio * (w_total * own + PSEUDO_N * target) / (w_total + PSEUDO_N)
#
# PSEUDO_N is hardcoded to 2. That looks gentle until you measure w_total, whose
# median is 4.30 -- so the target supplies 32% of the median athlete's sigma,
# 53% at the 25th percentile and 79% at the 10th. A constant carries NO
# between-athlete information, so whatever share it takes is subtracted from
# sigma's ability to rank anyone. Measured out of sample, sigma predicts future
# scatter at 0.057 while the underlying decay-weighted SD manages 0.101.
#
# Swapping WHICH constant is blended does not help (tested: the measured
# sigma_within scores 0.049, worse than the placeholder's 0.060). The question
# is the WEIGHT, not the value.
#
# NO PACKAGE EDIT NEEDED. The blend is invertible from quantities the estimator
# already returns, so the athlete's own pre-blend spread can be recovered and
# re-blended at any PSEUDO_N:
#   own = ((sigma_emitted / ratio) * (w + PSN) - PSN * target) / w
# and then scored against what the athlete ACTUALLY did next.
#
# Usage:  CITIUS_PSN_EVENT=AT-100Metres-M Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV   <- Sys.getenv("CITIUS_PSN_EVENT", "AT-100Metres-M")
CUT  <- as.Date(Sys.getenv("CITIUS_PSN_CUT", "2024-01-01"))
MINH <- as.integer(Sys.getenv("CITIUS_PSN_MINHALF", "6"))
cal  <- deployed_calibration(OUT)
reg  <- as.data.table(citius_events())[event_id == EV]
CV    <- reg$cv_prior[1]
FAM   <- reg$family[1]
RATIO <- as.data.table(cal$sigma_context)[family == FAM]$ratio[1]
PSN   <- citius:::.CITIUS_SIGMA_PSEUDO_N

h <- setDT(deployed_history(OUT, events = EV, from = CUT - 2920, to = Sys.Date()))
h[, athlete_id := as.character(athlete_id)]
h <- h[is.finite(mark) & !is.na(date)]
h[, half := fifelse(date < CUT, 1L, 2L)]
keep <- h[, .(n1 = sum(half==1L), n2 = sum(half==2L)), by=athlete_id][n1 >= MINH & n2 >= MINH]
h <- h[athlete_id %chin% keep$athlete_id]
stopifnot(nrow(h) > 0)
truth <- h[half == 2L, .(future_sd = sd(log(mark))), by = athlete_id]

ab <- as.data.table(estimate_ability(h[half == 1L], as_of = CUT, half_life = 365,
                                     calibration = cal, adjust_context = TRUE,
                                     adjust_race = FALSE))
d <- ab[event_id == EV, .(athlete_id = as.character(athlete_id), sigma, w_total)]
d <- merge(d, truth, by = "athlete_id")[is.finite(sigma) & w_total > 0]

# Recover the athlete's own pre-blend spread.
d[, own := ((sigma / RATIO) * (w_total + PSN) - PSN * CV) / w_total]
cat(sprintf("%s | %s athletes | ratio %.4f | cv_prior %.5f | deployed PSEUDO_N %s\n",
            EV, format(nrow(d), big.mark=","), RATIO, CV, format(PSN)))
cat(sprintf("recovered own-spread: median %.5f (truth median %.5f)\n\n",
            median(d$own), median(d$future_sd)))
# Sanity: re-blending at the deployed PSEUDO_N must reproduce the emitted sigma.
d[, check := RATIO * (w_total*own + PSN*CV)/(w_total + PSN)]
cat(sprintf("inversion check -- max |reblended - emitted| = %.2e (should be ~0)\n\n",
            max(abs(d$check - d$sigma))))

res <- rbindlist(lapply(c(0, 0.25, 0.5, 1, 2, 4, 8), function(p) {
  s <- RATIO * (d$w_total * d$own + p * CV) / (d$w_total + p)
  data.table(pseudo_n = p,
             constant_share = round(100 * median(p/(d$w_total + p)), 1),
             median_sigma = round(median(s), 5),
             pearson = round(cor(s, d$future_sd), 4),
             spearman = round(cor(s, d$future_sd, method = "spearman"), 4))
}))
cat("SWEEPING THE WEIGHT GIVEN TO THE EVENT-LEVEL CONSTANT\n")
cat("(pseudo_n 0 = the athlete's own spread, no constant at all)\n\n")
print(res)
cat(sprintf("\ntruth median %.5f | plain SD baseline scores ~0.091 pearson / 0.130 spearman\n",
            median(d$future_sd)))
