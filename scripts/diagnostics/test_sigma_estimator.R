# Does the model's sigma predict an athlete's FUTURE scatter better than a
# plain standard deviation would?
#
# WHY THIS TEST. sigma correlates only 0.30 with athletes' observed scatter
# (docs/incidents/sigma-does-not-track-consistency-2026-09-05.md), and Lyles
# sits at the 10th percentile for real consistency with an 86th-percentile
# sigma. Three explanations have been eliminated: the race-effect over-iteration
# (moves it to 0.35 only), round contamination (7% of variance), and shrinkage
# toward the event value (only ~11% of the blend at his evidence level). What is
# left is the robust estimator itself: `sigma = sigma_rob * k`, a one-sided
# "good side" spread rescaled by a global constant.
#
# THE TEST IS OUT OF SAMPLE AND THAT IS THE POINT. Correlating sigma with an
# athlete's scatter over the SAME races is circular -- both are computed from
# the same numbers, and the earlier 0.30 shares that flaw. Split each athlete's
# history in half instead: estimate on the first half, score against the actual
# scatter of the second. A spread estimate that cannot predict the same
# athlete's next-period spread is not measuring a property of the athlete.
#
# Usage:  CITIUS_SIGTEST_EVENT=AT-100Metres-M Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV  <- Sys.getenv("CITIUS_SIGTEST_EVENT", "AT-100Metres-M")
MINH <- as.integer(Sys.getenv("CITIUS_SIGTEST_MINHALF", "6"))
cal <- deployed_calibration(OUT)
ORI <- as.data.table(citius_events())[event_id == EV]$orientation[1]

h <- setDT(deployed_history(OUT, events = EV, from = Sys.Date() - 2920, to = Sys.Date()))
h[, athlete_id := as.character(athlete_id)]
h <- h[is.finite(mark) & !is.na(date)]
# ONE global split date, not a per-athlete one. A per-athlete split needs an
# estimate_ability() call per athlete (thousands of calls over full history) and
# buys nothing here: a fixed date is still strictly out of sample, and it has
# the side benefit that every athlete is estimated on the same information
# boundary rather than each on their own.
CUT <- as.Date(Sys.getenv("CITIUS_SIGTEST_CUT", "2024-01-01"))
h[, half := fifelse(date < CUT, 1L, 2L)]
keep <- h[, .(n1 = sum(half == 1L), n2 = sum(half == 2L)), by = athlete_id][n1 >= MINH & n2 >= MINH]
h <- h[athlete_id %chin% keep$athlete_id]
if (!nrow(h)) stop("no athletes with enough history either side of ", CUT)
cat(sprintf("%s: split at %s, %s athletes with >= %d results each side\n",
            EV, CUT, format(uniqueN(h$athlete_id), big.mark = ","), MINH))

ab <- as.data.table(estimate_ability(h[half == 1L], as_of = CUT, half_life = 365,
                                     calibration = cal, adjust_context = TRUE,
                                     adjust_race = FALSE))
ab1 <- ab[event_id == EV, .(athlete_id = as.character(athlete_id),
                            model_sigma = sigma, w_total)]
if (!nrow(ab1)) stop("no first-half estimates produced")

# The naive alternative: a plain SD of the athlete's own first-half log-marks.
naive <- h[half == 1L, .(plain_sd = sd(log(mark)), n1 = .N), by = athlete_id]
# The truth being predicted: their ACTUAL scatter in the second half.
truth <- h[half == 2L, .(future_sd = sd(log(mark)), n2 = .N), by = athlete_id]

d <- Reduce(function(a, b) merge(a, b, by = "athlete_id"), list(ab1, naive, truth))
d <- d[is.finite(model_sigma) & is.finite(plain_sd) & is.finite(future_sd)]
cat(sprintf("scored on %s athletes\n\n", format(nrow(d), big.mark = ",")))

cr <- function(x, y) c(pearson = cor(x, y), spearman = cor(x, y, method = "spearman"))
m1 <- cr(d$model_sigma, d$future_sd); m2 <- cr(d$plain_sd, d$future_sd)
cat("PREDICTING THE SAME ATHLETE'S SECOND-HALF SCATTER:\n")
cat(sprintf("  model sigma   pearson %.3f | spearman %.3f\n", m1[1], m1[2]))
cat(sprintf("  plain SD      pearson %.3f | spearman %.3f\n", m2[1], m2[2]))
cat(sprintf("\n  => the model's estimator is %s than a plain SD\n",
            if (m1[1] > m2[1]) "BETTER" else "WORSE"))
cat(sprintf("  median model sigma %.5f | median plain SD %.5f | median future %.5f\n",
            median(d$model_sigma), median(d$plain_sd), median(d$future_sd)))
# Does it compress differences between athletes? A spread estimate that is
# nearly constant cannot rank anyone, however unbiased its level.
cat(sprintf("\n  spread ACROSS athletes: model sigma sd %.5f | plain SD sd %.5f | future sd %.5f\n",
            sd(d$model_sigma), sd(d$plain_sd), sd(d$future_sd)))
cat(sprintf("  coefficient of variation: model %.3f | plain %.3f | future %.3f\n",
            sd(d$model_sigma)/mean(d$model_sigma), sd(d$plain_sd)/mean(d$plain_sd),
            sd(d$future_sd)/mean(d$future_sd)))
