# Re-fit per-event `sigma_within` so the simulated spread matches observed error,
# for a SPECIFIC history x cohort configuration.
#
# WHY THIS MUST BE RE-RUN PER CONFIGURATION. A scale correction fitted against one
# setup can have the wrong SIGN in another. Measured 2026-07-29: sigma runs 16%
# too NARROW under harvest x elite and ~12% too WIDE under corpus x all. Both
# tested well on their own data; combining them naively ships a correction
# pointing the wrong way.
#
# WHAT IS MEASURED. Within each race, each athlete's error minus that race's mean
# error. That removes whatever the whole field shared -- conditions, pacing, a
# fast track -- and leaves exactly the individual component `sigma` describes.
# Standardising by sigma should then give sd(z) = 1.
#
# Do NOT fit this against the raw error. Raw mark errors contain the shared race
# shock, so sd(z) exceeds 1 by construction and says nothing about sigma.
#
# WHY A HELD-OUT PERIOD. This estimates one parameter per event against observed
# error. Fitting and scoring on the same marks guarantees an improvement that
# means nothing.
#
# Usage:
#   CITIUS_REFIT_BT=backtest_corpus_elite.rds \
#   CITIUS_REFIT_CAL=calibration_corpus.rds \
#   CITIUS_REFIT_OUT=calibration_corpus_widened.rds \
#   Rscript scripts/refit_scale.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")

BT   <- Sys.getenv("CITIUS_REFIT_BT",  "backtest_corpus_elite.rds")
CAL  <- Sys.getenv("CITIUS_REFIT_CAL", "calibration_corpus.rds")
DEST <- Sys.getenv("CITIUS_REFIT_OUT", sub("[.]rds$", "_widened.rds", CAL))
CUT  <- as.Date(Sys.getenv("CITIUS_REFIT_CUT", "2024-01-01"))
MIN_N <- .env_int("CITIUS_REFIT_MIN_N", "150")

b <- readRDS(file.path(OUT, BT))
mt <- b$meta
cli::cli_alert_info(
  "Fitting against {.file {BT}} - history {.val {mt$history %||% 'unknown'}}, cohort {.val {mt$cohort %||% 'unknown'}}."
)
cli::cli_alert_warning("The result is valid ONLY for that configuration.")

p <- setDT(copy(b$predictions))[is.finite(median_mark)]
p[, athlete_id := as.character(athlete_id)]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
truth <- unique(ch[!is.na(race_key) & !is.na(mark) & mark > 0,
  .(race_id = race_key, athlete_id = as.character(athlete_id), actual = mark,
    event_id, comp_start)], by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events()[, c("event_id", "family", "orientation")])
truth <- merge(truth, reg, by = "event_id", all.x = TRUE)
cal <- readRDS(file.path(OUT, CAL))
ev <- as.data.table(cal$events)

d <- merge(p, truth, by = c("race_id", "athlete_id"))
d[, err := orientation * (log(median_mark) - log(actual))]
d <- merge(d[is.finite(err)], ev[, .(event_id, sigma_within)], by = "event_id")
d[, n_in := .N, by = race_id]
d <- d[n_in >= 4]
# Remove what the field shared; correct the variance lost to estimating the mean.
d[, e_ind := (err - mean(err)) / sqrt((.N - 1) / .N), by = race_id]

n_all <- nrow(d)
fit <- d[!is.na(comp_start) & comp_start < CUT]
cli::cli_alert_info(
  "Fitting on {format(nrow(fit), big.mark = ',')} of {format(n_all, big.mark = ',')} marks (before {CUT})."
)
if (!nrow(fit)) cli::cli_abort("No marks before the cut date.")

byev <- fit[, .(n = .N, f = sd(e_ind) / mean(sigma_within)), by = .(event_id, family)]
byfa <- fit[, .(ff = sd(e_ind) / mean(sigma_within)), by = family]
byev <- merge(byev, byfa, by = "family")
# Thin events borrow their family's factor rather than fitting noise.
byev[, factor := fifelse(n >= MIN_N, f, ff)]
# Bound it. An unbounded scale on a thin event can swing wildly, and the
# correction is meant to be a nudge, not a rewrite.
byev[, factor := pmax(pmin(factor, 1.8), 0.6)]

cat("\n=== widening factor by family (1.0 = sigma already correct) ===\n")
print(byev[, .(events = .N, marks = sum(n), factor = round(median(factor), 3)),
           by = family][order(-factor)])
cat(sprintf("\npooled sd(z) before refit: %.3f  (1.0 is correct)\n",
            sd(fit$e_ind / fit$sigma_within, na.rm = TRUE)))

e2 <- copy(ev)
e2[byev, on = "event_id", sigma_within := sigma_within * i.factor]
cal2 <- cal; cal2$events <- e2
attr(cal2, "refit") <- list(from = BT, history = mt$history, cohort = mt$cohort,
                            cut = CUT, at = Sys.time())
saveRDS(cal2, file.path(OUT, DEST))
cli::cli_alert_success("Wrote {.file {DEST}}")
cli::cli_alert_info("A/B it before adopting: the fit is held out, the adoption is not.")
