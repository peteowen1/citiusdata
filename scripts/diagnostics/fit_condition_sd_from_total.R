# What SHOULD condition_sd be, so the predicted distribution matches reality?
#
# THE BUG (2026-09-05). simulate_event() draws an athlete's own sigma and then
# adds a shared condition_sd on top. Measured on the men's 100m:
#
#   sigma (emitted)                 0.00772
#   condition_sd                    0.01450
#   implied total                   0.01640   = sqrt(0.00772^2 + 0.01450^2)
#   ACTUAL spread of future marks   0.01372
#
# The predicted distribution is ~20% too wide, which is what gave Noah Lyles a
# 5% chance of beating the world record.
#
# WHY. sigma is computed on context-REMOVED residuals, while condition_sd is
# `sqrt(var(c_r) - bias)` (calibrate.R:406) -- straight from the race effects,
# which are inflated ~1.7x by decompose_races() running 400 sweeps where the
# signal is extracted in 2 (see race-shock-is-real-but-oversized-2026-09-05.md).
# So the two terms are calibrated on different bases and double-count.
#
# THE FIX MEASURED HERE. The total is directly observable: an athlete's actual
# spread of future marks IS sigma + conditions combined. So
#
#   condition_sd = sqrt(max(total_observed^2 - sigma^2, 0))
#
# per event, fitted on a window and validated out of sample. This makes the two
# terms consistent by construction rather than hoping two independent
# estimates happen to add up.
#
# Usage:  Rscript citiusdata/scripts/diagnostics/fit_condition_sd_from_total.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT  <- here::here("citiusdata", "data")
CUT  <- as.Date(Sys.getenv("CITIUS_CSD_CUT", "2024-01-01"))
MINH <- as.integer(Sys.getenv("CITIUS_CSD_MINHALF", "6"))
MINA <- as.integer(Sys.getenv("CITIUS_CSD_MINATH", "40"))
DEST <- Sys.getenv("CITIUS_CSD_OUT", "condition_sd_from_total.csv")
cal  <- deployed_calibration(OUT)
say  <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

reg <- as.data.table(citius_events())[, .(event_id, family)]
evc <- as.data.table(cal$events)
evs <- evc[calibrated %in% TRUE]$event_id
say(sprintf("%d calibrated events", length(evs)))

res <- rbindlist(lapply(evs, function(EV) {
  h <- tryCatch(setDT(deployed_history(OUT, events = EV, from = CUT - 2920, to = Sys.Date())),
                error = function(e) NULL)
  if (is.null(h) || !nrow(h)) return(NULL)
  h[, athlete_id := as.character(athlete_id)]
  h <- h[is.finite(mark) & !is.na(date)]
  h[, half := fifelse(date < CUT, 1L, 2L)]
  keep <- h[, .(n1 = sum(half==1L), n2 = sum(half==2L)), by = athlete_id][n1 >= MINH & n2 >= MINH]
  if (nrow(keep) < MINA) return(NULL)
  h <- h[athlete_id %chin% keep$athlete_id]
  ab <- tryCatch(as.data.table(estimate_ability(h[half==1L], as_of = CUT, half_life = 365,
                                                calibration = cal, adjust_context = TRUE,
                                                adjust_race = FALSE)), error = function(e) NULL)
  if (is.null(ab)) return(NULL)
  a <- ab[event_id == EV, .(athlete_id = as.character(athlete_id), sigma)]
  # The TOTAL the simulator is trying to reproduce: what athletes actually did.
  tot <- h[half==2L, .(total_obs = sd(log(mark))), by = athlete_id]
  d <- merge(a, tot, by = "athlete_id")[is.finite(sigma) & is.finite(total_obs)]
  if (nrow(d) < MINA) return(NULL)
  s2 <- median(d$sigma)^2; t2 <- median(d$total_obs)^2
  data.table(event_id = EV,
             athletes = nrow(d),
             sigma = median(d$sigma),
             total_observed = median(d$total_obs),
             condition_sd_deployed = evc[event_id == EV]$condition_sd[1],
             condition_sd_fitted = sqrt(max(t2 - s2, 0)))
}))
if (!nrow(res)) stop("no events scored")
res <- merge(res, reg, by = "event_id", all.x = TRUE)
res[, implied_total_deployed := sqrt(sigma^2 + condition_sd_deployed^2)]
res[, over_wide_pct := 100 * (implied_total_deployed / total_observed - 1)]

say(sprintf("fitted on %d events", nrow(res)))
cat("\nHOW OVER-WIDE IS THE DEPLOYED PREDICTED DISTRIBUTION? (by family)\n\n")
print(res[, .(events = .N,
              sigma = round(median(sigma), 5),
              cond_deployed = round(median(condition_sd_deployed), 5),
              cond_fitted = round(median(condition_sd_fitted), 5),
              total_deployed = round(median(implied_total_deployed), 5),
              total_observed = round(median(total_observed), 5),
              over_wide_pct = round(median(over_wide_pct), 1)), by = family][order(-over_wide_pct)])
cat(sprintf("\npooled: deployed total %.5f vs observed %.5f => %+.1f%% too wide\n",
            median(res$implied_total_deployed), median(res$total_observed),
            100*(median(res$implied_total_deployed)/median(res$total_observed) - 1)))
cat(sprintf("events where the deployed total is TOO NARROW: %d of %d\n",
            sum(res$over_wide_pct < 0), nrow(res)))
fwrite(res, file.path(OUT, DEST))
say(sprintf("wrote %s", DEST))
