# Fit recency half-life per INDIVIDUAL EVENT (event_id) and evaluate MAE vs PER-FAMILY.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("Loading athletics corpus...")
x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
x <- flag_implausible(x)[!is.na(perf) & !is.na(event_id) & !is.na(date)]

elite_file <- file.path(OUT, "elite_cohort.rds")
if (file.exists(elite_file)) {
  elite <- readRDS(elite_file)
  x <- x[as.character(athlete_id) %in% as.character(elite)]
}

reg <- .citius_event_registry[, c("event_id", "family")]
dt <- merge(x, reg, by = "event_id", all.x = TRUE, sort = FALSE)
dt <- dt[!is.na(family)]

setorder(dt, athlete_id, event_id, date)
dt[, n_tot := .N, by = .(athlete_id, event_id)]
usable <- dt[n_tot >= 4L]

candidates <- c(14, 30, 45, 60, 90, 135, 180, 270, 365, 540, 730, 1095, 1825, 3650)

say("Fitting half-life per INDIVIDUAL EVENT...")
scored_event <- data.table::rbindlist(lapply(candidates, function(hl) {
  usable[, {
    target <- perf[.N]
    t_date <- date[.N]
    past <- seq_len(.N - 1L)
    w <- 0.5^(as.numeric(t_date - date[past]) / hl)
    pred <- if (sum(w) > 0) stats::weighted.mean(perf[past], w) else NA_real_
    .(family = data.table::first(family), err = abs(target - pred))
  }, by = .(athlete_id, event_id)][, .(half_life = hl, mae = mean(err, na.rm = TRUE),
                                       n = sum(!is.na(err))), by = .(family, event_id)]
}))

best_event <- scored_event[, .SD[which.min(mae)], by = .(family, event_id)][n >= 50L]
lo <- min(candidates); hi <- max(candidates)
best_event[, identified := half_life > lo & half_life < hi]

say(sprintf("Fitted per-event half-life for %d events (%d identified)", nrow(best_event), sum(best_event$identified)))
print(head(best_event[order(event_id)], 20))

fitted_family <- readRDS(file.path(OUT, "half_life_fitted.rds"))

score_hl_spec <- function(mode) {
  usable[, {
    target <- perf[.N]
    t_date <- date[.N]
    past <- seq_len(.N - 1L)
    fam <- data.table::first(family)
    ev <- data.table::first(event_id)
    hl_val <- if (mode == "365") 365
    else if (mode == "family") {
      v <- fitted_family$half_life[match(fam, fitted_family$family)]
      if (is.na(v) || !is.finite(v)) 365 else v
    } else if (mode == "event") {
      v <- best_event$half_life[match(ev, best_event$event_id)]
      if (is.na(v) || !is.finite(v)) {
        # Fallback to family
        vf <- fitted_family$half_life[match(fam, fitted_family$family)]
        if (is.na(vf) || !is.finite(vf)) 365 else vf
      } else v
    }
    w <- 0.5^(as.numeric(t_date - date[past]) / hl_val)
    pred <- if (sum(w) > 0) stats::weighted.mean(perf[past], w) else NA_real_
    .(family = fam, event_id = ev, err = abs(target - pred))
  }, by = .(athlete_id, event_id)]
}

say("Evaluating MAE for 365-day, per-family, and per-event...")
r365 <- score_hl_spec("365")
rfam <- score_hl_spec("family")
reve <- score_hl_spec("event")

tot_365 <- 100 * (exp(mean(r365$err, na.rm = TRUE)) - 1)
tot_fam <- 100 * (exp(mean(rfam$err, na.rm = TRUE)) - 1)
tot_eve <- 100 * (exp(mean(reve$err, na.rm = TRUE)) - 1)

f365 <- r365[, .(m365 = 100 * (exp(mean(err, na.rm = TRUE)) - 1)), by = family]
ffam <- rfam[, .(mfam = 100 * (exp(mean(err, na.rm = TRUE)) - 1)), by = family]
feve <- reve[, .(meve = 100 * (exp(mean(err, na.rm = TRUE)) - 1)), by = family]
comp <- merge(f365, merge(ffam, feve, by = "family"), by = "family")

comp[, diff_fam_vs_365 := round(mfam - m365, 4)]
comp[, diff_eve_vs_fam := round(meve - mfam, 4)]

cat("\n=== MAE COMPARISON: 365-DAY vs PER-FAMILY vs PER-EVENT ===\n")
print(comp[order(family)])

cat(sprintf("\nOVERALL MAE SUMMARY:\n"))
cat(sprintf("  Global 365-day Baseline : %.4f%%\n", tot_365))
cat(sprintf("  Per-Family Half-Life    : %.4f%%  (diff vs 365: %+.4f pp)\n", tot_fam, tot_fam - tot_365))
cat(sprintf("  Per-Event Half-Life     : %.4f%%  (diff vs family: %+.4f pp)\n", tot_eve, tot_eve - tot_fam))
