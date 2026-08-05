# Compare predictive MAE between fitted per-family half-lives and global 365-day scalar baseline.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

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
dt[, idx := seq_len(.N), by = .(athlete_id, event_id)]
dt[, n_tot := .N, by = .(athlete_id, event_id)]
usable <- dt[n_tot >= 4L] # min_history = 3L + 1

fitted_hl <- readRDS(file.path(OUT, "half_life_fitted.rds"))

score_hl_spec <- function(hl_spec) {
  usable[, {
    target <- perf[.N]
    t_date <- date[.N]
    past <- seq_len(.N - 1L)
    fam <- data.table::first(family)
    hl_val <- if (is.numeric(hl_spec)) hl_spec else {
      v <- hl_spec$half_life[match(fam, hl_spec$family)]
      if (is.na(v) || !is.finite(v)) 365 else v
    }
    w <- 0.5^(as.numeric(t_date - date[past]) / hl_val)
    pred <- if (sum(w) > 0) stats::weighted.mean(perf[past], w) else NA_real_
    .(family = fam, err = abs(target - pred))
  }, by = .(athlete_id, event_id)]
}

say("Scoring 365-day scalar baseline...")
res_365 <- score_hl_spec(365)
say("Scoring fitted per-family half-life...")
res_fit <- score_hl_spec(fitted_hl)

fam_365 <- res_365[, .(mae_365 = mean(err, na.rm = TRUE), n = .N), by = family]
fam_fit <- res_fit[, .(mae_fit = mean(err, na.rm = TRUE)), by = family]
comp <- merge(fam_365, fam_fit, by = "family")

comp[, mae_365_pct := round(100 * (exp(mae_365) - 1), 4)]
comp[, mae_fit_pct := round(100 * (exp(mae_fit) - 1), 4)]
comp[, diff_pct := round(mae_fit_pct - mae_365_pct, 4)]
comp[, rel_imp_pct := round(100 * (mae_365 - mae_fit) / mae_365, 2)]

setorder(comp, -rel_imp_pct)

tot_365 <- mean(res_365$err, na.rm = TRUE)
tot_fit <- mean(res_fit$err, na.rm = TRUE)
tot_365_pct <- 100 * (exp(tot_365) - 1)
tot_fit_pct <- 100 * (exp(tot_fit) - 1)
tot_rel_imp <- 100 * (tot_365 - tot_fit) / tot_365

cat("\n=== PREDICTIVE MAE COMPARISON: GLOBAL 365-DAY vs FITTED PER-FAMILY ===\n")
print(comp[, .(family, half_life = fitted_hl$half_life[match(family, fitted_hl$family)],
              n, mae_365_pct, mae_fit_pct, diff_pct, rel_imp_pct)])

cat(sprintf("\nOVERALL MAE (all families combined):\n"))
cat(sprintf("  Global 365-day Half-Life: %.4f%%\n", tot_365_pct))
cat(sprintf("  Fitted Per-Family       : %.4f%%\n", tot_fit_pct))
cat(sprintf("  Absolute Difference     : %+.4f pp\n", tot_fit_pct - tot_365_pct))
cat(sprintf("  Relative Improvement    : %+.2f%%\n", tot_rel_imp))
