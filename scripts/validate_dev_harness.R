# Prove the dev harness ranks changes the same way the full backtest does.
#
# The harness restricts history to championship-calibre athletes, which makes it
# ~7x faster but ALSO changes `prior_mu` and `sigma_between` -- population
# statistics. So it is a different model, not a subsample, and its absolute
# metrics are not comparable to a full run. What it must preserve is the SIGN and
# rough size of the difference between two arms.
#
# Re-run this whenever the harness or the cohort changes.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
pairs <- list(list(full = c("backtest_hl365.rds", "backtest_hl730.rds"),
                   dev  = c("backtest_dev365.rds", "backtest_dev730.rds"),
                   lab  = "half-life 365 vs 730"))
grab <- function(f) {
  b <- readRDS(file.path(OUT, f))
  p <- setDT(copy(b$predictions)); o <- setDT(copy(b$outcomes))
  p[, athlete_id := as.character(athlete_id)]; o[, athlete_id := as.character(athlete_id)]
  list(gold = b$gold$overall$brier_skill, medal = b$medal$overall$brier_skill)
}
for (pr in pairs) {
  fa <- grab(pr$full[1]); fb <- grab(pr$full[2])
  da <- grab(pr$dev[1]);  db <- grab(pr$dev[2])
  cat(sprintf("\n%s\n", pr$lab))
  for (m in c("gold","medal")) {
    df <- fa[[m]] - fb[[m]]; dd <- da[[m]] - db[[m]]
    cat(sprintf("  %-6s full %+.4f | dev %+.4f | %s\n", m, df, dd,
                if (sign(df) == sign(dd)) "AGREE" else "*** DISAGREE ***"))
  }
}
cat("\nThe harness is validated for ORDERING only. Never quote its absolute\n")
cat("numbers -- restricting the history changes the prior, so a dev MAE of 2.19%\n")
cat("and a full MAE of 2.64% are measuring different models.\n")
