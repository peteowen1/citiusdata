# One metric set for every model version, so improvements can be compared rather
# than asserted.
#
# The project has been scored almost entirely on RANKINGS -- Brier skill for gold
# and medals. That is the product, but it is only half the model: `median_mark`
# is a point prediction of the actual time or distance, and until now nothing
# ever checked it. A change can sharpen placings while making predicted times
# worse, and nobody would have noticed.
#
# Two families of metric, deliberately kept apart:
#
#   ABILITY  - how close the predicted mark is to the mark actually recorded.
#              Measured in log units (the model's own scale) and as a percentage
#              of the mark, which is comparable across a 9.8s sprint and a
#              2:01 marathon.
#   OUTCOME  - Brier skill for gold and medals, favourite hit rate, and the
#              calibration gap in the top probability band.
#
# Usage:  Rscript scripts/model_scoreboard.R [backtest_file.rds ...]
#         with no arguments, scores every backtest_*.rds present.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
args <- commandArgs(trailingOnly = TRUE)
files <- if (length(args)) args else
  sort(basename(list.files(OUT, pattern = "^backtest.*\\.rds$")))
files <- setdiff(files, c("backtest_cache"))

# Actual marks, deduplicated. championship_results carries a row per performance
# and ties can duplicate an athlete within a race; keeping both would weight
# those races twice.
truth <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
truth <- unique(truth[!is.na(race_key) & !is.na(mark) & mark > 0,
                      .(race_id = race_key, athlete_id = as.character(athlete_id),
                        actual = mark, event_id)],
                by = c("race_id", "athlete_id"))
reg <- citius_events()[, c("event_id", "family", "orientation")]
truth <- merge(truth, reg, by = "event_id", all.x = TRUE)

score_one <- function(f) {
  b <- tryCatch(readRDS(file.path(OUT, f)), error = function(e) NULL)
  if (is.null(b) || is.null(b$predictions)) return(NULL)
  p <- setDT(copy(b$predictions)); o <- setDT(copy(b$outcomes))
  p[, athlete_id := as.character(athlete_id)]
  o[, athlete_id := as.character(athlete_id)]
  pr <- merge(p, o, by = c("race_id", "athlete_id"))

  # --- OUTCOME -------------------------------------------------------------
  g <- b$gold$overall; m <- b$medal$overall
  fav <- pr[, .SD[which.max(p_gold)], by = race_id]
  top <- pr[p_gold > 0.7]
  cal_top <- if (nrow(top)) mean(top$hit) - mean(top$p_gold) else NA_real_

  # --- ABILITY -------------------------------------------------------------
  # Predicted mark against the mark actually recorded. No-marks are excluded:
  # a DNF has no time, and foul_rate already models the event of not recording
  # one. Including them would score the model on a quantity it never predicted.
  ab <- merge(p[is.finite(median_mark), .(race_id, athlete_id, pred = median_mark)],
              truth, by = c("race_id", "athlete_id"))
  ab[, err := orientation * (log(pred) - log(actual))]   # oriented: >0 = predicted BETTER than actual
  ab <- ab[is.finite(err)]
  if (!nrow(ab)) return(NULL)   # swimming arms score against a different truth

  data.table(
    model = sub("^backtest_?|\\.rds$", "", f),
    # outcome
    races = g$n_races,
    gold_skill = round(g$brier_skill, 4),
    medal_skill = round(m$brier_skill, 4),
    fav_hit = round(mean(fav$hit), 4),
    top_gap = round(cal_top, 4),
    # ability
    n_marks = nrow(ab),
    mae_log = round(mean(abs(ab$err)), 5),
    mae_pct = round(100 * mean(abs(exp(ab$err) - 1)), 3),
    rmse_log = round(sqrt(mean(ab$err^2)), 5),
    bias_log = round(mean(ab$err), 5),
    cor_mark = round(stats::cor(ab$pred, ab$actual), 4)
  )
}

s <- rbindlist(lapply(files, score_one), fill = TRUE)
if (!nrow(s)) { cli::cli_alert_warning("Nothing to score."); quit(save = "no") }
setorder(s, -gold_skill)

cli::cli_h2("Outcome metrics — how well placings are predicted")
print(s[, .(model, races, gold_skill, medal_skill, fav_hit, top_gap)])

cli::cli_h2("Ability metrics — how well MARKS are predicted")
cat("mae_pct is the average error as a percentage of the mark.\n")
cat("bias_log > 0 means the model predicts athletes BETTER than they run.\n\n")
print(s[, .(model, n_marks, mae_log, mae_pct, rmse_log, bias_log, cor_mark)])

# Per family for the best model: spread differs by an order of magnitude between
# a sprint and a throw, so a pooled number hides where the error lives.
best <- s$model[1]
bf <- files[sub("^backtest_?|\\.rds$", "", files) == best][1]
b <- readRDS(file.path(OUT, bf))
ab <- merge(setDT(copy(b$predictions))[is.finite(median_mark),
              .(race_id, athlete_id = as.character(athlete_id), pred = median_mark)],
            truth, by = c("race_id", "athlete_id"))
ab[, err := orientation * (log(pred) - log(actual))]
cli::cli_h2(paste("Ability error by family —", best))
print(ab[is.finite(err), .(n = .N, mae_pct = round(100 * mean(abs(exp(err) - 1)), 2),
                           bias_pct = round(100 * mean(exp(err) - 1), 2)),
         by = family][order(-n)])

saveRDS(s, file.path(OUT, "model_scoreboard.rds"))
fwrite(s, file.path(OUT, "model_scoreboard.csv"))
cli::cli_alert_success("Wrote model_scoreboard.rds and .csv")
