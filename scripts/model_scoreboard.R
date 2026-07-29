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

  # Log loss alongside Brier, because they disagree in a way that matters here.
  # Both are proper scoring rules, but Brier is quadratic and log loss is
  # unbounded: a confident miss costs Brier at most 1 and costs log loss
  # arbitrarily much. Over-confidence is this model's known failure mode, so the
  # metric that punishes it hardest belongs in the table.
  #
  # A simulated probability of exactly 0 for an athlete who wins gives an
  # infinite loss, which is an artefact of finite simulation rather than of the
  # model. Probabilities are clipped, and the number clipped is REPORTED -- a
  # silent clip would quietly cap the penalty this metric exists to impose.
  EPS <- 1 / (2 * 10000)                       # half a simulation at N_SIMS
  ll <- function(p, y) {
    p <- pmin(pmax(p, EPS), 1 - EPS)
    -mean(y * log(p) + (1 - y) * log(1 - p))
  }
  # Baseline: uniform within race, the same reference the Brier skill uses.
  pr[, n_in_race := .N, by = race_id]
  base_gold <- ll(1 / pr$n_in_race, as.numeric(pr$hit))
  base_med  <- ll(pmin(3 / pr$n_in_race, 1 - EPS), as.numeric(pr$hit_medal))
  ll_gold <- ll(pr$p_gold, as.numeric(pr$hit))
  ll_med  <- ll(pr$p_medal, as.numeric(pr$hit_medal))
  n_clip <- sum(pr$p_gold < EPS | pr$p_gold > 1 - EPS) +
            sum(pr$p_medal < EPS | pr$p_medal > 1 - EPS)

  # AUC separates DISCRIMINATION from calibration. Brier and log loss mix the
  # two, so a change that only re-scales probabilities moves them while leaving
  # the ordering identical. If AUC is flat and Brier improves, the gain was
  # calibration; if AUC moves, the model genuinely tells athletes apart better.
  auc <- function(p, y) {
    y <- as.logical(y)
    if (!any(y) || all(y)) return(NA_real_)
    r <- data.table::frank(p, ties.method = "average")
    (sum(r[y]) - sum(y) * (sum(y) + 1) / 2) / (sum(y) * sum(!y))
  }
  auc_gold <- auc(pr$p_gold, pr$hit)
  auc_med  <- auc(pr$p_medal, pr$hit_medal)

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
    gold_ll = round(ll_gold, 5),
    medal_ll = round(ll_med, 5),
    gold_ll_skill = round(1 - ll_gold / base_gold, 4),
    medal_ll_skill = round(1 - ll_med / base_med, 4),
    gold_auc = round(auc_gold, 4),
    medal_auc = round(auc_med, 4),
    fav_hit = round(mean(fav$hit), 4),
    top_gap = round(cal_top, 4),
    n_clipped = n_clip,
    # ability
    n_marks = nrow(ab),
    # A metric without an interval invites reading noise as a result. Today's
    # arms differ by ~0.1pp of MAE; the interval says whether that is a finding.
    mae_lo = round(100*(mean(abs(exp(ab$err)-1)) -
                        1.96*sd(abs(exp(ab$err)-1))/sqrt(nrow(ab))), 3),
    mae_hi = round(100*(mean(abs(exp(ab$err)-1)) +
                        1.96*sd(abs(exp(ab$err)-1))/sqrt(nrow(ab))), 3),
    mae_log = round(mean(abs(ab$err)), 5),
    mae_pct = round(100 * mean(abs(exp(ab$err) - 1)), 3),
    rmse_log = round(sqrt(mean(ab$err^2)), 5),
    rmse_pct = round(100 * sqrt(mean((exp(ab$err) - 1)^2)), 3),
    bias_pct = round(100 * mean(exp(ab$err) - 1), 3),
    cor_mark = round(stats::cor(ab$pred, ab$actual), 4)
  )
}

s <- rbindlist(lapply(files, score_one), fill = TRUE)
if (!nrow(s)) { cli::cli_alert_warning("Nothing to score."); quit(save = "no") }
setorder(s, -gold_skill)

cli::cli_h2("GOLD — who wins")
print(s[, .(model, races, brier_skill = gold_skill, logloss = gold_ll,
            ll_skill = gold_ll_skill, auc = gold_auc, fav_hit, top_gap)])

cli::cli_h2("MEDAL — who finishes top three")
print(s[, .(model, brier_skill = medal_skill, logloss = medal_ll,
            ll_skill = medal_ll_skill, auc = medal_auc)])
if (any(s$n_clipped > 0)) cli::cli_alert_info(
  "Probabilities clipped at 1/20000 for log loss: {sum(s$n_clipped)} across all models.")

cli::cli_h2("Ability metrics — how well MARKS are predicted")
cat("mae_pct is the average error as a percentage of the mark.\n")
cat("bias_log > 0 means the model predicts athletes BETTER than they run.\n\n")
print(s[, .(model, n_marks, mae_pct, ci95 = paste0(mae_lo, "-", mae_hi),
            rmse_pct, bias_pct, cor_mark)])
cat("
ci95 is on MAE. Arms whose intervals overlap heavily are not separated by
")
cat("this data -- use the PAIRED test, which removes race-to-race variation.
")

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
