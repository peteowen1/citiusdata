# Standing diagnostic harness for a backtest result.
#
# Point it at any backtest .rds and it runs every check that has caught a real
# problem so far. Built because conclusions kept being drawn from one summary
# statistic and then reversing: the "S-shaped miscalibration" turned out to be a
# field-size effect, and the field-size effect turned out to be a scoring key
# that merged gala sections into 86-athlete mega-races.
#
#   CITIUS_BT   which backtest file to read (default backtest.rds)
#
# The FIRST check is the integrity one. If races have multiple winners, nothing
# below it means anything -- probabilities sum to 1 and outcomes do not.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
BT <- Sys.getenv("CITIUS_BT", "backtest.rds")
bt <- readRDS(file.path(OUT, BT))
cli::cli_alert_info("Diagnosing {.file {BT}}.")

m <- merge(bt$predictions[, .(race_id, athlete_id, p_gold, p_medal)],
           bt$outcomes[, .(race_id, athlete_id, hit, hit_medal)],
           by = c("race_id", "athlete_id"))
m[, field := .N, by = race_id]

cli::cli_h2("1. Integrity — one race, one winner")
w <- m[, .(winners = sum(hit), medals = sum(hit_medal), field = field[1]), by = race_id]
cat(sprintf("  races scored          : %s\n", format(nrow(w), big.mark = ",")))
cat(sprintf("  with exactly 1 winner : %s (%.1f%%)\n",
            format(sum(w$winners == 1), big.mark = ","), 100 * mean(w$winners == 1)))
cat(sprintf("  with >1 winner        : %s (%.1f%%)  <- ties are ~0.4%% in athletics\n",
            format(sum(w$winners > 1), big.mark = ","), 100 * mean(w$winners > 1)))
cat(sprintf("  with 0 winners        : %s (%.1f%%)  <- winner outside our field\n",
            format(sum(w$winners == 0), big.mark = ","), 100 * mean(w$winners == 0)))
if (mean(w$winners > 1) > 0.02) {
  cli::cli_alert_danger(
    "More than 2% of races have multiple winners. Check the scoring KEY before reading anything below."
  )
}

# Everything downstream uses races with exactly one winner, so a scoring-key
# defect cannot masquerade as a calibration defect.
clean <- w[winners == 1]$race_id
mc <- m[race_id %in% clean]
cli::cli_alert_info("{length(clean)} race{?s} with exactly one winner used below.")

cli::cli_h2("2. Reliability by predicted probability")
mc[, bin := cut(p_gold, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), include.lowest = TRUE)]
rel <- mc[, .(n = .N, predicted = round(mean(p_gold), 4),
              observed = round(mean(hit), 4),
              gap = round(mean(hit) - mean(p_gold), 4)), by = bin][order(bin)]
rel[, se := round(sqrt(observed * (1 - observed) / n), 4)]
rel[, sigmas := round(gap / se, 1)]
print(rel)
cat("\n  |sigmas| above ~2 is a real gap; below that is sampling noise.\n")

cli::cli_h2("3. Reliability by FIELD SIZE")
setorder(mc, race_id, -p_gold)
fav <- mc[, .(p = p_gold[1], hit = hit[1], field = .N), by = race_id]
fav[, fb := cut(field, c(0, 6, 8, 12, 20, 500), include.lowest = TRUE)]
fr <- fav[, .(races = .N, predicted = round(mean(p), 4), observed = round(mean(hit), 4),
              gap = round(mean(hit) - mean(p), 4)), by = fb][order(fb)]
fr[, se := round(sqrt(observed * (1 - observed) / races), 4)]
fr[, sigmas := round(gap / se, 1)]
print(fr)
cat("\n  A monotone gradient here previously meant merged sections, not a model defect.\n")

cli::cli_h2("4. Medal probabilities are coherent")
cat(sprintf("  mean sum(p_gold) per race : %.4f  (target 1)\n",
            mc[, sum(p_gold), by = race_id][, mean(V1)]))
cat(sprintf("  mean sum(p_medal) per race: %.4f  (target 3, or field size if smaller)\n",
            mc[, sum(p_medal), by = race_id][, mean(V1)]))

cli::cli_h2("5. Headline skill on clean races")
o <- mc[, .(race_id, athlete_id, hit)]
g <- score_predictions(mc[, .(race_id, athlete_id, p_gold)], o, "p_gold")
md <- score_predictions(mc[, .(race_id, athlete_id, p_medal)],
                        mc[, .(race_id, athlete_id, hit = hit_medal)], "p_medal")
cat(sprintf("  gold  brier %.4f vs %.4f  skill %+.3f\n", g$overall$brier,
            g$overall$brier_baseline, g$overall$brier_skill))
cat(sprintf("  medal brier %.4f vs %.4f  skill %+.3f\n", md$overall$brier,
            md$overall$brier_baseline, md$overall$brier_skill))
cat(sprintf("  races beating baseline: %d of %d (%.0f%%)\n",
            sum(g$by_race$skill > 0), nrow(g$by_race), 100 * mean(g$by_race$skill > 0)))
