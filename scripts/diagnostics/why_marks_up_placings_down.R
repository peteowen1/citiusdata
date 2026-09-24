# How can predicted MARKS improve while gold/medal Brier gets WORSE?
#
# Pete's question, 2026-09-17, on the middle-only altitude arm: middle marks
# improved -0.0262pp at t = -4.60 while medal Brier worsened +0.29% (p = 0.022).
# That looks contradictory until you look at actual fields, so this prints
# them -- the repo rule is to walk concrete rows before theorising, because an
# aggregate can be true and reassuring while the mechanism underneath is not
# what anyone assumed.
#
# Usage:
#   CITIUS_WY_A=bt_cache_alt_ctrl_sim CITIUS_WY_B=bt_cache_alt_midonly_sim \
#     Rscript citiusdata/scripts/diagnostics/why_marks_up_placings_down.R

suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
OUT <- here::here("citiusdata", "data")
A <- Sys.getenv("CITIUS_WY_A", "bt_cache_alt_ctrl_sim")
B <- Sys.getenv("CITIUS_WY_B", "bt_cache_alt_midonly_sim")

grab <- function(dir) {
  d <- file.path(OUT, dir)
  fs <- setdiff(list.files(d, pattern = "\\.rds$"), "_arm.rds")
  rbindlist(lapply(fs, function(f) {
    o <- readRDS(file.path(d, f)); if (!length(o)) return(NULL)
    rbindlist(lapply(o, function(r) {
      if (is.null(r$pred) || is.null(r$outc)) return(NULL)
      p <- as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id),
                                     p_gold, p_medal, median_mark)]
      oc <- as.data.table(r$outc)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal)]
      merge(p, oc, by = c("race_id", "athlete_id"))
    }), fill = TRUE)
  }), fill = TRUE)
}
a <- grab(A); b <- grab(B)
m <- merge(a, b, by = c("race_id", "athlete_id"), suffixes = c("_a", "_b"))

# Actual marks + event + altitude, from the store the arms themselves read.
st <- as.data.table(open_dataset(file.path(OUT, "athletics_corpus_store")) |>
  dplyr::select(race_key, athlete_id, mark, event_id, alt_m) |>
  dplyr::collect())
st[, athlete_id := as.character(athlete_id)]
act <- unique(st[!is.na(mark) & mark > 0, .(race_id = race_key, athlete_id,
                                            actual = mark, event_id, alt_m)],
              by = c("race_id", "athlete_id"))
m <- merge(m, act, by = c("race_id", "athlete_id"))
fam <- as.data.table(citius_events())[, .(event_id, family, orientation)]
m <- merge(m, fam, by = "event_id", all.x = TRUE)
m <- m[family == "middle" & is.finite(p_gold_a) & is.finite(p_gold_b)]

# Per-race: did marks improve, did gold Brier worsen?
m[, `:=`(ape_a = abs(median_mark_a - actual) / actual,
         ape_b = abs(median_mark_b - actual) / actual)]
per <- m[, .(
  mae_a = mean(ape_a), mae_b = mean(ape_b),
  gb_a = mean((p_gold_a - hit_a)^2), gb_b = mean((p_gold_b - hit_b)^2),
  n = .N, alt = max(alt_m, na.rm = TRUE)), by = race_id]
per[, `:=`(marks_better = mae_b < mae_a, gold_worse = gb_b > gb_a)]

cat(sprintf("middle-distance races with both arms: %d\n", nrow(per)))
cat(sprintf("  marks better AND gold worse : %d\n", per[marks_better & gold_worse, .N]))
cat(sprintf("  marks better AND gold better: %d\n", per[marks_better & !gold_worse, .N]))
# `per[!marks_better]` is ambiguous to data.table when the argument is a single
# symbol -- it looks for the name in the calling scope first. Parenthesise.
cat(sprintf("  marks worse                 : %d\n", per[(marks_better) == FALSE, .N]))

show <- per[(marks_better) & (gold_worse)][order(-(gb_b - gb_a))][1:2]
for (rid in show$race_id) {
  r <- m[race_id == rid][order(-p_gold_a)]
  cat(sprintf("\n================ %s  (%s, %d athletes, venue alt %s m) ================\n",
              rid, r$event_id[1], nrow(r),
              if (is.finite(r$alt_m[1])) format(round(r$alt_m[1])) else "unknown"))
  cat("pred_A / pred_B are the two arms' predicted marks; actual is what happened.\n")
  cat("WON marks the athlete who actually won.\n\n")
  p <- r[, .(won = fifelse(hit_a, "WON", ""),
             pred_A = round(median_mark_a, 2), pred_B = round(median_mark_b, 2),
             shift = round(median_mark_b - median_mark_a, 3),
             actual = round(actual, 2),
             err_A = round(abs(median_mark_a - actual), 2),
             err_B = round(abs(median_mark_b - actual), 2),
             pgold_A = round(p_gold_a, 3), pgold_B = round(p_gold_b, 3))]
  print(utils::head(p, 10))
  w <- r[hit_a == TRUE]
  if (nrow(w)) cat(sprintf("\n  winner's p_gold: %.3f -> %.3f  (%+.3f)   mark error: %.2f -> %.2f\n",
                           w$p_gold_a[1], w$p_gold_b[1], w$p_gold_b[1] - w$p_gold_a[1],
                           abs(w$median_mark_a[1] - w$actual[1]), abs(w$median_mark_b[1] - w$actual[1])))
  cat(sprintf("  race mark MAE: %.4f%% -> %.4f%%   |   gold Brier: %.4f -> %.4f\n",
              100 * mean(r$ape_a), 100 * mean(r$ape_b),
              mean((r$p_gold_a - r$hit_a)^2), mean((r$p_gold_b - r$hit_b)^2)))
}
