suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
suppressMessages({ library(data.table); library(arrow) })
OUT <- "C:/dev/citiusverse/citiusdata/data"
load_arm <- function(dir) {
  d <- file.path(OUT, dir); fs <- setdiff(list.files(d, pattern = "\\.rds$"), "_arm.rds")
  out <- rbindlist(lapply(fs, function(f) { o <- readRDS(file.path(d, f)); if (!length(o)) return(NULL)
    rbindlist(lapply(o, function(r) { if (is.null(r$pred) || !"median_mark" %in% names(r$pred)) return(NULL)
      as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), median_mark)] }), fill = TRUE) }), fill = TRUE)
  unique(out, by = c("race_id", "athlete_id"))
}
pa <- load_arm("bt_cache_adjm_ctrl"); pb <- load_arm("bt_cache_adjm_on")
champs <- as.data.table(open_dataset(file.path(OUT, "athletics_corpus_store")) |> dplyr::select(race_key, athlete_id, mark, event_id, alt_m) |> dplyr::collect())
champs[, athlete_id := as.character(athlete_id)]
vm <- as.data.table(read_parquet(file.path(OUT, "venue_by_race.parquet"))); champs <- merge(champs, vm, by = "race_key", all.x = TRUE)
act <- unique(champs[!is.na(mark) & mark > 0, .(race_id = race_key, athlete_id, actual = mark, event_id, venue_city, alt_m)], by = c("race_id", "athlete_id"))
m <- merge(merge(pa, pb, by = c("race_id", "athlete_id"), suffixes = c("_a", "_b")), act, by = c("race_id", "athlete_id"))
m <- m[grepl("Hurdles", event_id)]
m[, `:=`(ape_a = abs(median_mark_a - actual)/actual, ape_b = abs(median_mark_b - actual)/actual)]
m[, moved := median_mark_a != median_mark_b]
cat("hurdles rows in the run-3 pool; MAE % of mark, lower is better; diff = treatment - control\n")
print(m[moved == TRUE, .(rows = .N, races = uniqueN(race_id), mae_ctrl = round(100*mean(ape_a), 3), mae_adj = round(100*mean(ape_b), 3), diff_pp = round(100*(mean(ape_b) - mean(ape_a)), 3)), by = event_id][order(-diff_pp)])
cat("\nby venue (rows >= 15), hurdles, moved rows:\n")
print(m[moved == TRUE, .(rows = .N, diff_pp = round(100*(mean(ape_b) - mean(ape_a)), 3), alt_m = round(mean(alt_m, na.rm = TRUE))), by = venue_city][rows >= 15][order(-diff_pp)])
cat("\ndirection: does the treatment predict FASTER or SLOWER hurdles marks? mean(pred_b - pred_a) in % of control:\n")
print(m[moved == TRUE, .(mean_shift_pct = round(100*mean((median_mark_b - median_mark_a)/median_mark_a), 3), mean_actual_vs_ctrl_pct = round(100*mean((actual - median_mark_a)/median_mark_a), 3)), by = event_id])
