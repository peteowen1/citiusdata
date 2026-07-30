# Rebuild a backtest artefact from its per-meet cache.
#
# The backtest caches every meet as it goes and assembles at the end, so a
# failure in the assembly step throws away hours of completed work for no
# reason. This recovers it.
#
# Needed 2026-07-31 because I edited backtest_athletics.R while an arm was
# running. The file warns against exactly that -- Rscript evaluates top-level
# expressions AS IT READS THEM rather than parsing the whole file first, so an
# edit lands in the middle of a run. All 380 meets completed; only the assembly
# died.
#
# Usage:
#   CITIUS_ASM_CACHE=backtest_cache_flat CITIUS_ASM_OUT=backtest_flat.rds \
#     Rscript scripts/assemble_from_cache.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_ASM_CACHE", "backtest_cache_flat"))
DEST  <- Sys.getenv("CITIUS_ASM_OUT", "backtest_flat.rds")
stopifnot("cache directory not found" = dir.exists(CACHE))

fs <- list.files(CACHE, pattern = "[.]rds$", full.names = TRUE)
blobs <- unlist(lapply(fs, readRDS), recursive = FALSE)
blobs <- Filter(function(b) is.list(b) && !is.null(b$pred), blobs)
cli::cli_alert_info("{length(fs)} cached meet{?s} -> {length(blobs)} scored race{?s}")
if (!length(blobs)) cli::cli_abort("Nothing in the cache to assemble.")

pred <- rbindlist(lapply(blobs, `[[`, "pred"), fill = TRUE)
outc <- rbindlist(lapply(blobs, `[[`, "outc"), fill = TRUE)
cov <- outc[, .(wp = any(hit)), by = race_id]
keep <- cov[wp == TRUE]$race_id
cli::cli_alert_info("{nrow(cov)} race{?s}; winner in field for {length(keep)} ({round(100*length(keep)/nrow(cov))}%)")

gold <- score_predictions(pred[race_id %in% keep], outc[race_id %in% keep], "p_gold")
medal <- score_predictions(pred[race_id %in% keep],
                           outc[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")
cat(sprintf("gold  brier %.5f  skill %+.3f  (%d races)\n", gold$overall$brier,
            gold$overall$brier_skill, gold$overall$n_races))
cat(sprintf("medal brier %.5f  skill %+.3f\n", medal$overall$brier, medal$overall$brier_skill))

saveRDS(list(gold = gold, medal = medal, predictions = pred, outcomes = outc,
             meta = list(assembled_from_cache = basename(CACHE),
                         races_scored = length(keep), run_at = Sys.time(),
                         note = "assembled from cache after an assembly-step failure")),
        file.path(OUT, DEST))
cli::cli_alert_success("wrote {DEST}")
