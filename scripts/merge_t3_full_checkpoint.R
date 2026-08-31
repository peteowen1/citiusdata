# Merge the T3 full-backfill cache (harvest_t3_full_2026.R) into
# championship_results.rds via the DuckDB store. The full backfill itself is
# PAUSED (2026-08-31, hit a source-side rate-limit penalty that outlasted a
# short cooldown -- see harvest_t3_full_2026.R's header for the full
# diagnosis) -- this merges what was already fetched before stopping, since
# there's no reason to discard real data over a paused harvest.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_comp_cache_t3full")

fs <- list.files(CACHE, pattern = "[.]rds$", full.names = TRUE)
cli::cli_alert_info("{length(fs)} cached files")
blobs <- Filter(function(z) is.data.table(z) && nrow(z), lapply(fs, readRDS))
new <- rbindlist(blobs, fill = TRUE)
cli::cli_alert_success("{format(nrow(new), big.mark=',')} results across {uniqueN(new$competition_id)} competitions to merge")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
before_n <- nrow(ch); before_comp <- uniqueN(ch$competition_id)

backup <- file.path(OUT, "championship_results.pre_t3full_checkpoint_20260831.rds")
if (!file.exists(backup)) saveRDS(ch, backup)

dup <- intersect(unique(new$competition_id), unique(ch$competition_id))
if (length(dup)) new <- new[!competition_id %in% dup]
ch2 <- rbind(ch, new, fill = TRUE)
stopifnot(nrow(ch2) == before_n + nrow(new))
stopifnot(!any(ch2$competition_id == 0, na.rm = TRUE))

with_citius_db_connection(function(conn) {
  store_championship_results(conn, ch2, mode = "replace")
}, path = get_citius_db_path())

atomic_write <- function(f, path) {
  tmp <- paste0(path, ".tmp-write")
  saveRDS(f, tmp)
  ok <- file.rename(tmp, path)
  if (!ok) cli::cli_abort("atomic rename failed for {path}")
}
atomic_write(ch2, file.path(OUT, "championship_results.rds"))

cli::cli_alert_success("championship_results.rds: {format(before_n, big.mark=',')} -> {format(nrow(ch2), big.mark=',')} rows, {before_comp} -> {uniqueN(ch2$competition_id)} competitions")
