# ASSEMBLE THE CHUNKED PREP'S PER-FAMILY SHARDS INTO WHAT THE REST OF THE LAB
# READS. marks_pairs.R and build_fair_baseline.R were written against
# marks_lab_prep.R's layout -- one adj.rds, one base.rds, one sigma/<date>.rds
# per month, all combined across families -- because at T1 scale the whole
# corpus fit in memory as one table.
#
# marks_lab_prep_chunked.R never produces that layout. It CANNOT, by design:
# the whole point of chunking by family was that no step ever holds more than
# one family's data at once. So the two preps are not interchangeable outputs,
# they are two different physical layouts of the same logical cache, and this
# script is the missing step that converts one into the other. It only ever
# combines already-computed shards, so it is cheap: no history read, no
# estimate_ability() call, just data.table rbind of small tables.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_lab_assemble_chunked.R'
# Env: CITIUS_LAB_CACHE (must match what the chunked prep wrote to)
suppressMessages(library(data.table))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_t1t2"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

fam_dir <- file.path(CACHE, "fam")
stopifnot("no fam/ directory -- run marks_lab_prep_chunked.R first" = dir.exists(fam_dir))

# --- adj.rds: rbind every fam/adj_<family>.rds ------------------------------
af <- file.path(CACHE, "adj.rds")
if (!file.exists(af)) {
  adj_files <- list.files(fam_dir, pattern = "^adj_.*\\.rds$", full.names = TRUE)
  stopifnot("no adj_*.rds shards found" = length(adj_files) > 0)
  adj <- rbindlist(lapply(adj_files, readRDS), fill = TRUE)
  saveRDS(adj, af)
  say("adj.rds: %s rows from %d family shards", format(nrow(adj), big.mark = ","), length(adj_files))
  rm(adj); invisible(gc())
} else say("adj.rds already assembled, skipping")

# --- base.rds: rbind every fam/base_<family>.rds ----------------------------
bf <- file.path(CACHE, "base.rds")
if (!file.exists(bf)) {
  base_files <- list.files(fam_dir, pattern = "^base_.*\\.rds$", full.names = TRUE)
  stopifnot("no base_*.rds shards found" = length(base_files) > 0)
  base <- rbindlist(lapply(base_files, readRDS), fill = TRUE)
  saveRDS(base, bf)
  say("base.rds: %s rows from %d family shards", format(nrow(base), big.mark = ","), length(base_files))
  rm(base); invisible(gc())
} else say("base.rds already assembled, skipping")

# --- sigma/<date>.rds: rbind fam/sigma/<family>_<date>.rds across families --
#
# The chunked prep names sigma shards <family>_<date>.rds; marks_pairs.R reads
# by DATE ALONE (`sigma/<date>.rds`), combined across every family. Group the
# shard filenames by their trailing date and combine each group.
sig_dir <- file.path(CACHE, "sigma")
shards <- list.files(sig_dir, pattern = "^[a-z]+_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$")
stopifnot("no per-family sigma shards found" = length(shards) > 0)
dates <- sub("^[a-z]+_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.rds$", "\\1", shards)
by_date <- split(shards, dates)
n_done <- 0L; n_skip <- 0L
for (d in names(by_date)) {
  out <- file.path(sig_dir, paste0(d, ".rds"))
  if (file.exists(out)) { n_skip <- n_skip + 1L; next }
  parts <- lapply(file.path(sig_dir, by_date[[d]]), readRDS)
  combined <- rbindlist(parts, fill = TRUE)
  saveRDS(combined, out)
  n_done <- n_done + 1L
}
say("sigma: assembled %d monthly files (%d already present) from %d family shards",
    n_done, n_skip, length(shards))

say("ASSEMBLY COMPLETE")
