# Repair catalogue rows whose metadata was blanked by the augment scripts.
#
# WHAT HAPPENED. augment_catalogue_road_half_majors.R had a "self-repair" block
# that deleted rows and re-added them from three columns (competition_id, class,
# meet_tier), nulling comp_name, year, results, athletes, events, strength. It
# scoped the delete to class %chin% c("road_label", "world_other") - but it does
# not own `world_other`, which build_competition_catalogue.R assigns. Measured on
# the live file: world_other rows went 29 -> 37 and 16 of them lost every field
# except the three. Separately, both augment scripts never copied the matched
# name into comp_name at all, so 54/54 marathon_major and 203/203 road_label
# rows are anonymous.
#
# The scripts are fixed. This restores what the live file already lost.
#
# Restores in two passes, most trustworthy source first:
#   1. from competition_catalogue.parquet.bak, which predates the damage
#   2. comp_name from competition_name_lookup.parquet for anything still unnamed
# It never overwrites a value that is already present.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
BAK <- paste0(CAT, ".bak")
LOOK <- file.path(D, "competition_name_lookup.parquet")
stopifnot("catalogue missing" = file.exists(CAT),
          "backup missing - cannot restore" = file.exists(BAK))

cur <- setDT(read_parquet(CAT)); cur[, competition_id := as.character(competition_id)]
bak <- setDT(read_parquet(BAK)); bak[, competition_id := as.character(competition_id)]
meta <- setdiff(names(cur), c("competition_id", "class", "meet_tier"))
cat(sprintf("catalogue rows %s | backup rows %s | metadata columns: %s\n",
            format(nrow(cur), big.mark = ","), format(nrow(bak), big.mark = ","),
            paste(meta, collapse = ", ")))

blank <- function(dt) rowSums(!is.na(dt[, ..meta])) == 0L
cat(sprintf("\nBEFORE: %d rows carry no metadata at all\n", sum(blank(cur))))
print(cur[blank(cur), .N, by = .(class, meet_tier)][order(-N)])

# --- pass 1: restore from the backup ------------------------------------------
restored <- 0L
for (cn in intersect(meta, names(bak))) {
  m <- match(cur$competition_id, bak$competition_id)
  need <- is.na(cur[[cn]]) & !is.na(m) & !is.na(bak[[cn]][m])
  if (any(need)) {
    cur[[cn]][need] <- bak[[cn]][match(cur$competition_id[need], bak$competition_id)]
    cat(sprintf("  restored %-12s on %d rows from the backup\n", cn, sum(need)))
    restored <- restored + sum(need)
  }
}
cat(sprintf("pass 1 restored %d values\n", restored))

# --- pass 2: names from the cached lookup -------------------------------------
if ("comp_name" %in% names(cur) && file.exists(LOOK)) {
  nm <- setDT(read_parquet(LOOK)); nm[, competition_id := as.character(competition_id)]
  need <- is.na(cur$comp_name)
  hit <- match(cur$competition_id[need], nm$competition_id)
  fill <- nm$competition[hit]
  n_fill <- sum(!is.na(fill))
  if (n_fill) {
    idx <- which(need)[!is.na(fill)]
    cur$comp_name[idx] <- fill[!is.na(fill)]
  }
  cat(sprintf("pass 2 named %d rows from the lookup; %d still unnamed\n",
              n_fill, sum(is.na(cur$comp_name))))
}

cat(sprintf("\nAFTER: %d rows carry no metadata at all\n", sum(blank(cur))))
stopifnot("row count changed - repair must not add or drop rows" =
            nrow(cur) == nrow(read_parquet(CAT)),
          "duplicate competition ids" = !anyDuplicated(cur$competition_id))

tmp <- paste0(CAT, ".tmp")
write_parquet(cur, tmp); rm(cur, bak); invisible(gc())
ok <- file.copy(tmp, CAT, overwrite = TRUE)
stopifnot("could not replace the catalogue" = isTRUE(ok))
unlink(tmp)
cat("catalogue rewritten in place (row count and ids unchanged)\n")
