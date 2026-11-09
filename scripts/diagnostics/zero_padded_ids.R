# Zero-padded athlete ids: the same athlete under "014164603" and "14164603" (found 2026-11-10
# while verifying the version E switch). Sizes the split, the duplicated results, and the
# name coverage. Read-only. Run: powershell Rscript citiusdata/scripts/diagnostics/zero_padded_ids.R
suppressMessages(library(data.table))
co <- setDT(readRDS("C:/dev/citiusverse/citiusdata/data/athletics_corpus.rds"))
co[, id := as.character(athlete_id)]
cat("athlete_id class:", class(co$athlete_id), " rows:", nrow(co), "\n")
z <- unique(co[grepl("^0", id), .(id)])
z[, id0 := sub("^0+", "", id)]
ids <- unique(co$id)
z[, twin := id0 %in% ids]
cat("ids with leading zero:", nrow(z), " of which the stripped id also exists:", z[twin == TRUE, .N], "\n")
tw <- z[twin == TRUE]
cat("rows under the zero-padded twin ids:", co[id %in% tw$id, .N], "; rows under their stripped twins:", co[id %in% tw$id0, .N], "\n")
cat("by source, zero-padded twin rows:\n"); print(co[id %in% tw$id, .N, by = source][order(-N)])
cat("by source, stripped twin rows:\n"); print(co[id %in% tw$id0, .N, by = source][order(-N)])
cat("date range of zero-padded rows:", format(range(co[id %in% tw$id, date], na.rm = TRUE)), "\n")
ex <- tw$id[1]; cat("example", ex, "\n"); print(co[id %in% c(ex, sub("^0+", "", ex)), .N, by = .(id, source, event_id)][order(event_id)][1:10])

# ---- same athlete? double-counted? ----
co[, id := as.character(athlete_id)][, id0 := sub("^0+", "", id)]
tw <- unique(co[grepl("^0", id) & id0 %in% co$id, id])
z <- co[id %in% tw, .(id0, race_key, date, event_id, mark)]
p <- co[id %in% sub("^0+", "", tw), .(id0 = id, race_key, date, event_id, mark)]
same_race <- merge(z[!is.na(race_key)], p[!is.na(race_key)], by = c("id0", "race_key"))
same_day <- merge(z, p, by = c("id0", "date", "event_id"))
cat("zero-padded rows:", nrow(z), "\n")
cat("  same athlete + same race_key under the plain id too (double-counted):", uniqueN(same_race[, .(id0, race_key)]), "\n")
cat("  same athlete + same date + event (any mark):", uniqueN(same_day[, .(id0, date, event_id)]), "; marks equal:", same_day[mark.x == mark.y, .N], "\n")
nm <- setDT(readRDS("C:/dev/citiusverse/citiusdata/data/athlete_name_lookup.rds"))
nm[, id := as.character(athlete_id)]
k <- merge(nm[id %in% tw, .(id0 = sub("^0+", "", id), n1 = athlete_name)], nm[id %in% sub("^0+", "", tw), .(id0 = id, n2 = athlete_name)], by = "id0")
cat("twins with a name under both ids:", nrow(k), "; names identical:", k[n1 == n2, .N], "\n"); print(head(k[n1 != n2], 5))
