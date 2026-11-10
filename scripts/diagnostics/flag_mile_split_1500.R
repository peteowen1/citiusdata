# Flag 1500m results that are probably the 1500m split inside a mile race,
# stored as a race (found 2026-11-08: Kipyegon's 4:01.92 "1500m" at the 2026
# Prefontaine Classic sits beside her 4:17.80 mile, same day, same meet).
# Rule: a 1500m result sharing athlete + date with the same athlete's Mile
# result, and faster than it. FLAG ONLY (Pete, 2026-11-08): this writes
# data/flags/mile_split_1500.csv and changes nothing in the corpus or ratings.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
suppressMessages({ library(arrow); library(dplyr); library(data.table) })
D <- "C:/dev/citiusverse/citiusdata/data"
ds <- open_dataset(file.path(D, "athletics_corpus.parquet"))
mile <- setDT(ds %>% filter(discipline == "Mile") %>%
  select(athlete_id, date, mile_comp = comp_name, mile_mark = mark) %>% collect())
f1500 <- setDT(ds %>% filter(event_id %in% c("AT-1500Metres-M", "AT-1500Metres-W")) %>%
  select(athlete_id, event_id, date, comp_name, round, mark, mark_string, source) %>% collect())
cat("corpus rows: 1500m", nrow(f1500), " mile", nrow(mile), "\n")
j <- merge(f1500, mile, by = c("athlete_id", "date"), allow.cartesian = TRUE)
j <- j[mark < mile_mark]
j[, same_comp := !is.na(comp_name) & !is.na(mile_comp) & comp_name == mile_comp]
j <- unique(j, by = c("athlete_id", "event_id", "date", "mark"))
dir.create(file.path(D, "flags"), showWarnings = FALSE)
fwrite(j, file.path(D, "flags", "mile_split_1500.csv"))
cat("flagged", nrow(j), "rows;", j[same_comp == TRUE, .N], "with the same meet name;",
    uniqueN(j$athlete_id), "athletes\n")
print(j[, .N, by = event_id])
print(j[, .N, by = .(yr = year(date))][order(yr)][yr >= 2020])
