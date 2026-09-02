suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(arrow); library(jsonlite)
OUT <- here::here("citiusdata", "data")

# Same population as the meet registry: competitions already in the
# (trimmed) meet-level export.
ct <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
keep_ids <- ct[!(is.na(strength) & class == "unclassified")]$competition_id
cat(sprintf("meets in scope: %s\n", format(length(keep_ids), big.mark=",")))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, competition_id := as.character(competition_id)]
ch <- ch[competition_id %chin% as.character(keep_ids)]
cat(sprintf("rows for scoped meets: %s\n", format(nrow(ch), big.mark=",")))

is_final <- function(rnd) grepl("final", rnd, ignore.case=TRUE) & !grepl("semi", rnd, ignore.case=TRUE)
fin <- ch[!is.na(place) & is_final(round) & !is.na(mark) & !is.na(event_id)]

# NOTE: championship_results.rds already carries `orientation` (it's in the
# 33-column schema) -- merging citius_events()'s copy in would produce
# orientation.x/.y and break every bare reference below. Same collision hit
# earlier this session on a different script.
stopifnot("championship_results should already carry orientation" =
            "orientation" %in% names(fin))

# era-relative percentile, same shape as build_competition_catalogue.R
fin[, era := 4L * (year(date) %/% 4L)]
fin[, n_era := .N, by = .(event_id, era)]
fin[, pctl := fifelse(n_era >= 200L,
                       frank(mark * orientation, na.last = "keep") / sum(!is.na(mark)),
                       NA_real_), by = .(event_id, era)]
fin[is.na(pctl), pctl := frank(mark * orientation, na.last = "keep") / sum(!is.na(mark)), by = event_id]

wr <- fread(file.path(OUT, "world_records.csv"))
wr[, wr_mark := sapply(mark, function(m) tryCatch(parse_mark(m), error = function(e) NA_real_))]
wr <- wr[, .(event_id, wr_mark)]

by_event <- fin[, .(
  discipline = discipline[1],
  orientation = orientation[1],
  n_athletes = uniqueN(athlete_id),
  winning_mark = if (orientation[1] > 0) max(mark, na.rm=TRUE) else min(mark, na.rm=TRUE),
  event_strength = round(100 * mean(pctl, na.rm=TRUE), 1)
), by = .(competition_id, event_id)]

by_event <- merge(by_event, wr, by = "event_id", all.x = TRUE)
by_event[, wr_pct := fifelse(!is.na(wr_mark) & wr_mark > 0,
  round(fifelse(orientation > 0, 100 * winning_mark / wr_mark, 100 * wr_mark / winning_mark), 1),
  NA_real_)]
by_event[, winning_mark := round(winning_mark, 2)]
by_event[, c("orientation", "wr_mark") := NULL]

cat(sprintf("event rows: %s\n", format(nrow(by_event), big.mark=",")))

# COMPACT ENCODING. An array-of-objects repeats every key name and the
# competition_id on all 244k rows -- 36.7MB, over the artifact's 16MB cap.
# Emit an object keyed by competition_id whose value is an array of
# positional arrays instead: [discipline, n_athletes, winning_mark,
# event_strength, wr_pct]. The page's own JS knows the column order.
setorder(by_event, competition_id, -event_strength)
grp <- split(by_event, by = "competition_id", keep.by = FALSE)
out <- lapply(grp, function(d) {
  lapply(seq_len(nrow(d)), function(i)
    list(d$discipline[i], d$n_athletes[i], d$winning_mark[i],
         d$event_strength[i], d$wr_pct[i]))
})
j <- toJSON(out, na = "null", auto_unbox = TRUE, digits = 2)
cat(sprintf("JSON size (compact): %s\n", format(object.size(j), units = "MB")))
writeLines(j, file.path(OUT, "meet_registry_events.json"))
cat("wrote meet_registry_events.json\n")
