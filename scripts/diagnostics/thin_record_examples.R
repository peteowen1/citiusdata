# Thin-record version C on TODAY's ratings: who moves, by how much, against
# what they have actually run. Same deployed pipeline both ways; version C is
# deployed_thin_record() with enabled = TRUE (the exact function step 3 turns on).
# Changes nothing published. Output: data/flags/thin_record_examples.csv
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV <- DEPLOYED$thin_record$events
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date)
cat("rows", nrow(h), "athletes", uniqueN(h$athlete_id), "as_of", format(AS_OF), "\n")
cal <- deployed_calibration(OUT)
stopifnot(!isTRUE(DEPLOYED$thin_record$enabled))
now <- deployed_ability(h, as_of = AS_OF, calibration = cal)
cfg <- DEPLOYED$thin_record; cfg$enabled <- TRUE
vc <- deployed_thin_record(now, h, AS_OF, cal, cfg = cfg)
for (x in list(now, vc)) x[, athlete_id := as.character(athlete_id)]
stopifnot(nrow(vc) == nrow(now))
d <- merge(now[, .(athlete_id, event_id, a_now = ability, shrinkage, last_date)],
           vc[, .(athlete_id, event_id, a_c = ability)], by = c("athlete_id", "event_id"))
d <- d[as.numeric(AS_OF - last_date) <= 730]           # the ratings page's active rule
d[, `:=`(rank_now = frank(-a_now, ties.method = "min"), rank_c = frank(-a_c, ties.method = "min")), by = event_id]
rec <- h[order(date)][, .(races_12m = sum(date > AS_OF - 365), best_12m = suppressWarnings(min(mark[date > AS_OF - 365])),
                          races_3y = sum(date > AS_OF - 3 * 365), best_3y = suppressWarnings(min(mark[date > AS_OF - 3 * 365])),
                          last_mark = mark[.N]), by = .(athlete_id, event_id)]
d <- rec[d, on = c("athlete_id", "event_id")]
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
d[, id0 := sub("^0+", "", athlete_id)]; d <- nm[d, on = "id0"][, id0 := NULL]
d[, `:=`(mark_now = exp(-a_now), mark_c = exp(-a_c))]
fwrite(d, file.path(OUT, "flags", "thin_record_examples.csv"))
cat("total time", format(Sys.time() - t0), "\n")
