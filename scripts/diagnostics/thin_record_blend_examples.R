# Today's ratings under the TUNED "shrink twice" blend (H = 365 days), next to the
# deployed rating and version C, with each athlete's a / b / c weights and recent marks.
# Weights from data/flags/tune_thin_record_blend_levelfree_params.csv (fitted on 2024).
# The level shift c0 is NOT applied here: it moves every athlete the same way, so it
# changes no rank, and the ratings page has never carried it. Changes nothing published.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
H <- 365
par <- fread(file.path(OUT, "flags", "tune_thin_record_blend_levelfree_params.csv"))[H == "h365"]
stopifnot(nrow(par) == 1, is.finite(par$b1), is.finite(par$b2))
cat(sprintf("tuned on 2024: b1 %.4f  b2 %.4f\n", par$b1, par$b2))
EV <- DEPLOYED$thin_record$events
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date); cal <- deployed_calibration(OUT); ep <- deployed_event_params()
now <- deployed_ability(h, as_of = AS_OF, calibration = cal)
eph <- copy(ep)[event_id %in% EV, half_life := H]
lng <- .deployed_ability_raw(h, AS_OF, cal, eph)
for (x in list(now, lng)) x[, athlete_id := as.character(athlete_id)]
d <- merge(now[, .(athlete_id, event_id, a_now = ability, raw = ability_raw, s = shrinkage, floor = prior_mu, last_date)],
           lng[, .(athlete_id, event_id, long = ability_raw, sL = shrinkage)], by = c("athlete_id", "event_id"))
d <- d[as.numeric(AS_OF - last_date) <= 730]
d[, `:=`(wa = par$b1 * (1 - s), wc = par$b2 * s * (1 - sL))][, wb := 1 - wa - wc]
d[, a_new := floor + par$b1 * (1 - s) * (raw - floor) + par$b2 * s * (1 - sL) * (long - floor)]
d[, `:=`(rank_now = frank(-a_now, ties.method = "min"), rank_new = frank(-a_new, ties.method = "min")), by = event_id]
rec <- h[, .(races_12m = sum(date > AS_OF - 365), best_12m = suppressWarnings(min(mark[date > AS_OF - 365])),
             races_3y = sum(date > AS_OF - 3 * 365), best_3y = suppressWarnings(min(mark[date > AS_OF - 3 * 365]))),
         by = .(athlete_id, event_id)]
d <- rec[d, on = c("athlete_id", "event_id")]
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
d[, id0 := sub("^0+", "", athlete_id)]; d <- nm[d, on = "id0"][, id0 := NULL]
d[, `:=`(mark_now = exp(-a_now), mark_new = exp(-a_new), mark_raw = exp(-raw), mark_long = exp(-long), mark_floor = exp(-floor))]
fwrite(d, file.path(OUT, "flags", "thin_record_blend_examples.csv"))
cat("rows", nrow(d), "total time", format(Sys.time() - t0), "\n")
