# Today's ratings under VERSION E (the settings in its championship arm:
# recent half-life x TB_HR, kappa_R x TB_MR, own record at TB_H days, no floor),
# next to the deployed rating, with each athlete's a weight and recent marks.
#   rating_E = a*R + (1-a)*L,  a = wR / (wR + TB_MR*kR)
# Same refits as backtest_athletics.R's thin-record blend arm, on the full corpus at
# AS_OF = the newest result. Changes nothing published. Output:
# data/flags/thin_record_E_examples.csv. Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
TB_HR <- as.numeric(Sys.getenv("CITIUS_TB_HR", "2")); TB_MR <- as.numeric(Sys.getenv("CITIUS_TB_MR", "0.5"))
TB_H <- as.numeric(Sys.getenv("CITIUS_TB_H", "365"))
EV <- DEPLOYED$thin_record$events
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date); cal <- deployed_calibration(OUT); ep <- deployed_event_params()
now <- deployed_ability(h, as_of = AS_OF, calibration = cal)
rec <- .deployed_ability_raw(h, AS_OF, cal, copy(ep)[, half_life := half_life * TB_HR])
lng <- .deployed_ability_raw(h, AS_OF, cal, copy(ep)[event_id %in% EV, half_life := TB_H])
for (x in list(now, rec, lng)) x[, athlete_id := as.character(athlete_id)]
d <- now[, .(athlete_id, event_id, a_now = ability, last_date)]
d <- rec[, .(athlete_id, event_id, R = ability_raw, wR = w_total, kR = shrinkage * w_total / (1 - shrinkage))][d, on = c("athlete_id", "event_id")]
d <- lng[, .(athlete_id, event_id, L = ability_raw)][d, on = c("athlete_id", "event_id")]
d[, a := wR / (wR + TB_MR * kR)][, a_E := a * R + (1 - a) * L]
d[!is.finite(a_E), a_E := a_now]
d <- d[as.numeric(AS_OF - last_date) <= 730]
d[, `:=`(rank_now = frank(-a_now, ties.method = "min"), rank_E = frank(-a_E, ties.method = "min")), by = event_id]
r12 <- h[, .(races_12m = sum(date > AS_OF - 365), best_12m = suppressWarnings(min(mark[date > AS_OF - 365])),
             best_3y = suppressWarnings(min(mark[date > AS_OF - 3 * 365]))), by = .(athlete_id, event_id)]
d <- r12[d, on = c("athlete_id", "event_id")]
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
d[, id0 := sub("^0+", "", athlete_id)]; d <- nm[d, on = "id0"][, id0 := NULL]
d[, `:=`(mark_now = exp(-a_now), mark_E = exp(-a_E), mark_R = exp(-R), mark_L = exp(-L))]
fwrite(d, file.path(OUT, "flags", "thin_record_E_examples.csv"))
cat(sprintf("AS_OF %s, rows %d, settings hr %g mR %g H %g, %s\n", format(AS_OF), nrow(d), TB_HR, TB_MR, TB_H,
            format(Sys.time() - t0)))
