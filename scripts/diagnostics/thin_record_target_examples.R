# Option-2 target, EXAMPLE ROWS ONLY (model-design: walk rows with Pete before fitting).
# Target per athlete-event at a cut: best mark over the next 12 months; an athlete who
# does NOT race counts as REPLACEMENT LEVEL FOR THEIR AGE, so no-shows are scored
# (the next-race target only ever saw comebacks). Two candidate replacement levels:
#   (i)  median 12-month best of athletes the same age (+-1 y) who did race;
#   (ii) the 100th-best 12-month best among those same-age athletes.
# Changes nothing published. Arrow via PowerShell.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV <- DEPLOYED$thin_record$events; CUT <- as.Date(Sys.getenv("CUT", "2025-10-01"))
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
cal <- deployed_calibration(OUT); ep <- deployed_event_params()
past <- h[date < CUT]
a <- deployed_ability(past, as_of = CUT - 1, calibration = cal, event_params = ep)
eph <- copy(ep)[event_id %in% EV, half_life := 365]
l <- .deployed_ability_raw(past, CUT - 1, cal, eph)
for (x in list(a, l)) x[, athlete_id := as.character(athlete_id)]
d <- merge(a[, .(athlete_id, event_id, ability, raw = ability_raw, s = shrinkage, floor = prior_mu, last_date)],
           l[, .(athlete_id, event_id, long = ability_raw, sL = shrinkage)], by = c("athlete_id", "event_id"))
d <- d[as.numeric(CUT - last_date) <= 730]
# age at the cut: last known age carried forward
ag <- past[!is.na(age)][order(date)][, .(age_cut = age[.N] + as.numeric(CUT - date[.N]) / 365.25), by = athlete_id]
ag <- ag[, .(age_cut = max(age_cut)), by = athlete_id]
d <- ag[d, on = "athlete_id"]
nx <- h[date >= CUT & date < CUT + 365, .(next_best = max(perf), next_n = .N), by = .(athlete_id, event_id)]
d <- nx[d, on = c("athlete_id", "event_id")][is.na(next_n), next_n := 0L]
# replacement by event x integer age, from athletes who DID race (+-1 year window)
act <- d[next_n > 0 & !is.na(age_cut), .(event_id, a0 = floor(age_cut), next_best)]
grid <- unique(d[!is.na(age_cut), .(event_id, a0 = floor(age_cut))])
rep <- grid[, {
  x <- act[event_id == .BY$event_id & abs(a0 - .BY$a0) <= 1, sort(next_best, decreasing = TRUE)]
  .(n_same_age = length(x), rep_median = if (length(x)) median(x) else NA_real_,
    rep_100th = if (length(x) >= 100) x[100] else NA_real_)
}, by = .(event_id, a0)]
d[, a0 := floor(age_cut)]; d <- rep[d, on = c("event_id", "a0")]
cat(sprintf("cut %s: rated (active <=730 d) %d; raced in the next 12 months %d (%.1f%%)\n", format(CUT), nrow(d),
            d[next_n > 0, .N], 100 * d[, mean(next_n > 0)]))
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
d[, id0 := sub("^0+", "", athlete_id)]; d <- nm[d, on = "id0"][, id0 := NULL]
fwrite(d, file.path(OUT, "flags", sprintf("thin_record_target_examples_%s.csv", format(CUT))))
