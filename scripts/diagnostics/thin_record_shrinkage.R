# Thin-record shrinkage: elite middle/long-distance athletes whose rating is pulled
# far towards the whole-event mean because they raced little recently
# (found 2026-11-09 on Kipyegon: w_total 0.16, shrinkage 0.27, 3:57.8 -> 4:08.1).
# PROTOTYPE, for a walk-through with Pete. Changes nothing published.
#
# Three numbers per athlete:
#   now      = deployed ability (shrunk towards prior_mu, the mean of everyone in the event)
#   unshrunk = ability_raw (her own weighted results, no pull)
#   own-prior= the same shrinkage weight, but pulled towards the athlete's OWN long
#              record (ability_raw refitted with every event half-life x5) instead of
#              the event mean. One possible level-aware prior, not a fitted model.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date)
cat("rows", nrow(h), "athletes", uniqueN(h$athlete_id), "as_of", format(AS_OF), "\n")
cal <- deployed_calibration(OUT)
ep <- deployed_event_params()
cat("half-lives used:\n"); print(ep[event_id %in% EV, .(event_id, half_life)])

now <- deployed_ability(h, as_of = AS_OF, calibration = cal, event_params = ep)
ep_long <- copy(ep)[, half_life := half_life * 5]
lng <- .deployed_ability_raw(h, AS_OF, cal, ep_long)
for (x in list(now, lng)) x[, athlete_id := as.character(athlete_id)]
cat("rated", nrow(now), " long-run rated", nrow(lng), format(Sys.time() - t0), "\n")

d <- merge(now[, .(athlete_id, event_id, ability, ability_raw, shrinkage, w_total, prior_mu, last_date)],
           lng[, .(athlete_id, event_id, long_raw = ability_raw)], by = c("athlete_id", "event_id"))
d <- d[as.numeric(AS_OF - last_date) <= 730]           # the ratings page's active rule
d[, own_prior := (1 - shrinkage) * ability_raw + shrinkage * long_raw]
d[, `:=`(rank_now = frank(-ability, ties.method = "min"),
         rank_unshrunk = frank(-ability_raw, ties.method = "min"),
         rank_own = frank(-own_prior, ties.method = "min")), by = event_id]

# recent record, straight from the results
rec <- h[date > AS_OF - 365, .(races_12m = .N, best_12m = min(mark, na.rm = TRUE)), by = .(athlete_id, event_id)]
d <- rec[d, on = c("athlete_id", "event_id")][is.na(races_12m), races_12m := 0L]
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
d[, id0 := sub("^0+", "", athlete_id)]; d <- nm[d, on = "id0"][, id0 := NULL]
stopifnot(d[, mean(!is.na(athlete_name))] > 0)
fwrite(d, file.path(OUT, "flags", "thin_record_shrinkage.csv"))

fmt <- function(sec) ifelse(is.na(sec), NA, sprintf("%d:%05.2f", sec %/% 60, sec %% 60))
pick <- d[rank_unshrunk <= 25 & shrinkage >= 0.10][order(-shrinkage)]
cat("\nElite (unshrunk top 25) with shrinkage >= 0.10:", nrow(pick), "athlete-events\n")
show <- pick[, .(event = sub("AT-|Metres", "", event_id), athlete_name, races_12m,
                 last = format(last_date), shrink = round(shrinkage, 2), w = round(w_total, 2),
                 now = fmt(exp(-ability)), unshrunk = fmt(exp(-ability_raw)), own_prior = fmt(exp(-own_prior)),
                 rank_now, rank_unshrunk, rank_own)]
print(show[1:min(.N, 20)])
cat("\nFor scale: shrinkage among the unshrunk top 25, by event (median, max):\n")
print(d[rank_unshrunk <= 25, .(median = round(median(shrinkage), 3), max = round(max(shrinkage), 3), n = .N), by = event_id])
cat("total time", format(Sys.time() - t0), "\n")
