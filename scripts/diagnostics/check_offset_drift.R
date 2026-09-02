# How far forward do the family-pool offsets actually transfer?
#
# WHY THIS EXISTS. The same fix reads -2.72% marks MAE when offsets fit on
# [2023,2025) are applied ~1 year forward, and +1.35% WORSE when offsets fit on
# [2016,2020) are applied across 2020-2026. Same code, same mechanism; the only
# difference is the horizon the correction is asked to span. If the offsets
# drift, a single static table is the wrong shape for deployment no matter
# which window it is fit on, and the practical question becomes a refit
# cadence, not a fit window.
#
# Measured directly on the offsets themselves -- no backtest, no simulation.
# Fits the same quantity the real fit script uses (em = 100 x oriented log
# error, race-mean per event) on consecutive 2-year windows, then correlates
# consecutive and distant windows.
#
# ANCHORS:
#   A1 within-window split-half correlation, as the reliability CEILING. A
#      cross-window correlation must be read against this, not against 1.0 --
#      if the quantity is only measured at r=0.6 within a window, a
#      cross-window r of 0.5 is not evidence of drift.
#   A2 every reported window clears MIN_EVENTS shared events.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
ARM <- Sys.getenv("CITIUS_DRIFT_ARM", "backtest_tierctrl.rds")
MIN_RACES_EV <- 6L; MIN_EVENTS <- 10L
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s", ARM)

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d <- d[is.finite(act_perf) & is.finite(a_perf)]
d[, em := 100 * (a_perf - act_perf)]
d[, yr := year(date)]
say("population: %s rows, %s races, %d-%d",
    format(nrow(d), big.mark=","), format(uniqueN(d$race_id), big.mark=","),
    min(d$yr), max(d$yr))

# Event-level offset for a set of rows: race means first, then event mean, the
# same unit of replication the real fit uses.
ev_offsets <- function(dd) {
  rl <- dd[, .(m = mean(em)), by = .(race_id, event_id)]
  rl[, .(races = .N, off = mean(m)), by = event_id][races >= MIN_RACES_EV]
}

WINDOWS <- list(c(2016, 2017), c(2018, 2019), c(2020, 2021),
                c(2022, 2023), c(2024, 2026))
tabs <- lapply(WINDOWS, function(w) ev_offsets(d[yr >= w[1] & yr <= w[2]]))
names(tabs) <- vapply(WINDOWS, function(w) sprintf("%d-%d", w[1], w[2]), character(1))
cat("\n---- window sizes ----\n")
print(data.table(window = names(tabs),
                 events = vapply(tabs, nrow, integer(1)),
                 mean_offset = round(vapply(tabs, function(t) mean(t$off), numeric(1)), 3)),
      row.names = FALSE)

# A1: within-window reliability ceiling, by splitting each window's races in two.
cat("\n---- A1: within-window split-half reliability (the CEILING) ----\n")
set.seed(11)
rel <- rbindlist(lapply(seq_along(WINDOWS), function(i) {
  w <- WINDOWS[[i]]; dd <- d[yr >= w[1] & yr <= w[2]]
  rid <- unique(dd$race_id); h <- sample(rid, length(rid) %/% 2)
  a <- ev_offsets(dd[race_id %chin% h]); bq <- ev_offsets(dd[!race_id %chin% h])
  m <- merge(a, bq, by = "event_id")
  if (nrow(m) < MIN_EVENTS) return(NULL)
  data.table(window = names(tabs)[i], shared_events = nrow(m),
             r = round(cor(m$off.x, m$off.y), 3))
}))
print(rel, row.names = FALSE)
ceiling_r <- mean(rel$r, na.rm = TRUE)
say("mean within-window reliability ceiling: r = %.3f", ceiling_r)

cat("\n---- cross-window correlation of event offsets ----\n")
pairs <- CJ(i = seq_along(tabs), j = seq_along(tabs))[i < j]
res <- rbindlist(lapply(seq_len(nrow(pairs)), function(k) {
  i <- pairs$i[k]; j <- pairs$j[k]
  m <- merge(tabs[[i]], tabs[[j]], by = "event_id")
  if (nrow(m) < MIN_EVENTS) return(NULL)
  data.table(from = names(tabs)[i], to = names(tabs)[j],
             gap_yrs = WINDOWS[[j]][1] - WINDOWS[[i]][1],
             shared_events = nrow(m), r = round(cor(m$off.x, m$off.y), 3))
}))
setorder(res, gap_yrs)
print(res, row.names = FALSE)

cat("\n---- correlation vs horizon ----\n")
byg <- res[, .(pairs = .N, mean_r = round(mean(r), 3)), by = gap_yrs][order(gap_yrs)]
byg[, pct_of_ceiling := round(100 * mean_r / ceiling_r)]
print(byg, row.names = FALSE)

cat("\n==== VERDICT ====\n")
say("within-window ceiling r = %.3f", ceiling_r)
if (nrow(byg) >= 2) {
  near <- byg[gap_yrs == min(gap_yrs)]; far <- byg[gap_yrs == max(gap_yrs)]
  say("nearest horizon (%d yr): r = %.3f (%d%% of ceiling)", near$gap_yrs, near$mean_r, near$pct_of_ceiling)
  say("furthest horizon (%d yr): r = %.3f (%d%% of ceiling)", far$gap_yrs, far$mean_r, far$pct_of_ceiling)
  say("")
  say("A clear DECLINE with horizon means the offsets must be REFIT on a cadence")
  say("rather than shipped as a static table, and the cadence is roughly the")
  say("horizon at which r falls away from the ceiling. A FLAT profile means the")
  say("offsets are stable and the -2.72%% / +1.35%% difference is about something")
  say("else -- in which case do not blame drift.")
}
