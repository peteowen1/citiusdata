# MARKS LAB, PREP: build the cache the fast sweep runs on. Resumable.
#
# Split out of marks_lab_fast.R on 2026-09-07 after three runs were killed by
# memory pressure from other sessions, each time losing an hour of setup. The
# expensive work is now done once, written incrementally, and re-run cheaply:
# a kill costs only the month in flight.
#
# THREE CACHES, all in citiusdata/data/marks_lab_cache/:
#   adj.rds     history with every mark on a final-equivalent footing, plus the
#               parameter-free half of result_weight(). Neither depends on
#               as_of or half_life, so this is built once for all configs.
#   sigma/<month>.rds  one real estimate_ability() run per month at the deployed
#               config, giving the per-athlete sigma the sweep holds fixed and
#               the reference ability the sweep is verified against.
#   base.rds    the last-5 baseline per athlete-race.
#
# Memory: `h` is dropped as soon as adj and the baseline are built, and each
# month's estimate_ability runs in its own scope.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_lab_prep.R'
# Env: CITIUS_LAB_FROM/_TO, CITIUS_LAB_CAL, CITIUS_LAB_CACHE
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
FROM  <- as.Date(Sys.getenv("CITIUS_LAB_FROM", "2024-01-01"))
TO    <- as.Date(Sys.getenv("CITIUS_LAB_TO", "2026-09-01"))
CAL   <- Sys.getenv("CITIUS_LAB_CAL", DEPLOYED$calibration)
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache"))
dir.create(file.path(CACHE, "sigma"), recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
cal <- readRDS(file.path(OUT, CAL))

meta <- list(from = FROM, to = TO, cal = CAL, deployed_stamp = DEPLOYED$stamp,
             adjust_race = isTRUE(DEPLOYED$adjust_race), hl = DEPLOYED$hl_family,
             hl_global = DEPLOYED$half_life)
mf <- file.path(CACHE, "meta.rds")
if (file.exists(mf)) {
  old <- readRDS(mf)
  if (!identical(old[c("from","to","cal","adjust_race")], meta[c("from","to","cal","adjust_race")])) {
    say("cache was built for a different window or calibration -- clearing it")
    unlink(file.path(CACHE, "sigma"), recursive = TRUE)
    unlink(file.path(CACHE, c("adj.rds", "base.rds", "test.rds")))
    dir.create(file.path(CACHE, "sigma"), recursive = TRUE, showWarnings = FALSE)
  }
}
saveRDS(meta, mf)

# --- test set ----------------------------------------------------------------
if (!file.exists(file.path(CACHE, "test.rds"))) {
  ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
  ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
  test <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0 & !is.na(event_id),
                    .(race_key, athlete_id, event_id, date = as.Date(date), competition_id, round, mark)],
                 by = c("race_key", "athlete_id"))
  rm(ch); invisible(gc())
  ct <- as.data.table(open_dataset(file.path(OUT, "competition_catalogue.parquet")) |>
                        dplyr::select(competition_id, meet_tier) |> dplyr::collect())
  ct[, competition_id := as.character(competition_id)]
  test <- merge(test, unique(ct[!is.na(meet_tier)], by = "competition_id"), by = "competition_id")
  test <- test[meet_tier == "T1_elite" & date >= FROM & date < TO &
                 grepl("final", tolower(round)) & !grepl("semi|quarter", tolower(round))]
  reg <- as.data.table(citius_events())[, .(event_id, orientation, family)]
  test <- merge(test, reg, by = "event_id")[, act := orientation * log(mark)][is.finite(act)]
  test[, month := as.Date(format(date, "%Y-%m-01"))]
  saveRDS(test[, .(race_key, athlete_id, event_id, family, date, month, act)], file.path(CACHE, "test.rds"))
  say("test set cached: %s rows, %s races, %d events, %d months",
      format(nrow(test), big.mark = ","), format(uniqueN(test$race_key), big.mark = ","),
      uniqueN(test$event_id), uniqueN(test$month))
}
test <- readRDS(file.path(CACHE, "test.rds"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, family)]

# --- history: adjusted once, plus the last-5 baseline ------------------------
need_adj  <- !file.exists(file.path(CACHE, "adj.rds"))
need_base <- !file.exists(file.path(CACHE, "base.rds"))
months <- sort(unique(test$month))
need_sig <- months[!file.exists(file.path(CACHE, "sigma", paste0(format(months), ".rds")))]
if (need_adj || need_base || length(need_sig)) {
  store <- file.path(OUT, "athletics_corpus_store")
  cols <- intersect(c("athlete_id","event_id","date","perf","age","round","tier","meet_tier",
                      "competition_id","race_key","wind","momentum","indoor","venue_country"),
                    names(open_dataset(store)))
  t0 <- Sys.time()
  h <- as.data.table(read_results_store(store, events = unique(test$event_id),
                                        from = FROM - DEPLOYED$history_days, to = TO, columns = cols))
  h <- flag_implausible(h)[is.finite(perf)]
  h[, athlete_id := as.character(athlete_id)]; h[, date := as.Date(date)]
  say("history %s rows in %.0fs", format(nrow(h), big.mark = ","),
      as.numeric(difftime(Sys.time(), t0, units = "secs")))

  if (need_base) {
    hh <- h[, .(athlete_id, event_id, date, perf)]
    pk <- unique(test[, .(athlete_id, event_id, date)])
    jj <- hh[pk, on = .(athlete_id, event_id), allow.cartesian = TRUE][date < i.date]
    setorder(jj, athlete_id, event_id, i.date, -date)
    jj[, rk := seq_len(.N), by = .(athlete_id, event_id, i.date)]
    b5 <- jj[rk <= 5, .(base = mean(perf), n_prior = .N), by = .(athlete_id, event_id, date = i.date)]
    saveRDS(b5[n_prior >= 3L], file.path(CACHE, "base.rds"))
    rm(hh, pk, jj, b5); invisible(gc())
    say("last-5 baseline cached")
  }
  if (need_adj) {
    t0 <- Sys.time()
    # Keep the RAW mark beside the adjusted one, so the size of the context
    # adjustment itself becomes a swept parameter: perf(lambda) = raw + lambda *
    # (adjusted - raw). At lambda = 1 this is the deployed model exactly.
    raw_perf <- h$perf
    adj <- citius:::.adjust_history_to_target(copy(h), cal, isTRUE(DEPLOYED$adjust_race))
    adj[, perf_raw := raw_perf]
    adj[, w_static := result_weight(date, tier = tier, round = round, as_of = TO,
                                    half_life = Inf, calibration = cal,
                                    tier_class = citius:::.tier_class_of(adj))]
    adj <- adj[is.finite(perf) & is.finite(perf_raw) & is.finite(w_static) & w_static > 0,
               .(athlete_id, event_id, date, perf, perf_raw, w_static)]
    adj <- merge(adj, reg[, .(event_id, family)], by = "event_id")
    saveRDS(adj, file.path(CACHE, "adj.rds"))
    rm(adj); invisible(gc())
    say("adjusted history cached in %.0fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }
  # sigma reference, one month at a time so a kill costs one month.
  # Iterate by INDEX: `for (x in <Date vector>)` strips the Date class and hands
  # the body a bare number, which result_weight() then cannot subtract from.
  for (.i in seq_along(need_sig)) {
    cut <- need_sig[.i]
    t0 <- Sys.time()
    past <- h[date < cut]
    pf <- merge(past, reg[, .(event_id, family)], by = "event_id", all.x = TRUE)
    pf[is.na(family), family := ""]
    ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
      fam <- g$family[1]
      hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
      estimate_ability(g[, !"family"], as_of = cut, half_life = hl, calibration = cal,
                       adjust_race = isTRUE(DEPLOYED$adjust_race),
                       only = unique(test[month == cut]$athlete_id))
    }), fill = TRUE)
    saveRDS(ab[, .(athlete_id = as.character(athlete_id), event_id, sigma,
                   ref_ability = ability, ref_shrinkage = shrinkage, ref_w = w_total)],
            file.path(CACHE, "sigma", paste0(format(cut), ".rds")))
    rm(past, pf, ab); invisible(gc())
    say("sigma %s cached (%.0fs, %d of %d)", format(cut),
        as.numeric(difftime(Sys.time(), t0, units = "secs")), .i, length(need_sig))
  }
  rm(h); invisible(gc())
}
say("PREP COMPLETE: %d months of sigma, adj %s, base %s",
    length(list.files(file.path(CACHE, "sigma"))),
    file.exists(file.path(CACHE, "adj.rds")), file.exists(file.path(CACHE, "base.rds")))
