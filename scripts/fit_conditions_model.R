# Fit the conditions model per event: perf ~ s(wind) + s(log1p altitude) +
# indoor, with athlete, race and VENUE random effects, via gamm4 (mgcv smooths,
# lme4 engine -- the only combination that handles two large crossed random
# effects in memory here; mgcv::bam was killed at 17 GB on one).
#
# The venue term is the 2026-09-19 addition: piloted at 1.36% sd on the
# marathon, 0.89% on the 1500m, 0.50% on the 100m -- a course is worth more
# than a race's shock (docs/reference; NEXT-STEPS "adjusted marks v2").
#
# Fitted on a stratified sample of athletes per event (altitude bands, wind
# tails, indoor, then a random top-up to TARGET_ATHLETES) so the smooths see
# their extremes. The fit's job is the CURVES and the VARIANCE COMPONENTS;
# per-venue offsets for the full corpus are computed downstream in closed
# form (build_adjusted_marks.R), not read off the sample's BLUPs.
#
# Resumable: one .rds per event in gamm4_events_venue/, skipped if present.
# A watchdog kill costs one event. Set FIT_EVENTS="AT-100Metres-M,..." to
# restrict; FIT_DIR to write elsewhere.
suppressMessages({ library(arrow); library(data.table); library(gamm4); library(lme4) })
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
D <- here::here("citiusdata", "data")
DIR <- file.path(D, Sys.getenv("FIT_DIR", "gamm4_events_venue")); dir.create(DIR, showWarnings = FALSE)
TARGET_ATHLETES <- 1800L; PER_STRATUM <- 300L

# race -> venue map, cached: the corpus parquet is 4.6M rows and this needs one
# row per race. Rebuilt when the corpus is newer than the cache.
vmap_f <- file.path(D, "venue_by_race.parquet"); corpus_f <- file.path(D, "athletics_corpus.parquet")
if (!file.exists(vmap_f) || file.mtime(vmap_f) < file.mtime(corpus_f)) {
  vc <- setDT(read_parquet(corpus_f, col_select = c("race_key", "venue_city")))
  vc <- unique(vc[!is.na(venue_city) & nzchar(venue_city)], by = "race_key")
  write_parquet(vc, vmap_f); rm(vc); invisible(gc())
}
vc <- setDT(read_parquet(vmap_f))
cat(sprintf("race -> venue map: %s races\n", format(nrow(vc), big.mark = ",")))

store_events <- sub("^event_id=", "", grep("^event_id=AT-", list.files(file.path(D, "athletics_corpus_store")), value = TRUE))
want <- Sys.getenv("FIT_EVENTS", "")
EVENTS <- if (nzchar(want)) strsplit(want, ",")[[1]] else sort(store_events)
cat(sprintf("%d events to consider, %d already fitted\n", length(EVENTS), sum(file.exists(file.path(DIR, paste0(EVENTS, ".rds"))))))

fit_event <- function(EV, seed = 1) {
  c0 <- setDT(open_dataset(file.path(D, "athletics_corpus_store")) |>
    dplyr::filter(event_id == EV) |>
    dplyr::select(athlete_id, event_id, mark, perf, date, wind, indoor, alt_m, race_key) |> dplyr::collect())
  c0[, athlete_id := as.character(athlete_id)]
  c0 <- merge(c0, vc, by = "race_key", all.x = TRUE)
  has_wind <- mean(is.finite(c0$wind)) > 0.5
  if (has_wind) { c0[is.na(wind) & indoor == TRUE, wind := 0]; c0 <- c0[is.finite(wind) & wind >= -6 & wind <= 8] }
  c0 <- c0[is.finite(perf) & is.finite(mark) & mark > 0 & is.finite(alt_m) & !is.na(venue_city)]
  c0[, n_ath := .N, by = athlete_id]; c0 <- c0[n_ath >= 3]
  if (!nrow(c0) || uniqueN(c0$athlete_id) < 50) return(NULL)
  c0[, band := .altitude_band(alt_m)]
  c0[, alt_t := log1p(pmax(alt_m, 0))]
  has_indoor <- sum(c0$indoor == TRUE, na.rm = TRUE) >= 200 && uniqueN(c0[indoor == TRUE, athlete_id]) >= 20 && any(c0$indoor == FALSE, na.rm = TRUE)

  set.seed(seed)
  cap <- function(ids, n) { u <- unique(ids); if (length(u) > n) sample(u, n) else u }
  strata <- c(cap(c0[band == "800-1500", athlete_id], PER_STRATUM), cap(c0[band == "1500-2200", athlete_id], PER_STRATUM),
              cap(c0[band == ">2200", athlete_id], PER_STRATUM),
              if (has_wind) cap(c0[abs(wind) >= 3, athlete_id], PER_STRATUM) else character(0),
              if (has_indoor) cap(c0[indoor == TRUE, athlete_id], PER_STRATUM) else character(0))
  strat <- unique(strata)
  other <- setdiff(unique(c0$athlete_id), strat)
  top_up <- cap(other, max(0L, TARGET_ATHLETES - length(strat)))
  cs <- c0[athlete_id %in% c(strat, top_up)]
  cs[, `:=`(ath_f = factor(athlete_id), race_f = factor(race_key), venue_f = factor(venue_city),
            indoor_f = factor(fifelse(is.na(indoor), FALSE, indoor)))]
  rhs <- c(if (has_wind) "s(wind, k = 8)", "s(alt_t, k = 8)", if (has_indoor) "indoor_f")
  fml <- as.formula(paste("perf ~", paste(rhs, collapse = " + ")))
  m <- tryCatch(gamm4(fml, random = ~(1|ath_f) + (1|race_f) + (1|venue_f), data = cs),
                error = function(e) { cat("FIT ERROR", EV, ":", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  v <- as.data.frame(VarCorr(m$mer))
  sd_of <- function(g) { x <- v$sdcor[v$grp == g]; if (length(x)) x[1] else NA_real_ }
  list(event_id = EV, model = m, sample = cs, has_indoor = has_indoor, has_wind = has_wind,
       sd = c(athlete = sd_of("ath_f"), race = sd_of("race_f"), venue = sd_of("venue_f"), resid = sd_of("Residual")),
       fitted_at = Sys.time())
}

t_all <- Sys.time()
for (EV in EVENTS) {
  f <- file.path(DIR, paste0(EV, ".rds"))
  if (file.exists(f)) next
  t0 <- Sys.time()
  r <- fit_event(EV)
  if (is.null(r)) { cat(sprintf("%-32s skipped (too thin or failed)\n", EV)); next }
  saveRDS(r, f)
  cat(sprintf("%-32s %6d rows %5d ath %5d races %4d venues | sd%% ath %.2f race %.2f venue %.2f resid %.2f | %.0fs\n",
              EV, nrow(r$sample), uniqueN(r$sample$athlete_id), uniqueN(r$sample$race_key), uniqueN(r$sample$venue_city),
              100*r$sd["athlete"], 100*r$sd["race"], 100*r$sd["venue"], 100*r$sd["resid"], as.numeric(Sys.time()-t0, units="secs")))
  rm(r); invisible(gc())
}
cat(sprintf("done in %.1f min; %d fits on disk\n", as.numeric(Sys.time()-t_all, units="mins"), length(list.files(DIR, "[.]rds$"))))
