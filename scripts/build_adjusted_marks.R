# Build adjusted_marks.parquet: every performance corrected for what was not in
# the athlete's control, one column per component.
#
#   stage 1  wind_adj, venue_adj (altitude), indoor_adj
#            from the per-event conditions parameters (conditions_params/*.json,
#            exported from the gamm4 fits by export_conditions_params.R),
#            applied with citius::adjust_conditions() -- the same arithmetic ITG
#            can run live.
#   stage 2  race_shock: the shrunk, winsorised field-mean surprise after stage 1
#            and after each athlete's own level, via citius::race_shock().
#            Level = leave-one-out mean of the athlete's other shock-adjusted
#            marks in the same event and calendar year (>= 2 others, else no
#            expectation and the athlete is excluded from the field mean but
#            still receives the shock). Level and shock are re-estimated
#            alternately for N_ITER rounds -- the same alternating projection
#            calibrate() uses, on the cleaned marks.
#
#   adj_perf = perf - wind_adj - venue_adj - indoor_adj - race_shock
#
# Contract kept for form_ratings.R: race_key, athlete_id, event_id, wind_adj,
# venue_adj, indoor_adj. race_shock is a NEW column that form_ratings.R does
# NOT yet read -- calibrate() still fits its own c_r. Wiring one and retiring
# the other is a separate decision (double-count guard).
#
# Writes adjusted_marks.parquet (override with ADJ_OUT=<filename> to build a
# candidate beside the live file; form_ratings.R reads it via SEQ_ADJFILE).
#
# WHO READS THIS FILE. form_ratings.R (the FORM engine, arms + display-mark
# calibration) — and nothing else. The career model behind the ITG cards
# (deployed_ability -> estimate_ability + calibration) reads the raw store and
# carries its own altitude term and race shock; wiring adjusted marks into it
# is an open item (NEXT-STEPS "adjusted marks v2") and must switch that
# altitude term off at the same time or it is counted twice.
#
# ~4 min, ~2 GB peak. Killed twice by the memory watchdog on 2026-09-18 before
# intermediates were dropped ahead of the parquet write (arrow copies).
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
PDIR <- file.path(D, "conditions_params")
N_ITER <- 3L
reg <- as.data.table(citius_events())[, .(event_id, discipline, sex, family, orientation, unit)]

t_all <- Sys.time()
# Stage 0 -- the scoreable corpus with registry + alt_m joined -- is identical
# for every build until the corpus changes, and cost ~90s of each of the six
# builds run on 2026-09-19. Cached beside the corpus, keyed on its mtime.
corpus_f <- file.path(D, "athletics_corpus.parquet"); stage0_f <- file.path(D, "adjusted_marks_stage0.parquet")
if (file.exists(stage0_f) && file.mtime(stage0_f) > file.mtime(corpus_f)) {
  c0 <- setDT(read_parquet(stage0_f))
  cat(sprintf("stage 0 from cache: %s rows (corpus %s)\n", format(nrow(c0), big.mark=","), format(file.mtime(corpus_f), "%Y-%m-%d %H:%M")))
} else {
  c0 <- setDT(read_parquet(corpus_f,
                           col_select = c("athlete_id","event_id","race_key","mark","perf",
                                          "date","wind","legal","indoor","scoreable",
                                          "venue_city","venue_stadium","race_code","place","comp_name")))
  c0[, athlete_id := as.character(athlete_id)]
  .fill <- function(x) { u <- unique(x[!is.na(x) & nzchar(x)]); if (length(u) == 1L) u else x }
  c0[, venue_city    := .fill(venue_city),    by = race_key]
  c0[, venue_stadium := .fill(venue_stadium), by = race_key]
  c0 <- c0[scoreable == TRUE & is.finite(perf) & is.finite(mark) & mark > 0]
  .n_pre <- nrow(c0)
  c0 <- merge(c0, reg, by = "event_id")
  stopifnot("rows carry an event_id the registry does not have" = nrow(c0) == .n_pre)
  alt <- setDT(open_dataset(file.path(D, "athletics_corpus_store")) |>
    dplyr::select(race_key, alt_m) |> dplyr::collect())
  alt <- unique(alt[!is.na(race_key)], by = "race_key")
  c0 <- merge(c0, alt, by = "race_key", all.x = TRUE)
  write_parquet(c0, stage0_f)
}
cat(sprintf("scoreable performances: %s over %d events, %s to %s; alt_m on %.1f%%\n",
            format(nrow(c0), big.mark=","), uniqueN(c0$event_id), min(c0$date), max(c0$date), 100*mean(is.finite(c0$alt_m))))

# ---- stage 1 ---------------------------------------------------------------
c0[, `:=`(wind_adj = 0, venue_adj = 0, indoor_adj = 0, covered = FALSE)]
params <- list()
t0 <- Sys.time()
for (EV in sort(unique(c0$event_id))) {
  p <- conditions_params(EV, PDIR)
  if (is.null(p)) next
  params[[EV]] <- p
  # NA POLICY (decided 2026-09-19): an unknown altitude gets NO curve (never
  # imputed to sea level) but the row stays covered -- the venue offset below
  # is learnt on every row at that venue and carries its altitude implicitly.
  # 14% of rows in every family have a venue and no altitude; leaving them
  # uncovered cost the form engine measurably (v5 vs the August file).
  i <- c0[event_id == EV, which = TRUE]
  if (!length(i)) next
  a <- adjust_conditions(p, wind = c0$wind[i], alt_m = c0$alt_m[i], indoor = c0$indoor[i] %in% TRUE)
  set(c0, i, "wind_adj", a$wind_adj); set(c0, i, "venue_adj", a$venue_adj); set(c0, i, "indoor_adj", a$indoor_adj)
  set(c0, i, "covered", TRUE)
}
cat(sprintf("stage 1: %d events with parameters, applied in %.1fs; rows covered %s (%.1f%%)\n",
            length(params), as.numeric(Sys.time()-t0, units="secs"),
            format(sum(c0$covered), big.mark=","), 100*mean(c0$covered)))
c0[, cleaned := perf - wind_adj - venue_adj - indoor_adj]

# ---- stage 2 ---------------------------------------------------------------
# empirical variance components on the cleaned marks, per event (method of
# moments): var_resid = mean within-race variance of the residual; var_race =
# variance of field means minus its sampling part. Printed against the gamm4
# fit's values as a check; the empirical ones are what the shrinkage uses,
# because the level here is a leave-one-out season mean, not a fitted effect.
#
# VENUE (2026-09-19): a per-(event, venue_city) offset beside the altitude
# curve -- a course or a track, not its height. Closed form like the shock: the
# shrunk mean of the venue's race-level residual means, n_races * var_venue /
# (n_races * var_venue + var_race + var_resid / mean field), var_venue from the
# gamm4 fit. The form-engine A/B of 2026-09-18 showed the August file's per-city
# offsets beat an altitude-only v2 (t = 25.5); the pilot measured venue at
# 1.36% sd on the marathon. Written into venue_adj (= alt_adj + venue_off) so
# form_ratings.R's wind+venue+indoor sum carries it unchanged.
c0[, season := as.integer(format(as.Date(date), "%Y"))]
c0[, `:=`(race_shock = 0, venue_off = 0, alt_adj = venue_adj)]
vv <- rbindlist(lapply(params, function(p) data.table(event_id = p$event_id, var_venue = if (is.null(p$var_venue)) 0 else p$var_venue)))
c0 <- merge(c0, vv, by = "event_id", all.x = TRUE)
t0 <- Sys.time()
for (it in seq_len(N_ITER)) {
  c0[, adj0 := cleaned - venue_off - race_shock]
  c0[, `:=`(n_ae = .N, sum_ae = sum(adj0)), by = .(athlete_id, event_id, season)]
  c0[, level := fifelse(n_ae >= 3L, (sum_ae - adj0) / (n_ae - 1L), NA_real_)]
  c0[, resid := cleaned - level]
  if (it == 1L) {
    vc <- c0[covered == TRUE & is.finite(resid), {
      wr <- .SD[, .(n = .N, m = mean(resid), v = if (.N >= 2) var(resid) else NA_real_), by = race_key]
      list(var_resid_emp = mean(wr$v, na.rm = TRUE),
           var_race_emp  = max(var(wr[n >= 3]$m) - mean(mean(wr$v, na.rm = TRUE) / wr[n >= 3]$n), 1e-8))
    }, by = event_id]
    fit <- rbindlist(lapply(params, function(p) data.table(event_id = p$event_id, var_race_fit = p$var_race, var_resid_fit = p$var_resid)))
    vc <- merge(vc, fit, by = "event_id")
    cat("\nvariance components, sd in % of mark (empirical on cleaned marks vs the gamm4 fit); first 12 events:\n")
    print(vc[order(event_id)][1:12, .(event_id, sd_race_emp = round(100*sqrt(var_race_emp), 2), sd_race_fit = round(100*sqrt(var_race_fit), 2),
                                        sd_resid_emp = round(100*sqrt(var_resid_emp), 2), sd_resid_fit = round(100*sqrt(var_resid_fit), 2))])
    c0 <- merge(c0, vc[, .(event_id, var_race_emp, var_resid_emp)], by = "event_id", all.x = TRUE)
  }
  # Venue offset, HIERARCHICAL, from race-level means of the residual net of the
  # current shock. Three levels, each shrunk toward the one above (the August
  # build did family-city then stadium and beat a flat per-event version by
  # 1.2% in middle distance -- pooling is where thin events get their venue):
  #   family x city   : pooled over every event in the family, toward 0
  #   event  x city   : toward its family-city value
  #   event x stadium : toward its event-city value
  # Shrinkage weight n_races * var_venue / (n_races * var_venue + var_race + var_resid / field).
  # from the RAW residual, not resid - race_shock: a venue whose races are
  # reliably fast (a paced Diamond League track) carries that as venue signal,
  # forecastable before the gun; the shrinkage denominator's var_race term is
  # what accounts for race-level noise, so subtracting the shock first
  # double-shrinks (v4 did, and left middle distance 1.2% behind the August file)
  vr <- c0[covered == TRUE & is.finite(resid) & is.finite(var_race_emp) & var_venue > 0 & !is.na(venue_city),
           .(m = mean(resid), n = .N, family = family[1], venue_stadium = venue_stadium[1]),
           by = .(event_id, venue_city, race_key)]
  # one row per event, finite components only: an event with no empirical
  # variance (too thin) must not turn its whole family's pooled variance NA --
  # that zeroed every middle-distance and walk venue offset on the first run
  evv <- unique(c0[is.finite(var_race_emp) & is.finite(var_resid_emp) & is.finite(var_venue),
                   .(event_id, family, var_venue, var_race_emp, var_resid_emp)], by = "event_id")
  shrink <- function(n_races, var_venue, var_race, var_resid, nbar) n_races * var_venue / (n_races * var_venue + var_race + var_resid / nbar)
  fc <- vr[, .(n_races = .N, m = mean(m), nbar = mean(n)), by = .(family, venue_city)]
  fv <- evv[, .(var_venue = mean(var_venue), var_race_emp = mean(var_race_emp), var_resid_emp = mean(var_resid_emp)), by = family]
  stopifnot("a family lost its pooled variance components" = all(is.finite(unlist(fv[, -1]))))
  fc <- merge(fc, fv, by = "family")
  fc[, off_fc := m * shrink(n_races, var_venue, var_race_emp, var_resid_emp, nbar)]
  ec <- vr[, .(n_races = .N, m = mean(m), nbar = mean(n)), by = .(event_id, family, venue_city)]
  ec <- merge(ec, evv, by = c("event_id", "family"))
  ec <- merge(ec, fc[, .(family, venue_city, off_fc)], by = c("family", "venue_city"))
  ec[, off_ec := off_fc + shrink(n_races, var_venue, var_race_emp, var_resid_emp, nbar) * (m - off_fc)]
  st <- vr[!is.na(venue_stadium) & nzchar(venue_stadium), .(n_races = .N, m = mean(m), nbar = mean(n)), by = .(event_id, family, venue_city, venue_stadium)]
  st <- merge(st, evv, by = c("event_id", "family"))
  st <- merge(st, ec[, .(event_id, venue_city, off_ec)], by = c("event_id", "venue_city"))
  st[, off_st := off_ec + shrink(n_races, var_venue, var_race_emp, var_resid_emp, nbar) * (m - off_ec)]
  c0[, venue_off := NULL]
  c0 <- merge(c0, ec[, .(event_id, venue_city, off_ec)], by = c("event_id", "venue_city"), all.x = TRUE, sort = FALSE)
  c0 <- merge(c0, st[, .(event_id, venue_city, venue_stadium, off_st)], by = c("event_id", "venue_city", "venue_stadium"), all.x = TRUE, sort = FALSE)
  c0[, venue_off := fcoalesce(off_st, off_ec, 0)]
  c0[, c("off_ec", "off_st") := NULL]
  vo <- ec[, .(event_id, venue_city, n_races, venue_off = off_ec)]
  vs <- st[, .(event_id, venue_city, venue_stadium, n_races, venue_off = off_st)]
  c0[covered == TRUE & is.finite(var_race_emp),
     race_shock := race_shock_loo(resid - venue_off, var_race_emp[1], var_resid_emp[1]), by = race_key]
  stopifnot("venue offsets contain NA" = !anyNA(vo$venue_off), !anyNA(vs$venue_off))
  cat(sprintf("iter %d: sd(venue_off) %.5f over %s city cells, %s stadium cells, sd(race_shock) %.5f, rows with an expectation %.1f%%\n",
              it, sd(vo$venue_off), format(nrow(vo), big.mark=","), format(nrow(vs), big.mark=","),
              sd(c0[covered == TRUE]$race_shock), 100*mean(is.finite(c0[covered == TRUE]$resid))))
}
cat(sprintf("stage 2 in %.1fs\n", as.numeric(Sys.time()-t0, units="secs")))
c0[, venue_adj := alt_adj + venue_off]
write_parquet(vo, file.path(D, "conditions_params", "venue_offsets.parquet"))
write_parquet(vs, file.path(D, "conditions_params", "stadium_offsets.parquet"))
cat(sprintf("venue offsets: %s (event, city) and %s (event, city, stadium) cells written to conditions_params/\n",
            format(nrow(vo), big.mark=","), format(nrow(vs), big.mark=",")))

c0[, adj_perf := cleaned - venue_off - race_shock]
c0[, adj_mark := perf_to_mark(adj_perf, orientation)]
c0[, adj_delta := adj_mark - mark]

# ---- does each stage help? within-athlete scatter, 4+ marks -----------------
cat("\n=== within-athlete sd of perf (log units), athlete-events with 4+ covered marks; lower is better ===\n")
c0[, n_ath := 0L][covered == TRUE, n_ath := .N, by = .(athlete_id, event_id)]
sc <- c0[covered == TRUE & n_ath >= 4, .(sd_raw = sd(perf), sd_s1 = sd(cleaned), sd_sv = sd(cleaned - venue_off), sd_s2 = sd(adj_perf)),
         by = .(athlete_id, event_id, family)]
sc <- sc[is.finite(sd_raw) & is.finite(sd_s2)]
res <- sc[, .(athlete_events = .N, sd_raw = round(mean(sd_raw), 5),
              conditions_pct = round(100*(mean(sd_s1)/mean(sd_raw) - 1), 2),
              plus_venue_pct = round(100*(mean(sd_sv)/mean(sd_raw) - 1), 2),
              plus_shock_pct = round(100*(mean(sd_s2)/mean(sd_raw) - 1), 2)), by = family]
print(res[order(plus_shock_pct)])
ov <- sc[, .(r = mean(sd_raw), s1 = mean(sd_s1), sv = mean(sd_sv), s2 = mean(sd_s2))]
cat(sprintf("OVERALL: raw %.5f -> conditions %.5f (%+.2f%%) -> +venue %.5f (%+.2f%%) -> +shock %.5f (%+.2f%%)\n",
            ov$r, ov$s1, 100*(ov$s1/ov$r-1), ov$sv, 100*(ov$sv/ov$r-1), ov$s2, 100*(ov$s2/ov$r-1)))
stopifnot("race shock made athletes LESS self-consistent" = ov$s2 < ov$sv)

keep <- c("race_key","athlete_id","event_id","discipline","sex","family","date","season","comp_name",
          "venue_city","place","mark","adj_mark","adj_delta","perf","adj_perf","wind","alt_m","indoor",
          "wind_adj","venue_adj","alt_adj","venue_off","indoor_adj","race_shock","level","legal","unit","covered")
c0[, setdiff(names(c0), keep) := NULL]; setcolorder(c0, keep)
rm(sc, alt, vc, fit); invisible(gc())        # the write copies; drop everything else first
OUT_PATH <- file.path(D, Sys.getenv("ADJ_OUT", "adjusted_marks.parquet"))
write_parquet(c0, OUT_PATH)
cat(sprintf("\nwrote %s: %s rows x %d cols in %.0fs total\n",
            basename(OUT_PATH), format(nrow(c0), big.mark=","), ncol(c0), as.numeric(Sys.time()-t_all, units="secs")))
