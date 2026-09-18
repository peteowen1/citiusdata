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
# Writes adjusted_marks_v2.parquet; swapping it over the live file is manual.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
PDIR <- file.path(D, "conditions_params")
N_ITER <- 3L
reg <- as.data.table(citius_events())[, .(event_id, discipline, sex, family, orientation, unit)]

t_all <- Sys.time()
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
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
cat(sprintf("scoreable performances: %s over %d events, %s to %s\n",
            format(nrow(c0), big.mark=","), uniqueN(c0$event_id), min(c0$date), max(c0$date)))

alt <- setDT(open_dataset(file.path(D, "athletics_corpus_store")) |>
  dplyr::select(race_key, alt_m) |> dplyr::collect())
alt <- unique(alt[!is.na(race_key)], by = "race_key")
c0 <- merge(c0, alt, by = "race_key", all.x = TRUE)
cat(sprintf("alt_m coverage after join: %.1f%% of %s rows\n", 100*mean(is.finite(c0$alt_m)), format(nrow(c0), big.mark=",")))

# ---- stage 1 ---------------------------------------------------------------
c0[, `:=`(wind_adj = 0, venue_adj = 0, indoor_adj = 0, covered = FALSE)]
params <- list()
t0 <- Sys.time()
for (EV in sort(unique(c0$event_id))) {
  p <- conditions_params(EV, PDIR)
  if (is.null(p)) next
  params[[EV]] <- p
  # unknown altitude -> uncovered (0), not imputed to sea level: NA policy still open
  i <- c0[event_id == EV & is.finite(alt_m), which = TRUE]
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
c0[, season := as.integer(format(as.Date(date), "%Y"))]
c0[, race_shock := 0]
t0 <- Sys.time()
for (it in seq_len(N_ITER)) {
  c0[, adj0 := cleaned - race_shock]
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
  c0[covered == TRUE & is.finite(var_race_emp),
     race_shock := race_shock_loo(resid, var_race_emp[1], var_resid_emp[1]), by = race_key]
  cat(sprintf("iter %d: sd(race_shock) %.5f, rows with an expectation %.1f%%\n",
              it, sd(c0[covered == TRUE]$race_shock), 100*mean(is.finite(c0[covered == TRUE]$resid))))
}
cat(sprintf("stage 2 in %.1fs\n", as.numeric(Sys.time()-t0, units="secs")))

c0[, adj_perf := cleaned - race_shock]
c0[, adj_mark := perf_to_mark(adj_perf, orientation)]
c0[, adj_delta := adj_mark - mark]

# ---- does each stage help? within-athlete scatter, 4+ marks -----------------
cat("\n=== within-athlete sd of perf (log units), athlete-events with 4+ covered marks; lower is better ===\n")
c0[, n_ath := 0L][covered == TRUE, n_ath := .N, by = .(athlete_id, event_id)]
sc <- c0[covered == TRUE & n_ath >= 4, .(sd_raw = sd(perf), sd_s1 = sd(cleaned), sd_s2 = sd(adj_perf)),
         by = .(athlete_id, event_id, family)]
sc <- sc[is.finite(sd_raw) & is.finite(sd_s2)]
res <- sc[, .(athlete_events = .N, sd_raw = round(mean(sd_raw), 5),
              conditions_pct = round(100*(mean(sd_s1)/mean(sd_raw) - 1), 2),
              plus_shock_pct = round(100*(mean(sd_s2)/mean(sd_raw) - 1), 2)), by = family]
print(res[order(plus_shock_pct)])
ov <- sc[, .(r = mean(sd_raw), s1 = mean(sd_s1), s2 = mean(sd_s2))]
cat(sprintf("OVERALL: raw %.5f -> conditions %.5f (%+.2f%%) -> +shock %.5f (%+.2f%%)\n",
            ov$r, ov$s1, 100*(ov$s1/ov$r-1), ov$s2, 100*(ov$s2/ov$r-1)))
stopifnot("race shock made athletes LESS self-consistent" = ov$s2 < ov$s1)

keep <- c("race_key","athlete_id","event_id","discipline","sex","family","date","season","comp_name",
          "venue_city","place","mark","adj_mark","adj_delta","perf","adj_perf","wind","alt_m","indoor",
          "wind_adj","venue_adj","indoor_adj","race_shock","level","legal","unit","covered")
c0[, setdiff(names(c0), keep) := NULL]; setcolorder(c0, keep)
rm(sc, alt, vc, fit); invisible(gc())        # the write copies; drop everything else first
OUT_PATH <- file.path(D, "adjusted_marks_v2.parquet")
write_parquet(c0, OUT_PATH)
out <- c0
cat(sprintf("\nwrote %s: %s rows x %d cols in %.0fs total. NOT swapped over the live file.\n",
            basename(OUT_PATH), format(nrow(out), big.mark=","), ncol(out), as.numeric(Sys.time()-t_all, units="secs")))
