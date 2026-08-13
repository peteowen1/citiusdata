# Re-measure the four stale effects on the current corpus.
#
# Every one was fitted on an old data vintage, and wind was additionally fitted
# with an estimator that was not the within estimator (it regressed a
# within-centred perf on RAW wind, attenuating the coefficient by between-athlete
# variation in mean exposure -- fixed 2026-08-03).
#
# The point is not to produce coefficients. It is to decide, for each, whether
# wiring it is worth anything, using the quadratic rule: removing an independent
# component of spread s from a total sigma cuts the error metric by about
# s^2 / 2*sigma^2. Against a sigma of ~2.6% of a mark, a feature worth 0.25% of
# a mark buys 0.5% of the metric and one worth 1% buys 7.6%. The whole model
# currently beats its baseline by 1.53%, so anything under ~0.3% of a mark is
# not visible.

suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
library(data.table); library(arrow)

D <- here::here("citiusdata", "data")
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

cols <- c("athlete_id", "event_id", "date", "perf", "mark", "wind", "indoor",
          "venue_country", "venue_city", "venue_stadium", "comp_name",
          "round", "tier", "race_key", "competition_id", "age", "sex")
say("reading corpus ...")
x <- as.data.table(read_parquet(file.path(D, "athletics_corpus.parquet"),
                                col_select = all_of(cols)))
x <- x[!is.na(perf) & !is.na(event_id) & !is.na(date)]
say("corpus: ", format(nrow(x), big.mark = ","), " rows, ",
    format(uniqueN(x$race_key), big.mark = ","), " races, ",
    uniqueN(x$event_id), " events")

reg <- as.data.table(citius_events())[, .(event_id, family)]
x <- merge(x, reg, by = "event_id", all.x = TRUE)

cal <- readRDS(file.path(D, "calibration_corpus_athfoul.rds"))
ev <- as.data.table(cal$events)[calibrated %in% TRUE, .(event_id, sigma_within)]

#' Turn a per-event effect into "what it is worth on the metric".
#'
#' `s` is the spread the effect explains, in log units. The quadratic rule then
#' says how much of the error metric it removes. Reported per event AND pooled
#' by the number of results the effect can actually reach -- an effect that is
#' large on one rare event is worth nothing overall.
worth <- function(eff, label, n_col = "n", beta_col = "beta", sd_col = NULL) {
  e <- merge(as.data.table(eff), ev, by = "event_id")
  if (!nrow(e)) { say(label, ": no calibrated events matched"); return(NULL) }
  if (is.null(sd_col)) {
    # spread explained = |beta| * sd(covariate) for that event
    e[, s := abs(get(beta_col)) * cov_sd]
  } else e[, s := get(sd_col)]
  e[, pct_of_metric := 100 * s^2 / (2 * sigma_within^2)]
  e[, reach := get(n_col)]
  setorder(e, -pct_of_metric)
  cat("\n---", label, "---\n")
  print(head(e[, .(event_id, n = reach, beta = round(get(beta_col), 5),
                   s_pct = round(100 * s, 3),
                   sigma_pct = round(100 * sigma_within, 2),
                   pct_of_metric = round(pct_of_metric, 2))], 10))
  pooled <- sum(e$reach * e$pct_of_metric, na.rm = TRUE) / sum(e$reach, na.rm = TRUE)
  cat(sprintf("  events: %d | results reached: %s | RESULT-WEIGHTED WORTH: %.3f%% of the metric\n",
              nrow(e), format(sum(e$reach), big.mark = ","), pooled))
  invisible(e)
}

# ---------------------------------------------------------------- 1. WIND ----
say("1/4 wind ...")
w <- x[!is.na(wind) & abs(wind) <= 6 & indoor %in% c(FALSE, NA)]
say("  wind readings: ", format(nrow(w), big.mark = ","),
    " (was 86,550 at v1)")
wf <- as.data.table(fit_wind_effect(w, min_n = 200L))
wf <- wf[!is.na(beta)]
cov_sd_w <- w[, .(cov_sd = sd(wind, na.rm = TRUE)), by = event_id]
wf <- merge(wf, cov_sd_w, by = "event_id")
saveRDS(wf, file.path(D, "wind_effect_corpus_refit.rds"))
worth(wf, "WIND (refit on corpus, corrected within estimator)")

# --------------------------------------------------------------- 2. VENUE ----
say("2/4 venue ...")
vf <- tryCatch(as.data.table(fit_venue_effect(x, venue_col = "venue_stadium",
                                              min_n = 30L)),
               error = function(e) { say("  venue fit failed: ", conditionMessage(e)); NULL })
if (!is.null(vf) && nrow(vf)) {
  saveRDS(vf, file.path(D, "venue_effect_corpus_refit.rds"))
  cat("\n--- VENUE (refit on corpus) ---\n")
  print(head(vf[order(-abs(get(intersect(c("ven_eff","effect","beta"), names(vf))[1])))], 8))
  vcol <- intersect(c("ven_eff", "effect", "beta"), names(vf))[1]
  s_v <- sd(vf[[vcol]], na.rm = TRUE)
  sig <- median(ev$sigma_within, na.rm = TRUE)
  cat(sprintf("  spread of venue effects: %.3f%% of a mark; sigma %.2f%%\n",
              100 * s_v, 100 * sig))
  cat(sprintf("  WORTH IF FULLY EXPLOITABLE: %.2f%% of the metric\n",
              100 * s_v^2 / (2 * sig^2)))
  cat("  (upper bound -- a venue effect is only usable where the venue is known in advance)\n")
}

# ------------------------------------------------------------- 3. TAIL DF ----
say("3/4 tail_df ...")
tdf <- tryCatch(fit_tail_df(x), error = function(e) { say("  failed: ", conditionMessage(e)); NULL })
if (!is.null(tdf)) {
  old <- tryCatch(readRDS(file.path(D, "tail_df.rds")), error = function(e) NA)
  cat("\n--- TAIL DF ---\n")
  cat("  refit on corpus:", if (is.list(tdf)) tdf$df else tdf, "\n")
  cat("  v0 artefact    :", if (is.list(old)) old$df else old, "\n")
  cat("  calibration currently carries:", cal$tail_df, "\n")
  saveRDS(tdf, file.path(D, "tail_df_corpus_refit.rds"))
}

# ------------------------------------------------------------ 4. ALTITUDE ----
say("4/4 altitude ...")
# Elevations are objective reference data for the venues that actually matter.
ALT <- data.table(
  venue_city = c("Mexico City", "Bogota", "Bogotá", "Addis Ababa", "Nairobi",
                 "Johannesburg", "Pretoria", "Denver", "Albuquerque", "Provo",
                 "El Paso", "Flagstaff", "Sestriere", "Font Romeu", "Ifrane",
                 "Toluca", "Quito", "La Paz", "Cochabamba", "Potchefstroom",
                 "Eldoret", "Iten", "Asmara", "Sucre"),
  elev_m = c(2240, 2640, 2640, 2355, 1795, 1753, 1339, 1609, 1619, 1387,
             1140, 2106, 2035, 1850, 1665, 2660, 2850, 3640, 2558, 1350,
             2100, 2400, 2325, 2810))
x[, elev_m := ALT$elev_m[match(venue_city, ALT$venue_city)]]
x[is.na(elev_m), elev_m := 0]
say("  results at a known-altitude venue: ",
    format(x[elev_m > 1000, .N], big.mark = ","),
    " (", round(100 * mean(x$elev_m > 1000), 2), "% of the corpus)")

# Within athlete-event, comparing altitude to sea level for athletes who did both.
alt <- x[, .(athlete_id, event_id, family, perf, elev_m)]
alt[, hi := elev_m > 1000]
both <- alt[, .(n_hi = sum(hi), n_lo = sum(!hi)), by = .(athlete_id, event_id)][n_hi > 0 & n_lo > 0]
alt <- merge(alt, both[, .(athlete_id, event_id)], by = c("athlete_id", "event_id"))
say("  athlete-events with BOTH altitude and sea-level marks: ", format(nrow(both), big.mark = ","))
if (nrow(alt)) {
  alt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]
  res <- alt[, .(n = .N, effect_pct = 100 * (mean(dev[hi]) - mean(dev[!hi]))),
             by = family][order(-effect_pct)]
  cat("\n--- ALTITUDE, within athlete-event, >1000m vs sea level ---\n")
  print(res)
  cat("  positive = FASTER/FURTHER at altitude (perf is higher-is-better)\n")
}

say("done in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
