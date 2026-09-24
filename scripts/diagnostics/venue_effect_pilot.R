# Is there a venue effect beyond altitude? Fit the conditions model with and
# without (1|venue) on three events and compare variance components.
suppressMessages({library(arrow);library(data.table);library(gamm4);library(lme4)})
D <- "C:/dev/citiusverse/citiusdata/data"
vc <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"), col_select = c("race_key","venue_city","venue_stadium")))
vc <- unique(vc[!is.na(venue_city) & nzchar(venue_city)], by = "race_key")

fit_one <- function(EV, with_venue, n_ath = 1800, seed = 1) {
  c0 <- setDT(open_dataset(file.path(D, "athletics_corpus_store")) |>
    dplyr::filter(event_id == EV) |>
    dplyr::select(athlete_id, mark, perf, date, wind, indoor, alt_m, race_key) |> dplyr::collect())
  c0[, athlete_id := as.character(athlete_id)]
  c0 <- merge(c0, vc, by = "race_key", all.x = TRUE)
  has_wind <- mean(is.finite(c0$wind)) > 0.5
  if (has_wind) { c0[is.na(wind) & indoor == TRUE, wind := 0]; c0 <- c0[is.finite(wind) & wind >= -6 & wind <= 8] }
  c0 <- c0[is.finite(perf) & mark > 0 & is.finite(alt_m) & !is.na(venue_city)]
  c0[, n := .N, by = athlete_id]; c0 <- c0[n >= 3]
  set.seed(seed); keep <- sample(unique(c0$athlete_id), min(n_ath, uniqueN(c0$athlete_id)))
  cs <- c0[athlete_id %in% keep]
  cs[, `:=`(ath_f = factor(athlete_id), race_f = factor(race_key), venue_f = factor(venue_city), alt_t = log1p(pmax(alt_m, 0)))]
  fml <- if (has_wind) perf ~ s(wind, k = 8) + s(alt_t, k = 8) else perf ~ s(alt_t, k = 8)
  rnd <- if (with_venue) ~(1|ath_f) + (1|race_f) + (1|venue_f) else ~(1|ath_f) + (1|race_f)
  t0 <- Sys.time()
  m <- gamm4(fml, random = rnd, data = cs)
  v <- as.data.frame(VarCorr(m$mer))
  sd_of <- function(g) { x <- v$sdcor[v$grp == g]; if (length(x)) x[1] else NA_real_ }
  data.table(event = EV, venue = with_venue, rows = nrow(cs), athletes = uniqueN(cs$athlete_id), races = uniqueN(cs$race_key),
             venues = uniqueN(cs$venue_city),
             sd_venue = 100*sd_of("venue_f"), sd_race = 100*sd_of("race_f"), sd_resid = 100*sd_of("Residual"),
             ll = as.numeric(logLik(m$mer)), secs = round(as.numeric(Sys.time()-t0, units="secs"), 1))
}
res <- rbindlist(lapply(c("AT-Marathon-M", "AT-1500Metres-M", "AT-100Metres-M"), function(EV)
  rbind(fit_one(EV, FALSE), fit_one(EV, TRUE))))
cat("sd in % of mark; a venue sd near the race sd means venue identity carries as much as a whole race's shock\n")
print(res)
