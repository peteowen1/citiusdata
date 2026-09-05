# How big IS a race shock, really?
#
# `calibration$race` fits a shared effect c_r per race. On the Gout Gout 200m
# (5 of 6 athletes PB'd) the fitted c_r said +1.013 s while the field has
# actually averaged +0.587 s slower since -- the effect is real, and 1.73x too
# large. One race proves nothing about the scale, so this measures it across
# many.
#
# METHOD. For each race with a fitted c_r and a decent field, compare:
#   fitted    : c_r, what the model says the race was worth
#   observed  : mean(field's LATER marks in the same event) - mean(their marks
#               in the race), which is what the shock actually cost them
# then regress observed on fitted. The SLOPE is the scale the model should be
# applying. Slope 1 means c_r is right; slope 0.6 means it over-corrects by
# 1/0.6.
#
# WHY LATER MARKS ARE THE RIGHT YARDSTICK: they are out of sample with respect
# to the race being measured, and they are what the model is ultimately trying
# to predict. Using the same race's residuals would be circular -- c_r was fitted
# on them.
#
# Usage:
#   CITIUS_SCALE_EVENT=AT-200Metres-M CITIUS_SCALE_MINFIELD=6 Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EVENTS <- trimws(strsplit(Sys.getenv("CITIUS_SCALE_EVENTS",
  "AT-100Metres-M,AT-200Metres-M,AT-400Metres-M,AT-800Metres-M,AT-LongJump-M,AT-ShotPut-M"), ",")[[1]])
MINF <- as.integer(Sys.getenv("CITIUS_SCALE_MINFIELD", "6"))
MINAFTER <- as.integer(Sys.getenv("CITIUS_SCALE_MINAFTER", "1"))
cal <- deployed_calibration(OUT)
rr <- as.data.table(cal$race)

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]

out <- rbindlist(lapply(EVENTS, function(EV) {
  ORI <- as.data.table(citius_events())[event_id == EV]$orientation[1]
  d <- ch[event_id == EV & is.finite(mark) & !is.na(race_key) & !is.na(date),
          .(athlete_id, race_key, date, mark)]
  if (!nrow(d)) return(NULL)
  d[, perf := ORI * log(mark)]
  setkey(d, athlete_id, date)
  races <- rr[event_id == EV & n_in_race >= MINF, .(race_key, c_r, n_in_race)]
  if (!nrow(races)) return(NULL)
  # Cap the work: a random sample of races is enough to fit one slope.
  set.seed(11L)
  if (nrow(races) > 1500L) races <- races[sample(.N, 1500L)]
  fld <- merge(d, races, by = "race_key")
  agg <- rbindlist(lapply(split(fld, fld$race_key), function(g) {
    rd <- g$date[1]
    lat <- d[athlete_id %chin% g$athlete_id & date > rd, .(m = mean(perf)), by = athlete_id]
    if (nrow(lat) < MINAFTER) return(NULL)
    gg <- merge(g[, .(athlete_id, perf)], lat, by = "athlete_id")
    if (!nrow(gg)) return(NULL)
    data.table(race_key = g$race_key[1], c_r = g$c_r[1], n_in_race = g$n_in_race[1],
               n_followed = nrow(gg),
               observed = mean(gg$perf) - mean(gg$m))  # + = race was FAST
  }))
  if (is.null(agg) || !nrow(agg)) return(NULL)
  fit <- stats::lm(observed ~ c_r, data = agg)
  data.table(event = EV, races = nrow(agg),
             median_field = median(agg$n_in_race),
             slope = round(unname(coef(fit)[2]), 3),
             intercept = round(unname(coef(fit)[1]), 5),
             r2 = round(summary(fit)$r.squared, 3),
             sd_c_r = round(sd(agg$c_r), 4), sd_obs = round(sd(agg$observed), 4))
}))

cat("HOW MUCH OF THE FITTED RACE EFFECT IS REAL?\n")
cat("slope = fraction of c_r that shows up in the field's later marks\n")
cat("(1.0 = c_r is correctly sized; 0.5 = it is twice too big)\n\n")
print(out)
if (nrow(out)) {
  w <- out[is.finite(slope)]
  cat(sprintf("\npooled median slope: %.3f  => suggested scale on c_r\n",
              median(w$slope)))
  cat(sprintf("range across events: %.3f to %.3f\n", min(w$slope), max(w$slope)))
}
