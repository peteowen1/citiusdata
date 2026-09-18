# Export each gamm4 conditions fit to a small JSON parameter file that a live
# adjuster (R or JS, no model runtime) can apply:
#   wind curve   : s(wind)  evaluated on a -6..8 m/s grid, step 0.1  (perf-log units)
#   alt curve    : s(alt_t) evaluated on a log1p(alt_m) grid          (perf-log units)
#   indoor       : one coefficient
#   var_race     : race-shock variance (sigma_r^2), from the fit
#   var_resid    : within-race residual variance (sigma_e^2), from the fit --
#                  NOTE the live adjuster should replace this with the
#                  forecast-error variance once forecast_marks exists.
# Curves are relative to the reference point (wind 0, altitude 0, outdoor).
#
# Output: citiusdata/data/conditions_params/<EVENT>.json  (one per event)
suppressMessages({ library(data.table); library(jsonlite); library(mgcv) })
# FIT_DIR: gamm4_events (2026-09-18, athlete + race random effects) or
# gamm4_events_venue (2026-09-19, + venue). OUT_DIR likewise.
DIR <- file.path("C:/dev/citiusverse/citiusdata/data", Sys.getenv("FIT_DIR", "gamm4_events_venue"))
OUT <- file.path("C:/dev/citiusverse/citiusdata/data", Sys.getenv("OUT_DIR", "conditions_params"))
dir.create(OUT, showWarnings = FALSE)

ids <- sub("\\.rds$", "", grep("^AT-.*\\.rds$", list.files(DIR), value = TRUE))
cat(sprintf("events with a fit: %d\n", length(ids)))
all_params <- list()
# Per-venue offsets are computed on the FULL corpus by build_adjusted_marks.R
# (closed form, using var_venue from these fits) and written beside the params.
# Order is therefore: export (curves + variances) -> build (offsets) -> export
# again to fold the offsets into the JSON the site reads. Absent = no lookup.
vo_f <- file.path(OUT, "venue_offsets.parquet")
VENUES <- if (file.exists(vo_f)) { v <- setDT(arrow::read_parquet(vo_f)); split(v, by = "event_id", keep.by = FALSE) } else list()
cat(sprintf("venue offsets on disk: %s (event, venue) pairs\n", format(sum(vapply(VENUES, nrow, 1L)), big.mark = ",")))

for (EV in ids) {
  r <- readRDS(file.path(DIR, paste0(EV, ".rds")))
  g <- r$model$gam; cs <- r$sample
  has_wind <- "wind" %in% names(cs); has_indoor <- isTRUE(r$has_indoor)
  ind_lv <- if (has_indoor) levels(cs$indoor_f) else NULL
  mk_nd <- function(wind, alt_t, indoor = "FALSE") {
    n <- max(length(wind), length(alt_t))
    d <- if (has_wind) data.frame(wind = rep(wind, length.out = n), alt_t = rep(alt_t, length.out = n))
         else data.frame(alt_t = rep(alt_t, length.out = n))
    if (has_indoor) d$indoor_f <- factor(indoor, levels = ind_lv)
    d
  }
  ref <- as.numeric(predict(g, newdata = mk_nd(0, 0)))

  wind_grid <- if (has_wind) seq(-6, 8, by = 0.1) else NULL
  wind_curve <- if (has_wind) as.numeric(predict(g, newdata = mk_nd(wind_grid, 0))) - ref else NULL
  amax <- max(cs$alt_m, na.rm = TRUE)
  alt_grid_m <- unique(round(expm1(seq(0, log1p(amax), length.out = 80))))
  alt_curve <- as.numeric(predict(g, newdata = mk_nd(0, log1p(alt_grid_m))) - ref)
  indoor_coef <- if (has_indoor) as.numeric(predict(g, newdata = mk_nd(0, 0, "TRUE")) - ref) else 0

  # variance components: from the fit object (venue fits) or the older sd table
  if (!is.null(r$sd)) {
    sdv <- r$sd
  } else {
    vc <- fread(file.path(DIR, paste0(EV, "_sd_table.csv")))
    sdv <- c(athlete = vc$sd_perf[vc$term == "athlete effect"], race = vc$sd_perf[vc$term == "race shock"],
             venue = NA_real_, resid = vc$sd_perf[vc$term == "residual"])
  }
  ref_mark <- exp(abs(median(cs$perf)))   # perf = orientation * log(mark); sign carries the orientation
  params <- list(
    event_id = EV, ref_mark = round(ref_mark, 2),
    fitted_on = list(n_rows = nrow(cs), n_athletes = uniqueN(cs$athlete_id), n_races = uniqueN(cs$race_key),
                     n_venues = if ("venue_city" %in% names(cs)) uniqueN(cs$venue_city) else NA),
    wind = if (has_wind) list(grid = wind_grid, curve = wind_curve) else NULL,
    altitude = list(grid_m = alt_grid_m, curve = alt_curve, max_m = amax),
    indoor_coef = indoor_coef, has_indoor = has_indoor,
    var_race = unname(sdv["race"])^2, var_resid = unname(sdv["resid"])^2,
    var_athlete = unname(sdv["athlete"])^2,
    var_venue = if (is.finite(sdv["venue"])) unname(sdv["venue"])^2 else NULL,
    venues = if (!is.null(VENUES[[EV]])) as.list(setNames(round(VENUES[[EV]]$venue_off, 6), VENUES[[EV]]$venue_city)) else NULL
  )
  write_json(params, file.path(OUT, paste0(EV, ".json")), auto_unbox = TRUE, digits = 8, null = "null")
  all_params[[EV]] <- params
}
# one combined file for the site: a single fetch covers every event
write_json(all_params, file.path(OUT, "_all.json"), auto_unbox = TRUE, digits = 8, null = "null")
cat(sprintf("wrote %d param files + _all.json (%.0f KB) to %s\n", length(ids),
            file.size(file.path(OUT, "_all.json"))/1024, OUT))
