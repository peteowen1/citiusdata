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
DIR <- "C:/dev/citiusverse/citiusdata/data/gamm4_events"
OUT <- "C:/dev/citiusverse/citiusdata/data/conditions_params"
dir.create(OUT, showWarnings = FALSE)

ids <- sub("\\.rds$", "", grep("^AT-.*\\.rds$", list.files(DIR), value = TRUE))
cat(sprintf("events with a fit: %d\n", length(ids)))
all_params <- list()

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

  vc <- fread(file.path(DIR, paste0(EV, "_sd_table.csv")))
  sd_of <- function(tm) { v <- vc$sd_perf[vc$term == tm]; if (length(v)) v else NA_real_ }
  params <- list(
    event_id = EV, ref_mark = vc$ref_mark[1],
    fitted_on = list(n_rows = nrow(cs), n_athletes = uniqueN(cs$athlete_id), n_races = uniqueN(cs$race_key)),
    wind = if (has_wind) list(grid = wind_grid, curve = wind_curve) else NULL,
    altitude = list(grid_m = alt_grid_m, curve = alt_curve, max_m = amax),
    indoor_coef = indoor_coef, has_indoor = has_indoor,
    var_race = sd_of("race shock")^2, var_resid = sd_of("residual")^2,
    var_athlete = sd_of("athlete effect")^2
  )
  write_json(params, file.path(OUT, paste0(EV, ".json")), auto_unbox = TRUE, digits = 8, null = "null")
  all_params[[EV]] <- params
}
# one combined file for the site: a single fetch covers every event
write_json(all_params, file.path(OUT, "_all.json"), auto_unbox = TRUE, digits = 8, null = "null")
cat(sprintf("wrote %d param files + _all.json (%.0f KB) to %s\n", length(ids),
            file.size(file.path(OUT, "_all.json"))/1024, OUT))
