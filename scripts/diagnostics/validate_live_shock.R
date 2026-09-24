# Does the closed-form live race shock reproduce lme4's race BLUP?
#   shock_live = mean(resid_i) * n*var_r / (n*var_r + var_e)
# where resid_i = cleaned perf minus the athlete's level (here: the fitted
# athlete BLUP, standing in for a pre-race forecast). Run on the gamm4 fitting
# sample so both quantities are computed from the same rows.
suppressMessages({ library(data.table); library(jsonlite); library(lme4); library(mgcv) })
EV <- commandArgs(TRUE); if (!length(EV)) EV <- "AT-100Metres-M"
DIR <- "C:/dev/citiusverse/citiusdata/data/gamm4_events"
r <- readRDS(file.path(DIR, paste0(EV, ".rds"))); g <- r$model$gam; m <- r$model$mer; cs <- copy(r$sample)
p <- fromJSON(file.path("C:/dev/citiusverse/citiusdata/data/conditions_params", paste0(EV, ".json")))
cat(sprintf("%s: %d rows, %d athletes, %d races\n", EV, nrow(cs), uniqueN(cs$athlete_id), uniqueN(cs$race_key)))

# --- live adjuster: interpolate the exported curves -------------------------
interp <- function(grid, curve, x) approx(grid, curve, xout = pmin(pmax(x, min(grid)), max(grid)), rule = 2)$y
cs[, wind_adj := if (!is.null(p$wind)) interp(p$wind$grid, p$wind$curve, wind) else 0]
cs[, venue_adj := interp(p$altitude$grid_m, p$altitude$curve, pmax(alt_m, 0))]
cs[, indoor_adj := if (p$has_indoor) fifelse(indoor_f == "TRUE", p$indoor_coef, 0) else 0]
cs[, cleaned := perf - wind_adj - venue_adj - indoor_adj]

# --- athlete level from the fit (stand-in for a forecast) --------------------
re <- ranef(m)
ath <- data.table(ath_f = rownames(re$ath_f), ath_bl = re$ath_f[[1]])
rac <- data.table(race_f = rownames(re$race_f), race_blup = re$race_f[[1]])
cs[, ath_f := as.character(ath_f)][, race_f := as.character(race_f)]
cs <- merge(cs, ath, by = "ath_f"); cs <- merge(cs, rac, by = "race_f")
icpt <- as.numeric(predict(g, newdata = cs[1:1, .(wind = 0, alt_t = 0, indoor_f = factor("FALSE", levels = levels(r$sample$indoor_f)))]))
# check the exported curves reproduce the gam's own term predictions
fx <- as.numeric(predict(g, newdata = cs)) - icpt
cat(sprintf("curve interpolation vs gam predict: max abs diff %.2e (perf units)\n", max(abs(fx - (cs$wind_adj + cs$venue_adj + cs$indoor_adj)))))

cs[, resid := cleaned - icpt - ath_bl]
k <- p$var_race; e <- p$var_resid
races <- cs[, .(n = .N, mean_resid = mean(resid), race_blup = race_blup[1]), by = race_f]
races[, shock_live := mean_resid * n * k / (n * k + e)]

cat(sprintf("\nlme4 race BLUP vs live closed form over %d races: cor %.4f, RMSE %.2e, sd(blup) %.2e\n",
            nrow(races), cor(races$race_blup, races$shock_live), sqrt(mean((races$race_blup - races$shock_live)^2)), sd(races$race_blup)))
cat("\nSample races (perf-log units; x100 = % of mark). shock_live should track race_blup:\n")
set.seed(1)
pick <- function(d, k) d[sample(.N, min(.N, k))]
print(table(cut(races$n, c(0, 1, 2, 4, 7, Inf))))
show <- rbind(pick(races[n >= 5], 3), pick(races[n %between% c(3, 4)], 3), pick(races[n <= 2], 2))
print(show[, .(race = substr(race_f, 1, 40), n, mean_resid = round(mean_resid, 4), shrink = round(n*k/(n*k+e), 2), shock_live = round(shock_live, 4), race_blup = round(race_blup, 4))])

# one full race, athlete by athlete
big <- races[n >= max(5, quantile(n, 0.99))][which.max(abs(race_blup))]$race_f
cat(sprintf("\nOne race in full: %s  (shock_live %.4f, lme4 %.4f)\n", big, races[race_f == big]$shock_live, races[race_f == big]$race_blup))
one <- cs[race_f == big, .(athlete_id, mark, wind, wind_adj = round(wind_adj, 4), venue_adj = round(venue_adj, 4),
                           cleaned = round(cleaned, 4), level = round(icpt + ath_bl, 4), resid = round(resid, 4))]
one[, adj_mark := round(exp(-(cleaned - races[race_f == big]$shock_live)), 2)]  # 100m: perf = -log(mark)
print(one[order(mark)])
