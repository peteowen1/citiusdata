# v2 of the GBM test. The v1 result (-11% RMSE, -15% MAE) was NOT evidence that
# a GBM helps -- it was evidence that my BASELINE was too weak.
#
# v1 used an expanding within-athlete-event mean as the stand-in for the
# structural model. That stand-in has no aging curve, no recency decay, no
# per-family sigma and no round/tier offsets. And the GBM's top features were
# `fam`, `n_prior`, `age`, `days_since` -- which are precisely those four
# missing pieces. The GBM was rediscovering what estimate_ability() already
# does, and scoring it as a gain.
#
# v2 fixes both halves:
#   BASELINE now includes recency decay at the fitted half-life, an aging
#   adjustment and round/tier context offsets -- the things the real model does.
#   FEATURES are restricted to what the structural model genuinely does NOT use.
#   `wind` is kept deliberately as a NEGATIVE CONTROL: it IS used by the model,
#   so if the GBM still leans on it, the baseline is still too weak and the
#   whole result should be discarded again.

suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
library(data.table); library(arrow); library(xgboost)
D <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep="")

x <- as.data.table(read_parquet(file.path(D, "athletics_corpus.parquet"),
      col_select = c("athlete_id","event_id","date","perf","wind","indoor",
                     "venue_city","round","tier","race_key","age","sex")))
x <- x[!is.na(perf) & !is.na(event_id) & !is.na(date) & !is.na(athlete_id)]
setorder(x, athlete_id, event_id, date)
reg <- as.data.table(citius_events())[, .(event_id, family)]
x <- merge(x, reg, by = "event_id", all.x = TRUE)
setorder(x, athlete_id, event_id, date)

cal <- readRDS(file.path(D, "calibration_corpus_athfoul.rds"))
x[, rc := .round_class(round)][, tc := .tier_class(tier)]

# --- context offsets, as the model applies them ------------------------------
# cal$round / cal$tier are DATA.TABLES (round_class|offset|sd|n|precision),
# not named vectors -- indexing them by name silently yields a list.
ro <- as.data.table(cal$round); to <- as.data.table(cal$tier)
x[, r_off := ro$offset[match(rc, ro$round_class)]]
x[, t_off := to$offset[match(tc, to$tier_class)]]
x[!is.finite(r_off), r_off := 0][!is.finite(t_off), t_off := 0]
# wind, as the model applies it
wb <- as.data.table(cal$wind)
x[, wbeta := wb$beta[match(event_id, wb$event_id)]]
x[!is.finite(wbeta), wbeta := 0]
x[, w := fifelse(is.finite(wind), wind, 0)]
x[, perf_adj := perf - r_off - t_off - wbeta * w]

# --- baseline: recency-weighted prior mean + aging ---------------------------
HL <- 365
x[, t_num := as.numeric(date)]
x[, idx := seq_len(.N), by = .(athlete_id, event_id)]
x[, gid := .GRP, by = .(athlete_id, event_id)]
# Computed into a side table and joined back. Returning `c(.SD, ...)` from `j`
# fails outright, and .SD-with-a-closure-per-group is the pattern CLAUDE.md
# records as costing 74% of estimate_ability()'s runtime.
pm_tbl <- x[, {
  n <- .N; pm <- rep(NA_real_, n)
  if (n > 3L) for (i in 4:n) {
    wi <- 0.5 ^ ((t_num[i] - t_num[1:(i - 1)]) / HL)
    pm[i] <- sum(wi * perf_adj[1:(i - 1)]) / sum(wi)
  }
  list(idx = idx, prior_mean = pm)
}, by = gid]
x <- merge(x, pm_tbl, by = c("gid", "idx"), all.x = TRUE)
x[, n_prior := idx - 1L]
x <- x[!is.na(prior_mean)]
say("rows with a recency-weighted prior: ", format(nrow(x), big.mark=","))

ag <- tryCatch(readRDS(file.path(D, "aging.rds")), error = function(e) NULL)
x[, resid := perf_adj - prior_mean]
say("baseline residual sd: ", round(100*sd(x$resid), 3), "% of a mark",
    "   (v1's weaker baseline: 3.935%)")

ALT <- data.table(
  venue_city = c("Mexico City","Bogota","Bogotá","Addis Ababa","Nairobi","Johannesburg",
                 "Pretoria","Denver","Albuquerque","Provo","El Paso","Flagstaff",
                 "Sestriere","Font Romeu","Ifrane","Toluca","Quito","La Paz",
                 "Cochabamba","Potchefstroom","Eldoret","Iten","Asmara","Sucre"),
  elev_m = c(2240,2640,2640,2355,1795,1753,1339,1609,1619,1387,1140,2106,
             2035,1850,1665,2660,2850,3640,2558,1350,2100,2400,2325,2810))
x[, elev_m := ALT$elev_m[match(venue_city, ALT$venue_city)]]
x[is.na(elev_m), elev_m := 0]
x[, month := as.integer(format(date, "%m"))]
x[, field_size := .N, by = race_key]
x[, days_since := as.numeric(date - shift(date)), by = .(athlete_id, event_id)]
x[is.na(days_since), days_since := 365]

# Features the structural model does NOT use, plus wind as a negative control.
FEATS <- c("month", "field_size", "elev_m", "days_since", "wind")
CUT <- as.Date("2024-01-01")
tr <- x[date < CUT]; te <- x[date >= CUT]
say("train ", format(nrow(tr), big.mark=","), " | test ", format(nrow(te), big.mark=","))

dtr <- xgb.DMatrix(as.matrix(tr[, ..FEATS]), label = tr$resid)
dte <- xgb.DMatrix(as.matrix(te[, ..FEATS]), label = te$resid)
tr[, fold := cut(date, breaks = 5, labels = FALSE)]
p <- list(objective="reg:squarederror", eta=0.05, max_depth=5,
          subsample=0.8, colsample_bytree=0.8, nthread=4)
cv <- xgb.cv(p, dtr, nrounds=300, folds=split(seq_len(nrow(tr)), tr$fold),
             early_stopping_rounds=20, verbose=0)
el <- as.data.table(cv$evaluation_log)
rc_ <- grep("test.*rmse.*mean", names(el), value=TRUE)[1]
best <- if (!is.na(rc_)) which.min(el[[rc_]]) else nrow(el)
m <- xgb.train(p, dtr, nrounds=best, verbose=0)
pred <- predict(m, dte)

r0 <- sqrt(mean(te$resid^2)); r1 <- sqrt(mean((te$resid-pred)^2))
m0 <- mean(abs(te$resid));    m1 <- mean(abs(te$resid-pred))
cat("\n================ RESULT (v2, honest baseline) ================\n")
cat(sprintf("baseline (structural-like, predict 0): RMSE %.5f  MAE %.5f\n", r0, m0))
cat(sprintf("+ xgboost on unused features         : RMSE %.5f  MAE %.5f\n", r1, m1))
cat(sprintf("\nRMSE change: %+.3f%%   MAE change: %+.3f%%\n",
            100*(r1-r0)/r0, 100*(m1-m0)/m0))
s <- sqrt(max(r0^2-r1^2,0))
cat(sprintf("spread explained: %.3f%% of a mark -> worth %.2f%% of the marks metric\n",
            100*s, 100*s^2/(2*r0^2)))
cat("\n--- feature importance (wind is the NEGATIVE CONTROL) ---\n")
imp <- xgb.importance(model=m); print(imp)
wg <- imp[Feature=="wind", Gain]
cat(sprintf("\nwind gain share: %.1f%%\n", 100*ifelse(length(wg), wg, 0)))
cat("If wind is still near the top, the baseline is STILL too weak and this\n")
cat("result should be discarded exactly as v1's was.\n")
saveRDS(list(rmse_base=r0, rmse_xgb=r1, imp=imp), file.path(D,"xgb_residual_v2.rds"))
