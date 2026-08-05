# Can a gradient-boosted model find signal the structural model misses?
#
# NOT a proposal to replace the structural model. That model encodes real
# physics -- log scale, orientation, two-way decomposition, EB shrinkage -- and
# works for an athlete with three results, which no GBM will. This asks a
# narrower and more useful question: after the structural prediction, is there
# anything LEFT that a flexible learner can find? If yes, the answer is to
# encode it structurally, not to ship the GBM.
#
# Two design points that decide whether the answer means anything:
#
#   TIME-RESPECTING SPLIT, NOT xgb.cv's RANDOM FOLDS. Random k-fold puts a
#   result from June in the training set and the same athlete's May result in
#   the test set. That leaks form and would report a fiction. Everything here
#   trains strictly before a date and tests strictly after. xgb.cv is used only
#   for choosing the number of rounds WITHIN the training window, on folds that
#   are themselves time-ordered.
#
#   THE BASELINE IS "PREDICT ZERO". The target is already a residual, so a model
#   with no signal should score exactly the residual's own sd. Any improvement
#   over that is the answer, and it must be compared against the quadratic rule:
#   against a sigma of ~2.6% of a mark, an explained spread of 0.25% is worth
#   0.5% of the marks metric and 1% is worth 7.6%.

suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
library(data.table); library(arrow)
ok <- requireNamespace("xgboost", quietly = TRUE)
if (!ok) { cat("xgboost not installed -- cannot run this test\n"); quit(save = "no") }
library(xgboost)

D <- "C:/dev/citiusverse/citiusdata/data"
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

cols <- c("athlete_id", "event_id", "date", "perf", "wind", "indoor",
          "venue_city", "venue_stadium", "round", "tier", "race_key",
          "competition_id", "age", "sex")
say("reading corpus ...")
x <- as.data.table(read_parquet(file.path(D, "athletics_corpus.parquet"),
                                col_select = all_of(cols)))
x <- x[!is.na(perf) & !is.na(event_id) & !is.na(date) & !is.na(athlete_id)]
setorder(x, athlete_id, event_id, date)

reg <- as.data.table(citius_events())[, .(event_id, family)]
x <- merge(x, reg, by = "event_id", all.x = TRUE)

CUT <- as.Date("2024-01-01")
say("split at ", as.character(CUT),
    " | before: ", format(x[date < CUT, .N], big.mark = ","),
    " | after: ", format(x[date >= CUT, .N], big.mark = ","))

# --- the structural prediction, built ONLY from data before each row ---------
# Expanding within-athlete-event mean of prior performances: the simplest honest
# stand-in for what estimate_ability() produces, and strictly causal.
x[, prior_mean := shift(cumsum(perf)) / shift(seq_len(.N)), by = .(athlete_id, event_id)]
x[, n_prior := shift(seq_len(.N)) ]
x <- x[!is.na(prior_mean) & n_prior >= 3L]
x[, resid := perf - prior_mean]
say("rows with >=3 prior results in the same event: ", format(nrow(x), big.mark = ","))
say("residual sd: ", round(100 * sd(x$resid), 3), "% of a mark")

# --- features the structural model does NOT currently use --------------------
ALT <- data.table(
  venue_city = c("Mexico City","Bogota","Bogotá","Addis Ababa","Nairobi","Johannesburg",
                 "Pretoria","Denver","Albuquerque","Provo","El Paso","Flagstaff",
                 "Sestriere","Font Romeu","Ifrane","Toluca","Quito","La Paz",
                 "Cochabamba","Potchefstroom","Eldoret","Iten","Asmara","Sucre"),
  elev_m = c(2240,2640,2640,2355,1795,1753,1339,1609,1619,1387,1140,2106,
             2035,1850,1665,2660,2850,3640,2558,1350,2100,2400,2325,2810))
x[, elev_m := ALT$elev_m[match(venue_city, ALT$venue_city)]]
x[is.na(elev_m), elev_m := 0]

x[, days_since := as.numeric(date - shift(date)), by = .(athlete_id, event_id)]
x[is.na(days_since), days_since := 365]
x[, month := as.integer(format(date, "%m"))]
x[, field_size := .N, by = race_key]
x[, round_cls := as.integer(factor(.round_class(round)))]
x[, tier_cls  := as.integer(factor(.tier_class(tier)))]
x[, fam       := as.integer(factor(family))]
x[, sex_i     := as.integer(factor(sex))]
x[, indoor_i  := as.integer(indoor %in% TRUE)]
x[is.na(wind), wind := 0]
x[is.na(age), age := median(x$age, na.rm = TRUE)]

FEATS <- c("wind", "indoor_i", "elev_m", "days_since", "month", "field_size",
           "round_cls", "tier_cls", "fam", "sex_i", "age", "n_prior")

tr <- x[date <  CUT]
te <- x[date >= CUT]
say("train ", format(nrow(tr), big.mark = ","), " | test ", format(nrow(te), big.mark = ","))

dtr <- xgb.DMatrix(as.matrix(tr[, ..FEATS]), label = tr$resid)
dte <- xgb.DMatrix(as.matrix(te[, ..FEATS]), label = te$resid)

# xgb.cv only to pick nrounds, on TIME-ORDERED folds within the training window.
say("xgb.cv for nrounds (time-ordered folds) ...")
tr[, fold := cut(date, breaks = 5, labels = FALSE)]
folds <- split(seq_len(nrow(tr)), tr$fold)
p <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 5,
          subsample = 0.8, colsample_bytree = 0.8, nthread = 4)
cv <- xgb.cv(p, dtr, nrounds = 400, folds = folds, early_stopping_rounds = 20,
             verbose = 0)
# `best_iteration` is empty in this xgboost build, so take it from the log.
el <- as.data.table(cv$evaluation_log)
rmse_col <- grep("test.*rmse.*mean", names(el), value = TRUE)[1]
best <- if (length(rmse_col) && !is.na(rmse_col)) which.min(el[[rmse_col]]) else nrow(el)
if (!length(best) || is.na(best) || best < 1L) best <- nrow(el)
say("best nrounds: ", best, " | cv rmse: ",
    if (!is.na(rmse_col)) signif(el[[rmse_col]][best], 5) else "n/a")

m <- xgb.train(p, dtr, nrounds = best, verbose = 0)
pred <- predict(m, dte)

rmse <- function(a, b) sqrt(mean((a - b)^2))
mae  <- function(a, b) mean(abs(a - b))
r0 <- rmse(te$resid, 0);          m0 <- mae(te$resid, 0)
r1 <- rmse(te$resid, pred);       m1 <- mae(te$resid, pred)

cat("\n================ RESULT ================\n")
cat(sprintf("baseline (structural alone, predict 0): RMSE %.5f  MAE %.5f\n", r0, m0))
cat(sprintf("structural + xgboost residual model  : RMSE %.5f  MAE %.5f\n", r1, m1))
cat(sprintf("\nRMSE change: %+.3f%%   MAE change: %+.3f%%\n",
            100 * (r1 - r0) / r0, 100 * (m1 - m0) / m0))
s_expl <- sqrt(max(r0^2 - r1^2, 0))
cat(sprintf("\nspread explained by the GBM: %.4f log units = %.3f%% of a mark\n",
            s_expl, 100 * s_expl))
sig <- r0
cat(sprintf("quadratic rule -> worth %.2f%% of the marks metric\n",
            100 * s_expl^2 / (2 * sig^2)))
cat("   (the whole model currently beats its baseline by 1.53%)\n")

cat("\n--- what it leant on ---\n")
imp <- xgb.importance(model = m)
print(head(imp, 12))

saveRDS(list(rmse_base = r0, rmse_xgb = r1, mae_base = m0, mae_xgb = m1,
             importance = imp, best_nrounds = best, cut = CUT),
        file.path(D, "xgb_residual_test.rds"))
say("saved xgb_residual_test.rds")
