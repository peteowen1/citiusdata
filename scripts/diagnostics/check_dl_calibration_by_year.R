# Year-by-year mark calibration check for Diamond League finals: is the
# CURRENT DEPLOYED model's predicted mark systematically biased relative to
# the actual result, and does that bias drift across years or concentrate in
# particular event families?
#
# Uses backtest_ctrl_now.rds (calibration_corpus_csigma_coast.rds -- the
# calibration currently in DEPLOYED) rather than a fresh run: it already
# carries 6,461 Diamond League predictions across 2016-2026, every year
# represented, which is more than the population ticket 03 sized its DL
# findings on (573 races). DL meets identified via the catalogue's `class`
# field, NOT `meet_tier` -- meet_tier has severe year-coverage gaps (checked
# directly 2026-08-29: several years show ZERO T1_elite rows across every
# discipline), but `class` is a name-matched, competition-level field with no
# such gap for a recognisably-branded series like Diamond League.
#
# Bias must be RAW, not centred. `marks MAE ctr` throughout this repo
# centres each race on its own mean specifically to isolate SPREAD from
# LOCATION -- using it here would remove exactly the level bias this script
# exists to detect.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

cat_tbl <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
dl_ids <- cat_tbl[class == "diamond_league"]$competition_id

b <- readRDS(file.path(D, "backtest_ctrl_now.rds"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm calibration: %s | stamp era: %s", b$meta$calibration, b$meta$run_at)

pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), median_mark)]
ch <- setDT(readRDS(file.path(D, "championship_results.rds")))
ch[, `:=`(competition_id = as.character(competition_id), athlete_id = as.character(athlete_id))]
act <- unique(ch[!is.na(race_key) & !is.na(mark),
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)])

d <- merge(pred, act, by = c("race_id", "athlete_id"))
d <- d[competition_id %in% dl_ids]
ev <- as.data.table(citius_events())[, .(event_id, discipline, sex, orientation, family)]
d <- merge(d, ev, by = "event_id")
d[, year := data.table::year(date)]

# Relative bias, sign-adjusted so POSITIVE always means "actual beat the
# prediction" (ran faster / threw farther / jumped higher than forecast),
# regardless of whether the event's raw units go up or down when you improve.
d[, bias_pct := orientation * (actual - median_mark) / median_mark * 100]

say("\n%s DL predictions, %s races, %s to %s",
    format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","),
    min(d$year), max(d$year))

by_year <- d[, .(n = .N, races = uniqueN(race_id),
                 mean_bias_pct = mean(bias_pct), se = sd(bias_pct) / sqrt(.N),
                 mae_pct = mean(abs(bias_pct))), by = year]
setorder(by_year, year)
by_year[, `:=`(ci_lo = mean_bias_pct - 1.96 * se, ci_hi = mean_bias_pct + 1.96 * se)]
cat("\n=== bias by YEAR (all DL finals; +ve = actual beat predicted) ===\n")
print(by_year[, .(year, n, races, mean_bias_pct = round(mean_bias_pct, 3),
                  ci_lo = round(ci_lo, 3), ci_hi = round(ci_hi, 3), mae_pct = round(mae_pct, 3))])

# Trend test: regress bias on year. A real drift should show up as a
# significant slope; noise should not, given 11 years and thousands of rows.
fit <- lm(bias_pct ~ year, data = d)
s <- summary(fit)
cat(sprintf("\nlinear trend: %.4f%% per year (p = %.4g), R2 = %.4f\n",
            coef(fit)["year"], s$coefficients["year", "Pr(>|t|)"], s$r.squared))

cat("\n=== bias by FAMILY x YEAR ===\n")
by_fam <- d[, .(n = .N, mean_bias_pct = mean(bias_pct)), by = .(family, year)]
print(dcast(by_fam, family ~ year, value.var = "mean_bias_pct"))
cat("\n(n per family x year, so a cell above can be read against its power)\n")
print(dcast(by_fam, family ~ year, value.var = "n", fill = 0))

cat("\n=== bias by FAMILY, pooled (with n and a t-test vs zero) ===\n")
fam_test <- d[, {
  tt <- t.test(bias_pct)
  .(n = .N, mean_bias_pct = mean(bias_pct), p = tt$p.value)
}, by = family]
print(fam_test[order(-abs(mean_bias_pct))])

cat("\n=== bias by EVENT, pooled (min 100 predictions, worst 15 by |bias|) ===\n")
ev_test <- d[, .(n = .N, mean_bias_pct = mean(bias_pct)), by = .(discipline, sex)]
ev_test <- ev_test[n >= 100]
print(ev_test[order(-abs(mean_bias_pct))][1:min(15, .N)])

saveRDS(d, file.path(D, "dl_calibration_by_year.rds"))
say("\nwrote dl_calibration_by_year.rds (%s rows)", format(nrow(d), big.mark = ","))
