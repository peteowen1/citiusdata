# The missing half of race adjustment: what shock to EXPECT in the target race.
#
# THE ASYMMETRY THIS FIXES. estimate_ability(adjust_race = TRUE) subtracts the
# fitted shock from every historical mark, making ability conditions-neutral.
# Nothing then adds the shock back for the race being FORECAST. So a
# championship final -- which runs fast -- is predicted from neutral ability and
# comes out systematically slow. That is what made raw marks MAE look worse
# when race adjustment was switched on, while the DIRECTIONAL bias it fixes was
# real and large (optimism for athletes coming off a shock: +0.638 -> +0.353,
# p = 8.09e-98).
#
# WHAT IS FORECASTABLE. Only ~9% of a shock can be predicted before the race
# (R2 0.093 from tier, round, family and meet strength). The rest is a race-day
# lottery that belongs in the SPREAD, not the centre. So this fits the small
# systematic part and leaves the remainder to condition_sd.
#
# The systematic part is real and ordered in both dimensions the model knows in
# advance -- on the deployed (inflated) scale, T1 final +2.1%, T1 heat +1.4%,
# T3 final +0.3%, T3 heat -0.1%.
#
# FITTED AGAINST THE CORRECTED EFFECTS, NOT THE DEPLOYED ONES. calibration$race
# is inflated ~1.7x by decompose_races() running 400 sweeps where the signal is
# extracted in 2, so fitting the add-back on the deployed table would bake that
# same inflation into the correction. Defaults to the EB-shrunk calibration.
#
# WHY NOT USE calibration$championship. It exists and is the wrong shape: a
# per-FAMILY constant (road -2.18%, throw +1.46%, sprint +0.94%), so it answers
# "is this a championship" as a binary per discipline and would apply the same
# correction to a T1 final and a T3 heat in the same event.
#
# Usage:  Rscript citiusdata/scripts/fit_expected_race_shock.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT  <- here::here("citiusdata", "data")
SRC  <- Sys.getenv("CITIUS_SHOCK_SRC", "calibration_race_eb_perevent.rds")
MINF <- as.integer(Sys.getenv("CITIUS_SHOCK_MINFIELD", "5"))
MINN <- as.integer(Sys.getenv("CITIUS_SHOCK_MINCELL", "30"))
DEST <- Sys.getenv("CITIUS_SHOCK_OUT", "expected_race_shock.csv")
say  <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

cal <- readRDS(file.path(OUT, SRC))
say(sprintf("fitting on %s (sd(c_r) = %.5f)", SRC, sd(as.data.table(cal$race)$c_r, na.rm = TRUE)))
r <- as.data.table(cal$race)[n_in_race >= MINF & is.finite(c_r)]

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
# Take ONLY competition_id from the results. calibration$race already carries
# its own `round`, and joining a second one produced round.x/round.y -- leaving
# a bare `round` to resolve to base::round(), which fails with "cannot coerce
# type 'special'". A merge that silently renames a column you then reference by
# its old name is the quiet version of this bug.
rk <- unique(ch[!is.na(race_key), .(race_key, competition_id)], by = "race_key")
rk[, competition_id := as.character(competition_id)]
ctl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
ctl[, competition_id := as.character(competition_id)]
d <- merge(r, rk, by = "race_key")
d <- merge(d, ctl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
stopifnot("calibration$race lost its round column" = "round" %in% names(d))
d[, round_class := .round_class(d$round)]
d <- merge(d, as.data.table(citius_events())[, .(event_id, family)], by = "event_id", all.x = TRUE)
d <- d[!is.na(meet_tier) & !is.na(family)]
say(sprintf("%s races with tier, round and family", format(nrow(d), big.mark = ",")))

# Cells: tier x round x family. Thin cells fall back to tier x round, then to
# tier -- the same least-pooled-with-data-behind-it chain the context offsets
# use. A cell with three races would otherwise ship a number that is mostly noise.
cell <- d[, .(n = .N, shock = mean(c_r)), by = .(meet_tier, round_class, family)]
tr   <- d[, .(n_tr = .N, shock_tr = mean(c_r)), by = .(meet_tier, round_class)]
ti   <- d[, .(n_ti = .N, shock_ti = mean(c_r)), by = .(meet_tier)]
cell <- merge(cell, tr, by = c("meet_tier", "round_class"), all.x = TRUE)
cell <- merge(cell, ti, by = "meet_tier", all.x = TRUE)
cell[, expected_shock := fifelse(n >= MINN, shock,
                          fifelse(n_tr >= MINN, shock_tr, shock_ti))]
cell[, source := fifelse(n >= MINN, "tier_round_family",
                  fifelse(n_tr >= MINN, "tier_round", "tier"))]

say(sprintf("%d cells, %d from the full tier x round x family key",
            nrow(cell), sum(cell$source == "tier_round_family")))
cat("\nEXPECTED SHOCK to add back, by tier and round (pooled over families):\n")
cat("(positive = the target race is expected to be FAST, so predictions must move with it)\n\n")
print(dcast(tr, meet_tier ~ round_class, value.var = "shock_tr"))
cat("\nas a percentage of a mark:\n")
tr2 <- copy(tr); tr2[, shock_tr := 100 * (exp(shock_tr) - 1)]
print(dcast(tr2, meet_tier ~ round_class, value.var = "shock_tr"))

fwrite(cell[, .(meet_tier, round_class, family, n, expected_shock, source)],
       file.path(OUT, DEST))
say(sprintf("wrote %s", DEST))
cat("\nTO USE: add expected_shock to ability at prediction time for the target\n")
cat("race's tier and round -- the counterpart of adjust_race stripping it from\n")
cat("history. Without both halves the correction is applied asymmetrically.\n")
