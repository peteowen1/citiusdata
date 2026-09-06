# WHERE is the marks level bias? Mean signed error by tier x round x family,
# on a backtest arm's own as-of predictions.
#
# WHY. The family-pool debias corrects a bias fitted with all tiers and rounds
# pooled (+1.3 to +2.0% optimism in sprint/hurdles/jump/throw, and the same
# numbers on a 2023-2025 refit). But PIT coverage on T1 FINALS (2023 and 2024,
# monthly as-of refit) shows those finals are centred WITHOUT the debias and
# pessimistic WITH it. Both can be true only if the bias lives in the other
# contexts -- heats, lower tiers -- and a family constant applied to a final
# corrects a bias the final does not have. This script says which it is, on
# the backtest's own predictions (proper per-meet as-of), so there is no
# staleness confound.
#
# em = 100 * (predicted perf - actual perf): positive = we predicted a better
# performance than happened (optimism), on the oriented log scale.
#
# Usage:  Rscript citiusdata/scripts/diagnostics/bias_by_context.R
# Env:    CITIUS_BIAS_ARM (default backtest_ctrl_tierfix.rds)
#         CITIUS_BIAS_FROM (default 2023-01-01), CITIUS_BIAS_TO (2026-09-01)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT  <- here::here("citiusdata", "data")
ARM  <- Sys.getenv("CITIUS_BIAS_ARM", "backtest_ctrl_tierfix.rds")
FROM <- as.Date(Sys.getenv("CITIUS_BIAS_FROM", "2023-01-01"))
TO   <- as.Date(Sys.getenv("CITIUS_BIAS_TO", "2026-09-01"))
say <- function(...) cat(sprintf(...), "\n", sep = "")

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
keep <- intersect(c("race_key", "athlete_id", "mark", "event_id", "date", "round", "tier", "meet_tier", "place"), names(ch))
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0, ..keep]
setnames(act, "race_key", "race_id")
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
d[, `:=`(act_perf = orientation * log(mark), a_perf = orientation * log(a_mark))]
d <- d[is.finite(act_perf) & is.finite(a_perf) & date >= FROM & date < TO]
d[, em := 100 * (a_perf - act_perf)]
d[, ae := abs(em)]
d[, round_class := .round_class(round)]
tier_col <- if ("meet_tier" %in% names(d)) "meet_tier" else "tier"
d[, tier_lab := get(tier_col)]
say("arm %s | %s..%s | %s rows, %s races | tier column %s",
    ARM, format(FROM), format(TO), format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","), tier_col)
stopifnot(nrow(d) > 1000)

# Race-level means first (a 40-entrant marathon must not outvote a 8-lane final),
# then the mean of race means, with the count of races so thin cells are visible.
rl <- d[, .(em = mean(em), ae = mean(ae), n = .N), by = .(race_id, tier_lab, round_class, family)]
ctx <- rl[, .(races = .N, entrants = sum(n), bias_pct = round(mean(em), 3), mae_pct = round(mean(ae), 3),
              se = round(sd(em) / sqrt(.N), 3)), by = .(tier_lab, round_class, family)]
setorder(ctx, tier_lab, round_class, -races)

cat("\n=== signed error by tier x round (all families), race-weighted; + = optimistic ===\n")
print(rl[, .(races = .N, bias_pct = round(mean(em), 3), mae_pct = round(mean(ae), 3),
             se = round(sd(em) / sqrt(.N), 3)), by = .(tier_lab, round_class)][order(tier_lab, round_class)])

cat("\n=== T1 finals by family (the product's medal cards) ===\n")
print(ctx[round_class == "final" & grepl("T1", tier_lab)][order(-races)])

cat("\n=== T1 heats by family ===\n")
print(ctx[round_class == "heat" & grepl("T1", tier_lab)][order(-races)])

cat("\n=== by family, all contexts pooled (what fit_family_pool_offsets.R sees) ===\n")
print(rl[, .(races = .N, bias_pct = round(mean(em), 3)), by = family][order(-races)])

fwrite(ctx, file.path(OUT, "bias_by_context.csv"))
say("wrote bias_by_context.csv")
