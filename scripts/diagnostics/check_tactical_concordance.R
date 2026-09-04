# Does the `tactical` registry flag actually explain concordance, or is
# middle-distance's poor concordance (0.510, worst of any family) something
# else wearing a tactical-race costume?
#
# CAMPAIGN CLAIM (2026-08-29, untested until now): "Consistent with
# citius/R/events.R's documented tactical flag... this is not a bias problem
# -- do not try to fix it with a location adjustment." That claim was never
# actually checked against concordance BY TACTICAL FLAG; it was inferred from
# events.R's own comment. This script checks it directly.
#
# METHOD. Read-only against calibration_sweep_data.rds (check_calibration_
# sweep.R's saved population, deployed calibration backtest_ctrl_now.rds,
# T1+T2+T3, merged races excluded). VINTAGE CAVEAT: this data predates
# 2026-09-01/02's project_tier/family-debias/sigma-scale work. Ordering
# (which concordance measures) is sensitive to those fixes since they change
# median_mark, not just level -- so this is a diagnostic about the FLAG's
# explanatory power, not a claim about the currently deployed arm's
# concordance. Re-run on a fresh sweep before quoting a number for promotion.
#
# ANCHOR CHECKS:
#   A1 every event in the population must resolve a tactical flag (no silent
#      NA join) -- an unmatched event would make the split meaningless.
#   A2 field size distribution must be checked BOTH ways before trusting a raw
#      tactical-vs-non split: tactical events (800m+) often have deeper
#      championship fields than technical ones, and concordance is sensitive
#      to field size on its own.
suppressMessages(library(data.table))
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
D <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

f <- file.path(D, "calibration_sweep_data.rds")
stopifnot("run check_calibration_sweep.R first" = file.exists(f))
d <- as.data.table(readRDS(f))
say("loaded %s predictions, %s races (vintage: %s, PRE-DATES the 2026-09-01/02 fixes)",
    format(nrow(d), big.mark=","), format(uniqueN(d$race_id), big.mark=","), file.info(f)$mtime)

# `d` already carries discipline/family from check_calibration_sweep.R's own
# merge; only tactical is new here. Merging the others again would collide
# and get suffixed .x/.y, silently breaking every bare reference below.
stopifnot("expected discipline/family already on d" = all(c("discipline", "family") %in% names(d)))
reg <- as.data.table(citius_events())[, .(event_id, tactical)]
d <- merge(d, reg, by = "event_id", all.x = TRUE)
stopifnot("A1: some events failed to resolve a tactical flag" = d[, sum(is.na(tactical))] == 0)

# concordance per race (already have median_mark, actual_place in this saved pop)
conc_by_race <- d[, if (.N >= 4 && uniqueN(median_mark) > 1)
                    .(rho = suppressWarnings(stats::cor(-orientation * median_mark, actual_place,
                                                        method = "spearman")),
                      n = .N, tactical = tactical[1], family = family[1], discipline = discipline[1])
                  else .(rho = NA_real_, n = .N, tactical = tactical[1], family = family[1],
                         discipline = discipline[1]),
                  by = race_id]
conc_by_race <- conc_by_race[!is.na(rho)]
say("races with computable concordance: %s", format(nrow(conc_by_race), big.mark=","))

cat("\n================ A2: field size by tactical flag (confound check) ================\n")
print(conc_by_race[, .(races = .N, mean_field_n = round(mean(n), 1),
                       median_field_n = median(n)), by = tactical])

cat("\n================ concordance: tactical vs non-tactical (T1+T2+T3 pooled) ================\n")
wm <- function(x, w) weighted.mean(x, w)
overall <- conc_by_race[, .(races = .N, concordance = round(wm(rho, n), 4)), by = tactical]
print(overall)

cat("\n================ concordance by discipline, tactical events only ================\n")
tac <- conc_by_race[tactical == TRUE]
print(tac[, .(races = .N, concordance = round(wm(rho, n), 4)), by = discipline][order(concordance)])

cat("\n================ concordance by discipline, non-tactical events only (min 15 races) ================\n")
nontac <- conc_by_race[tactical == FALSE]
print(nontac[, .(races = .N, concordance = round(wm(rho, n), 4)),
             by = discipline][races >= 15][order(concordance)])

# A2 resolved formally: does concordance still separate on tactical AFTER
# controlling for field size? Bin field size, compare within bins.
cat("\n================ concordance by tactical flag, WITHIN field-size bins ================\n")
conc_by_race[, field_bin := cut(n, c(3, 6, 8, 10, 30), labels = c("4-6","7-8","9-10","11+"))]
print(conc_by_race[!is.na(field_bin), .(races = .N, concordance = round(wm(rho, n), 4)),
                   by = .(field_bin, tactical)][order(field_bin, tactical)])

cat("\n================ VERDICT ================\n")
tac_c <- overall[tactical == TRUE]$concordance
non_c <- overall[tactical == FALSE]$concordance
say("tactical events: %.3f | non-tactical events: %.3f | gap: %+.3f", tac_c, non_c, tac_c - non_c)
say("")
say("Middle-distance specifically was 0.510 in the original campaign finding.")
say("If tactical events sit systematically below non-tactical ACROSS field-size")
say("bins, the registry flag is doing real explanatory work. If the gap")
say("collapses within bins, it was a field-size confound, not tactics.")
