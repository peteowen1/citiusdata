# Meet strength from CURRENT FORM, exponentially weighted -- the deployed basis
# from 2026-09-03.
#
# WHY THIS REPLACED CAREER-BEST. The previous basis scored each finalist by
# `a_q = max(pctl)`: their best mark in that event over their WHOLE CAREER. That
# has lookahead. `max()` spans every race an athlete ever ran, so a 2019 meet was
# tiered using marks set in 2024, and meet_tier feeds the race weights the model
# trains on. A backtest scored against tiers built from future information is
# flattering itself.
#
# That is also why the A/B could not adjudicate. Career-best beat EW on
# concordance (sealed 71.549 vs 70.884, and 74.28 vs 74.17 on the fixed
# 887-final majors panel), but a leaking baseline can win BECAUSE it leaks -- it
# knows things the honest arm does not. The measurement compares a clean
# estimator against a compromised one, so it cannot settle which is better; it
# can only say the clean one scores lower, which is what removing an advantage
# looks like. Adopted on that basis: the leak is real and known, the concordance
# gap is not attributable.
#
# Full record: docs/plans/STRENGTH-METRIC-EXPERIMENT-2026-09-03.md.
#
# WHAT THIS COMPUTES. For every performance, the athlete's form as of that race:
# an exponentially-weighted mean of their PRIOR marks in that event, decayed by
# the family's own fitted half-life from _deployed.R. Two properties matter:
#
#   * strictly prior -- shift() by one, so no race informs its own rating and
#     nothing after the meet is visible. This is the whole point.
#   * the half-life is a MEASURED constant, not a hand-picked window, which the
#     repo's no-hand-tuned-constants rule requires. That is why EW was taken
#     forward over the mean-of-last-5 variant: the two agree at r=0.999, so the
#     one with nothing to justify wins.
#
# NOT the model's own rating, deliberately: tier decides what the model sees ->
# model produces ratings -> ratings would decide tier. Raw observed marks keep
# the measurement independent.
#
# Usage:  Rscript citiusdata/scripts/build_strength_ew.R
# Writes: data/strength_ew.parquet  (competition_id, strength_ew, races_won_ew)
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(arrow)
source(here::here("citiusdata", "scripts", "_env.R"))
citius_version_guard(strict = TRUE)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, competition_id := as.character(competition_id)]
ch <- ch[!is.na(perf) & !is.na(event_id) & !is.na(date)]
cat(sprintf("rows with a usable performance: %s\n", format(nrow(ch), big.mark = ",")))

# COLLAPSE DUAL-CLASSIFIED RUNS TO ONE PERFORMANCE.
#
# The same physical race is often recorded twice: once as the open race and once
# as a national championship contested inside it. Javier Guerra's 2020-02-23
# marathon appears as place 10 (tenth overall) and place 1 (first Spaniard) --
# two race_keys, two rows, one run. Both rows are TRUE, which is why the corpus
# dedup keeps them: its key includes `place` deliberately, because a heat and a
# final on the same day at the same mark ARE two performances and differ far
# more reliably in place than in round.
#
# But a WEIGHTED MEAN is not idempotent the way max() is. The career-best basis
# was structurally immune to this -- repeating a value cannot change a maximum --
# so the defect was latent until EW was adopted, and became live the moment it
# was. Measured on Guerra: form_ew -8.9856 without the duplicate, -8.9765 with
# it. The duplicate reweights the average toward that date.
#
# Blast radius: 35,351 corpus rows (0.48%), 17,661 run-groups of which 17,521
# (99.2%) are this one-competition/differing-place pattern; 8,999 rows and 3,781
# athletes inside the scored T1/T2 2020+ population.
#
# Keyed WITHOUT `place`, which is precisely what identifies "same run, two
# classifications". Fixed here in the CONSUMER rather than in the corpus,
# because dropping `place` from the corpus dedup would collapse genuine
# heat/final pairs everywhere else.
before <- nrow(ch)
ch[, .mk := round(mark, 4)]
setorder(ch, athlete_id, event_id, date, .mk, place)
ch <- unique(ch, by = c("athlete_id", "event_id", "date", ".mk"))
ch[, .mk := NULL]
cat(sprintf("collapsed dual-classified runs: %s -> %s rows (%s removed)\n",
            format(before, big.mark = ","), format(nrow(ch), big.mark = ","),
            format(before - nrow(ch), big.mark = ",")))

ev <- as.data.table(citius_events())[, .(event_id, family)]
ch <- merge(ch, ev, by = "event_id", all.x = TRUE)
hl_map <- DEPLOYED$hl_family
ch[, hl := fifelse(!is.na(family) & family %chin% names(hl_map),
                   unname(hl_map[family]), DEPLOYED$half_life)]
cat(sprintf("half-lives: %s (default %s)\n",
            paste(sprintf("%s=%s", names(hl_map), hl_map), collapse = ", "),
            DEPLOYED$half_life))

# EWMA over strictly prior races, as a weighted cumulative sum so it stays
# C-speed: decay every mark to a common origin, cumsum, then shift() by one to
# exclude the current race. An frollapply() with a custom function would run
# R-level code 4M times, which citius/CLAUDE.md forbids on the big dimension.
#
# THE ORDER WITHIN A DAY MUST BE DETERMINISTIC, AND IT WASN'T. `setorder` by
# date alone leaves same-day rows (a heat and a final on the same day is the
# common case) in whatever order the merge() just above happened to produce --
# merge() gives no row-order guarantee, so a heat's "prior form" could draw on
# that same day's final, exactly the leakage this file's own header says the
# strictly-prior design exists to prevent. Found by review 2026-09-04: affects
# 568,090 rows (~14% of the post-dedup table), 277,526 same-day groups.
#
# Fixed with a round-sequence tiebreak, reusing .round_class() (citius/R/
# ability.R) rather than inventing a second round classifier -- this package
# already paid for getting round precedence wrong once (see that function's
# own comment: naive "final" pattern matching classified 14,764 semi-final
# results as finals). "other" (unrecognised round codes, e.g. combined-event
# group markers) sorts first as the conservative choice: it cannot then draw
# leaked info from a same-day heat/semi/final, only the reverse.
.seq <- c(other = 0L, heat = 1L, quarter = 2L, semi = 3L, final = 4L)
ch[, .rseq := .seq[.round_class(round)]]
ch[is.na(.rseq), .rseq := 0L]
setorder(ch, athlete_id, event_id, date, .rseq)
ch[, .rseq := NULL]
ch[, d := as.numeric(date)]
ch[, wk := 2^((d - min(d)) / hl), by = .(athlete_id, event_id)]
ch[, `:=`(csw = cumsum(wk), cswp = cumsum(wk * perf)), by = .(athlete_id, event_id)]
ch[, `:=`(psw = shift(csw, 1L), pswp = shift(cswp, 1L)), by = .(athlete_id, event_id)]
ch[, form_ew := fifelse(!is.na(psw) & psw > 0, pswp / psw, NA_real_)]
cat(sprintf("rows with an EW form value: %s of %s (%.1f%%)\n",
            format(ch[!is.na(form_ew), .N], big.mark = ","),
            format(nrow(ch), big.mark = ","),
            100 * ch[!is.na(form_ew), .N] / nrow(ch)))
cat("  (the remainder are first-ever races in an event -- genuinely priorless)\n")

# From here the pipeline is IDENTICAL to the career-best version. Only WHAT is
# being ranked changed; how it is aggregated into a meet score did not.
ch[, era := 4L * (year(date) %/% 4L)]
ch[, n_era := .N, by = .(event_id, era)]
ch[, p := fifelse(n_era >= 200L,
                  frank(form_ew, na.last = "keep") / sum(!is.na(form_ew)),
                  NA_real_), by = .(event_id, era)]
ch[is.na(p) & !is.na(form_ew),
   p := frank(form_ew, na.last = "keep") / sum(!is.na(form_ew)), by = event_id]

fin <- ch[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
            !grepl("semi", round, ignore.case = TRUE) & !is.na(p)]

# Road "finals" are the whole mass field, so averaging every finisher dilutes
# them to nothing. Same top-10 restriction the career-best metric uses.
road <- as.data.table(citius_events())[family == "road", event_id]
fin[event_id %chin% road, .rk := frank(-perf, ties.method = "first"),
    by = .(competition_id, event_id)]
fin <- fin[is.na(.rk) | .rk <= 10L][, .rk := NULL]

q <- fin[, .(q = mean(p), n_ath = .N), by = .(competition_id, event_id, era)][n_ath >= 4]
q[, n_meets := .N, by = .(event_id, era)]
q <- q[n_meets >= 3]
# STEP 4, the one that is always forgotten: rank the meet against OTHER MEETS
# contesting the same event in the same era. So 100 means "strongest field of
# any meet running these events in this era", NOT "everyone held a world record".
q[, ev_pct := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
out <- q[, .(strength_ew = round(mean(ev_pct), 1), races_won_ew = .N), by = competition_id]

MIN_EVENTS <- 5L
road_only <- q[, .(all_road = all(event_id %chin% road)), by = competition_id]
out <- merge(out, road_only, by = "competition_id", all.x = TRUE)
out[races_won_ew < MIN_EVENTS & !(!is.na(all_road) & all_road), strength_ew := NA_real_]
out[, all_road := NULL]

stopifnot("no meets scored" = nrow(out) > 0,
          "strength_ew out of range" =
            all(is.na(out$strength_ew) | (out$strength_ew >= 0 & out$strength_ew <= 100)))
cat(sprintf("\ncompetitions with an EW strength: %s (of %s scored)\n",
            format(out[!is.na(strength_ew), .N], big.mark = ","),
            format(nrow(out), big.mark = ",")))
write_parquet(out, file.path(OUT, "strength_ew.parquet"))
cat("wrote strength_ew.parquet\n")
