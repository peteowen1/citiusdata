# Alternative meet-strength basis: CURRENT FORM instead of career best.
#
# WHY. The deployed `strength` scores each finalist by `a_q = max(pctl)` --
# the best they have ever been in that event, over their whole career. Two
# problems with that as a measure of "how strong was this field":
#
#   1. RELEVANCE. A field of past-peak ex-champions scores as elite. This
#      codebase's own evidence says current form is the better signal:
#      score_arm.R uses a five-race baseline as THE reference, and
#      form_ratings.R exists because current form beats career record.
#   2. LOOKAHEAD. max() spans the whole career, so a 2019 meet is scored
#      using marks set in 2024. That is a leak, not just a relevance
#      question, and it is the stronger argument of the two.
#
# WHAT THIS COMPUTES. For every performance, the athlete's form as of that
# race: the mean of their best `BEST_K` marks among their previous `LAST_N`
# performances in that event -- strictly prior races (shift by 1), so no
# race can inform its own rating and nothing after the meet is visible.
#
# NOT THE MODEL'S OWN RATING, deliberately. Using it would close a loop:
# tier decides what the model sees -> model produces ratings -> ratings
# decide tier. Raw observed marks keep the measurement independent.
#
# BEST_K/LAST_N are NOT measured values -- they are a starting point from
# Pete's prior ("best 5 of the last 5-ish"), matching score_arm.R's own
# five-race baseline. Sweep them before treating either as settled; see
# the hypothesis-registry entry.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(arrow)
OUT <- here::here("citiusdata", "data")

LAST_N <- as.integer(Sys.getenv("STR_LAST_N", "5"))
stopifnot(LAST_N >= 1)
cat(sprintf("form basis: mean of the previous %d races (adaptive window)\n", LAST_N))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, competition_id := as.character(competition_id)]
ch <- ch[!is.na(perf) & !is.na(event_id) & !is.na(date)]
cat(sprintf("rows with a usable performance: %s\n", format(nrow(ch), big.mark = ",")))

# Form as of each race: mean of the previous LAST_N races, same athlete and
# event. shift() first so a race never informs its own rating.
#
# WHY MEAN-OF-LAST-N AND NOT BEST-K-OF-LAST-N (which is the actual
# hypothesis): best-of-K needs a custom function per window, and
# frollapply() with one both breaks on groups shorter than the window and
# would run R-level code 4M times -- the exact "never loop at R level over
# the big dimension" failure citius/CLAUDE.md warns about. frollmean with
# an adaptive window is C-speed and handles short histories natively. This
# is therefore v1 of the idea, testing recency-vs-career-best at all; if it
# shows promise, best-K-of-last-N is the refinement worth paying for.
setorder(ch, athlete_id, event_id, date)
ch[, prior := shift(perf, 1L), by = .(athlete_id, event_id)]
# Adaptive window: use as many prior races as exist, capped at LAST_N.
# Without this every athlete with a short history scores NA and any field
# containing them becomes silently unmeasurable.
ch[, k := pmin(seq_len(.N) - 1L, LAST_N), by = .(athlete_id, event_id)]
cat("computing rolling form...\n")
ch[k > 0L, form := frollmean(prior, k, adaptive = TRUE, align = "right",
                             fill = NA_real_, na.rm = TRUE, hasNA = TRUE),
   by = .(athlete_id, event_id)]
cat(sprintf("rows with a form value: %s of %s (%.1f%%)\n",
            format(ch[!is.na(form), .N], big.mark = ","),
            format(nrow(ch), big.mark = ","),
            100 * ch[!is.na(form), .N] / nrow(ch)))

# Percentile within event+era, exactly as the deployed metric does -- the
# only thing changing is WHAT is being ranked (form, not career best).
ch[, era := 4L * (year(date) %/% 4L)]
ch[, n_era := .N, by = .(event_id, era)]
ch[, fpctl := fifelse(n_era >= 200L, frank(form, na.last = "keep") / sum(!is.na(form)),
                      NA_real_), by = .(event_id, era)]
ch[is.na(fpctl) & !is.na(form),
   fpctl := frank(form, na.last = "keep") / sum(!is.na(form)), by = event_id]

fin <- ch[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
            !grepl("semi", round, ignore.case = TRUE) & !is.na(fpctl)]

# Same road-race top-10 restriction as the deployed metric: a road "final"
# is the whole mass field, so averaging over every finisher dilutes it.
road_events <- as.data.table(citius_events())[family == "road", event_id]
fin[event_id %chin% road_events, .rk := frank(-perf, ties.method = "first"),
    by = .(competition_id, event_id)]
fin <- fin[is.na(.rk) | .rk <= 10L][, .rk := NULL]

ev_q <- fin[, .(q = mean(fpctl), n_ath = .N), by = .(competition_id, event_id, era)]
ev_q <- ev_q[n_ath >= 4]
ev_q[, n_meets := .N, by = .(event_id, era)]
ev_q <- ev_q[n_meets >= 3]
ev_q[, ev_pct := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
out <- ev_q[, .(strength_r = round(mean(ev_pct), 1),
                races_won_r = .N), by = competition_id]

MIN_EVENTS <- 5L
road_only <- ev_q[, .(all_road = all(event_id %chin% road_events)), by = competition_id]
out <- merge(out, road_only, by = "competition_id", all.x = TRUE)
out[races_won_r < MIN_EVENTS & !(!is.na(all_road) & all_road), strength_r := NA_real_]
out[, all_road := NULL]

cat(sprintf("\ncompetitions with a recency strength: %s\n", format(nrow(out), big.mark = ",")))
write_parquet(out, file.path(OUT, "strength_recency.parquet"))
cat("wrote strength_recency.parquet\n")

# How different is it from the deployed metric?
ct <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cmp <- merge(ct[, .(competition_id, comp_name, class, meet_tier, strength)],
             out, by = "competition_id")
both <- cmp[!is.na(strength) & !is.na(strength_r)]
cat(sprintf("\ncomparable on %s meets | correlation %.3f | median |diff| %.1f\n",
            format(nrow(both), big.mark = ","),
            cor(both$strength, both$strength_r), median(abs(both$strength - both$strength_r))))
cat("\nbiggest disagreements (deployed high, recency low = stale-reputation fields):\n")
print(both[order(strength - strength_r)][.N:(.N-9)][, .(comp_name, class, strength, strength_r)])
cat("\nbiggest the other way (recency high, deployed low):\n")
print(both[order(strength_r - strength)][.N:(.N-9)][, .(comp_name, class, strength, strength_r)])
