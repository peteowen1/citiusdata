# SUPERSEDED by check_era_effect3.R -- DO NOT TRUST THESE NUMBERS.
#
# Two bugs, found after this ran: (1) the "top" tier code list here included
# "F", which citius/R/ability.R's .tier_class() maps to LOW, not top -- so
# this filter pulled in club/minor-meet marks, not elite ones, and its 5M-row
# "F" bucket dominated everything. (2) it used the catalogue's meet_tier,
# which has entire YEARS with zero T1_elite rows across every discipline
# (checked directly: 2013/2015/2017/2018/2020/2025/2026 all-zero) -- a
# pre-existing catalogue coverage gap, not a tech-era signal.
#
# Kept for the record, not deleted, per this repo's convention for a
# documented confound (see build_calibration_coasting.R's own header).
#
# Size a secular (era) trend before building anything: do shoe/spike tech
# eras show up as a level shift in T1 marks that a shoe-insensitive control
# population does not? Ad hoc sizing script, not part of the shipping pipeline.
#
# Two hypothesised breaks:
#   Road "supershoes" (carbon-plated) ~2017 onward: marathon, half marathon,
#   10,000m, 5000m.
#   Track "super spikes" ~2021 onward (Tokyo 2020 Games, held 2021): 800m,
#   1500m, and sprints.
#   Control (no footwear/equipment change): Shot Put, Discus Throw, Hammer
#   Throw -- throwers don't run in the implement they're being timed on.
#
# Method: annual mean of the TOP 20 marks per event/sex/year (not all marks),
# specifically to blunt the "more people compete now" depth confound -- top-20
# should already be near-saturated at elite fields even in a thinner year.
# Restricted to T1_elite via the catalogue's meet_tier, the same anchor-guarded
# field every other finding in this repo uses, not the feed's incoherent tier.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

ev <- as.data.table(citius_events())
targets <- c("Marathon", "Half Marathon", "10,000 Metres", "5000 Metres",
             "1500 Metres", "800 Metres", "200 Metres", "100 Metres",
             "Shot Put", "Discus Throw", "Hammer Throw")
ev_t <- ev[discipline %in% targets & sport == "Athletics"]
cat("Matched", uniqueN(ev_t$discipline), "of", length(targets), "target disciplines:\n")
print(ev_t[, .(event_id, discipline, sex, orientation)][order(discipline, sex)])
missing <- setdiff(targets, unique(ev_t$discipline))
if (length(missing)) cat("MISSING (check spelling against events.R):", paste(missing, collapse = ", "), "\n")

corpus <- setDT(readRDS(file.path(D, "athletics_corpus.rds")))
corpus <- corpus[event_id %in% ev_t$event_id & !is.na(mark) & !is.na(date)]

cat_tbl <- setDT(arrow::read_parquet(file.path(D, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
corpus[, competition_id := as.character(competition_id)]
corpus <- merge(corpus, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
cov <- 100 * mean(!is.na(corpus$meet_tier))
cat(sprintf("meet_tier attached to %.1f%% of matched rows\n", cov))
stopifnot("join must not silently match nothing" = cov > 50)

t1 <- corpus[meet_tier == "T1_elite"]
# discipline/sex/orientation already live on the corpus itself (verified by
# inspection) -- no need to bring them in from the registry a second time.
t1[, year := data.table::year(date)]
t1 <- t1[year >= 2010 & year <= 2026]

# Rank within event/sex/year by ORIENTED mark (best first), keep top 20.
t1[, oriented := mark * orientation]
data.table::setorder(t1, discipline, sex, year, -oriented)
t1[, rk := seq_len(.N), by = .(discipline, sex, year)]
top20 <- t1[rk <= 20]

annual <- top20[, .(n = .N, mean_mark = mean(mark)), by = .(discipline, sex, year)]
data.table::setorder(annual, discipline, sex, year)
annual[, orientation := ev_t$orientation[match(discipline, ev_t$discipline)]]
# pct change: positive = FASTER/FARTHER (improvement), regardless of orientation
annual[, pct_change := c(NA, orientation[1] * diff(mean_mark) / mean_mark[-.N] * 100), by = .(discipline, sex)]

cat("\n=== annual n (top-20 cap; anything under 15 most years means thin T1 coverage) ===\n")
print(dcast(annual, discipline + sex ~ year, value.var = "n"))

# Pre/post comparison per hypothesised break.
size_break <- function(d, brk, label) {
  a <- annual[discipline == d]
  pre  <- a[year >= brk - 5 & year < brk]
  post <- a[year >= brk & year <= brk + 5]
  data.table(discipline = d, break_year = brk, window = label,
             pre_avg_pct = mean(pre$pct_change, na.rm = TRUE),
             post_avg_pct = mean(post$pct_change, na.rm = TRUE),
             pre_n_years = nrow(pre), post_n_years = nrow(post),
             pre_mean_mark = mean(pre$mean_mark), post_mean_mark = mean(post$mean_mark))
}

road <- c("Marathon", "Half Marathon", "10,000 Metres", "5000 Metres")
spike <- c("1500 Metres", "800 Metres", "200 Metres", "100 Metres")
control <- c("Shot Put", "Discus Throw", "Hammer Throw")

res <- rbindlist(c(
  lapply(road,    size_break, brk = 2017, label = "supershoe (2017)"),
  lapply(spike,   size_break, brk = 2021, label = "superspike (2021)"),
  lapply(control, size_break, brk = 2017, label = "control vs 2017"),
  lapply(control, size_break, brk = 2021, label = "control vs 2021")
))
res[, level_shift_pct := post_mean_mark / pre_mean_mark - 1]
res[, level_shift_pct := level_shift_pct * ifelse(discipline %in% ev_t[orientation == -1]$discipline, -100, 100)]

cat("\n=== pre/post break, averaged both sexes (n years each side; level_shift_pct = + is improvement) ===\n")
print(res[, .(pre_avg_pct = mean(pre_avg_pct), post_avg_pct = mean(post_avg_pct),
              level_shift_pct = mean(level_shift_pct), pre_n = sum(pre_n_years), post_n = sum(post_n_years)),
          by = .(discipline, window)][order(window, discipline)])

saveRDS(list(annual = annual, res = res), file.path(D, "era_effect_check.rds"))
cat("\nwrote era_effect_check.rds\n")
