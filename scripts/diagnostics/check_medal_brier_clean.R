# Re-derive the family and family x sex medal-Brier cuts on combined_full as a
# SINGLE unwrapped table each, because the console-wrapped two-block output
# from check_brier_cuts_sweep.R is error-prone to read by eye -- confirmed
# error-prone by catching myself about to misattribute the family-level
# +8.10% result to jump when it is actually throw.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2018-01-01")

b <- readRDS(file.path(OUT, "backtest_combined_full.rds"))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_gold = p_gold, a_medal = p_medal)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)],
           by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]

hist <- deployed_history(OUT, events = unique(d$event_id), from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last", .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]

evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE, .(event_id, sigma_within)]
d <- merge(d, evs, by = "event_id", all.x = TRUE)
d[!is.finite(sigma_within), sigma_within := median(evs$sigma_within, na.rm = TRUE)]
sim_rows <- rbindlist(lapply(split(d, d$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1], ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id, b_medal = mp$p_medal)
}))
d <- merge(d, sim_rows, by = c("race_id", "athlete_id"))

d[, fs := paste(family, sex, sep = "|")]
one_cut <- function(by_col, min_races = 15L) {
  lv <- d[, unique(get(by_col))]
  rows <- lapply(lv, function(v) {
    sub <- d[get(by_col) == v]
    nr <- uniqueN(sub$race_id); if (nr < min_races) return(NULL)
    a <- sub[, .(v = mean((a_medal - hit_medal)^2)), by = race_id]
    b2 <- sub[, .(v = mean((b_medal - hit_medal)^2)), by = race_id]
    mm <- merge(a, b2, by = "race_id")
    tt <- t.test(mm$v.x, mm$v.y, paired = TRUE)
    data.table(level = v, races = nr, arm = round(mean(mm$v.x),4), base = round(mean(mm$v.y),4),
               rel_pct = round(100*(mean(mm$v.x)-mean(mm$v.y))/mean(mm$v.y),2), p = signif(tt$p.value,3))
  })
  rbindlist(rows)[order(-rel_pct)]
}
cat("==== FAMILY (clean, single table) ====\n"); print(one_cut("family"), row.names = FALSE)
cat("\n==== FAMILY x SEX (clean, single table) ====\n"); print(one_cut("fs", min_races = 10L), row.names = FALSE)
