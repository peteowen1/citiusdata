# Where does the model's DISCRIMINATION (gold/medal Brier) lose to last-5, cut
# every way there is decent sample size to ask -- the Brier-side counterpart
# of check_marks_cuts_sweep.R.
#
# WHY THIS IS A SEPARATE SCRIPT, NOT MORE COLUMNS ON THE MARKS SWEEP. Gold/
# medal Brier depend on relative ordering within a race, marks MAE does not --
# measured 2026-09-01: project_tier and the family-pool debias are both
# UNIFORM per-race additive shifts to `ability`, and a race-level baseline
# such a shift cannot change WHO beats WHOM, only the predicted level. Their
# gold Brier, medal Brier and favourite-wins came back BYTE-IDENTICAL across
# ctrl/tier05/tier05fd (0.17118 medal Brier, to five digits, all three arms) --
# a structural invariant, not something either fix could ever have moved. So
# whatever is wrong with Brier is untouched by today's marks fix and needs its
# own cut sweep to find.
#
# METHOD. Same last-5 baseline SIMULATION as score_arm.R's default branch
# (event-sigma flavour) -- this is the expensive part (simulate_event() over
# every race), built ONCE and reused across every cut below. Per-race Brier
# (byrace()), same as score_arm.R -- the unit of replication is already the
# race, so no cluster-SE correction is needed on top (unlike the marks sweep,
# which operated on athlete-rows).
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  the T1 elite pooled gold/medal Brier here must match score_arm.R's own
#       number (0.07598/0.17118 @ 2025-01-01 holdout) to the digit -- if it
#       doesn't, this script's population or baseline differs from the one the
#       campaign has been deciding on.
#   A2  every reported cut level clears MIN_RACES or is not reported.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_BRIER_HOLDOUT", "2025-01-01"))
MIN_RACES <- 15L
ARM <- Sys.getenv("CITIUS_BRIER_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s | MIN_RACES %d", ARM, format(HOLDOUT), MIN_RACES)

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_gold = p_gold, a_medal = p_medal, a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)],
           by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date,
            competition_id, sex_code, wind, indoor, age, venue_country)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- last-5 baseline SIMULATION, event-sigma flavour, built ONCE -----------
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]

ev_all <- as.data.table(deployed_calibration(OUT)$events)
evs <- ev_all[calibrated %in% TRUE, .(event_id, sigma_within)]
d <- merge(d, evs, by = "event_id", all.x = TRUE)
d[!is.finite(sigma_within), sigma_within := median(evs$sigma_within, na.rm = TRUE)]

say("simulating last-5 baseline over %s races (this is the slow part, ~2-3 min)...",
    format(uniqueN(d$race_id), big.mark = ","))
sim_rows <- rbindlist(lapply(split(d, d$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1],
                   ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id,
             b_gold = mp$p_gold, b_medal = mp$p_medal)
}))
d <- merge(d, sim_rows, by = c("race_id", "athlete_id"))
d <- d[date >= HOLDOUT]

t1 <- d[meet_tier == "T1_elite"]
say("\nT1 elite population: %s rows, %s races", format(nrow(t1), big.mark=","), format(uniqueN(t1$race_id), big.mark=","))

EPS <- 1e-4
llf <- function(p, y) { p <- pmin(pmax(p, EPS), 1 - EPS); -(y * log(p) + (1 - y) * log(1 - p)) }
byrace <- function(dd, col_a, col_b, y) dd[, .(a = mean((get(col_a) - get(y))^2), b = mean((get(col_b) - get(y))^2)),
                                          by = race_id]

cat("\n==== ANCHOR A1: pooled T1 numbers must match score_arm.R ====\n")
gB <- byrace(t1, "a_gold", "b_gold", "hit"); mB <- byrace(t1, "a_medal", "b_medal", "hit_medal")
say("gold Brier: arm %.5f vs base %.5f (score_arm.R reference: 0.07598/0.07777)", mean(gB$a), mean(gB$b))
say("medal Brier: arm %.5f vs base %.5f (score_arm.R reference: 0.17118/0.16995)", mean(mB$a), mean(mB$b))

run_cut <- function(dd, by_col, label, min_races = MIN_RACES) {
  cat(sprintf("\n---- cut: %s ----\n", label))
  lv <- dd[!is.na(get(by_col)), unique(get(by_col))]
  rows <- list()
  for (v in lv) {
    sub <- dd[get(by_col) == v]
    nr <- uniqueN(sub$race_id)
    if (nr < min_races) next
    g <- byrace(sub, "a_gold", "b_gold", "hit")
    m <- byrace(sub, "a_medal", "b_medal", "hit_medal")
    tg <- t.test(g$a, g$b, paired = TRUE); tm <- t.test(m$a, m$b, paired = TRUE)
    rows[[length(rows) + 1L]] <- data.table(
      level = as.character(v), races = nr,
      gold_arm = round(mean(g$a), 4), gold_base = round(mean(g$b), 4),
      gold_rel = round(100 * (mean(g$a) - mean(g$b)) / mean(g$b), 2), gold_p = signif(tg$p.value, 3),
      medal_arm = round(mean(m$a), 4), medal_base = round(mean(m$b), 4),
      medal_rel = round(100 * (mean(m$a) - mean(m$b)) / mean(m$b), 2), medal_p = signif(tm$p.value, 3))
  }
  if (!length(rows)) { cat("  no level clears MIN_RACES\n"); return(invisible(NULL)) }
  res <- rbindlist(rows)[order(-medal_rel)]
  print(res, row.names = FALSE)
  invisible(res)
}

cat("\n\n================ BRIER CUT SWEEP: T1 elite, model vs last-5 ================\n")
cat("positive rel = model WORSE (higher Brier) than last-5 on that cut.\n")

t1[, fs := paste(family, sex, sep = "|")]
t1[, season_q := paste0("Q", quarter(date))]
t1[, era := fifelse(indoor, "indoor", "outdoor")]
t1[, age_cov := !is.na(age)]
t1[age_cov == TRUE, age_bin := cut(age, c(-Inf, 22, 25, 28, 31, Inf),
                                    labels = c("<=22", "23-25", "26-28", "29-31", "32+"))]

run_cut(t1, "family", "family")
run_cut(t1, "fs", "family x sex", min_races = 10L)
run_cut(t1, "sex", "sex", min_races = 10L)
run_cut(t1[, yr := as.character(year(date))], "yr", "season year")
run_cut(t1, "season_q", "quarter of year")
run_cut(t1, "era", "indoor vs outdoor", min_races = 10L)
run_cut(t1, "venue_country", "venue country")
run_cut(t1[age_cov == TRUE], "age_bin", "age band", min_races = 10L)

# ---- added 2026-09-01: event, meet, wind, field size ----------------------
# The four cuts the sweep was missing when the standing goal became "medal
# Brier must beat last-5 too". Event and field size are the two most likely to
# localise a DISCRIMINATION problem: Brier is a within-race ordering metric, so
# it should degrade where the ordering is genuinely hardest (deep fields) or
# where the event's own signal is weakest -- neither of which the level-shift
# fixes could ever have touched.
run_cut(t1, "event_id", "individual event", min_races = 10L)

# Meet, two ways. meet_tier is the coarse decision-relevant one; the named meet
# catches a single venue/organiser dragging a tier's average.
run_cut(d, "meet_tier", "meet tier (ALL tiers, not just T1)", min_races = 10L)
if ("competition_id" %in% names(t1)) {
  t1[, comp_chr := as.character(competition_id)]
  run_cut(t1, "comp_chr", "individual meet (T1)", min_races = 10L)
}

# Wind, binned. Legal-limit +2.0 is the meaningful boundary, and NA is its own
# level rather than being dropped -- absence of a wind reading is informative
# (it means an event or venue that does not record one), which is exactly the
# missing-is-informative trap. Reported as an explicit "no reading" level.
t1[, wind_bin := fifelse(is.na(wind), "no reading",
                  fifelse(wind < -1, "headwind < -1",
                   fifelse(wind < 0, "-1 to 0",
                    fifelse(wind < 1, "0 to +1",
                     fifelse(wind <= 2, "+1 to +2", "over +2 (illegal)")))))]
run_cut(t1, "wind_bin", "wind band", min_races = 10L)

# Field size: computed per race, then attached to every row of that race.
t1[, field_size := uniqueN(athlete_id), by = race_id]
t1[, field_bin := cut(field_size, c(-Inf, 6, 8, 10, 12, Inf),
                      labels = c("<=6", "7-8", "9-10", "11-12", "13+"))]
run_cut(t1, "field_bin", "field size", min_races = 10L)

cat("\nDone.\n")
