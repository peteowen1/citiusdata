# Export a per-event, per-cut metrics table for the interactive artifact.
#
# Uses backtest_combined_full.rds (project_tier 0.5 + family-pool debias,
# offsets fit [2016,2018)/applied 2018+ + sigma scale 0.785) -- the definitive
# arm the standing goal was answered on, holdout 2018-01-01 where the debias
# is actually live. Reuses the same last-5 baseline construction as
# score_arm.R / check_brier_cuts_sweep.R throughout, so the numbers here are
# directly comparable to every number already quoted in the campaign doc.
#
# Writes ONE tidy long-format table (event x metric x arm/base/rel/p, plus
# every filter dimension: family, sex, discipline, meet_tier, wind_bin,
# field_bin, year) as JSON for the artifact to load and slice client-side --
# no server, no recompute per filter change.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow)); suppressMessages(library(jsonlite))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date("2018-01-01")
say <- function(...) cat(sprintf(...), "\n", sep = "")

b <- readRDS(file.path(OUT, "backtest_combined_full.rds"))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            p_gold, p_medal, median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal, merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, actual = mark, actual_place = place,
                   event_id, date, competition_id, sex_code, wind, indoor, venue_country)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]
d[, year := data.table::year(date)]
d[, bias_pct := orientation * (actual - median_mark) / median_mark * 100]
say("population: %s rows, %s races, %d events", format(nrow(d), big.mark=","),
    format(uniqueN(d$race_id), big.mark=","), uniqueN(d$event_id))

# ---- last-5 baseline, same construction as every other script tonight -----
hist <- deployed_history(OUT, events = unique(d$event_id), from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last", .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
d[, b_mark := exp(l5 / orientation)]
say("rows with a last-5 baseline: %s / %s races", format(nrow(d), big.mark=","), format(uniqueN(d$race_id), big.mark=","))

evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE, .(event_id, sigma_within)]
d <- merge(d, evs, by = "event_id", all.x = TRUE)
d[!is.finite(sigma_within), sigma_within := median(evs$sigma_within, na.rm = TRUE)]
say("simulating last-5 baseline probabilities over %s races...", format(uniqueN(d$race_id), big.mark=","))
sim_rows <- rbindlist(lapply(split(d, d$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1], ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id, b_gold = mp$p_gold, b_medal = mp$p_medal)
}))
d <- merge(d, sim_rows, by = c("race_id", "athlete_id"))

d[, b_perf := orientation * log(b_mark)]
d[, act_perf := orientation * log(actual)]
d[, a_perf := orientation * log(median_mark)]
d[, err_a := 100 * abs(a_perf - act_perf)]
d[, err_b := 100 * abs(b_perf - act_perf)]

EPS <- 1e-4
llf <- function(p, y) { p <- pmin(pmax(p, EPS), 1 - EPS); -(y * log(p) + (1 - y) * log(1 - p)) }
d[, gll_a := llf(p_gold, hit)]; d[, gll_b := llf(b_gold, hit)]
d[, mll_a := llf(p_medal, hit_medal)]; d[, mll_b := llf(b_medal, hit_medal)]
d[, gbr_a := (p_gold - hit)^2]; d[, gbr_b := (b_gold - hit)^2]
d[, mbr_a := (p_medal - hit_medal)^2]; d[, mbr_b := (b_medal - hit_medal)^2]

d[, wind_bin := fifelse(is.na(wind), "no reading",
                 fifelse(wind < -1, "headwind < -1",
                  fifelse(wind < 0, "-1 to 0",
                   fifelse(wind < 1, "0 to +1",
                    fifelse(wind <= 2, "+1 to +2", "over +2 (illegal)")))))]
d[, field_n := .N, by = race_id]
d[, field_bin := as.character(cut(field_n, c(-Inf, 6, 8, 10, 12, Inf),
                                  labels = c("<=6","7-8","9-10","11-12","13+")))]
d[, era := fifelse(indoor, "indoor", "outdoor")]

# ---- per-race helper: pairs the SAME race for a cluster-correct mean -------
race_metric <- function(dd, acol, bcol, kind) {
  r <- dd[, .(a = mean(get(acol)), b = mean(get(bcol)), n = .N), by = race_id]
  if (nrow(r) < 3) return(list(n = sum(r$n), races = nrow(r), arm = NA_real_, base = NA_real_, rel = NA_real_, p = NA_real_))
  tt <- tryCatch(t.test(r$a, r$b, paired = TRUE), error = function(e) NULL)
  list(n = sum(r$n), races = nrow(r), arm = mean(r$a), base = mean(r$b),
       rel = 100 * (mean(r$a) - mean(r$b)) / mean(r$b),
       p = if (is.null(tt)) NA_real_ else tt$p.value)
}
metric_defs <- list(
  marks_mae   = c("err_a", "err_b"),
  gold_brier  = c("gbr_a", "gbr_b"),
  medal_brier = c("mbr_a", "mbr_b"),
  gold_logloss  = c("gll_a", "gll_b"),
  medal_logloss = c("mll_a", "mll_b")
)

# ---- build one row per (event_id, metric), plus the filter columns needed
# for client-side slicing: family, sex, discipline, meet_tier(fixed T1 here),
# wind_bin, field_bin, era, year. Each row also carries the event's OWN
# aggregate (all filters "All") so the artifact can show an unfiltered view
# by default and let toggles narrow from there.
build_rows <- function(dd, group_cols) {
  rbindlist(lapply(names(metric_defs), function(met) {
    ab <- metric_defs[[met]]
    g <- dd[, {
      rm <- race_metric(.SD, ab[1], ab[2], met)
      .(n = rm$n, races = rm$races, arm = rm$arm, base = rm$base, rel = rm$rel, p = rm$p)
    }, by = group_cols]
    g[, metric := met]
    g[]
  }))
}

cat("\nbuilding event-level rows (unfiltered)...\n")
by_event <- build_rows(d, c("event_id", "discipline", "family", "sex"))
setnames(by_event, "event_id", "key"); by_event[, dim := "event"]

cat("building family-level rows...\n")
by_family <- build_rows(d, c("family"))
setnames(by_family, "family", "key"); by_family[, dim := "family"]; by_family[, discipline := NA_character_]; by_family[, sex := NA_character_]; by_family[, family := NA_character_]

cat("building sex-level rows...\n")
by_sex <- build_rows(d, c("sex"))
setnames(by_sex, "sex", "key"); by_sex[, dim := "sex"]; by_sex[, discipline := NA_character_]; by_sex[, family := NA_character_]; by_sex[, sex := NA_character_]

cat("building wind-band rows (min 15 races)...\n")
by_wind <- build_rows(d, c("wind_bin"))
setnames(by_wind, "wind_bin", "key"); by_wind[, dim := "wind"]; by_wind[, discipline := NA_character_]; by_wind[, family := NA_character_]; by_wind[, sex := NA_character_]

cat("building field-size rows...\n")
by_field <- build_rows(d, c("field_bin"))
setnames(by_field, "field_bin", "key"); by_field[, dim := "field_size"]; by_field[, discipline := NA_character_]; by_field[, family := NA_character_]; by_field[, sex := NA_character_]

cat("building year rows...\n")
by_year <- build_rows(d, c("year"))
by_year[, year := as.character(year)]
setnames(by_year, "year", "key"); by_year[, dim := "year"]; by_year[, discipline := NA_character_]; by_year[, family := NA_character_]; by_year[, sex := NA_character_]

cat("building event x sex rows (for the family|sex style cut)...\n")
by_event_sex <- build_rows(d, c("event_id", "discipline", "family", "sex"))
# already covered by by_event -- skip duplicate; family x sex separately:
by_fs <- build_rows(d, c("family", "sex"))
by_fs[, key := paste(family, sex, sep = "|")]; by_fs[, dim := "family_sex"]; by_fs[, discipline := NA_character_]

all_cols <- c("dim", "key", "discipline", "family", "sex", "metric", "n", "races", "arm", "base", "rel", "p")
out <- rbindlist(list(by_event[, ..all_cols], by_family[, ..all_cols], by_sex[, ..all_cols],
                      by_wind[, ..all_cols], by_field[, ..all_cols], by_year[, ..all_cols],
                      by_fs[, ..all_cols]), fill = TRUE)
out <- out[races >= 5 | dim == "event"]  # event rows kept even if thin, flagged in UI by n
out[, `:=`(arm = round(arm, 5), base = round(base, 5), rel = round(rel, 2),
           p = ifelse(is.na(p), NA, signif(p, 3)))]

say("\nfinal table: %d rows across %d dims, %d metrics", nrow(out), uniqueN(out$dim), uniqueN(out$metric))
write_json(out, file.path(OUT, "event_metrics_artifact.json"), auto_unbox = FALSE, na = "null", digits = 6)
say("wrote event_metrics_artifact.json")

# also write population metadata for the artifact header
meta <- list(arm = "backtest_combined_full.rds", holdout = format(HOLDOUT),
            tier = "T1_elite", races = uniqueN(d$race_id), rows = nrow(d),
            date_span = c(format(min(d$date)), format(max(d$date))),
            generated_at = format(Sys.time()))
write_json(meta, file.path(OUT, "event_metrics_artifact_meta.json"), auto_unbox = TRUE, digits = 6)
say("wrote event_metrics_artifact_meta.json")
