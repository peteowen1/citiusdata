# RACE SHOCK, done the way Pete specified (2026-09-06):
#   1. measure each race's EXCESS shock -- how much better the whole field went
#      than a race of that kind normally does;
#   2. regress the athlete's NEXT performance on that excess, so the number
#      that carries forward is the fitted persistence, not a guess.
#
# WHY THE FIRST ATTEMPT WAS WRONG. It stripped the WHOLE race effect (referenced
# to a top-final mean) from history and added a cell mean back for the forecast
# race; the two halves were on different scales and every final came out ~2%
# pessimistic. Here the strip is only (1 - beta) * excess: a race that ran
# exactly as its kind usually does is left alone; a Gout Gout day (the whole
# field PBs) has a large excess, and the part of it that does NOT predict the
# athlete's next races is removed. Nothing is added back -- the tier and round
# context adjustments already move an athlete from the conditions they raced
# in to the conditions they are entering.
#
# DEFINITIONS (oriented log scale, higher = better):
#   c_r      shrunk shared race effect from the EB calibration (calibration$race)
#   E_cell   mean c_r over races of the same event x tier class x round class
#            (>= MIN_CELL races, else family x tier x round, else the event mean)
#   excess_r = c_r - E_cell
#   a_loo    the athlete's decomposition ability EXCLUDING the shocked race
#   y        next performance within GAP days, relative to a_loo and to the
#            next race's own E_cell:  perf_next - a_loo - E_cell_next
#   beta     slope of y on excess_r -- the share of a shock that persists
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/fit_race_shock_persistence.R'
# Env: CITIUS_SHOCK_CAL (default calibration_race_eb_perevent.rds),
#      CITIUS_SHOCK_OUT (default <cal>_persist.rds), CITIUS_SHOCK_GAP (180),
#      CITIUS_SHOCK_MIN_CELL (10)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
OUT  <- here::here("citiusdata", "data")
CAL  <- Sys.getenv("CITIUS_SHOCK_CAL", "calibration_race_eb_perevent.rds")
DST  <- Sys.getenv("CITIUS_SHOCK_OUT", sub("\\.rds$", "_persist.rds", CAL))
GAP  <- as.integer(Sys.getenv("CITIUS_SHOCK_GAP", "180"))
MINC <- as.integer(Sys.getenv("CITIUS_SHOCK_MIN_CELL", "10"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

cal <- readRDS(file.path(OUT, CAL))
stopifnot(inherits(cal, "citius_calibration"), !is.null(cal$race), !is.null(cal$ability))
rr <- as.data.table(cal$race)[is.finite(c_r)]
ab <- as.data.table(cal$ability)[, .(athlete_id = as.character(athlete_id), event_id, a_i, n_ab = n)]
say("calibration %s: %s races with a shrunk effect, %s athlete-events", CAL,
    format(nrow(rr), big.mark = ","), format(nrow(ab), big.mark = ","))

# --- 1. the excess per race --------------------------------------------------
reg <- as.data.table(citius_events())[, .(event_id, family)]
rr[, round_class := .round_class(round)]
rr[, tier_class := .tier_class(tier)]
rr <- merge(rr, reg, by = "event_id")
cell_ev  <- rr[, .(n_cell = .N, e_ev = mean(c_r)), by = .(event_id, tier_class, round_class)]
cell_fam <- rr[, .(n_fcell = .N, e_fam = mean(c_r)), by = .(family, tier_class, round_class)]
ev_mean  <- rr[, .(e_event = mean(c_r)), by = event_id]
rr <- merge(rr, cell_ev, by = c("event_id", "tier_class", "round_class"), all.x = TRUE)
rr <- merge(rr, cell_fam, by = c("family", "tier_class", "round_class"), all.x = TRUE)
rr <- merge(rr, ev_mean, by = "event_id", all.x = TRUE)
rr[, e_cell := fifelse(n_cell >= MINC, e_ev, fifelse(n_fcell >= MINC, e_fam, e_event))]
rr[, excess := c_r - e_cell]
say("excess: sd %.4f (c_r sd %.4f); %.1f%% of races on an event cell, %.1f%% on a family cell",
    sd(rr$excess), sd(rr$c_r), 100 * mean(rr$n_cell >= MINC), 100 * mean(rr$n_cell < MINC & rr$n_fcell >= MINC))
expected <- rr[, .(event_id, family, tier_class, round_class, e_cell, n_cell)]
expected <- unique(expected, by = c("event_id", "tier_class", "round_class"))

# --- 2. next-race persistence ------------------------------------------------
store <- file.path(OUT, "athletics_corpus_store")
x <- as.data.table(read_results_store(store, columns = c("athlete_id", "event_id", "date", "perf", "race_key", "wind")))
x <- x[is.finite(perf) & !is.na(race_key) & !is.na(date)]
x[, athlete_id := as.character(athlete_id)]
x[, date := as.Date(date)]
say("corpus rows %s", format(nrow(x), big.mark = ","))
x <- merge(x, ab, by = c("athlete_id", "event_id"))
x <- merge(x, rr[, .(race_key, c_r, excess, e_cell, family, tier_class, round_class)], by = "race_key")
x <- x[n_ab >= 2L]
say("rows with an ability and a race effect: %s", format(nrow(x), big.mark = ","))

# --- race-level features that might separate "the day" from "real form" -----
# pb: did this athlete beat their own best-before in this race (needs 3+ prior
# results so a debutant's first mark is not a "PB"). pb_frac: share of the
# field that did. wind_mean: the race's mean legal reading where recorded.
setorder(x, athlete_id, event_id, date)
x[, n_before := seq_len(.N) - 1L, by = .(athlete_id, event_id)]
x[, best_before := shift(cummax(perf), 1L), by = .(athlete_id, event_id)]
x[, pb := n_before >= 3L & is.finite(best_before) & perf > best_before]
race_feat <- x[, .(pb_frac = if (sum(n_before >= 3L) >= 3L) mean(pb[n_before >= 3L]) else NA_real_,
                   wind_mean = if ("wind" %in% names(x)) mean(wind[is.finite(wind)]) else NA_real_,
                   month_shock = as.integer(format(date[1], "%m"))), by = race_key]
x <- merge(x, race_feat, by = "race_key")
x[!is.finite(pb_frac), pb_frac := 0]
x[!is.finite(wind_mean), wind_mean := 0]
# leave-one-out ability: a_i is mean(perf - c_r) over the athlete-event's rows
x[, a_loo := (n_ab * a_i - (perf - c_r)) / (n_ab - 1)]
setorder(x, athlete_id, event_id, date)
x[, `:=`(perf_next = shift(perf, -1L), date_next = shift(date, -1L), e_next = shift(e_cell, -1L),
         excess_next = shift(excess, -1L),
         # the athlete's own residual in the race BEFORE the shock: their form
         # trajectory going in, so an athlete already improving is not read as
         # "the shock persisted"
         prev_resid = shift(perf - a_i - c_r, 1L)), by = .(athlete_id, event_id)]
p <- x[is.finite(perf_next) & as.numeric(date_next - date) <= GAP & as.numeric(date_next - date) > 0]
p[, gap := as.numeric(date_next - date)]
p[, y := perf_next - a_loo - e_next]
p[, x_ex := excess]
p[, month_next := factor(format(date_next, "%m"))]
p[!is.finite(prev_resid), prev_resid := 0]
say("%s athlete race pairs within %d days", format(nrow(p), big.mark = ","), GAP)
stopifnot(nrow(p) > 10000)

# CONTROLS (2026-09-06, 23:45). The raw slope reads in-season progression as
# persistence: a fast field in June is fast again in July because it is July.
# Month-of-next-race dummies absorb that; the athlete's own residual in the
# race before the shock absorbs an individual trajectory. CITIUS_SHOCK_CONTROLS=0
# gives the raw slope for comparison; both are printed.
CONTROLS <- Sys.getenv("CITIUS_SHOCK_CONTROLS", "1") == "1"
fit <- function(d, controls = CONTROLS) {
  if (nrow(d) < 200) return(list(beta = NA_real_, se = NA_real_))
  f <- if (controls && length(unique(d$month_next)) > 1) y ~ x_ex + prev_resid + month_next else y ~ x_ex
  m <- stats::lm(f, data = d)
  s <- summary(m)$coefficients
  list(beta = unname(s["x_ex", "Estimate"]), se = unname(s["x_ex", "Std. Error"]))
}
overall <- fit(p)
raw_overall <- fit(p, controls = FALSE)
by_tier_raw <- p[, {f <- fit(.SD, controls = FALSE); .(n = .N, beta_raw = round(f$beta, 4))}, by = tier_class][order(-n)]
by_fam <- p[, {f <- fit(.SD); .(n = .N, beta = round(f$beta, 4), se = round(f$se, 4), sd_excess = round(sd(x_ex), 4))}, by = family][order(-n)]
p[, gap_band := cut(gap, c(0, 14, 30, 60, 120, 180), include.lowest = TRUE)]
by_gap <- p[, {f <- fit(.SD); .(n = .N, beta = round(f$beta, 4), se = round(f$se, 4))}, by = gap_band][order(gap_band)]
p[, ex_band := cut(x_ex, c(-Inf, -0.02, -0.01, -0.003, 0.003, 0.01, 0.02, Inf))]
by_size <- p[, .(n = .N, mean_excess = round(mean(x_ex), 4), mean_y = round(mean(y), 4),
                 implied_beta = round(mean(y) / mean(x_ex), 3)), by = ex_band][order(ex_band)]
by_tier <- p[, {f <- fit(.SD); .(n = .N, beta = round(f$beta, 4), se = round(f$se, 4))}, by = tier_class][order(-n)]

# --- per-race beta: does the persistence depend on what the race looked like? --
# y ~ excess * (tier + pb_frac + wind + big) + controls. The interaction
# coefficients say how beta moves with each feature; the fitted beta for every
# race in the calibration is stored so the strip can use it directly.
p[, big := as.numeric(x_ex > 0.02)]
p[, tier_f := factor(tier_class, levels = c("low", "mid", "high", "top"))]
m_int <- stats::lm(y ~ x_ex * (tier_f + pb_frac + wind_mean + big) + prev_resid + month_next, data = p)
co <- summary(m_int)$coefficients
cat("\n=== how beta moves with the race's features (interaction terms with the excess) ===\n")
print(round(co[grepl("^x_ex", rownames(co)), c("Estimate", "Std. Error")], 4))
rr_feat <- merge(rr[, .(race_key, event_id, tier_class, excess, c_r)], race_feat, by = "race_key", all.x = TRUE)
rr_feat[!is.finite(pb_frac), pb_frac := 0]; rr_feat[!is.finite(wind_mean), wind_mean := 0]
rr_feat[, big := as.numeric(excess > 0.02)]
rr_feat[, tier_f := factor(tier_class, levels = c("low", "mid", "high", "top"))]
# beta_r = d y / d excess at this race's features (month and prev_resid drop out)
beta_of <- function(d) {
  b <- co[, "Estimate"]
  out <- b["x_ex"] + ifelse(d$tier_f == "mid", b["x_ex:tier_fmid"], 0) +
    ifelse(d$tier_f == "high", b["x_ex:tier_fhigh"], 0) + ifelse(d$tier_f == "top", b["x_ex:tier_ftop"], 0) +
    b["x_ex:pb_frac"] * d$pb_frac + b["x_ex:wind_mean"] * d$wind_mean + b["x_ex:big"] * d$big
  unname(out)
}
rr_feat[, beta_race := pmin(pmax(beta_of(rr_feat), 0), 1)]
by_race <- rr_feat[, .(race_key, beta = round(beta_race, 4), pb_frac = round(pb_frac, 3), wind_mean = round(wind_mean, 2), big)]
cat("\nper-race beta: quantiles over all races\n"); print(round(quantile(by_race$beta, c(0.01, 0.1, 0.5, 0.9, 0.99)), 3))
cat("per-race beta for big excess (> 2%) races with pb_frac >= 0.5:\n")
print(round(quantile(by_race[big == 1 & pb_frac >= 0.5]$beta, c(0.1, 0.5, 0.9)), 3))

cat("\n=== persistence of a race's excess into the athlete's next race (beta = slope of y on excess) ===\n")
cat(sprintf("overall: beta %.4f (se %.4f) on %s pairs | controls %s | raw slope %.4f\n", overall$beta, overall$se,
            format(nrow(p), big.mark = ","), CONTROLS, raw_overall$beta))
cat("\nby family:\n"); print(by_fam)
cat("\nby gap to the next race:\n"); print(by_gap)
cat("\nby size of the excess (mean_y / mean_excess = implied beta; + excess = the field went better than usual):\n"); print(by_size)
cat("\nby tier class of the shocked race (with controls; beta_raw = plain slope):\n"); print(merge(by_tier, by_tier_raw[, .(tier_class, beta_raw)], by = "tier_class")[order(-n)])
cat("\nReading: beta is the share of a shock that shows up in the athlete's next result.\n")
cat("The strip removes (1 - beta) * excess from that historical mark. beta ~ 0 means\n")
cat("the whole excess was the day, not the athlete; beta ~ 1 means it was real form.\n")

cal$race_shock <- list(beta = overall$beta, beta_se = overall$se,
                       # Tier of the shocked race is the dominant structure (top 0.53,
                       # high 0.72, mid 0.86, low 1.02 on the first fit) and is what the
                       # strip uses first; family is kept for the record only.
                       by_tier = by_tier[, .(tier_class, beta, se, n)],
                       # per-race beta from the interaction fit; the strip uses
                       # this first, then by_tier, then the overall value
                       by_race = by_race[, .(race_key, beta)],
                       interaction_coefficients = co[grepl("^x_ex", rownames(co)), c("Estimate", "Std. Error")],
                       by_gap = by_gap,
                       by_family = by_fam[, .(family, beta, se, n)],
                       expected = expected, gap_days = GAP, min_cell = MINC,
                       fitted_at = Sys.time(), pairs = nrow(p))
saveRDS(cal, file.path(OUT, DST))
fwrite(by_fam, file.path(OUT, "race_shock_persistence_by_family.csv"))
fwrite(by_gap, file.path(OUT, "race_shock_persistence_by_gap.csv"))
fwrite(by_size, file.path(OUT, "race_shock_persistence_by_size.csv"))
say("wrote %s with $race_shock (beta %.4f, %d family rows, %s expected cells)", DST, overall$beta, nrow(by_fam), format(nrow(expected), big.mark = ","))
