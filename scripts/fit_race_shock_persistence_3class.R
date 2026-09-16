# 3-CLASS VARIANT of fit_race_shock_persistence.R, 2026-09-16.
#
# Promoted from a scratch experiment: this is the refit that must ship
# ALONGSIDE the tier_class -> race_tier (R1/R2/R3) rename, per
# docs/reference/tier-terminology.md. Its output
# (calibration_race_eb_perevent_persist_3class_TEST.rds) is the artefact to
# reuse rather than refitting a third time.
#
# Refits race_shock under the 3-class tier_class vocabulary (top: DF/GW/OW/GL/A,
# mid: B/C/D, low: E/F) WITHOUT touching the committed package source -- .tier_class()
# below is defined in this script's own global environment AFTER devtools::load_all(),
# which R resolves before the package's attached internal version for every bare
# call inside this script. citius/R/ability.R stays on the deployed 4-class mapping
# throughout this run.
#
# Writes to a clearly experimental filename (CITIUS_SHOCK_OUT below), NOT the
# name build_calibration_compose.R actually reads (calibration_race_eb_perevent_persist5.rds)
# -- this is a measurement, not a promotion. Promoting is a separate, deliberate step.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})

.tier_class <- function(tier) {
  t <- toupper(trimws(as.character(tier)))
  known <- c("OW", "GW", "GL", "A", "B", "C", "D", "DF", "E", "F")
  out <- rep("mid", length(t))
  out[t %in% c("DF", "GW", "OW", "GL", "A")] <- "top"
  out[t %in% c("B", "C", "D")] <- "mid"
  out[t %in% c("E", "F")] <- "low"
  out[is.na(t)] <- "mid"
  out
}

OUT  <- here::here("citiusdata", "data")
CAL  <- Sys.getenv("CITIUS_SHOCK_CAL", "calibration_race_eb_perevent.rds")
DST  <- "calibration_race_eb_perevent_persist_3class_TEST.rds"
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
x[, a_loo := (n_ab * a_i - (perf - c_r)) / (n_ab - 1)]
setorder(x, athlete_id, event_id, date)
x[, `:=`(perf_next = shift(perf, -1L), date_next = shift(date, -1L), e_next = shift(e_cell, -1L),
         excess_next = shift(excess, -1L),
         prev_resid = shift(perf - a_i - c_r, 1L)), by = .(athlete_id, event_id)]
p <- x[is.finite(perf_next) & as.numeric(date_next - date) <= GAP & as.numeric(date_next - date) > 0]
p[, gap := as.numeric(date_next - date)]
p[, y := perf_next - a_loo - e_next]
p[, x_ex := excess]
p[, month_next := factor(format(date_next, "%m"))]
p[!is.finite(prev_resid), prev_resid := 0]
say("%s athlete race pairs within %d days", format(nrow(p), big.mark = ","), GAP)
stopifnot(nrow(p) > 10000)

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

p[, big := as.numeric(x_ex > 0.02)]
p[, tier_f := factor(tier_class, levels = c("low", "mid", "top"))]   # 3 classes, not 4
p[, family_f := factor(family, levels = c("sprint", "hurdles", "jump", "throw", "middle", "distance", "road", "walk", "combined"))]
m_int <- stats::lm(y ~ x_ex * (tier_f + family_f + pb_frac + wind_mean + big) + prev_resid + month_next, data = p)
co <- summary(m_int)$coefficients
cat("\n=== how beta moves with the race's features (interaction terms with the excess) ===\n")
print(round(co[grepl("^x_ex", rownames(co)), c("Estimate", "Std. Error")], 4))
rr_feat <- merge(rr[, .(race_key, event_id, tier_class, excess, c_r)], race_feat, by = "race_key", all.x = TRUE)
rr_feat[!is.finite(pb_frac), pb_frac := 0]; rr_feat[!is.finite(wind_mean), wind_mean := 0]
rr_feat[, big := as.numeric(excess > 0.02)]
rr_feat[, tier_f := factor(tier_class, levels = c("low", "mid", "top"))]
rr_feat <- merge(rr_feat, reg, by = "event_id", all.x = TRUE)
rr_feat[, family_f := factor(family, levels = levels(p$family_f))]
beta_of <- function(d) {
  b <- co[, "Estimate"]
  fam_term <- vapply(as.character(d$family_f), function(fm) {
    nm <- paste0("x_ex:family_f", fm); if (nm %in% names(b)) unname(b[nm]) else 0 }, numeric(1))
  out <- b["x_ex"] + fam_term + ifelse(d$tier_f == "mid", b["x_ex:tier_fmid"], 0) +
    ifelse(d$tier_f == "top", b["x_ex:tier_ftop"], 0) +
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
cat("\nby size of the excess:\n"); print(by_size)
cat("\nby tier class of the shocked race (3-class, with controls; beta_raw = plain slope):\n")
print(merge(by_tier, by_tier_raw[, .(tier_class, beta_raw)], by = "tier_class")[order(-n)])

cal$race_shock <- list(beta = overall$beta, beta_se = overall$se,
                       by_tier = by_tier[, .(tier_class, beta, se, n)],
                       by_race = by_race[, .(race_key, beta)],
                       interaction_coefficients = co[grepl("^x_ex", rownames(co)), c("Estimate", "Std. Error")],
                       by_gap = by_gap,
                       by_family = by_fam[, .(family, beta, se, n)],
                       expected = expected, gap_days = GAP, min_cell = MINC,
                       fitted_at = Sys.time(), pairs = nrow(p),
                       tier_class_vocab = "3class_2026-09-16_TEST")
saveRDS(cal, file.path(OUT, DST))
say("wrote %s with $race_shock (beta %.4f, %d family rows, %s expected cells)", DST, overall$beta, nrow(by_fam), format(nrow(expected), big.mark = ","))
say("EXPERIMENTAL FILE, not wired to anything. Compare against the deployed race_shock before deciding to compose/promote.")
