# Binned, evidence-shrunk recalibration of p_medal -- the middle ground
# between the two failed recalibration attempts tonight.
#
# WHAT THE FIRST TWO TAUGHT. Isotonic (check_isotonic_recal_gain.R): fully
# flexible, free-form monotone curve, OVERFIT the 678-race fit window and
# made things worse (T1 +0.53%, throw +1.19%, Javelin W +1.68%, all
# significant). Platt (check_platt_recal_gain.R): a single global logistic
# transform, found essentially the identity (a=0.012, b=1.004) -- there is no
# smooth aggregate over/under-confidence trend to correct, consistent with
# the earlier finding that TOTAL medal probability handed out already
# matches medals won. Together these bracket the problem: the distortion is
# LOCAL (specific probability bands) and NON-MONOTONIC-SHAPED (under- then
# over-confident), which a single global transform structurally cannot see,
# but a fully free curve cannot estimate reliably from 678 races either.
#
# THE MIDDLE GROUND: bin p_medal into deciles, compute observed-vs-predicted
# rate per bin on the FIT window, then SHRINK each bin's correction toward
# the identity (no correction) by evidence -- same empirical-Bayes pattern as
# fit_family_pool_offsets.R, pseudo-count blending toward a prior instead of
# either trusting the raw bin mean (isotonic's failure mode) or forcing one
# global shape (Platt's). PSEUDO_N controls how much evidence a bin needs
# before its correction is trusted.
#
# OUT-OF-SAMPLE, same discipline as the other two: fit on [FIT_LO,
# FIT_HOLDOUT), apply unchanged to >= FIT_HOLDOUT.
#
# ANCHORS: A1 fit/test disjoint. A2 map non-trivial (else nothing to test).
# A3 PSEUDO_N=Inf must reproduce the identity exactly (shrinks everything to
# raw p_medal, zero correction) -- a control on the shrinkage machinery
# itself, not just the recalibration idea.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
FIT_HOLDOUT <- as.Date(Sys.getenv("CITIUS_BIN_HOLDOUT", "2022-01-01"))
FIT_LO      <- as.Date(Sys.getenv("CITIUS_BIN_FIT_LO", "2016-01-01"))
ARM         <- Sys.getenv("CITIUS_BIN_ARM", "backtest_tierctrl.rds")
PSEUDO_N    <- as.numeric(Sys.getenv("CITIUS_BIN_PSEUDO_N", "30"))
N_BINS      <- 10L
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | fit [%s, %s) | test >= %s | pseudo_n %.0f",
    ARM, format(FIT_LO), format(FIT_HOLDOUT), format(FIT_HOLDOUT), PSEUDO_N)

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id), p_medal, p_gold)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                         hit, hit_medal, merged)],
           by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite"]

fit <- d[date >= FIT_LO & date < FIT_HOLDOUT]
tst <- d[date >= FIT_HOLDOUT]
stopifnot("A1: fit and test races overlap" =
            length(intersect(unique(fit$race_id), unique(tst$race_id))) == 0)
say("fit: %s rows / %s races | test: %s rows / %s races",
    format(nrow(fit),big.mark=","), format(uniqueN(fit$race_id),big.mark=","),
    format(nrow(tst),big.mark=","), format(uniqueN(tst$race_id),big.mark=","))
stopifnot("fit population too small" = nrow(fit) >= 500)

# Fit: decile bins on p_medal, observed rate per bin, shrunk toward the bin's
# OWN mean predicted p (= identity/no correction) by evidence.
brks <- quantile(fit$p_medal, seq(0, 1, length.out = N_BINS + 1), na.rm = TRUE)
brks[1] <- -Inf; brks[length(brks)] <- Inf
fit[, bin := cut(p_medal, unique(brks), include.lowest = TRUE)]
bin_tbl <- fit[, .(n = .N, mean_pred = mean(p_medal), observed = mean(hit_medal)), by = bin]
bin_tbl[, w := n / (n + PSEUDO_N)]
bin_tbl[, shrunk := w * observed + (1 - w) * mean_pred]
say("\nbin table (fit window):")
print(bin_tbl[order(mean_pred)])

bin_edges <- unique(brks)
recal <- function(p) {
  b_idx <- cut(p, bin_edges, include.lowest = TRUE, labels = FALSE)
  bt <- bin_tbl[order(mean_pred)]  # same order as cut() levels since brks is increasing
  out <- bt$shrunk[b_idx]
  out[is.na(out)] <- p[is.na(out)]  # fallback: no correction if binning fails
  pmin(pmax(out, 1e-4), 1 - 1e-4)
}
adj_range <- range(recal(fit$p_medal) - fit$p_medal)
say("\nbinned-shrunk adjustment range on fit data: [%.4f, %.4f]", adj_range[1], adj_range[2])
stopifnot("A2: map is trivially close to the identity -- nothing to correct" =
            diff(range(adj_range)) > 0.01 || max(abs(adj_range)) > 0.01)

# A3: PSEUDO_N = Inf must reproduce raw p_medal exactly (w -> 0 everywhere).
bin_tbl_inf <- copy(bin_tbl)[, w := 0][, shrunk := mean_pred]
stopifnot("A3: infinite pseudo-n does not collapse to using mean_pred (control check)" =
            all(abs(bin_tbl_inf$shrunk - bin_tbl$mean_pred) < 1e-9))

hist <- deployed_history(OUT, events = unique(tst$event_id), from = min(tst$date) - 3650, to = max(tst$date))
hist <- hist[!is.na(perf) & !is.na(date)]; hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date); g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- tst[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last", .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
tst <- merge(tst, m[, .(race_id, athlete_id, l5)], by = c("race_id", "athlete_id"))[!is.na(l5)]
tst[, p_medal_recal := recal(p_medal)]

reg2 <- as.data.table(citius_events())[, .(event_id, orientation)]
evs <- as.data.table(deployed_calibration(OUT)$events)[calibrated %in% TRUE, .(event_id, sigma_within)]
tst <- merge(tst, reg2, by = "event_id"); tst <- merge(tst, evs, by = "event_id")
sim_rows <- rbindlist(lapply(split(tst, tst$race_id), function(r) {
  ab <- data.table(athlete_id = r$athlete_id, event_id = r$event_id[1], ability = r$l5, sigma = r$sigma_within)
  mp <- medal_probs(simulate_event(ab, n_sims = 4000L, condition_sd = 0, seed = 11L))
  data.table(race_id = r$race_id[1], athlete_id = mp$athlete_id, b_medal = mp$p_medal)
}))
tst <- merge(tst, sim_rows, by = c("race_id", "athlete_id"))

report <- function(dd, label) {
  if (uniqueN(dd$race_id) < 15) { say("%s: too few races (%d), skipped", label, uniqueN(dd$race_id)); return(invisible(NULL)) }
  a  <- dd[, .(v = mean((p_medal        - hit_medal)^2)), by = race_id]
  ar <- dd[, .(v = mean((p_medal_recal  - hit_medal)^2)), by = race_id]
  bl <- dd[, .(v = mean((b_medal        - hit_medal)^2)), by = race_id]
  m1 <- merge(a, bl, by = "race_id"); t1 <- t.test(m1$v.x, m1$v.y, paired = TRUE)
  m2 <- merge(ar, bl, by = "race_id"); t2 <- t.test(m2$v.x, m2$v.y, paired = TRUE)
  m3 <- merge(a, ar, by = "race_id"); t3 <- t.test(m3$v.y, m3$v.x, paired = TRUE)
  cat(sprintf("\n---- %s (%d races) ----\n", label, uniqueN(dd$race_id)))
  say("  raw       vs last-5: %+.2f%% p=%.3g", 100*(mean(m1$v.x)-mean(m1$v.y))/mean(m1$v.y), t1$p.value)
  say("  recal     vs last-5: %+.2f%% p=%.3g", 100*(mean(m2$v.x)-mean(m2$v.y))/mean(m2$v.y), t2$p.value)
  say("  recal vs raw (isolates the recalibration): %+.2f%% p=%.3g (negative = recal HELPS)",
      100*(mean(m3$v.y)-mean(m3$v.x))/mean(m3$v.x), t3$p.value)
}
cat("\n================ T1 AGGREGATE ================\n")
report(tst, "T1 all")
cat("\n================ THROW ONLY ================\n")
report(tst[family == "throw"], "throw")
