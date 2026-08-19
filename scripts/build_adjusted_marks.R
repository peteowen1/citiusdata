# What a mark is WORTH: the raw performance corrected for wind and for venue.
#
# WHY. A +3.5 long jump and a -1.2 long jump currently enter the model as equals,
# and a 5000m in Addis Ababa is compared directly with one in Monaco. Neither is
# a fair comparison, and both are correctable from what we already measure.
#
# TWO CORRECTIONS, both measured rather than assumed:
#   WIND   - a GAM smooth per event (check_wind_effect.R). Non-linear and
#            asymmetric: 0.049 s per m/s at no wind in the men's 100m, 0.071
#            into a 2 m/s headwind, 0.034 at a 4 m/s tailwind, and a headwind
#            costs about 1.5x what the same tailwind returns.
#   VENUE  - the average within-athlete deviation at a place, tier-controlled and
#            shrunk toward zero by sample size. Validated against known elevation
#            (check_venue_effect.R): correlation -0.851 for distance events, and
#            high-altitude venues run about 0.9% slower. The sprint drag benefit
#            is real physics but ~0.01 s over 100m, below the noise here, so the
#            venue term does the work it can measure and no more.
#
# WHAT IS DELIBERATELY NOT CORRECTED. The engine's own race shock. It already
# removes what a field shared on the day, and wind is part of that - so applying
# both would double-count. The intended order is: correct the MARK here, then let
# the shock estimate whatever conditions remain. That is why this writes a
# corpus-level artefact rather than changing the engine.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D      <- here::here("citiusdata", "data")
VKAP   <- as.numeric(Sys.getenv("ADJ_VENUE_KAPPA", "400"))  # shrinkage on venue n
MINA   <- as.integer(Sys.getenv("ADJ_MIN_ATH", "3"))
# Same argument as the wind fit: the venue effect is estimated from marks the
# engine is scored on, so cap the ESTIMATION window while still applying the
# result everywhere.
VMAXY  <- as.integer(Sys.getenv("ADJ_MAX_YEAR", "9999"))
WCURVE <- Sys.getenv("ADJ_WIND_CURVES", "wind_effect_curves.json")
AOUT   <- Sys.getenv("ADJ_OUT", "adjusted_marks.parquet")
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family,
                                                  orientation, unit)]

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","race_key","mark","perf",
                                        "date","wind","legal","indoor","scoreable",
                                        "venue_city","tier","place","comp_name")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf) & is.finite(mark) & mark > 0]
# An inner join drops any corpus event_id absent from the registry, silently and
# partially. This verse has lost whole events that way before - the 20km walk and
# the half marathon. Count it.
.n_pre <- nrow(c0)
c0 <- merge(c0, reg, by = "event_id")
if (nrow(c0) < .n_pre)
  stop(sprintf("%s performance(s) carry an event_id the registry does not have",
               format(.n_pre - nrow(c0), big.mark = ",")))
cat(sprintf("scoreable performances: %s over %d events\n",
            format(nrow(c0), big.mark = ","), uniqueN(c0$event_id)))

# --- 1. WIND ------------------------------------------------------------------
wf <- file.path(D, WCURVE)
stopifnot("wind curves missing - run check_wind_effect.R first" = file.exists(wf))
wc <- as.data.table(jsonlite::fromJSON(wf)$curves)
stopifnot("wind curve file has no rows" = nrow(wc) > 0)
# delta_pct IS A PERF QUANTITY, NOT A MARK QUANTITY. check_wind_effect.R computes
# it as 100 * (exp(eff) - 1) directly from `eff`, the oriented perf effect, so it
# is always positive when the wind helped. It is NOT the % change in the reported
# `mark` column: for track a helping wind LOWERS the time, so the mark change runs
# the opposite sign. Read it as perf and the inversion below needs no orientation;
# read it as a mark and orientation looks necessary - which is precisely the flip
# described next.
#
# SIGN, and it was wrong first time. check_wind_effect.R computes
#     delta_pct = 100 * (exp(eff) - 1),  eff = the PERF the wind added
# with no orientation applied, because perf is already oriented so that higher is
# better in both directions. So inverting eff back out needs no orientation
# either: eff = log1p(delta_pct/100), for track and field alike. Applying
# orientation a second time here flipped the correction for TRACK events, adding
# the wind effect instead of removing it - which showed up immediately as
# within-athlete scatter RISING 6.7% in the sprints and 2.4% in the hurdles while
# the jumps, same correction, improved 1.3%. Track versus field was the tell.
wc[, wind_perf := log1p(delta_pct / 100)]
wind_of <- function(ev, w) {
  g <- wc[event_id == ev][order(wind)]
  if (!nrow(g)) return(rep(0, length(w)))
  stats::approx(g$wind, g$wind_perf, xout = pmin(pmax(w, min(g$wind)), max(g$wind)),
                rule = 2)$y
}
c0[, wind_adj := 0]
for (EV in unique(wc$event_id)) {
  i <- c0[, which(event_id == EV & is.finite(wind) & (is.na(indoor) | indoor == FALSE))]
  if (length(i)) set(c0, i, "wind_adj", wind_of(EV, c0$wind[i]))
}
cat(sprintf("wind corrected: %s performances across %d events\n",
            format(sum(c0$wind_adj != 0), big.mark = ","), uniqueN(wc$event_id)))
stopifnot("no performance received a wind correction" = sum(c0$wind_adj != 0) > 1000)

# --- 2. VENUE -----------------------------------------------------------------
# Estimated per venue and FAMILY, because altitude pushes sprints and distance in
# opposite directions and a single per-venue number would average them away.
v <- c0[!is.na(venue_city) & nzchar(venue_city) & (is.na(indoor) | indoor == FALSE)]
v[, n_ath := .N, by = .(athlete_id, event_id)]
v <- v[n_ath >= MINA]
v <- v[as.integer(format(date, "%Y")) <= VMAXY]
cat(sprintf("venue effects estimated up to %s (%s marks)\n",
            ifelse(VMAXY > 9000, "all years", as.character(VMAXY)),
            format(nrow(v), big.mark = ",")))
v[, y := perf - wind_adj]                                  # wind removed first
v[, y := y - mean(y), by = .(athlete_id, event_id)]        # then ability
v[is.na(tier) | !nzchar(tier), tier := "unknown"]
# then meet occasion - but LEAVE THE VENUE ITSELF OUT of the tier mean. A plain
# `y - mean(y), by = .(tier, family)` lets a venue that dominates a tier demean
# away its own effect: Zurich is 50-56% of every family's Diamond League Final
# rows, so roughly half its true venue effect was being absorbed into the
# "occasion" term before venue_adj was ever estimated. Measured, not hypothetical.
v[, `:=`(t_sum = sum(y), t_n = .N), by = .(tier, family)]
v[, `:=`(vt_sum = sum(y), vt_n = .N), by = .(tier, family, venue_city)]
v[, others_n := t_n - vt_n]
# with too few other venues in the tier the leave-one-out mean is noisier than
# the plain one, so fall back rather than trade bias for variance
v[, t_mean := fifelse(others_n >= 30, (t_sum - vt_sum) / pmax(others_n, 1), t_sum / t_n)]
cat(sprintf("tier demeaning: %.1f%% of rows use a leave-one-out tier mean\n",
            100 * mean(v$others_n >= 30)))
v[, y := y - t_mean]
v[, c("t_sum", "t_n", "vt_sum", "vt_n", "others_n", "t_mean") := NULL]
ve <- v[, .(n_v = .N, raw_eff = mean(y)), by = .(venue_city, family)]
ve[, venue_adj := raw_eff * n_v / (n_v + VKAP)]            # shrink small venues
cat(sprintf("venue effects: %s venue-family cells, shrinkage kappa %.0f\n",
            format(nrow(ve), big.mark = ","), VKAP))
print(ve[order(-abs(venue_adj))][1:8, .(venue_city, family, n_v,
                                        raw = round(raw_eff, 4),
                                        shrunk = round(venue_adj, 4))])
c0 <- merge(c0, ve[, .(venue_city, family, venue_adj)],
            by = c("venue_city", "family"), all.x = TRUE)
c0[!is.finite(venue_adj), venue_adj := 0]

# --- 3. the adjusted mark -----------------------------------------------------
c0[, adj_perf := perf - wind_adj - venue_adj]
c0[, adj_mark := exp(fifelse(orientation == -1, -adj_perf, adj_perf))]
c0[, adj_delta := adj_mark - mark]

cat("\n=== how big are the corrections? ===\n")
print(c0[wind_adj != 0 | venue_adj != 0,
         .(performances = .N,
           median_abs_change = round(stats::median(abs(adj_delta)), 3),
           p95_abs_change = round(stats::quantile(abs(adj_delta), .95), 3)),
         by = .(family, unit)][order(-performances)])

# --- 4. DOES IT HELP? ----------------------------------------------------------
# The test that matters: if these corrections remove real noise, an athlete's
# marks should be MORE consistent after adjustment. If within-athlete scatter
# does not fall, the corrections are moving numbers around without adding
# information - and a smaller scatter cannot be got by accident, because the
# corrections know nothing about which athlete produced which mark.
t <- c0[, n_ath := .N, by = .(athlete_id, event_id)][n_ath >= 4]
sc <- t[, .(sd_raw = stats::sd(perf), sd_adj = stats::sd(adj_perf)),
        by = .(athlete_id, event_id, family)]
sc <- sc[is.finite(sd_raw) & is.finite(sd_adj)]
cat(sprintf("\n=== within-athlete scatter, %s athlete-events with 4+ marks ===\n",
            format(nrow(sc), big.mark = ",")))
res <- sc[, .(athlete_events = .N,
              sd_raw = round(mean(sd_raw), 5), sd_adj = round(mean(sd_adj), 5),
              pct_change = round(100 * (mean(sd_adj) / mean(sd_raw) - 1), 2),
              improved = round(100 * mean(sd_adj < sd_raw), 1)), by = family]
setorder(res, pct_change)
print(res)
cat("\npct_change negative = the athlete looks MORE consistent once corrected,\n")
cat("which is what removing a real nuisance effect does. improved is the share\n")
cat("of individual athletes who got tighter, so one big case cannot carry it.\n")
overall <- sc[, .(sd_raw = mean(sd_raw), sd_adj = mean(sd_adj))]
cat(sprintf("\nOVERALL: %.5f -> %.5f (%+.2f%%)\n", overall$sd_raw, overall$sd_adj,
            100 * (overall$sd_adj / overall$sd_raw - 1)))

out <- c0[, .(race_key, athlete_id, event_id, discipline, sex, family, date,
              comp_name, venue_city, place, mark, adj_mark, adj_delta,
              wind, wind_adj, venue_adj, legal, unit)]
f <- file.path(D, AOUT)
write_parquet(out, f)
cat(sprintf("\nwrote %s (%s performances)\n", basename(f), format(nrow(out), big.mark = ",")))
