# Predict a combined-event score by SIMULATING the ten marks, not by rating the
# points total as if it were one event.
#
# WHY. A decathlon score is a deterministic function of ten marks. Rating the
# total directly discards that, and the cost is measurable: the combined family
# is the worst in every cross-event experiment (Heptathlon M -1.949 pp) because
# a points total correlates moderately with a dozen events and borrows from all
# of them at once. Simulating from components borrows at the right level - a
# decathlete's 100m IS a 100m, and it is already rated as one.
#
# WHAT THIS PRODUCES. For every athlete with component ratings, a DISTRIBUTION
# over the points total: expected score, spread, and percentiles. A points total
# cannot give a range; this can, and the range is what a medal projection needs.
#
# THE DAY EFFECT, and it is not a detail. Ten independent draws would understate
# the spread of real totals badly, because a decathlon happens on one weekend and
# an athlete has one weekend's form: a good day lifts all ten. A shared factor
# across the ten draws models that, and its size is FITTED to reproduce the
# observed spread of actual totals rather than guessed - see the calibration
# block, which is the part of this script that can fail loudly.
#
# THIS DOES NOT REPLACE THE MEASURED TOTAL. An athlete who scored 8,500 did that,
# whatever their components predicted. The simulated score is a PRIOR; the actual
# total is evidence. Both are written, and which serves the ranking better is a
# question for check_combined_ranking.R, not an assumption made here.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "ce_scoring.R"))
D     <- here::here("citiusdata", "data")
# FOLLOW THE DEPLOYED ARM, not the arm that happened to be current the day this
# was written. This defaulted to "base4" - an 18 August development arm - while
# form_display_marks.R blended the resulting simulation into the PUBLISHED
# combined-event ranking. Nothing failed: the file existed, the blend fired, and
# the log said the feature was on, so the decathlon ranking was quietly built
# from component ratings two engine promotions out of date. The tag is recorded
# in the output and checked by the display for exactly that reason.
TAG   <- Sys.getenv("STATE_TAG", "final")
NSIM  <- .env_int("CE_NSIM", "600")
SEED  <- .env_int("CE_SEED", "20260818")
DAYSD <- Sys.getenv("CE_DAY_SD", "")   # blank = fit it below
# IMPUTATION. Requiring a rating in every slot left only 46.7% of active
# decathletes simulable, which disqualifies the simulation as a ranking key
# whatever its accuracy - half the field would vanish from the table. But most
# of the missing are missing ONE or TWO slots, and a decathlete's other eight
# components say a great deal about the ninth: an athlete two thirds of a
# standard deviation above the field everywhere is not average at pole vault.
# So impute a missing slot from the athlete OWN average standing across the
# slots they do have, with inflated variance so the simulation knows it is
# guessing. Calibration is re-measured afterwards; if imputing breaks it, it is
# not worth the coverage.
MINFRAC <- .env_num("CE_MIN_SLOTS", "0.7")  # share of slots needed
IMPINF  <- .env_num("CE_IMPUTE_INFLATE", "2.5")
FILLHL  <- .env_num("CE_FILL_HALFLIFE", "730")  # days
# FILL FROM THE ATHLETE OWN MARKS BEFORE GUESSING. Measured 2026-08-18: guessing
# a missing slot from the athlete average standing added +300 points of bias to
# the decathlon and dropped coverage from 93% to 67%, because athletes go
# unrated precisely in the events they are WORST at - mean z of points -0.194
# for unrated slots against +0.353 for rated ones. Imputing at their average is
# optimistic by construction.
#
# The guess was never necessary: 95-99% of missing ratings have a real decathlon
# mark sitting in combined_components.parquet. The engine has no rating for them
# only because its corpus starts in 2020. So rate the slot from those marks
# instead - a time-weighted mean in perf space, with variance from the event own
# typical spread and inflated a little because it is not a fitted rating.

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family, orientation)]
st  <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
vcol <- intersect(c("v", "v_pre", "var"), names(st))[1]
stopifnot("state has no variance column" = !is.na(vcol))
setnames(st, vcol, "v")
st <- st[is.finite(R) & is.finite(v) & v > 0]
cat(sprintf("state %s: %s athlete-events\n", TAG, format(nrow(st), big.mark = ",")))

# --- ratings to marks ---------------------------------------------------------
# perf is oriented so HIGHER IS BETTER in both directions: -log(seconds) for
# track, +log(metres) for field. So a mark is exp(orientation * perf), and a
# shared "good day" is a positive shift in perf for every discipline at once.
tab <- merge(CE_TABLE, reg[, .(event_id, orientation)], by = "event_id")
stopifnot("a scoring slot has no orientation in the registry" = !anyNA(tab$orientation),
          "orientation must be -1 or +1" = all(tab$orientation %in% c(-1, 1)))
mark_of <- function(perf, o) exp(o * perf)

# ANCHOR: a rating must turn back into a plausible mark before anything is
# simulated from it. This is the check that catches an orientation flip, which
# would otherwise produce a 0.09-second 100m and a silently absurd score.
chk <- merge(st[, .(athlete_id, event_id, R)], tab, by = "event_id")
chk[, mk := mark_of(R, orientation)]
anchor <- chk[, .(n = .N, median_mark = round(stats::median(mk), 2)), by = .(event_id, kind)]
cat("\n=== anchor: do ratings turn back into plausible marks? ===\n")
print(anchor[order(event_id)][1:12])
# The probe must EXIST before its bound means anything. Written first as
# `anchor[event_id == "AT-100Metres-M", median_mark] %between% c(9, 14)`, which
# on a missing row yields numeric(0) -> logical(0), and stopifnot(logical(0))
# PASSES. The comment above calls this the check that catches an orientation
# flip; it would have caught nothing and written a garbage simulation. Same
# mechanism as all(logical(0)) being TRUE.
.anchor_ok <- function(ev, lo, hi) {
  v <- anchor[event_id == ev, median_mark]
  if (length(v) != 1L || !is.finite(v))
    stop(sprintf("anchor event %s is absent from the state - cannot verify orientation", ev))
  isTRUE(v >= lo && v <= hi)
}
stopifnot(
  "100m ratings do not produce ~9-14 second marks - orientation is wrong" =
    .anchor_ok("AT-100Metres-M", 9, 14),
  "shot put ratings do not produce ~8-24 metre marks" =
    .anchor_ok("AT-ShotPut-M", 8, 24),
  "1500m ratings do not produce ~190-400 second marks" =
    .anchor_ok("AT-1500Metres-M", 190, 400))
cat("all three anchors pass (each verified present, not merely unfalsified)\n")

# --- a rating for every slot the athlete has ever contested in a combined event
# perf = orientation * log(mark), time-weighted so recent marks dominate. This is
# the athlete OWN evidence, not a guess about them.
IMPPEN <- .env_num("CE_IMPUTE_PENALTY", "-0.55")
mkraw <- setDT(read_parquet(file.path(D, "combined_components.parquet"),
                            col_select = c("ce", "athlete_id", "event_id", "mark", "tdate")))
mkraw[, athlete_id := as.character(athlete_id)]
mkraw <- merge(mkraw, tab[, .(ce, event_id, orientation)], by = c("ce", "event_id"))
mkraw <- mkraw[is.finite(mark) & mark > 0]
ASOFM <- max(mkraw$tdate)
mkraw[, w := 2^(-as.numeric(ASOFM - tdate) / FILLHL)]
mkraw[, pf := orientation * log(mark)]
MK <- mkraw[, .(R_fill = sum(w * pf) / sum(w), marks = .N), by = .(ce, athlete_id, event_id)]
cat(sprintf("fill-from-marks table: %s athlete-slots across %d combined events (half-life %.0f d)\n",
            format(nrow(MK), big.mark = ","), uniqueN(MK$ce), FILLHL))
rm(mkraw); invisible(gc())

# --- who can be simulated -----------------------------------------------------
sim_one_ce <- function(CE, day_sd) {
  slots <- tab[ce == CE]
  s <- merge(st[, .(athlete_id, event_id, R, v, n_eff)], slots, by = "event_id")
  if (!nrow(s)) return(NULL)
  # --- impute missing slots from the athlete own standing ---------------------
  # event level and spread, over everyone rated in that slot
  lev <- s[, .(mu = mean(R), sg = stats::sd(R), vmed = stats::median(v)), by = event_id]
  s <- merge(s, lev, by = "event_id")
  s[, zi := fifelse(is.finite(sg) & sg > 0, (R - mu) / sg, 0)]
  have <- s[, .(got = .N, zbar = mean(zi)), by = athlete_id]
  keep <- have[got >= ceiling(MINFRAC * nrow(slots))]
  if (!nrow(keep)) return(NULL)
  s <- s[athlete_id %chin% keep$athlete_id]
  s[, imputed := FALSE]
  miss <- merge(CJ(athlete_id = keep$athlete_id, event_id = slots$event_id),
                s[, .(athlete_id, event_id, seen = TRUE)],
                by = c("athlete_id", "event_id"), all.x = TRUE)[is.na(seen)]
  if (nrow(miss)) {
    miss <- miss[, .(athlete_id, event_id)]
    # 1. fill from the athlete OWN combined-event marks wherever they exist
    fill <- merge(miss, MK[ce == CE, .(athlete_id, event_id, R_fill, marks)],
                  by = c("athlete_id", "event_id"), all.x = TRUE)
    fill <- merge(fill, lev, by = "event_id")
    fill <- merge(fill, keep[, .(athlete_id, zbar)], by = "athlete_id")
    fill[, from_marks := is.finite(R_fill)]
    # 2. fall back to the athlete average standing only where no mark exists,
    #    and PENALISE it: unrated slots average -0.55 sd below the athlete own
    #    level, which is why the unpenalised version ran 300 points hot
    fill[, R := fifelse(from_marks, R_fill, mu + (zbar + IMPPEN) * sg)]
    fill[, `:=`(v = vmed * fifelse(from_marks, 1.5, IMPINF), n_eff = 0,
                zi = (R - mu) / sg, imputed = TRUE)]
    fill[, c("R_fill", "marks", "zbar", "from_marks") := NULL]
    fill <- merge(fill, slots, by = "event_id")
    stopifnot("filled rows do not match the real ones column for column" =
                setequal(names(fill), names(s)))
    s <- rbind(s, fill[, names(s), with = FALSE])
  }
  s <- s[, .SD[1], by = .(athlete_id, event_id)]
  setorder(s, athlete_id, event_id)
  stopifnot("imputation did not produce a full slate for every athlete" =
              s[, .N, by = athlete_id][, all(N == nrow(slots))])
  ath <- unique(s$athlete_id); k <- nrow(slots)
  M  <- matrix(s$R,    nrow = length(ath), byrow = TRUE)
  SD <- matrix(sqrt(s$v), nrow = length(ath), byrow = TRUE)
  ord <- s[athlete_id == ath[1], event_id]
  sl  <- slots[match(ord, slots$event_id)]
  tots <- matrix(0, nrow = length(ath), ncol = NSIM)
  for (i in seq_len(NSIM)) {
    day  <- stats::rnorm(length(ath), 0, day_sd)          # one form for the weekend
    perf <- M + SD * matrix(stats::rnorm(length(M)), nrow = length(ath)) + day
    mk   <- exp(sweep(perf, 2, sl$orientation, "*"))
    pts  <- matrix(ce_points(as.vector(mk),
                             rep(sl$kind, each = length(ath)),
                             rep(sl$A, each = length(ath)),
                             rep(sl$B, each = length(ath)),
                             rep(sl$C, each = length(ath))), nrow = length(ath))
    tots[, i] <- rowSums(pts)
  }
  data.table(ce = CE, athlete_id = ath,
             sim_mean = round(rowMeans(tots)),
             sim_sd   = round(apply(tots, 1, stats::sd), 1),
             sim_p05  = round(apply(tots, 1, stats::quantile, .05)),
             sim_p50  = round(apply(tots, 1, stats::median)),
             sim_p95  = round(apply(tots, 1, stats::quantile, .95)),
             min_n_eff = s[, min(n_eff), by = athlete_id]$V1,
             imputed_slots = s[, sum(imputed), by = athlete_id]$V1,
             filled_slots  = s[, sum(imputed & n_eff == 0 & v < vmed * IMPINF),
                               by = athlete_id]$V1)
}

# --- calibrate the day effect -------------------------------------------------
# Fit day_sd so the simulated spread matches the spread actually observed in
# repeat performances. Ten independent draws understate it; too large a day
# effect overstates it. The target is the within-athlete sd of real totals.
comp <- setDT(read_parquet(file.path(D, "combined_components.parquet")))
obs <- unique(comp[complete == TRUE, .(tid, ce, athlete_id, stored_points)])
rep_sd <- obs[, .N, by = .(ce, athlete_id)][N >= 3]
tgt <- merge(obs, rep_sd[, .(ce, athlete_id)], by = c("ce", "athlete_id"))[
  , .(sd = stats::sd(stored_points)), by = .(ce, athlete_id)][
  , .(target_sd = stats::median(sd), athletes = .N), by = ce]
cat("\n=== target spread: within-athlete sd of real totals ===\n")
print(tgt)

set.seed(SEED)
if (nzchar(DAYSD)) {
  DAY <- setNames(rep(as.numeric(DAYSD), length(CE_EVENTS)), CE_EVENTS)
  cat(sprintf("\nday effect fixed at %s by CE_DAY_SD\n", DAYSD))
} else {
  cat("\n=== fitting the day effect per combined event ===\n")
  DAY <- numeric(0)
  for (CE in CE_EVENTS) {
    t_i <- tgt[ce == CE, target_sd]
    if (!length(t_i) || !is.finite(t_i)) {
      # every other branch of this loop prints a line; this one used to fall
      # through in silence, leaving a combined event with no day effect at all
      cat(sprintf("  %-18s NO TARGET SPREAD (needs athletes with 3+ totals) -\n", CE))
      cat("                     day_sd forced to 0, so its simulated range is TOO NARROW\n")
      DAY[CE] <- 0; next
    }
    grid <- c(0, 0.004, 0.008, 0.012, 0.016, 0.022, 0.030)
    got <- vapply(grid, function(g) {
      x <- sim_one_ce(CE, g); if (is.null(x)) NA_real_ else stats::median(x$sim_sd)
    }, numeric(1))
    best <- grid[which.min(abs(got - t_i))]
    DAY[CE] <- best
    cat(sprintf("  %-18s target sd %6.1f | simulated %s | chose day_sd %.3f\n",
                CE, t_i, paste(round(got), collapse = " "), best))
    if (best == max(grid))
      cat("    NOTE: chose the top of the grid, so this is not an interior optimum\n")
  }
}

set.seed(SEED)
res <- rbindlist(lapply(CE_EVENTS, function(CE) sim_one_ce(CE, DAY[[CE]])), fill = TRUE)
stopifnot("nothing was simulated" = nrow(res) > 0)
res[, day_sd := DAY[ce]]
cat(sprintf("\nsimulated %s athlete-combined-events over %d draws\n",
            format(nrow(res), big.mark = ","), NSIM))
print(res[, .(athletes = .N, median_sim = as.numeric(stats::median(sim_mean)),
              median_sd = as.numeric(stats::median(sim_sd))), by = ce][order(ce)])

# --- does it agree with what athletes actually score? -------------------------
best <- obs[, .(actual_best = max(stored_points), actual_mean = mean(stored_points),
                perfs = .N), by = .(ce, athlete_id)]
j <- merge(res, best, by = c("ce", "athlete_id"))
cat(sprintf("\n=== agreement with real totals (%s athletes with both) ===\n",
            format(nrow(j), big.mark = ",")))
print(j[, .(athletes = .N,
            cor_mean = round(stats::cor(sim_mean, actual_mean), 3),
            cor_best = round(stats::cor(sim_mean, actual_best), 3),
            bias_vs_mean = round(mean(sim_mean - actual_mean)),
            mae = round(mean(abs(sim_mean - actual_mean)))), by = ce][order(ce)])
cat("\ncoverage: how often a real total lands inside the simulated 5-95% band\n")
jj <- merge(obs, res, by = c("ce", "athlete_id"))
print(jj[, .(performances = .N,
             inside = round(100 * mean(stored_points >= sim_p05 &
                                       stored_points <= sim_p95), 1)), by = ce][order(ce)])
cat("A calibrated simulation covers about 90%. Far below means the spread is too\n")
cat("narrow; far above means it is too wide to discriminate between athletes.\n")
cat("NOTE: this is in-sample - component ratings include the very performances\n")
cat("being predicted. It tests coherence, not forecasting skill.\n")

res[, state_tag := TAG]   # travels with the data so the display can check it
f <- file.path(D, "combined_simulated.parquet")
write_parquet(res, f)
cat(sprintf("\nwrote %s (%s rows)\n", basename(f), format(nrow(res), big.mark = ",")))
