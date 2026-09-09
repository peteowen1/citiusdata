# Fit the per-event parameter tables and write them as ONE artefact.
#
# Four parameters now accept a per-event table: `half_life`, `races_half_life`,
# `trim_tactical` and `context_scale`. Each is fitted the same way -- grid
# argmin per event on the FIT YEARS, then shrunk twice: the event toward its
# family, the family toward the global value, each in proportion to its own
# evidence.
#
#   family_f = (kappa_f * global   + n_f * family_raw_f) / (kappa_f + n_f)
#   event_e  = (kappa_e * family_f + n_e * event_raw_e)  / (kappa_e + n_e)
#
# The kappas come from each parameter's own held-out sweep
# (diagnostics/marks_hier_params.R), choosing the setting that improved pooled
# error WITHOUT losing an event. They are deliberately strong: the average event
# moves only a little from its family, which is what stops an event fitted on a
# single row -- the men's weight throw, whose raw fit sits at the top of the
# grid on one observation -- from doing damage.
#
# WHY ONE ARTEFACT. Four separate files invite three of them being current and
# one stale, with nothing to say which. A single table with every parameter as a
# column is checked, versioned and passed as a unit, and the stamp records which
# fit produced it.
#
# NOT A PROMOTION. This writes the tables; nothing reads them until an arm or
# `_deployed.R` points at them, and every one of these parameters moves
# `ability`, so the medal arm gates them all.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/fit_event_params.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
# WAC-class weights, so the parameters are fitted for the races we care about.
# Same table the scorecard uses; scripts/_score_weights.R.
source(here::here("citiusdata", "scripts", "_score_weights.R"))
OUT   <- here::here("citiusdata", "data")
# THE DEFAULT MUST BE THE CACHE THE SHIPPED ARTEFACT IS BUILT FROM.
#
# It used to default to `marks_lab_cache_2020` while the deployed
# `event_params.rds` was fitted on `marks_lab_cache_t1t2` -- 1.7M rows and 78
# events against 12.8M rows and 86 events. A plain re-run therefore silently
# produced a DIFFERENT table from the validated one: 12 athletics events lost
# their fit entirely (AT-1000Metres-M, AT-600Metres-M, AT-5KilometresRoad-M and
# the walks among them) because the smaller cache has no rows for them. Nothing
# failed; the artefact just quietly got worse. Done exactly that on 2026-09-09.
#
# The name is echoed below with its size so the run says out loud which corpus
# it fitted on, and the stamp records it in the artefact.
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_t1t2"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
if (!dir.exists(CACHE)) cli::cli_abort(
  "Fit cache {.path {CACHE}} does not exist. Set {.envvar CITIUS_LAB_CACHE}.")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
# Size of the corpus this fit is about to run on, said before any of it is used:
# the cache is the single input that most changes the answer, and swapping it is
# invisible in every downstream artefact except the stamp.
say("cache %s: %s rows, %d events", basename(CACHE),
    format(nrow(pairs), big.mark = ","), data.table::uniqueN(pairs$event_id))
# THE CACHE'S `tactical` FLAG IS THE UNGATED ONE. It was built before
# estimate_ability() started gating the calibration's override by family, so it
# still marks every throw and every sprint. Fitting `trim_tactical` against it
# would produce values tuned to a flag the package no longer sets -- the trim
# calibrated to compensate for trimming that will not happen.
#
# Masked here rather than re-prepping the cache: the gate is a pure function of
# family, so applying it to the column is exactly equivalent and costs nothing.
pairs[, tactical := tactical & family %in% citius:::.CITIUS_TACTICAL_FAMILIES]
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))

# --- HOW MUCH SHOULD A T2 ROW COUNT WHEN FITTING? ---------------------------
# 0.037 was chosen to equalise RACE COUNT alone (1641/44944, T1's share of the
# combined total), reasoning that T2's 27x race-count advantage needed
# offsetting so it could not steer the answer. That reasoning double-counted a
# correction the WAC class weight was already making.
#
# Measured 2026-09-08 on the fit-years population: T1_elite's WAC-weighted mass
# is 186,421 against T2_strong's 120,482 -- T1 already outweighs T2 BEFORE any
# tier adjustment, despite having 19x fewer races, because T1 races are so much
# more densely championship-class (T1_elite is 5% of fit-year races and 97.66%
# of fit-year weight at the old 0.037; T2 contributed just 2.34%, close to
# nothing). A single T1_elite/OW row outweighed a T2_strong/F row 5,454:1.
# Stacking TIER_W on top of the WAC weight corrected an imbalance the WAC
# weight had already fixed on its own, and came close to nullifying the entire
# reason T2 was added -- 27x more races bought almost no stability.
#
# The knob is the weight a T2 row carries relative to a T1 row, on top of its
# WAC class weight:
#
#   0      T1 only. What the T1-only cache has always done.
#   1      every row's WAC weight taken at face value, T2's included. Measured
#          split: T1 60.7% / T2 39.3% of fit weight -- T1 still leads, as the
#          forecast target, but T2 has a real voice instead of a token one.
#
# SCORING IS UNAFFECTED by this knob and applies WAC class weight alone, never
# TIER_W, in either scoring mode (T1-only or CITIUS_SCORE_ALL_TIERS=1). A
# weight here only changes which rows INFORM the parameters.
#
# Inert on a T1-only cache, where every row is T1 and the weight is 1 throughout.
TIER_W <- suppressWarnings(as.numeric(Sys.getenv("CITIUS_LAB_TIER_WEIGHT", "1")))
if (!is.finite(TIER_W) || TIER_W < 0) {
  # SAY SO. A typo, stray whitespace or an accidentally-quoted value parses to
  # NA, silently becomes 1, and the run then looks identical to a deliberate
  # TIER_W=1 run in every log line -- including the "fit weights:" line below,
  # which would report 1.000 either way. _score_weights.R validates its own
  # CITIUS_SCORE_WEIGHTS strictly and stop()s on the same class of bad input;
  # two closely related weighting knobs written the same week should not fail in
  # opposite directions. Warned rather than fatal because 1 is a sane default
  # and this is a lab knob, not a shipping one. Added after review, 2026-09-09.
  .raw <- Sys.getenv("CITIUS_LAB_TIER_WEIGHT", "1")
  say("CITIUS_LAB_TIER_WEIGHT=%s is not a finite number >= 0; using 1", .raw)
  TIER_W <- 1
}
if (!"meet_tier" %in% names(test)) {
  test[, meet_tier := "T1_elite"]
  say("cache predates the meet_tier column; treating every row as T1")
}
# TWO WEIGHTS MULTIPLY HERE, and they answer different questions.
#   TIER_W       stops the 27x more numerous T2 races dominating a T1+T2 fit
#   WAC weight   makes an Olympic final count 10x a category F meet
# A row's weight is the product: a T2 club final counts 0.037 * 1, a T1 Olympic
# final 1 * 10. Fitting on the unweighted mean is what let today's parameters be
# chosen by races the project does not forecast.
test <- attach_score_weight(test, OUT)
test[, row_w := fifelse(meet_tier == "T1_elite", 1, TIER_W) * sw]
say("fit weights: %s T1 rows at 1.0, %s non-T1 rows at %.3f (effective n %.0f)",
    format(sum(test$meet_tier == "T1_elite"), big.mark = ","),
    format(sum(test$meet_tier != "T1_elite"), big.mark = ","),
    TIER_W, sum(test$row_w))

# grid, and the (kappa_family, kappa_event) each parameter earned on its sweep
#
# PRECISION_SCALE SWEPT AND WAS DROPPED, 2026-09-08 (marks_hier_params.R,
# CITIUS_HIER_PARAM=prec). Held-out beat count and pooled MAE were IDENTICAL
# to two decimals across every one of the 30 (kappa_family, kappa_event)
# combinations swept, and almost every event's raw per-event fit landed on
# the existing global default (0) already -- the flat, suspicious-agreement
# pattern this project treats as "no real signal", not a finding to force
# into a table. Left out of SPEC entirely rather than added at a constant 0,
# which would claim a per-event fit that was never actually made.
#
# PEAK_GAMMA SWEPT AND ADDED, same day, same tool (CITIUS_HIER_PARAM=peak).
# Unlike precision_scale this one moved: held-out beats 77 -> 78 of 82 and
# pooled MAE -6.98% -> -7.68% at (kappa_family=0, kappa_event=800), with real
# per-family spread (hurdles +0.5, distance -0.5, combined +1.5) and a genuine
# gain on Discus M, one of Pete's three named target events (held-out gap
# -3.10% -> -3.82%). kappa_family=0 means no family-level shrinkage at all --
# the sweep's own verdict, not a default.
SPEC <- list(
  context_scale   = list(grid = seq(0, 1.5, by = 0.25),               kap = c(5000, 1600), glob = fit$adj),
  trim_tactical   = list(grid = c(0, 0.1, 0.15, 0.25, 0.4),           kap = c(5000, 100),  glob = fit$trim),
  half_life       = list(grid = c(60, 90, 180, 270, 365, 540, 730, 1095), kap = c(5000, 400), glob = fit$hl),
  races_half_life = list(grid = c(2, 3, 5, 8, 12, 20, 40, Inf),       kap = c(5000, Inf),  glob = fit$rhl),
  peak_gamma      = list(grid = seq(-1.5, 1.5, by = 0.5),             kap = c(0, 800),     glob = 0))

hl_default <- function(fam) {
  v <- rep(fit$hl, length(fam)); hv <- unlist(DEPLOYED$hl_family)
  if (length(hv)) { i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
predict_at <- function(nm, vals) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  g <- function(x) if (identical(nm, x)) vals else rep(SPEC[[x]]$glob, nrow(pp))
  hlv <- if (identical(nm, "half_life")) vals else hl_default(pp$family)
  w <- pp$w_static * 0.5^(pp$age_days / hlv)
  rh <- g("races_half_life")
  w <- w * data.table::fifelse(is.finite(rh) & rh > 0, 0.5^(pp$.k / rh), 1)
  pk <- g("peak_gamma")
  if (any(pk != 0)) {
    # `:=` updates by reference IN PLACE, so row order stays aligned with `pp`
    # (and hence with `w`) -- a regroup-and-extract (`pp[, .(.q=...), by=pid]$.q`)
    # would silently misalign, the exact bug class .k/.rhl exist to avoid.
    # frank()'s default na.last=TRUE ranks an NA `perf` as the GROUP'S BEST
    # mark, not a visible failure -- an NA reaching here (this pipeline's own
    # convention is to filter it upstream, but that is an invariant, not a
    # guarantee) would silently corrupt the whole pid's peak-gamma weight.
    stopifnot("NA perf reaching peak_gamma rank" = !anyNA(pp$perf))
    pp[, .q := data.table::frank(perf, ties.method = "first") / .N, by = pid]
    w <- w * (pp$.q^pk)
    pp[, .q := NULL]
  }
  cs <- g("context_scale")
  p_use <- pp$perf_raw + cs * (pp$perf - pp$perf_raw)
  tv <- g("trim_tactical")
  keep <- !(pp$tactical & !is.na(pp$rk) & tv > 0 & pp$rk <= floor(pp$grp_n * tv))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  merge(merge(test, m[, .(athlete_id, event_id, month, pred)],
              by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
}

fit_one <- function(nm) {
  sp <- SPEC[[nm]]
  # WEIGHTED sum of absolute error, and a weighted count, so the argmin below is
  # the one the target population would choose rather than the one 27x more
  # numerous T2 rows would.
  curve <- rbindlist(lapply(sp$grid, function(v) {
    predict_at(nm, rep(v, nrow(pairs)))[date < SPLIT,
      .(v = v, sae = sum(row_w * abs(pred - act)), n = sum(row_w)),
      by = .(event_id, family)]
  }))
  ev <- curve[, .(sae = sum(sae), n = sum(n)), by = .(event_id, family, v)]
  ev <- ev[ev[, .I[which.min(sae / n)], by = event_id]$V1][, .(event_id, family, raw = v, n_e = n)]
  fm <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)]
  fm <- fm[fm[, .I[which.min(sae / n)], by = family]$V1][, .(family, fam_raw = v, n_f = n)]
  kf <- sp$kap[1]; ke <- sp$kap[2]
  fm[, fam := if (is.infinite(kf)) sp$glob else (kf * sp$glob + n_f * fam_raw) / (kf + n_f)]
  e <- merge(ev, fm[, .(family, fam)], by = "family", all.x = TRUE)
  e[is.na(fam), fam := sp$glob]
  e[, val := if (is.infinite(ke)) fam else (ke * fam + n_e * raw) / (ke + n_e)]
  say("%-16s global %-8s | family range %.3f to %.3f | event range %.3f to %.3f",
      nm, format(sp$glob), min(fm$fam), max(fm$fam), min(e$val), max(e$val))
  list(event = e[, .(event_id, family, val)], family = fm[, .(family, fam_raw, fam)])
}

res <- lapply(names(SPEC), fit_one); names(res) <- names(SPEC)
tab <- Reduce(function(a, b) merge(a, b, by = c("event_id", "family"), all = TRUE),
              lapply(names(SPEC), function(nm) {
                x <- copy(res[[nm]]$event); setnames(x, "val", nm); x
              }))
# Every event the registry knows, so a table cannot silently omit one and leave
# the caller's default in place without saying so.
reg <- as.data.table(citius_events())[, .(event_id, family)]
tab <- merge(reg, tab, by = c("event_id", "family"), all.x = TRUE)
# AN EVENT WITH NO FIT DATA INHERITS ITS FAMILY, NOT THE GLOBAL.
#
# Filling straight to the global was a real regression the moment this table was
# promoted (2026-09-09). `half_life`'s global is the marks-lab 180 days, so
# AT-HalfMarathonRaceWalk-M/W -- which have no rows in the fit cache -- went from
# the `hl_family` walk value of 730 to 180, a 4x cut, while every OTHER walk event
# in the same table fitted between 381 and 828. Nothing about those two events
# says "forget form four times faster than your siblings"; the absence of fit data
# says nothing at all, which is exactly when the parent estimate should be used.
#
# The family value is itself a fit (shrunk toward the global in proportion to its
# own evidence, above), so it is the honest parent. The global is only correct
# where the family has no fit either -- for a family with no fitted events there
# is genuinely nothing better to say.
#
# `fitted` records which rows are real fits so a consumer never has to
# reverse-engineer it from a value fingerprint, which is how this was found.
fam_val <- lapply(res, function(r) stats::setNames(r$family$fam, r$family$family))
tab[, fitted := !is.na(context_scale)]
n_miss <- sum(!tab$fitted)
if (n_miss) {
  say("%d of %d registry events had no fit data; filling from their family", n_miss, nrow(tab))
  for (nm in names(SPEC)) {
    idx <- which(is.na(tab[[nm]]))
    if (!length(idx)) next
    v <- unname(fam_val[[nm]][tab$family[idx]])
    n_fam <- sum(!is.na(v))
    v[is.na(v)] <- SPEC[[nm]]$glob
    set(tab, idx, nm, v)
    say("  %-16s %d from family, %d from the global %s",
        nm, n_fam, length(idx) - n_fam, format(SPEC[[nm]]$glob))
  }
  # Say WHICH events, by name: 36 of these are swim events nothing forecasts, and
  # a bare count cannot tell you that the other 4 are live athletics events.
  say("  unfitted events: %s", paste(tab[!(fitted)]$event_id, collapse = ", "))
}
# TRIM IS UNIDENTIFIABLE WHERE THE FLAG NEVER FIRES. It is only ever read when
# `tactical` is TRUE, and the family gate means that is never true for sprints,
# hurdles, jumps or throws. The fit therefore sees a flat error curve for those
# events and returns whichever grid point came first -- a number with no meaning
# that a reader would take for a finding, and one that would go live the moment
# the gate changed. Set them to the global value and say so.
never_tac <- setdiff(unique(tab$family), citius:::.CITIUS_TACTICAL_FAMILIES)
n_reset <- tab[family %in% never_tac, .N]
tab[family %in% never_tac, trim_tactical := SPEC$trim_tactical$glob]
say("trim_tactical reset to the global %.2f for %d events in %d families the gate never flags: %s",
    SPEC$trim_tactical$glob, n_reset, length(never_tac), paste(never_tac, collapse = ", "))

stopifnot("a parameter column is unpopulated" =
            all(vapply(names(SPEC), function(nm) all(is.finite(tab[[nm]]) | is.infinite(tab[[nm]])),
                       logical(1))))

# THE FILL'S OWN EVIDENCE, in the same run that performs it. An unfitted event
# whose family DID fit must not come out at the global -- that is the exact
# defect above, and a fill that silently no-ops looks identical to one that works.
chk <- tab[!(fitted) & family %in% names(fam_val$half_life)]
if (nrow(chk)) {
  bad <- chk[abs(half_life - SPEC$half_life$glob) < 1e-9 &
               abs(fam_val$half_life[family] - SPEC$half_life$glob) > 1e-9]
  if (nrow(bad)) cli::cli_abort(
    "unfitted event{?s} {.field {bad$event_id}} kept the global half_life
     {SPEC$half_life$glob} although their famil{?y/ies} fitted something else --
     the family fill did not apply.")
  say("family fill verified on %d unfitted event%s: %s", nrow(chk),
      if (nrow(chk) == 1) "" else "s",
      paste(sprintf("%s %.0fd", chk$event_id, chk$half_life), collapse = ", "))
}
attr(tab, "stamp") <- sprintf("fit_event_params %s | cache %s | split %s",
                              format(Sys.Date()), basename(CACHE), format(SPLIT))
saveRDS(tab, file.path(OUT, "event_params.rds"))
fwrite(tab, file.path(OUT, "event_params.csv"))
cat("\n=== family values ===\n")
print(Reduce(function(a, b) merge(a, b, by = "family"),
             lapply(names(SPEC), function(nm) {
               x <- res[[nm]]$family[, .(family, round(fam, 3))]; setnames(x, "V2", nm); x })))
cat("\n=== the events furthest from the global values ===\n")
tab[, dist := abs(context_scale - SPEC$context_scale$glob) / SPEC$context_scale$glob +
      abs(trim_tactical - SPEC$trim_tactical$glob) / max(SPEC$trim_tactical$glob, 1e-9)]
print(head(tab[order(-dist), .(event_id, family, context_scale = round(context_scale, 3),
                               trim_tactical = round(trim_tactical, 3),
                               half_life = round(half_life), races_half_life = round(races_half_life, 1))], 12))
say("wrote event_params.rds and event_params.csv (%d events)", nrow(tab))
