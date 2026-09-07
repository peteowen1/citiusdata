# THREE-LEVEL HIERARCHY for a model parameter: global -> family -> event.
#
# Pete: "maybe the prior should be hierarchical so it deviates away from the
# family prior based on info it has, and the family prior can deviate from the
# overall prior based on info -- so if distance is 1.5ish then Marathon W
# shrinks towards that rather than 0.5".
#
# Right, and it fixes the obvious defect in the two-level version. There, an
# event with little data was pulled toward the GLOBAL value, which for a road
# event is nonsense: road and distance events collectively want a large context
# adjustment (their times swing hugely with course and weather) while sprints
# want a small one. Pulling a thin road event toward 0.5 drags it away from
# everything its own family knows.
#
#   family_f = (kappa_f * global   + n_f * family_raw_f) / (kappa_f + n_f)
#   event_e  = (kappa_e * family_f + n_e * event_raw_e)  / (kappa_e + n_e)
#
# Each level deviates from the one above in proportion to its own evidence, so a
# 601-row marathon earns most of its own value, a 1-row weight throw inherits
# its family's, and a family with few events inherits the global one. Both
# strengths are swept and judged on the HELD-OUT years.
#
# WHAT "RAW FIT" MEANS, since it is easy to over-read: for each unit, the grid
# value with the lowest mean absolute error ON THE FIT YEARS. It is a grid
# argmin, so it can only take grid values and it will sit on an edge when the
# curve is flat or the data thin. That is precisely what the shrinkage is for.
#
# THE VERDICT IS ON EVENTS BEATEN. Pooled error is dominated by a handful of
# high-volume events, so a config can win on it while dropping whole events --
# which is the trade this project keeps refusing, since the goal is per event.
# A config counts only if it does not lose events AND improves error.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_hier_params.R'
# Env: CITIUS_HIER_PARAM  which parameter to fit hierarchically (default "adj")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
MINR  <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "5"))
PARAM <- Sys.getenv("CITIUS_HIER_PARAM", "adj")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))
GRIDS <- list(adj = seq(0, 1.5, by = 0.25),
              hl  = c(60, 90, 180, 270, 365, 540, 730, 1095),
              rhl = c(2, 3, 5, 8, 12, 20, 40, 1e6),
              trim = c(0, 0.1, 0.15, 0.25, 0.4),
              shrink = c(0, 0.25, 0.5, 1, 2))
stopifnot("unknown CITIUS_HIER_PARAM" = PARAM %in% names(GRIDS))
GRID <- GRIDS[[PARAM]]

hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) { hv <- unlist(hl_map); i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
# `pmap` is a named vector event_id -> value for PARAM; unnamed events use the
# global value from `fit`.
predict_at <- function(pmap = NULL) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  val <- function(nm) {
    v <- rep(fit[[nm]], nrow(pp))
    if (identical(nm, PARAM) && !is.null(pmap)) {
      i <- match(pp$event_id, names(pmap)); v[!is.na(i)] <- unname(pmap[i[!is.na(i)]])
    }
    v
  }
  hlv <- if (identical(PARAM, "hl") && !is.null(pmap)) val("hl") else
    hl_of(pp$family, fit$hl, DEPLOYED$hl_family)
  w <- pp$w_static * 0.5^(pp$age_days / hlv)
  rhlv <- val("rhl")
  w <- w * data.table::fifelse(is.finite(rhlv) & rhlv > 0, 0.5^(pp$.k / rhlv), 1)
  p_use <- pp$perf_raw + val("adj") * (pp$perf - pp$perf_raw)
  trimv <- val("trim")
  keep <- !(pp$tactical & !is.na(pp$rk) & trimv > 0 & pp$rk <= floor(pp$grp_n * trimv))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  shr <- fit$shrink
  m[, kap := shr * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
frame <- function(pmap = NULL)
  merge(merge(test, predict_at(pmap), by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
summarise <- function(d, label) {
  e <- d[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
             b = mean(100 * abs(base_m - act))), by = .(event_id, family)][races >= MINR]
  data.table(config = label, beat = sum(e$m < e$b), of = nrow(e),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}

# --- raw fits at every level, on the FIT YEARS only --------------------------
say("fitting %s hierarchically over %d grid values", PARAM, length(GRID))
curve <- rbindlist(lapply(GRID, function(v) {
  d <- frame(stats::setNames(rep(v, uniqueN(pairs$event_id)), unique(pairs$event_id)))
  d[date < SPLIT, .(v = v, sae = sum(abs(pred - act)), n = .N), by = .(event_id, family)]
}))
ev_raw  <- curve[, .(sae = sum(sae), n = sum(n)), by = .(event_id, family, v)]
ev_raw  <- ev_raw[ev_raw[, .I[which.min(sae / n)], by = event_id]$V1][, .(event_id, family, ev_v = v, n_e = n)]
fam_raw <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)]
fam_raw <- fam_raw[fam_raw[, .I[which.min(sae / n)], by = family]$V1][, .(family, fam_v = v, n_f = n)]
glob    <- curve[, .(sae = sum(sae), n = sum(n)), by = v][, .(v, mae = sae / n)]
g_pool  <- glob$v[which.min(glob$mae)]
# THE ANCHOR IS THE DEPLOYED VALUE, not the pooled-error argmin.
#
# Pooled error is dominated by high-volume events, so its argmin (0.75 for the
# adjustment scale) is not the value the goal metric wants (0.50, what the
# coordinate descent chose): flat at 0.75 beats 36 of 44 events held out, flat
# at 0.50 beats 40. Anchoring the hierarchy at 0.75 would build every family and
# event deviation on top of a config that has already given away four events,
# and then credit the hierarchy with clawing some of them back.
g_v <- fit[[PARAM]]
say("global anchor: %s = %g (deployed). Pooled-error argmin would be %g.",
    PARAM, g_v, g_pool)
cat("\n=== family raw fits (fit years) ===\n")
print(fam_raw[order(-n_f)])

compose <- function(kf, ke) {
  f <- copy(fam_raw)
  f[, fam_shrunk := if (is.infinite(kf)) g_v else (kf * g_v + n_f * fam_v) / (kf + n_f)]
  e <- merge(ev_raw, f[, .(family, fam_shrunk)], by = "family", all.x = TRUE)
  e[is.na(fam_shrunk), fam_shrunk := g_v]
  e[, out := if (is.infinite(ke)) fam_shrunk else (ke * fam_shrunk + n_e * ev_v) / (ke + n_e)]
  stats::setNames(e$out, e$event_id)
}
cat("\n=== hierarchy swept: kappa_family x kappa_event, HELD OUT ===\n")
res <- rbindlist(lapply(c(0, 200, 1000, 5000, Inf), function(kf)
  rbindlist(lapply(c(0, 100, 400, 800, 1600, Inf), function(ke) {
    d <- frame(compose(kf, ke))[date >= SPLIT]
    cbind(kappa_family = kf, kappa_event = ke, summarise(d, "held out")[, !"config"])
  }))))
print(dcast(res, kappa_family ~ kappa_event, value.var = "beat"), nrows = 20)
cat("\n(cells are events beaten out of", res$of[1], "-- pooled error below)\n")
print(dcast(res, kappa_family ~ kappa_event, value.var = "vs_last5"))

# THE REFERENCE IS WHAT WE ACTUALLY RUN, not the pooled-MAE optimum.
#
# `g_v` above is the grid value minimising POOLED fit-year error, and pooled
# error is dominated by high-volume events -- it lands on 0.75 while the
# deployed value from the coordinate descent is 0.50. Those are not the same
# model and they do not score the same: measured held out, 0.75 flat beats 36
# of 44 events and 0.50 flat beats 40. Comparing the hierarchy against 0.75
# would credit it with recovering damage the anchor itself caused.
cat("
=== reference rows: flat, at both candidate global values ===
")
ref <- rbindlist(lapply(unique(c(g_v, fit[[PARAM]])), function(v) {
  d <- frame(stats::setNames(rep(v, uniqueN(pairs$event_id)), unique(pairs$event_id)))[date >= SPLIT]
  cbind(global = v, summarise(d, sprintf("flat %s = %g", PARAM, v)))
}))
print(ref)
best_flat <- ref[which.max(beat)]
cat(sprintf("
using the better flat reference: %s = %g, %d of %d, %+.2f%%, MAE %.4f
",
            PARAM, best_flat$global, best_flat$beat, best_flat$of, best_flat$vs_last5, best_flat$mae))

flat <- best_flat[, .(kappa_family = Inf, kappa_event = Inf, beat, of, mae, vs_last5)]
cat(sprintf("\nflat global (%s = %g everywhere): %d of %d, %+.2f%%, MAE %.4f\n",
            PARAM, g_v, flat$beat, flat$of, flat$vs_last5, flat$mae))
dom <- res[beat >= flat$beat & mae < flat$mae][order(mae)]
if (nrow(dom)) {
  d1 <- dom[1]
  cat(sprintf("BEST that loses no events: kappa_family %s, kappa_event %s -> %d of %d, %+.2f%%\n",
              format(d1$kappa_family), format(d1$kappa_event), d1$beat, d1$of, d1$vs_last5))
  two <- res[is.infinite(kappa_family)][beat >= flat$beat & mae < flat$mae][order(mae)]
  if (nrow(two)) cat(sprintf("  best WITHOUT the family level (kappa_family Inf): %+.2f%% at kappa_event %s\n",
                             two[1]$vs_last5, format(two[1]$kappa_event)))
  cat("\n=== what it does to a few events ===\n")
  pm <- compose(d1$kappa_family, d1$kappa_event)
  show <- c("AT-Marathon-W", "AT-HalfMarathon-M", "AT-5000Metres-M", "AT-100Metres-M",
            "AT-200Metres-M", "AT-ShotPut-M", "AT-WeightThrow-M")
  show <- show[show %in% names(pm)]
  print(merge(ev_raw[event_id %in% show, .(event_id, family, raw_fit = ev_v, rows = n_e)],
              data.table(event_id = show, hierarchical = round(unname(pm[show]), 3)),
              by = "event_id")[order(family)])
} else {
  cat(sprintf("no hierarchical setting matches flat's %d events while beating its MAE.\n", flat$beat))
}
fwrite(res, file.path(OUT, sprintf("marks_hier_%s.csv", PARAM)))
say("wrote marks_hier_%s.csv", PARAM)
