# MARKS OPT: sweep prediction parameters against the last-5 baseline, per event.
#
# Runs on the pair table built and GATE-VERIFIED by marks_pairs.R, which
# reproduces estimate_ability() exactly (mean, sd and max error all 0.0000% of
# a mark over 13,674 predictions). A config costs about a quarter of a second,
# so this is an optimisation loop rather than a diagnosis.
#
# The gate is re-run here before anything is swept: the cache can go stale
# (a new calibration, a new deployed config) and a scorer that no longer
# matches the model must not be allowed to recommend a parameter.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_opt.R'
# Env: CITIUS_OPT_WHAT  "halflife" | "trim" | "both" (default both)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache"))
WHAT  <- Sys.getenv("CITIUS_OPT_WHAT", "both")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
# the months actually scored (those with their own reference run)
test  <- readRDS(file.path(CACHE, if (file.exists(file.path(CACHE, "test_scored.rds"))) "test_scored.rds" else "test.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))
say("pairs %s rows | %s predictions", format(nrow(pairs), big.mark = ","), format(nrow(k), big.mark = ","))

reg_fam <- as.data.table(citius_events())[, .(event_id, family)]
hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) { hv <- unlist(hl_map); i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
trim_keep <- function(d, frac) if (frac <= 0) rep(TRUE, nrow(d)) else
  !(d$tactical & !is.na(d$rk) & d$rk <= floor(d$grp_n * frac))
# adj_scale: how much of the context adjustment to apply (1 = deployed, 0 = use
#   the raw marks). shrink: multiplier on the shrinkage pseudo-count (1 =
#   deployed, 0 = no shrinkage toward the field at all).
# adj_scale may be a single number or a named per-family vector.
# blend: the MARK is predicted as (1 - b) * ability + b * (the athlete's recent
#   form), leaving the ranking untouched -- the same split as sigma_marks. b may
#   also be a named per-family vector. Recent form here is the same last-5 mean
#   the baseline uses, computed from the athlete's own prior marks.
sc <- function(hl_global = DEPLOYED$half_life, hl_map = DEPLOYED$hl_family,
               trim = 0.25, debias = TRUE, adj_scale = 1, shrink = 1, blend = 0) {
  as_vec <- function(x) if (length(x) == 1 && is.null(names(x))) rep(as.numeric(x), nrow(pairs)) else {
    v <- rep(1, nrow(pairs)); i <- match(pairs$family, names(x)); v[!is.na(i)] <- x[i[!is.na(i)]]; v }
  sc_row <- as_vec(adj_scale)
  pairs[, w := w_static * 0.5^(age_days / hl_of(family, hl_global, hl_map))]
  pairs[, p_use := perf_raw + sc_row * (perf - perf_raw)]
  r <- pairs[trim_keep(pairs, trim), .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := shrink * (sigma^2 / sigma_between^2)]
  m[, shrinkage := kap / (w_total + kap)]
  m[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  if (debias) m <- deployed_debias(m)
  # The blend is applied in ev_of(), where the RACE DATE is available. Joining
  # the baseline here on `month` silently matched nothing (predictions are keyed
  # by month start, the baseline by race date), so every blend was a no-op that
  # looked like "the lever does not matter".
  m[, .(athlete_id, event_id, month, pred = ability)]
}

# --- gate --------------------------------------------------------------------
g <- merge(sc(debias = FALSE), k[, .(athlete_id, event_id, month, ref_ability)],
           by = c("athlete_id", "event_id", "month"))
e <- 100 * (g$pred - g$ref_ability)
say("gate: mean %+.4f%%, sd %.4f%%, max %.4f%% on %s predictions",
    mean(e), sd(e), max(abs(e)), format(nrow(g), big.mark = ","))
if (abs(mean(e)) > 0.001 || max(abs(e)) > 0.01) stop("gate failed - the cache no longer matches the model")

# --- scoring -----------------------------------------------------------------
ev_of <- function(p, label, blend = 0) {
  d <- merge(merge(test, p, by = c("athlete_id", "event_id", "month")),
             b5, by = c("athlete_id", "event_id", "date"))
  if (!identical(blend, 0)) {
    bl <- if (length(blend) == 1 && is.null(names(blend))) rep(as.numeric(blend), nrow(d)) else
      { v <- rep(0, nrow(d)); i <- match(d$family, names(blend)); v[!is.na(i)] <- blend[i[!is.na(i)]]; v }
    stopifnot("blend needs a finite baseline on every row" = all(is.finite(d$base)))
    d[, pred := (1 - bl) * pred + bl * base]
  }
  d[, `:=`(ae_m = 100 * abs(pred - act), ae_b = 100 * abs(base - act))]
  ev <- d[, .(races = uniqueN(race_key), n = .N, m = mean(ae_m), b = mean(ae_b),
              se = sd(ae_m - ae_b) / sqrt(.N)), by = .(event_id, family)][races >= 10]
  ev[, `:=`(gap = 100 * (m - b) / b, t = (m - b) / se, beat = m < b, config = label)][]
}
report <- function(ev, label) {
  cat(sprintf("%-30s beat %2d/%2d | pooled %.3f vs %.3f (%+5.1f%%)\n", label,
              sum(ev$beat), nrow(ev), weighted.mean(ev$m, ev$n), weighted.mean(ev$b, ev$n),
              100 * (weighted.mean(ev$m, ev$n) - weighted.mean(ev$b, ev$n)) / weighted.mean(ev$b, ev$n)))
  invisible(ev)
}
res <- list(report(ev_of(sc(), "deployed"), "deployed"))

if (WHAT %in% c("halflife", "both")) {
  cat("\n=== global half-life (family overrides removed) ===\n")
  for (x in c(45, 60, 90, 120, 180, 270, 365, 540, 730)) {
    r <- ev_of(sc(hl_global = x, hl_map = list()), sprintf("hl=%g", x))
    res[[length(res) + 1]] <- report(r, sprintf("half-life %g", x))
  }
  cat("\n=== per family, holding the rest deployed ===\n")
  for (fam in c("sprint", "hurdles", "jump", "throw", "road", "middle", "distance")) {
    for (x in c(45, 90, 180, 365, 730)) {
      # DEPLOYED$hl_family is a named NUMERIC VECTOR, not a list, so modifyList
      # rejects it. Build the override as a vector.
      hm <- DEPLOYED$hl_family; hm[[fam]] <- x
      r <- ev_of(sc(hl_map = hm), sprintf("%s hl=%g", fam, x))
      r <- r[family == fam]
      if (nrow(r)) {
        cat(sprintf("  %-8s hl %4g  beat %2d/%2d  pooled %.3f vs %.3f (%+5.1f%%)\n", fam, x,
                    sum(r$beat), nrow(r), weighted.mean(r$m, r$n), weighted.mean(r$b, r$n),
                    100 * (weighted.mean(r$m, r$n) - weighted.mean(r$b, r$n)) / weighted.mean(r$b, r$n)))
        res[[length(res) + 1]] <- r[, config := sprintf("%s hl=%g", fam, x)][]
      }
    }
  }
}
if (WHAT %in% c("blendval")) {
  # OUT OF SAMPLE. Pick the blend per family on the EARLY years, score it on the
  # LATE ones. A single parameter per family over ~19k predictions is a mild fit,
  # but "beat last-5" is partly self-referential once you blend toward last-5, so
  # the weight must not be chosen on the data it is judged on.
  CUT <- as.Date(Sys.getenv("CITIUS_OPT_SPLIT", "2024-01-01"))
  p0 <- sc()
  grid <- c(0, 0.15, 0.3, 0.4, 0.5, 0.6, 0.7, 0.85)
  d0 <- merge(merge(test, p0, by = c("athlete_id", "event_id", "month")),
              b5, by = c("athlete_id", "event_id", "date"))
  d0[, ae_b := 100 * abs(base - act)]
  fitset <- d0[date < CUT]; testset <- d0[date >= CUT]
  say("fit %s rows (< %s) | test %s rows (>= %s)", format(nrow(fitset), big.mark = ","),
      format(CUT), format(nrow(testset), big.mark = ","), format(CUT))
  curve <- rbindlist(lapply(grid, function(x)
    fitset[, .(blend = x, mae = mean(100 * abs((1 - x) * pred + x * base - act))), by = family]))
  bestb <- curve[, .SD[which.min(mae)], by = family][, .(family, blend)]
  cat("
blend chosen on the fit years:
"); print(bestb)
  bv <- setNames(bestb$blend, bestb$family)
  score_on <- function(d, bl) {
    v <- if (length(bl) == 1) rep(bl, nrow(d)) else
      { u <- rep(0, nrow(d)); i <- match(d$family, names(bl)); u[!is.na(i)] <- bl[i[!is.na(i)]]; u }
    e <- d[, .(races = uniqueN(race_key), n = .N,
               m = mean(100 * abs((1 - v) * pred + v * base - act)), b = mean(ae_b)), by = .(event_id, family)]
    e[races >= 10][, `:=`(gap = 100 * (m - b) / b, beat = m < b)][]
  }
  cat("
=== scored on the HELD-OUT years only ===
")
  for (nm in c("no blend", "fitted per family", "flat 0.5")) {
    bl <- switch(nm, "no blend" = 0, "fitted per family" = bv, "flat 0.5" = 0.5)
    e <- score_on(testset, bl)
    cat(sprintf("%-20s beat %2d/%2d | pooled %.3f vs %.3f (%+.1f%%)
", nm, sum(e$beat), nrow(e),
                weighted.mean(e$m, e$n), weighted.mean(e$b, e$n),
                100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) / weighted.mean(e$b, e$n)))
    if (nm == "fitted per family") {
      cat("
  by family on the held-out years:
")
      print(e[, .(events = .N, beat = sum(beat), model = round(weighted.mean(m, n), 3),
                  last5 = round(weighted.mean(b, n), 3),
                  gap = round(100 * (weighted.mean(m, n) - weighted.mean(b, n)) / weighted.mean(b, n), 1)),
               by = family][order(gap)])
      fwrite(e, file.path(OUT, "marks_blend_heldout.csv"))
    }
  }
  say("wrote marks_blend_heldout.csv"); quit(status = 0L)
}
if (WHAT %in% c("blend")) {
  cat("
=== recency blend for the MARK only (0 = ability, 1 = the last-5 mean) ===
")
  tab <- rbindlist(lapply(c(0, 0.15, 0.3, 0.5, 0.7), function(x) {
    r <- ev_of(sc(), sprintf("blend=%g", x), blend = x)
    report(r, sprintf("blend %g", x))
    r[, .(blend = x, events = .N, beat = sum(beat), model = weighted.mean(m, n),
          last5 = weighted.mean(b, n)), by = family]
  }), fill = TRUE)
  cat("
by family (model MAE at each blend, then events beaten):
")
  print(dcast(tab, family ~ blend, value.var = c("model", "beat"))[
    , lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
  fwrite(tab, file.path(OUT, "marks_blend_by_family.csv"))
  say("wrote marks_blend_by_family.csv"); quit(status = 0L)
}
if (WHAT %in% c("adjfam")) {
  # Per FAMILY, does removing the context adjustment help? Last-5 averages RAW
  # marks; we average adjusted ones. If adjusted is worse exactly where we lose,
  # the adjustment layer is the defect rather than the averaging.
  cat("
=== context adjustment scale, BY FAMILY (model MAE; last5 for reference) ===
")
  fams <- sort(unique(ev_of(sc(), "x")$family))
  tab <- rbindlist(lapply(c(0, 0.5, 1, 1.5), function(x) {
    r <- ev_of(sc(adj_scale = x), sprintf("adj=%g", x))
    r[, .(scale = x, events = .N, beat = sum(beat),
          model = weighted.mean(m, n), last5 = weighted.mean(b, n)), by = family]
  }), fill = TRUE)
  w <- dcast(tab, family ~ scale, value.var = c("model", "beat"))
  w <- merge(w, tab[scale == 1, .(family, last5 = round(last5, 3))], by = "family")
  print(w[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
  fwrite(tab, file.path(OUT, "marks_adj_by_family.csv"))
  say("wrote marks_adj_by_family.csv"); quit(status = 0L)
}
if (WHAT %in% c("shrink", "both")) {
  cat("
=== shrinkage toward the field (1 = deployed, 0 = none) ===
")
  for (x in c(0, 0.25, 0.5, 1, 2)) {
    r <- ev_of(sc(shrink = x), sprintf("shrink=%g", x))
    res[[length(res) + 1]] <- report(r, sprintf("shrink %g", x))
  }
  cat("
=== context adjustment scale (1 = deployed, 0 = raw marks) ===
")
  for (x in c(0, 0.5, 1, 1.5)) {
    r <- ev_of(sc(adj_scale = x), sprintf("adj=%g", x))
    res[[length(res) + 1]] <- report(r, sprintf("adjustment %g", x))
  }
  cat("
=== the family debias, on and off ===
")
  for (x in c(TRUE, FALSE)) {
    r <- ev_of(sc(debias = x), sprintf("debias=%s", x))
    res[[length(res) + 1]] <- report(r, sprintf("debias %s", x))
  }
}
if (WHAT %in% c("trim", "both")) {
  cat("\n=== tactical trim fraction (55 of 122 events are flagged tactical) ===\n")
  for (x in c(0, 0.1, 0.25, 0.4)) {
    r <- ev_of(sc(trim = x), sprintf("trim=%g", x))
    res[[length(res) + 1]] <- report(r, sprintf("trim %g", x))
  }
}
all <- rbindlist(res, fill = TRUE)
fwrite(all, file.path(OUT, "marks_opt.csv"))
cat("\n=== per event: the best config found, worst events first ===\n")
best <- all[, .SD[which.min(gap)], by = event_id]
print(best[order(-gap), .(event_id, family, races, config, model = round(m, 3),
                          last5 = round(b, 3), gap = round(gap, 1), beat)], nrows = 60)
dep <- all[config == "deployed"]
cat(sprintf("\ndeployed beats last-5 on %d of %d events; best-per-event would beat on %d\n",
            sum(dep$beat), nrow(dep), sum(best$beat)))
say("wrote marks_opt.csv")
