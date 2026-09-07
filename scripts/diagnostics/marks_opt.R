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
test  <- readRDS(file.path(CACHE, "test.rds"))
b5    <- readRDS(file.path(CACHE, "base.rds"))
say("pairs %s rows | %s predictions", format(nrow(pairs), big.mark = ","), format(nrow(k), big.mark = ","))

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
sc <- function(hl_global = DEPLOYED$half_life, hl_map = DEPLOYED$hl_family,
               trim = 0.25, debias = TRUE, adj_scale = 1, shrink = 1) {
  pairs[, w := w_static * 0.5^(age_days / hl_of(family, hl_global, hl_map))]
  pairs[, p_use := if (adj_scale == 1) perf else perf_raw + adj_scale * (perf - perf_raw)]
  r <- pairs[trim_keep(pairs, trim), .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := shrink * (sigma^2 / sigma_between^2)]
  m[, shrinkage := kap / (w_total + kap)]
  m[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  if (debias) m <- deployed_debias(m)
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
ev_of <- function(p, label) {
  d <- merge(merge(test, p, by = c("athlete_id", "event_id", "month")),
             b5, by = c("athlete_id", "event_id", "date"))
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
