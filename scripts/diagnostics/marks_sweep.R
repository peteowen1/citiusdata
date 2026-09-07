# MARKS SWEEP: score a parameter config against last-5, per event, in seconds.
#
# Reads the cache built by marks_lab_prep.R. Nothing here touches the store,
# adjusts history, or calls estimate_ability: the adjusted marks and the
# parameter-free half of the weighting are already on disk, so a config is a
# weighted group-by. See marks_lab_prep.R's header for what is cached and why.
#
# THE GATE. sigma is held fixed from a real estimate_ability() run at the
# deployed config, so shrinkage moves only through w_total. Before any sweep
# number is printed, the deployed config is scored here and checked against
# that run's own ability. If it does not reproduce it within tolerance the
# script stops: a fast scorer that disagrees with the model is worse than no
# scorer.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_sweep.R'
# Env:
#   CITIUS_SWEEP  "global:60,90,180,365" or "sprint:60,90,120" (a family name),
#                 or "none" to score the deployed config alone
#   CITIUS_SWEEP_TOL   tolerance in % of a mark for the gate (default 0.02)
#   CITIUS_LAB_CACHE   cache directory (default marks_lab_cache)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache"))
TOL   <- as.numeric(Sys.getenv("CITIUS_SWEEP_TOL", "0.02"))
SWEEP <- Sys.getenv("CITIUS_SWEEP", "global:60,90,120,180,270,365,540")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
need <- file.path(CACHE, c("test.rds", "adj.rds", "base.rds", "meta.rds"))
if (!all(file.exists(need))) stop("cache incomplete; run marks_lab_prep.R first")
meta <- readRDS(file.path(CACHE, "meta.rds"))
test <- readRDS(file.path(CACHE, "test.rds"))
adj  <- readRDS(file.path(CACHE, "adj.rds"))
b5   <- readRDS(file.path(CACHE, "base.rds"))
months <- sort(unique(test$month))
sig <- rbindlist(lapply(months, function(m) {
  f <- file.path(CACHE, "sigma", paste0(format(m), ".rds"))
  if (!file.exists(f)) return(NULL)
  readRDS(f)[, month := m][]
}), fill = TRUE)
stopifnot("sigma cache is missing months" = uniqueN(sig$month) == length(months))
say("cache: %s test rows, %s adjusted history rows, %d months, calibration %s",
    format(nrow(test), big.mark = ","), format(nrow(adj), big.mark = ","), length(months), meta$cal)

# --- the scorer --------------------------------------------------------------
hl_names <- names(DEPLOYED$hl_family)
score <- function(hl_global = DEPLOYED$half_life, hl_map = DEPLOYED$hl_family) {
  hlv <- if (length(hl_map)) unlist(hl_map) else numeric(0)
  # The half-life is a per-ROW property (via family), so set it once per config
  # as a column, in place. Then each month filters and aggregates in ONE pass:
  # `adj[date < cut, ..., by = ]` never materialises the subset, which is what
  # killed three runs -- a 5.9M-row copy per month, twenty times over.
  set(adj, j = "hl", value = if (length(hlv))
    fifelse(adj$family %chin% names(hlv), hlv[match(adj$family, names(hlv))], hl_global)
    else rep(hl_global, nrow(adj)))
  out <- rbindlist(lapply(months, function(cut) {
    ae <- adj[date < cut, {
      w <- w_static * 0.5^(as.numeric(cut - date) / hl)
      sw <- sum(w)
      .(ability_raw = sum(w * perf) / sw, w_total = sw)
    }, by = .(athlete_id, event_id)][is.finite(ability_raw)]
    pri <- ae[, .(prior_mu = mean(ability_raw), sigma_between = sd(ability_raw)), by = event_id]
    ae <- merge(ae, pri, by = "event_id")
    ae <- merge(ae, sig[month == cut, .(athlete_id, event_id, sigma)], by = c("athlete_id", "event_id"))
    ae[!is.finite(sigma_between) | sigma_between <= 0, sigma_between := sigma]
    ae[, shrinkage := ((sigma^2) / (sigma_between^2)) / (w_total + (sigma^2) / (sigma_between^2))]
    ae[, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
    ae[, month := cut][, .(month, athlete_id, event_id, ability)]
  }), fill = TRUE)
  out <- deployed_debias(out)
  merge(test, out[, .(month, athlete_id, event_id, pred = ability)],
        by = c("athlete_id", "event_id", "month"))
}
evaluate <- function(p, label) {
  d <- merge(p, b5, by = c("athlete_id", "event_id", "date"))
  d[, `:=`(ae_m = 100 * abs(pred - act), ae_b = 100 * abs(base - act))]
  ev <- d[, .(races = uniqueN(race_key), n = .N, m = mean(ae_m), b = mean(ae_b),
              se = sd(ae_m - ae_b) / sqrt(.N)), by = .(event_id, family)][races >= 10]
  ev[, `:=`(gap = 100 * (m - b) / b, t = (m - b) / se, beat = m < b, config = label)]
  cat(sprintf("%-34s beat %2d/%2d | pooled %.3f vs %.3f (%+.1f%%)\n", label,
              sum(ev$beat), nrow(ev), weighted.mean(ev$m, ev$n), weighted.mean(ev$b, ev$n),
              100 * (weighted.mean(ev$m, ev$n) - weighted.mean(ev$b, ev$n)) / weighted.mean(ev$b, ev$n)))
  ev[]
}

# --- gate --------------------------------------------------------------------
t0 <- Sys.time()
dep <- score()
say("deployed config scored in %.1fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))
g <- merge(dep, sig[, .(month, athlete_id, event_id, ref_ability)],
           by = c("month", "athlete_id", "event_id"))
err <- 100 * (g$pred - g$ref_ability)
say("GATE vs estimate_ability on %s rows: mean %+.4f%%, sd %.4f%%, max |err| %.4f%%",
    format(nrow(g), big.mark = ","), mean(err), sd(err), max(abs(err)))
if (!is.finite(mean(err)) || abs(mean(err)) > TOL || sd(err) > 5 * TOL) {
  cat("\nGATE FAILED - not reporting a sweep from this scorer.\n")
  cat("Something in the chain does depend on as_of or half_life and is being held fixed.\n")
  quit(status = 1L)
}
say("gate passed (tolerance %.2f%%)", TOL)

cat("\n")
res <- list(evaluate(dep, "deployed"))
if (!identical(SWEEP, "none")) {
  kind <- sub(":.*$", "", SWEEP); vals <- as.numeric(strsplit(sub("^[^:]*:", "", SWEEP), ",")[[1]])
  cat(sprintf("\n=== sweep %s ===\n", SWEEP))
  for (x in vals) {
    t0 <- Sys.time()
    p <- if (kind == "global") score(hl_global = x, hl_map = list()) else
      score(hl_map = utils::modifyList(DEPLOYED$hl_family, setNames(list(x), kind)))
    r <- evaluate(p, sprintf("%s = %g", kind, x))
    r[, secs := round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)]
    res[[length(res) + 1]] <- r
  }
}
all <- rbindlist(res, fill = TRUE)
fwrite(all, file.path(OUT, "marks_sweep.csv"))
if (length(res) > 1) {
  cat("\nper event, the config that fits it best:\n")
  best <- all[, .SD[which.min(gap)], by = event_id][order(gap)]
  print(best[, .(event_id, family, config, races, model = round(m, 3), last5 = round(b, 3),
                 gap = round(gap, 1), beat)], nrows = 60)
}
say("wrote marks_sweep.csv")
