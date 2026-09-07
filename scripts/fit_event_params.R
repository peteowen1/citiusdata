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
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
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

# grid, and the (kappa_family, kappa_event) each parameter earned on its sweep
SPEC <- list(
  context_scale   = list(grid = seq(0, 1.5, by = 0.25),               kap = c(5000, 1600), glob = fit$adj),
  trim_tactical   = list(grid = c(0, 0.1, 0.15, 0.25, 0.4),           kap = c(5000, 100),  glob = fit$trim),
  half_life       = list(grid = c(60, 90, 180, 270, 365, 540, 730, 1095), kap = c(5000, 400), glob = fit$hl),
  races_half_life = list(grid = c(2, 3, 5, 8, 12, 20, 40, Inf),       kap = c(5000, Inf),  glob = fit$rhl))

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
  curve <- rbindlist(lapply(sp$grid, function(v) {
    predict_at(nm, rep(v, nrow(pairs)))[date < SPLIT,
      .(v = v, sae = sum(abs(pred - act)), n = .N), by = .(event_id, family)]
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
miss <- tab[is.na(context_scale)]
if (nrow(miss)) {
  say("%d registry events had no fit data; filling with the global values", nrow(miss))
  for (nm in names(SPEC)) set(tab, which(is.na(tab[[nm]])), nm, SPEC[[nm]]$glob)
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
