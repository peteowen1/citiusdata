# Per-EVENT parameters, done the only way they can survive: shrunk.
#
# THE CASE FOR THEM. The 100m wants a context-adjustment scale near 0 (held out,
# gap -5.2% and excess 0.050) while everything else wants 0.5 to 1 (-7.1% to
# -8.0%). One global number is serving populations that pull opposite ways, and
# it is not even a family effect -- the 100m differs from the 200m and 400m
# inside its own family, so `sprint = 0.25` cannot express it either.
#
# THE CASE AGAINST. Per-family fitting has already overfit badly here: nine free
# weights over ~7k fit rows beat 16 of 35 held out where a flat value beat 28.
# Per event is 70 parameters over the same data. A raw per-event fit is
# guaranteed to look brilliant on the fit years and is very likely worthless.
#
# SO: fit each event's value on the FIT YEARS ONLY, then shrink it toward the
# global value and sweep how far:
#
#   adj_e(lambda) = (1 - lambda) * adj_global + lambda * adj_e_fitted
#
# lambda 0 is the flat model we have; lambda 1 is the raw per-event fit. If the
# held-out optimum is 0, per-event parameters are refuted and that is a real
# answer. If it is interior, they are worth having AND the sweep says how much
# to trust them -- which is the same empirical-Bayes logic the package already
# applies to abilities, applied one level up to the parameters themselves.
#
# Every event's fitted value is printed with its held-out race count beside it,
# because a value fitted on 30 races is not the same kind of object as one
# fitted on 400 and the table should not let them look alike.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_event_params.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
MINR  <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "5"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))

hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) { hv <- unlist(hl_map); i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
# `adj_map` is a named vector event_id -> scale; anything unnamed gets `p$adj`.
predict_at <- function(p, adj_map = NULL) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  w <- pp$w_static * 0.5^(pp$age_days / hl_of(pp$family, p$hl, DEPLOYED$hl_family))
  if (is.finite(p$rhl) && p$rhl > 0) w <- w * 0.5^(pp$.k / p$rhl)
  a <- rep(p$adj, nrow(pp))
  if (!is.null(adj_map)) {
    i <- match(pp$event_id, names(adj_map))
    a[!is.na(i)] <- unname(adj_map[i[!is.na(i)]])
  }
  p_use <- pp$perf_raw + a * (pp$perf - pp$perf_raw)
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pp)) else
    !(pp$tactical & !is.na(pp$rk) & pp$rk <= floor(pp$grp_n * p$trim))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
frame <- function(p, adj_map = NULL)
  merge(merge(test, predict_at(p, adj_map), by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
per_event <- function(d) d[, .(races = uniqueN(race_key), n = .N,
                               m = mean(100 * abs(pred - act)),
                               b = mean(100 * abs(base_m - act))), by = .(event_id, family)]
summarise <- function(d, label) {
  e <- per_event(d)[races >= MINR]
  data.table(config = label, beat = sum(e$m < e$b), of = nrow(e),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}

# --- fit one adjustment scale per event, on the FIT YEARS ONLY ---------------
GRID <- seq(0, 1.5, by = 0.25)
say("fitting a per-event adjustment scale on rows before %s over %d grid values",
    format(SPLIT), length(GRID))
mae_by <- rbindlist(lapply(GRID, function(a) {
  d <- frame(fit, stats::setNames(rep(a, uniqueN(pairs$event_id)), unique(pairs$event_id)))
  d[date < SPLIT, .(a = a, mae = mean(100 * abs(pred - act)), n = .N), by = event_id]
}))
best <- mae_by[mae_by[, .I[which.min(mae)], by = event_id]$V1]
setnames(best, "a", "adj_fit")
say("fitted %d events; median %.2f, 5-95%% %.2f to %.2f",
    nrow(best), median(best$adj_fit), quantile(best$adj_fit, .05), quantile(best$adj_fit, .95))

# --- how far to trust each event, ACCORDING TO ITS OWN EVIDENCE -------------
# A flat lambda is the wrong shrinkage: it gives an event fitted on 1 row the
# same trust as one fitted on 601. The right form is the one the package already
# uses for abilities -- shrink toward the prior by a pseudo-count, so an event
# deviates from the global value in proportion to how much data it has:
#
#   adj_e(kappa) = (kappa * adj_global + n_e * adj_e_fitted) / (kappa + n_e)
#
# kappa is "how many rows of evidence the global value is worth". kappa = Inf is
# the flat model; kappa = 0 is the raw per-event fit; in between, a 601-row
# event moves most of the way to its own value while a 5-row event barely
# budges. Swept, and judged on the HELD-OUT years.
#
# THE VERDICT IS ON EVENTS BEATEN, not on pooled MAE. The goal is to beat a
# last-5 baseline in every event, and pooled error is dominated by the handful
# of high-volume events -- an earlier version of this script judged on MAE and
# reported per-event parameters as a win while they were quietly costing three
# events. MAE is printed beside it so the trade is visible either way.
cat("
=== shrinking each event toward the global value by its own evidence ===
")
KAPPA <- c(0, 25, 50, 100, 200, 400, 800, 1600, Inf)
res <- rbindlist(lapply(KAPPA, function(kap) {
  a_e <- if (is.infinite(kap)) rep(fit$adj, nrow(best)) else
    (kap * fit$adj + best$n * best$adj_fit) / (kap + best$n)
  d <- frame(fit, stats::setNames(a_e, best$event_id))
  cbind(kappa = kap,
        moved = round(mean(abs(a_e - fit$adj)), 3),
        rbind(summarise(d[date <  SPLIT], "fit years"),
              summarise(d[date >= SPLIT], "HELD OUT")))
}))
print(dcast(res, kappa + moved ~ config, value.var = c("beat", "mae", "vs_last5")))
hold <- res[config == "HELD OUT"]
bestk <- hold[which.max(beat)]                       # the goal metric
bestm <- hold[which.min(mae)]                        # the pooled one, for contrast
cat(sprintf("
most events beaten held out: kappa %s -> %d of %d (%+.2f%%, MAE %.4f)
",
            format(bestk$kappa), bestk$beat, bestk$of, bestk$vs_last5, bestk$mae))
cat(sprintf("lowest pooled MAE held out:  kappa %s -> %d of %d (%+.2f%%, MAE %.4f)
",
            format(bestm$kappa), bestm$beat, bestm$of, bestm$vs_last5, bestm$mae))
flat <- hold[is.infinite(kappa)]
cat(sprintf("flat global (kappa Inf):     %d of %d (%+.2f%%, MAE %.4f)
",
            flat$beat, flat$of, flat$vs_last5, flat$mae))
# A config only counts as an improvement if it does not LOSE events. Ties on
# events and wins on error is a win; wins on error and drops three events is
# the trade this project keeps refusing, because the goal is stated per event.
dom <- hold[beat >= flat$beat & mae < flat$mae][order(mae)]
if (nrow(dom)) {
  d1 <- dom[1]
  cat(sprintf("=> per-event EARNS its place at kappa %s: %d of %d events (same as flat)
",
              format(d1$kappa), d1$beat, d1$of))
  cat(sprintf("   and pooled %+.2f%% against flat's %+.2f%%. Average event moves %.3f
",
              d1$vs_last5, flat$vs_last5, d1$moved))
  cat("   from the global value -- a nudge, not a free-for-all.
")
} else {
  cat(sprintf("=> per-event is REFUTED: nothing matches flat's %d events while beating
   its MAE. Any pooled gain is bought with whole events.
",
              flat$beat))
}
cat(sprintf("
for contrast, the lowest-MAE setting (kappa %s) costs %d events to buy
%+.2f%% of pooled error.
",
            format(bestm$kappa), flat$beat - bestm$beat, bestm$vs_last5 - flat$vs_last5))

# What the shrinkage actually does to the event that motivated all this.
# DOES THE 100m EFFECT REPLICATE? An earlier diagnostic found the 100m much
# better at adjustment 0 than at 0.5 -- but that was measured on the HELD-OUT
# years. Setting a parameter from it would be selecting on the test set, and the
# first thing to ask is whether the fit years agree. They do not: the per-event
# argmin above puts both 100m events at 0.50, the global value. The full curve
# is printed so the disagreement is visible rather than asserted.
cat("
=== does the 100m preference replicate? fit-year MAE by adjustment ===
")
cur <- dcast(mae_by[event_id %in% c("AT-100Metres-M", "AT-100Metres-W",
                                    "AT-200Metres-M", "AT-400Metres-M")],
             event_id ~ a, value.var = "mae")
print(cur)
cat("lowest per row is the fit-year choice. Compare with the held-out result in
")
cat("diagnostics/marks_100m_optimism.R, which preferred 0 on the 100m.
")

cat("
=== the 100m, which is why we are here ===
")
show <- c("AT-100Metres-M", "AT-100Metres-W", "AT-200Metres-M", "AT-400Metres-M", "AT-Marathon-W")
for (kap in c(0, 50, 800, Inf)) {
  a_e <- if (is.infinite(kap)) rep(fit$adj, nrow(best)) else
    (kap * fit$adj + best$n * best$adj_fit) / (kap + best$n)
  names(a_e) <- best$event_id
  cat(sprintf("kappa %6s: %s
", format(kap),
              paste(sprintf("%s %.2f", sub("^AT-", "", show), a_e[show]), collapse = "  ")))
}
cat(sprintf("(global value %.2f; per-event fitted values are %s)
", fit$adj,
            paste(sprintf("%s %.2f", sub("^AT-", "", show),
                          best$adj_fit[match(show, best$event_id)]), collapse = "  ")))

# --- what the per-event fit actually says ------------------------------------
held <- per_event(frame(fit)[date >= SPLIT])[, .(event_id, races_held_out = races)]
tab <- merge(best[, .(event_id, adj_fit, fit_rows = n)], held, by = "event_id", all.x = TRUE)
tab <- merge(tab, as.data.table(citius_events())[, .(event_id, family)], by = "event_id", all.x = TRUE)
cat("\n=== fitted scale per event, furthest from the global value first ===\n")
tab[, dist := abs(adj_fit - fit$adj)]
print(head(tab[order(-dist), .(event_id, family, adj_fit, fit_rows, races_held_out)], 20))
cat(sprintf("\nglobal value is %.2f. events at the grid edges: %d at 0, %d at %.2f\n",
            fit$adj, sum(tab$adj_fit == 0), sum(tab$adj_fit == max(GRID)), max(GRID)))
fwrite(tab, file.path(OUT, "marks_event_adj.csv"))
say("wrote marks_event_adj.csv")
