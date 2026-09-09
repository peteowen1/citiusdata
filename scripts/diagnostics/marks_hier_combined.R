# Both hierarchies at once: does stacking them help, or do they cancel?
#
# Fitted separately, each earns its place held out without losing an event:
#   adjustment scale   40 of 44, -6.80%   (flat -6.28%)
#   tactical trim      40 of 44, -6.74%   (flat -6.28%)
#
# Neither is licensed by the other, and gains of this size have a habit of not
# adding -- both act on the same weighted mean of the same marks, so they can
# easily be correcting the same thing twice. That is exactly the double-count
# this repo shipped and had to withdraw on 2026-09-07. So this measures the
# pair rather than assuming.
#
# THE TRIM RESULT IS THE MORE INTERESTING ONE and worth stating plainly. Fitted
# per family on the fit years, the trim wants:
#
#   middle 0.40   distance 0.40   combined 0.40   <- genuinely tactical
#   road 0.25     walk 0.25
#   hurdles 0.15  sprint 0.10
#   jump 0.00     throw 0.00                      <- not tactical at all
#
# That is what "tactical" is supposed to mean -- a slow time reflecting racing
# rather than ability -- rediscovered from the data alone. It also independently
# condemns the calibration's tactical override, which flags all 10 throws and
# all 8 sprints as tactical and had the model trimming away an athlete's worst
# shot puts as though they were sit-and-kick 1500m races.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_hier_combined.R'
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
TARGETS <- c("AT-100Metres-M", "AT-PoleVault-M", "AT-DiscusThrow-M")
GRIDS <- list(adj  = seq(0, 1.5, by = 0.25),
              trim = c(0, 0.1, 0.15, 0.25, 0.4),
              hl   = c(60, 90, 180, 270, 365, 540, 730, 1095))
# family, event -- each pair is the best setting from that parameter's own sweep
# that lost no events. rhl and shrink are NOT here: swept alone, rhl gained
# 0.06pp and shrink was refuted outright, every family wanting the global 0.
KAPPA <- list(adj = c(5000, 1600), trim = c(5000, 100), hl = c(5000, 400))

hl_of <- function(fam) {
  v <- rep(fit$hl, length(fam)); hv <- unlist(DEPLOYED$hl_family)
  if (length(hv)) { i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
# maps: named list of event_id -> value, one entry per parameter being varied
predict_at <- function(maps = list()) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  val <- function(nm) {
    v <- rep(fit[[nm]], nrow(pp))
    if (!is.null(maps[[nm]])) {
      i <- match(pp$event_id, names(maps[[nm]])); v[!is.na(i)] <- unname(maps[[nm]][i[!is.na(i)]])
    }
    v
  }
  hlv <- if (!is.null(maps$hl)) val("hl") else hl_of(pp$family)
  w <- pp$w_static * 0.5^(pp$age_days / hlv)
  if (is.finite(fit$rhl) && fit$rhl > 0) w <- w * 0.5^(pp$.k / fit$rhl)
  p_use <- pp$perf_raw + val("adj") * (pp$perf - pp$perf_raw)
  tv <- val("trim")
  keep <- !(pp$tactical & !is.na(pp$rk) & tv > 0 & pp$rk <= floor(pp$grp_n * tv))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
frame <- function(maps = list())
  merge(merge(test, predict_at(maps), by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))

# --- fit each parameter's hierarchy on the FIT YEARS -------------------------
build <- function(nm) {
  curve <- rbindlist(lapply(GRIDS[[nm]], function(v) {
    mp <- list(); mp[[nm]] <- stats::setNames(rep(v, uniqueN(pairs$event_id)), unique(pairs$event_id))
    frame(mp)[date < SPLIT, .(v = v, sae = sum(abs(pred - act)), n = .N), by = .(event_id, family)]
  }))
  ev <- curve[, .(sae = sum(sae), n = sum(n)), by = .(event_id, family, v)]
  ev <- ev[ev[, .I[which.min(sae / n)], by = event_id]$V1][, .(event_id, family, ev_v = v, n_e = n)]
  fm <- curve[, .(sae = sum(sae), n = sum(n)), by = .(family, v)]
  fm <- fm[fm[, .I[which.min(sae / n)], by = family]$V1][, .(family, fam_v = v, n_f = n)]
  kf <- KAPPA[[nm]][1]; ke <- KAPPA[[nm]][2]
  fm[, fam_shrunk := (kf * fit[[nm]] + n_f * fam_v) / (kf + n_f)]
  e <- merge(ev, fm[, .(family, fam_shrunk)], by = "family", all.x = TRUE)
  e[is.na(fam_shrunk), fam_shrunk := fit[[nm]]]
  e[, out := (ke * fam_shrunk + n_e * ev_v) / (ke + n_e)]
  list(map = stats::setNames(e$out, e$event_id), family = fm[, .(family, fam_v, fam_shrunk)])
}
say("fitting the adjustment hierarchy"); A <- build("adj")
say("fitting the trim hierarchy");       T <- build("trim")
say("fitting the half-life hierarchy");  H <- build("hl")
cat("\n=== what each family wants, fitted on the fit years ===\n")
print(merge(A$family[, .(family, adj_raw = fam_v, adj_used = round(fam_shrunk, 3))],
            T$family[, .(family, trim_raw = fam_v, trim_used = round(fam_shrunk, 3))],
            by = "family")[order(-trim_raw)])

summarise <- function(d, label) {
  e <- d[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
             b = mean(100 * abs(base_m - act))), by = .(event_id, family)][races >= MINR]
  data.table(config = label, beat = sum(e$m < e$b), of = nrow(e),
             mae = round(weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}
cat("\n=== held out, 44 events ===\n")
cfg <- list(flat = list(), adj_only = list(adj = A$map), trim_only = list(trim = T$map),
            hl_only = list(hl = H$map), both = list(adj = A$map, trim = T$map),
            all_three = list(adj = A$map, trim = T$map, hl = H$map))
print(rbindlist(lapply(names(cfg), function(nm) summarise(frame(cfg[[nm]])[date >= SPLIT], nm))))

cat("\n=== the three target events, held out ===\n")
tgt <- rbindlist(lapply(names(cfg), function(nm) {
  frame(cfg[[nm]])[date >= SPLIT & event_id %in% TARGETS, {
    dd <- 100 * (abs(pred - act) - abs(base_m - act))
    ci <- stats::t.test(dd)$conf.int
    .(config = nm, races = uniqueN(race_key),
      gap = round(100 * (mean(abs(pred - act)) - mean(abs(base_m - act))) /
                    mean(abs(base_m - act)), 2),
      ci95 = sprintf("[%+.3f, %+.3f]", ci[1], ci[2]),
      sep = data.table::fifelse(ci[2] < 0, "model", data.table::fifelse(ci[1] > 0, "LAST5", "no")))
  }, by = event_id]
}))
print(dcast(tgt, event_id + races ~ config, value.var = "gap"))
print(tgt[config == "all_three", .(event_id, races, gap, ci95, sep)])
# --- THE RANKED SCORECARD ---------------------------------------------------
# Every scored event under the combined hierarchy, with a PAIRED interval, so a
# gap on 17 races cannot be read the same way as one on 400. Sorted best first.
rank <- frame(cfg$all_three)[date >= SPLIT, {
  dd <- 100 * (abs(pred - act) - abs(base_m - act))
  ci <- if (.N >= 5L && stats::sd(dd) > 0) stats::t.test(dd)$conf.int else c(NA_real_, NA_real_)
  .(races = uniqueN(race_key), n = .N,
    model = round(mean(100 * abs(pred - act)), 3),
    last5 = round(mean(100 * abs(base_m - act)), 3),
    gap = round(100 * (mean(abs(pred - act)) - mean(abs(base_m - act))) /
                  mean(abs(base_m - act)), 1),
    ci95 = sprintf("[%+.3f, %+.3f]", ci[1], ci[2]),
    verdict = data.table::fifelse(!is.finite(ci[1]), "too few",
              data.table::fifelse(ci[2] < 0, "model better",
              data.table::fifelse(ci[1] > 0, "LAST-5 BETTER", "not separated"))))
}, by = .(event_id, family)][races >= MINR][order(gap)]
cat("
=== TOP 10 best predicted, combined hierarchy, held out ===
")
print(head(rank[, .(event_id, races, model, last5, gap, ci95, verdict)], 10))
cat("
=== BOTTOM 10, worst first ===
")
print(head(rank[order(-gap), .(event_id, races, model, last5, gap, ci95, verdict)], 10))
cat(sprintf("
of %d scored events: %d separated in our favour, %d against, %d not separated.
",
            nrow(rank), sum(rank$verdict == "model better"),
            sum(rank$verdict == "LAST-5 BETTER"), sum(rank$verdict == "not separated")))
fwrite(rank, file.path(OUT, "marks_hier_scorecard.csv"))

fwrite(data.table(event_id = names(A$map), adj = unname(A$map), trim = unname(T$map),
                  hl = unname(H$map[names(A$map)])),
       file.path(OUT, "marks_hier_event_params.csv"))
say("wrote marks_hier_event_params.csv")
