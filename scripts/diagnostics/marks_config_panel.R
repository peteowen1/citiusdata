# Score a handful of NAMED configs on the fit years and the held-out years side
# by side, against the fair baseline.
#
# WHY. The coordinate descent in marks_fit.R, re-run against the fair baseline,
# chose half-life 730 with races-since decay OFF -- and that config scores 11 of
# 35 held out where the deployed one scores 19. It also declined races decay
# entirely, which the marks lab measured as the single biggest available gain.
# A descent that picks a config this bad is either overfitting its window or
# reporting a bug, and the way to tell them apart is to stop searching and
# simply score the configs anyone would actually propose, on both windows.
#
# The panel uses marks_fit.R's own machinery, per-family half-life overrides
# included, so if the descent and the lab disagree the difference is visible
# here rather than hidden in a search path.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_config_panel.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]

hl_of <- function(fam, hl_global, hl_map) {
  v <- rep(hl_global, length(fam))
  if (length(hl_map)) { hv <- unlist(hl_map); i <- match(fam, names(hv)); v[!is.na(i)] <- hv[i[!is.na(i)]] }
  v
}
predict_at <- function(p) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  hlv <- if (isTRUE(p$families)) hl_of(pp$family, p$hl, DEPLOYED$hl_family) else rep(p$hl, nrow(pp))
  w <- pp$w_static * 0.5^(pp$age_days / hlv)
  if (is.finite(p$rhl) && p$rhl > 0) w <- w * 0.5^(pp$.k / p$rhl)
  p_use <- pp$perf_raw + p$adj * (pp$perf - pp$perf_raw)
  keep <- if (p$trim <= 0) rep(TRUE, nrow(pp)) else
    !(pp$tactical & !is.na(pp$rk) & pp$rk <= floor(pp$grp_n * p$trim))
  r <- data.table(pid = pp$pid, w = w, p_use = p_use)[keep,
        .(ability_raw = sum(w * p_use) / sum(w), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := p$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  m[, .(athlete_id, event_id, month, pred)]
}
frame <- function(p) {
  d <- merge(merge(test, predict_at(p), by = c("athlete_id", "event_id", "month")),
             bm, by = c("athlete_id", "event_id", "month"))
  stopifnot("fair baseline missing on some rows" = all(is.finite(d$base_m)))
  d
}
ev <- function(d) {
  e <- d[, .(races = uniqueN(race_key), n = .N, m = mean(100 * abs(pred - act)),
             b = mean(100 * abs(base_m - act))), by = .(event_id, family)][races >= 10]
  e[, beat := m < b][]
}
line <- function(p, label) {
  d <- frame(p)
  out <- lapply(list(fit = d[date < SPLIT], hold = d[date >= SPLIT]), function(x) {
    e <- ev(x)
    list(beat = sum(e$beat), of = nrow(e), mae = weighted.mean(e$m, e$n),
         b = weighted.mean(e$b, e$n))
  })
  data.table(config = label,
             fit = sprintf("%2d/%2d  %+.2f%%", out$fit$beat, out$fit$of,
                           100 * (out$fit$mae - out$fit$b) / out$fit$b),
             held_out = sprintf("%2d/%2d  %+.2f%%", out$hold$beat, out$hold$of,
                                100 * (out$hold$mae - out$hold$b) / out$hold$b),
             hold_mae = round(out$hold$mae, 4))
}
base <- list(hl = 365, trim = 0.25, shrink = 1, adj = 1, rhl = Inf, families = TRUE)
p <- function(...) modifyList(base, list(...))

res <- rbindlist(list(
  line(p(),                              "deployed (365, races off)"),
  line(p(hl = 730),                      "730, races off"),
  line(p(rhl = 5),                       "365, races 5"),
  line(p(hl = 730, rhl = 5),             "730, races 5"),
  line(p(hl = 540, rhl = 5),             "540, races 5"),
  line(p(rhl = 8),                       "365, races 8"),
  line(p(hl = 730, rhl = 8),             "730, races 8"),
  line(p(families = FALSE),              "365, races off, NO family overrides"),
  line(p(hl = 730, rhl = 5, families = FALSE), "730, races 5, NO family overrides")))
cat("\n=== fair baseline, per-event beaten and pooled gap ===\n")
print(res)

# DOES THE FIT WINDOW AGREE WITH THE HELD-OUT ONE? If a config that wins on
# 2020-2023 loses on 2024+, the window is not representative and no amount of
# searching it will help. 2020-2022 is the COVID racing calendar: athletes
# raced far less, which flatters a long half-life and starves a races-since
# term of the very thing it counts.
cat("\n=== does the fit window rank configs the same way as held out? ===\n")
f <- as.numeric(sub("%.*", "", sub(".*  ", "", res$fit)))
h <- as.numeric(sub("%.*", "", sub(".*  ", "", res$held_out)))
cat(sprintf("Spearman(fit gap, held-out gap) across %d configs: %.3f\n",
            length(f), stats::cor(f, h, method = "spearman")))
cat(sprintf("best on fit: %s | best held out: %s\n",
            res$config[which.min(f)], res$config[which.min(h)]))

cat("\n=== races per athlete-month, by year: is the fit window even comparable? ===\n")
kk <- merge(k[, .(pid, month)], pairs[, .(n_hist = .N), by = pid], by = "pid")
kk[, yr := as.integer(format(month, "%Y"))]
print(kk[, .(athlete_months = .N, median_history = median(n_hist)), by = yr][order(yr)])
fwrite(res, file.path(OUT, "marks_config_panel.csv"))
say("wrote marks_config_panel.csv")
