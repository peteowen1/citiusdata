# Small continuous pull towards the R1 median for athletes back from an absence
# (Pete 2026-11-10: "a bit of regression here performs better"). Follows
# returners_after_a_year.R, where a flat 10% pull was best at 73 athletes but within noise.
#
#   pred = (1 - f) * E + f * F,   f = p * (1 - 0.5^(days since last race in ANY event / H))
#   E = version E (a*R + (1-a)*L; recent half-life x2, kappa_R x0.5, long 365 d)
#   F = the event's median R1 perf over the 730 days before the cut
# f is ~0 for an athlete who just raced and rises towards p with time away (continuous).
# Grid p x H; each setting level-corrected on 2024; p, H chosen on the 2024 cuts; verdict
# on the 2025 cuts for all R1 racers and for those away 90+/180+/365+ days, against two
# targets: mean R1 perf over the next 12 months, and the FIRST R1 race back. Paired
# athlete-bootstrap CI vs no pull. Reads data/flags/tune_E_cache + the corpus.
# Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A")
P <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5); H <- c(90, 180, 365, 730, 1460)
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[, athlete_id := as.character(athlete_id)]
anyd <- co[!is.na(date), .(athlete_id, date)]
h <- co[event_id %in% EV & !is.na(perf)]; rm(co); invisible(gc())
h[, r1 := toupper(trimws(race_code)) %in% R1]
fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE); stopifnot(length(fs) == 8)
out <- list()
for (cu in sort(unique(r$cut))) {
  cu <- as.Date(cu, origin = "1970-01-01"); d <- r[cut == cu]
  fl <- h[r1 == TRUE & date < cu & date >= cu - 730, .(floor = median(perf)), by = event_id]
  la <- anyd[date < cu, .(last_any = max(date)), by = athlete_id]
  nx <- h[r1 == TRUE & date >= cu & date < cu + 365][order(date)][
    , .(t_mean = mean(perf), t_first = perf[1]), by = .(athlete_id, event_id)]
  d <- fl[d, on = "event_id"]; d <- la[d, on = "athlete_id"]
  out[[length(out) + 1]] <- nx[d, on = c("athlete_id", "event_id"), nomatch = NULL]
}
s <- rbindlist(out, fill = TRUE)
s[, a := wR_2 / (wR_2 + 0.5 * kR_2)][, E := a * R_2 + (1 - a) * long_365]
s <- s[is.finite(E) & is.finite(floor) & !is.na(last_any)]
s[, away := pmax(as.numeric(cut - last_any), 0)]
s[, fit_set := year(cut) == 2024]
cat(sprintf("R1 racers: 2024 %d rows, 2025 %d rows; 2025 away 90+ %d, 180+ %d, 365+ %d\n",
            s[fit_set == TRUE, .N], s[fit_set == FALSE, .N], s[fit_set == FALSE & away > 90, .N],
            s[fit_set == FALSE & away > 180, .N], s[fit_set == FALSE & away > 365, .N]))
pred <- function(x, p, hl) { f <- p * (1 - 0.5^(x$away / hl)); (1 - f) * x$E + f * x$floor }
mae <- function(p, y) 100 * mean(abs(y - p))
boot <- function(x, y, p1, p2) {
  g <- x[, .(d = sum(abs(get(y) - get(p1)) - abs(get(y) - get(p2))), n = .N), by = athlete_id]
  set.seed(42); b <- vapply(1:2000, function(k) { q <- g[sample(.N, .N, TRUE)]; sum(q$d) / sum(q$n) }, 0)
  c(100 * sum(g$d) / sum(g$n), 100 * quantile(b, c(.025, .975)), nrow(g))
}
fit <- s[fit_set == TRUE]
for (tg in c("t_mean", "t_first")) {
  grid <- CJ(p = P, hl = H)
  grid[, fit_mae := mapply(function(p, hl) { q <- pred(fit, p, hl); mae(q + median(fit[[tg]] - q), fit[[tg]]) }, p, hl)]
  grid[, fit_mae_away180 := mapply(function(p, hl) { q <- pred(fit, p, hl); c0 <- median(fit[[tg]] - q)
    k <- fit$away > 180; mae(q[k] + c0, fit[[tg]][k]) }, p, hl)]
  best <- grid[p > 0][which.min(fit_mae_away180)]
  cat(sprintf("\n===== target: %s =====\n", if (tg == "t_mean") "mean R1 mark over the next 12 months" else "FIRST R1 race back"))
  cat("2024 fit, marks MAE % for athletes away 180+ days (lower is better), rows p x columns H:\n")
  print(dcast(grid, p ~ hl, value.var = "fit_mae_away180")[, lapply(.SD, function(v) round(v, 3))])
  cat(sprintf("chosen on 2024 (away 180+): p %.2f, H %d\n", best$p, best$hl))
  V <- s[fit_set == FALSE]
  c00 <- median(fit[[tg]] - pred(fit, 0, 365)); qb <- pred(fit, best$p, best$hl); c0b <- median(fit[[tg]] - qb)
  V[, p0 := pred(V, 0, 365) + c00][, pb := pred(V, best$p, best$hl) + c0b]
  cat("2025 verdict (held out). MAE % none vs chosen pull; gain = none minus pull, 95% CI by athlete (> 0 = pull helps):\n")
  for (lo in c(0, 90, 180, 365)) {
    x <- V[away > lo]; b <- boot(x, tg, "p0", "pb")
    cat(sprintf("  away > %3d d: rows %5d athletes %4d | none %.3f | pull %.3f | gain %+.3f [%+.3f, %+.3f]\n",
                lo, nrow(x), b[4], mae(x$p0, x[[tg]]), mae(x$pb, x[[tg]]), b[1], b[2], b[3]))
  }
  fwrite(grid, file.path(OUT, "flags", sprintf("tune_return_pull_%s.csv", tg)))
}
cat("total time", format(Sys.time() - t0), "\n")
