# Backtest: does pulling a thin-record athlete towards their OWN long record predict
# their next race better than pulling them towards the whole-event mean (deployed)?
# Follows thin_record_shrinkage.R (2026-11-09, Kipyegon 4:08). Changes nothing published.
#
# Design (fixed before running; the x5 half-life is the prototype's, NOT tuned here):
#   - 8 cut dates, quarterly 2024-01-01 .. 2025-10-01; 800/1500/5000 m both sexes.
#   - At each cut: history = results dated BEFORE the cut; rate with the deployed
#     config (A) and with the own-record prior (B = same shrinkage weight, towards
#     ability_raw refitted with every event half-life x5).
#   - Target: each rated athlete-event's FIRST result in (cut, cut + 120 days].
#     Error = perf_actual - ability (log-mark scale, higher perf = faster).
#   - Metrics, both judge an ABILITY change (citiusverse CLAUDE.md): marks MAE (%),
#     and concordance (pairwise order) within races with 2+ scored athletes.
#   - Reported by shrinkage bucket at the cut, because B only differs where the pull
#     is large. Paired bootstrap by athlete for the MAE gap.
#   - TIME SINCE LAST RACE (Pete, 2026-11-09): arms C = B + gap offset and D = A + gap
#     offset. The offset is the mean error by gap bucket (days from last result to the
#     cut), pooled over events, FITTED ON THE 2024 CUTS ONLY and applied to the 2025
#     cuts. All four arms are compared on the 2025 cuts (held out); the 2024 numbers
#     are in-sample for C and D and printed only for reference.
# Known leaks, same for A and B so the comparison stays fair: the deployed
# calibration and event params were fitted on the full corpus, and
# flag_implausible() runs on the full corpus.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
CUTS <- seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "3 months")
# SMOKE=1: two cuts only (one per year), to time one unit and check the code end to end.
if (identical(Sys.getenv("SMOKE"), "1")) CUTS <- as.Date(c("2024-01-01", "2025-01-01"))
HORIZON <- 120

co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
cat("rows", nrow(h), "athletes", uniqueN(h$athlete_id), "dates", format(range(h$date)), "\n")
cal <- deployed_calibration(OUT)
ep <- deployed_event_params()
ep_long <- copy(ep)[, half_life := half_life * 5]

res <- list()
for (i in seq_along(CUTS)) {
  cut <- CUTS[i]; t1 <- Sys.time()
  past <- h[date < cut]
  a <- deployed_ability(past, as_of = cut - 1, calibration = cal, event_params = ep)
  l <- .deployed_ability_raw(past, cut - 1, cal, ep_long)
  for (x in list(a, l)) x[, athlete_id := as.character(athlete_id)]
  ab <- merge(a[, .(athlete_id, event_id, ability, ability_raw, shrinkage, w_total, last_date)],
              l[, .(athlete_id, event_id, long_raw = ability_raw)], by = c("athlete_id", "event_id"))
  ab <- ab[as.numeric(cut - last_date) <= 730]
  ab[, ability_B := (1 - shrinkage) * ability_raw + shrinkage * long_raw]
  nxt <- h[date >= cut & date < cut + HORIZON][order(date)]
  nxt <- nxt[, .SD[1], by = .(athlete_id, event_id)][, .(athlete_id, event_id, date, race_key, perf)]
  s <- merge(ab, nxt, by = c("athlete_id", "event_id"))
  s[, cut := cut]
  res[[i]] <- s
  cat(sprintf("cut %s: rated %d, scored %d, %.1f min\n", format(cut), nrow(ab), nrow(s),
              as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  rm(past, a, l, ab, nxt); invisible(gc())
}
r <- rbindlist(res)
stopifnot(nrow(r) > 300, r[, mean(!is.na(ability_B))] > 0.99)
r[, gap_days := as.numeric(cut - last_date)]
r[, gap := cut(gap_days, c(-Inf, 60, 120, 240, 365, Inf), c("<60d", "60-120d", "120-240d", "240-365d", ">365d"))]
r[, fit_set := year(cut) == 2024]
r[, `:=`(err_A0 = perf - ability, err_B0 = perf - ability_B)]
off <- r[fit_set == TRUE, .(off_A = mean(err_A0), off_B = mean(err_B0), n_fit = .N), by = gap]
# RELATIVE to the <60d group (smoke run 2026-11-09: every bucket, even <60d, ran ~1.2-1.6%
# slow, i.e. a level bias, not a gap effect). Subtracting the <60d offset makes C and D
# measure ONLY the extra effect of a longer gap. The level bias is reported on its own.
lvl <- off[gap == "<60d"]
stopifnot(nrow(lvl) == 1)
cat(sprintf("
Level bias (all next races, 2024 cuts, gap <60d): A %+.3f%%  B %+.3f%%  (negative = ran slower than rated)
",
            100 * lvl$off_A, 100 * lvl$off_B))
off[, `:=`(off_A = off_A - lvl$off_A, off_B = off_B - lvl$off_B)]
cat("
Time-since-last-race offsets RELATIVE to <60d, fitted on 2024 cuts (negative = slower than a recent racer):
")
print(off[order(gap)][, .(gap, n_fit, off_A_pct = round(100 * off_A, 3), off_B_pct = round(100 * off_B, 3))])
r <- off[r, on = "gap"][is.na(off_A), `:=`(off_A = 0, off_B = 0)]
r[, `:=`(ability_C = ability_B + off_B, ability_D = ability + off_A)]
r[, `:=`(err_A = perf - ability, err_B = perf - ability_B, err_C = perf - ability_C, err_D = perf - ability_D)]
r[, bucket := cut(shrinkage, c(-Inf, 0.05, 0.2, 0.5, Inf), c("<0.05", "0.05-0.2", "0.2-0.5", ">=0.5"))]
fwrite(r, file.path(OUT, "flags", if (identical(Sys.getenv("SMOKE"), "1")) "backtest_thin_record_prior_SMOKE.csv" else "backtest_thin_record_prior.csv"))

boot_gap <- function(x, e1, e2) {   # paired, resampling athletes: mean(|e1| - |e2|)
  g <- x[, .(d = sum(abs(get(e1)) - abs(get(e2))), n = .N), by = athlete_id]
  set.seed(42)
  b <- vapply(1:2000, function(k) { s <- g[sample(.N, .N, TRUE)]; sum(s$d) / sum(s$n) }, 0)
  100 * quantile(b, c(.025, .975))
}
conc <- function(x, col) {   # share of within-race pairs ordered correctly
  x <- x[!is.na(race_key)][, if (.N >= 2) .SD, by = race_key]
  if (!nrow(x)) return(c(NA, 0))
  v <- x[, { p <- get(col); o <- perf; ii <- combn(.N, 2)
             d1 <- sign(p[ii[1, ]] - p[ii[2, ]]); d2 <- sign(o[ii[1, ]] - o[ii[2, ]])
             .(ok = sum(d1 == d2 & d2 != 0), n = sum(d2 != 0)) }, by = race_key]
  c(100 * sum(v$ok) / sum(v$n), sum(v$n))
}
mae <- function(x, e) 100 * mean(abs(x[[e]]))
report <- function(x, label) {
  cat(sprintf("
== %s ==  (marks error %%, lower is better; gap vs A, above 0 = arm beats A, 95%% CI by athlete)
", label))
  for (b in c("ALL", levels(x$bucket), levels(x$gap))) {
    y <- if (b == "ALL") x else if (b %in% levels(x$bucket)) x[bucket == b] else x[gap == b]
    tag <- if (b %in% levels(x$gap)) paste("gap", b) else paste("pull", b)
    if (b == "ALL") tag <- "ALL"
    if (nrow(y) < 30) { cat(sprintf("%-15s n %d, too few
", tag, nrow(y))); next }
    cB <- boot_gap(y, "err_A", "err_B"); cC <- boot_gap(y, "err_A", "err_C"); cD <- boot_gap(y, "err_A", "err_D")
    cat(sprintf("%-15s n %6d  A %.3f  B %.3f [%+.3f,%+.3f]  C %.3f [%+.3f,%+.3f]  D %.3f [%+.3f,%+.3f]
",
                tag, nrow(y), mae(y, "err_A"), mae(y, "err_B"), cB[1], cB[2], mae(y, "err_C"), cC[1], cC[2],
                mae(y, "err_D"), cD[1], cD[2]))
  }
  cc <- vapply(c("ability", "ability_B", "ability_C", "ability_D"), function(k) conc(x, k)[1], 0)
  cat(sprintf("concordance %% (higher is better): A %.3f  B %.3f  C %.3f  D %.3f  (pairs %d)
",
              cc[1], cc[2], cc[3], cc[4], conc(x, "ability")[2]))
}
cat("
Arms: A deployed; B own-record pull; C = B + time-since-race offset; D = A + time-since-race offset.
")
report(r[fit_set == FALSE], "HELD OUT: 2025 cuts (the verdict)")
report(r[fit_set == TRUE], "2024 cuts (in-sample for C and D, reference only)")
cat("
By event, held out, marks error %:
")
print(r[fit_set == FALSE, .(n = .N, A = round(mae(.SD, "err_A"), 3), B = round(mae(.SD, "err_B"), 3),
            C = round(mae(.SD, "err_C"), 3), D = round(mae(.SD, "err_D"), 3)), by = event_id])
cat("total time", format(Sys.time() - t0), "\n")
