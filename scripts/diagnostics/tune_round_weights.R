# How to weight heats/semis, and should a round the athlete WON count less (Pete
# 2026-11-10, after Werro (6-0 vs Bol in 2026 finals) was rated 1.3 s behind Bol because
# she jogs rounds she wins). Version E refitted under each rule, judged on R1 races.
#
# Rules (per historical result in a heat/quarter/semi; finals always weight 1):
#   w = rw * (if the athlete placed 1st in that round: win_w else 1)
#   rw in {1, 0.5, 0.25, 0}; win_w in {1, 0.5, 0}; plus coast_semis (the existing
#   per-athlete coasting correction extended from heats to semis) at rw = win_w = 1.
# Each rule refits BOTH version E pieces (recent: half-lives x2; long: 365 d) with
# estimate_ability(weight_col =, coast_semis =), deployed settings otherwise, only= the
# athletes scored. E = a*R + (1-a)*L, a = wR / (wR + 0.5*kR).
# Scored on R1 racers in the 12 months after each cut (athletes who raced): PRIMARY
# within-race concordance on those R1 races (order), SECONDARY marks MAE vs mean R1
# perf (level-corrected per rule on 2024). Rule chosen on the 4 2024 cuts, verdict on
# the 4 2025 cuts, paired bootstrap CIs. Werro/Bol at the last cut.
# Per cut x rule results cached in data/flags/round_weight_cache/ (resumable).
# Arrow via PowerShell.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "flags", "round_weight_cache"); dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A")
CUTS <- seq(as.Date("2024-01-01"), as.Date("2025-10-01"), by = "3 months")
if (identical(Sys.getenv("SMOKE"), "1")) CUTS <- as.Date(c("2024-01-01", "2025-01-01"))
RULES <- rbind(CJ(rw = c(1, 0.5, 0.25, 0), win_w = c(1, 0.5, 0), cs = FALSE)[!(rw == 0 & win_w < 1)],
               data.table(rw = 1, win_w = 1, cs = TRUE))
RULES[, rule := sprintf("rw%g_win%g%s", rw, win_w, ifelse(cs, "_coastsemis", ""))]
cat("rules:", nrow(RULES), "\n")

co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
h[, `:=`(rc = .round_class(round), r1 = toupper(trimws(race_code)) %in% R1)]
cat("rows", nrow(h), "| round classes:", paste(h[, .N, by = rc][order(-N), sprintf("%s %d", rc, N)], collapse = ", "), "\n")
cal <- deployed_calibration(OUT); ep <- deployed_event_params()
fit <- function(past, as_of, epx, ids, cs) {
  estimate_ability(past, as_of = as_of, half_life = epx[, .(event_id, family, half_life)],
                   races_half_life = epx[, .(event_id, family, races_half_life)],
                   trim_tactical = epx[, .(event_id, family, trim_tactical)],
                   context_scale = epx[, .(event_id, family, context_scale)],
                   peak_gamma = if ("peak_gamma" %in% names(epx)) epx[, .(event_id, family, peak_gamma)] else 0,
                   calibration = cal, adjust_race = isTRUE(DEPLOYED$adjust_race),
                   only = ids, weight_col = "wc", coast_semis = cs)
}
ep_r <- copy(ep)[, half_life := half_life * 2]
ep_l <- copy(ep)[event_id %in% EV, half_life := 365]

res <- list()
for (cu in as.list(CUTS)) {
  past <- h[date < cu]
  tg <- h[r1 == TRUE & date >= cu & date < cu + 365]
  ids <- unique(tg$athlete_id)
  for (k in seq_len(nrow(RULES))) {
    rl <- RULES[k]; f <- file.path(CACHE, sprintf("%s_%s.rds", format(cu), rl$rule))
    if (file.exists(f)) { res[[length(res) + 1]] <- readRDS(f); next }
    t1 <- Sys.time()
    past[, wc := 1]
    past[rc %in% c("heat", "quarter", "semi"), wc := rl$rw]
    past[rc %in% c("heat", "quarter", "semi") & !is.na(place) & place == 1, wc := wc * rl$win_w]
    R <- suppressWarnings(fit(past, cu - 1, ep_r, ids, rl$cs))
    L <- suppressWarnings(fit(past, cu - 1, ep_l, ids, rl$cs))
    for (x in list(R, L)) x[, athlete_id := as.character(athlete_id)]
    e <- merge(R[, .(athlete_id, event_id, R = ability_raw, wR = w_total, kR = shrinkage * w_total / (1 - shrinkage))],
               L[, .(athlete_id, event_id, L = ability_raw)], by = c("athlete_id", "event_id"))
    e[, a := wR / (wR + 0.5 * kR)][, E := a * R + (1 - a) * L]
    e <- e[is.finite(E), .(athlete_id, event_id, E)][, `:=`(cut = cu, rule = rl$rule)]
    saveRDS(e, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f)
    res[[length(res) + 1]] <- e
    cat(sprintf("cut %s rule %-18s rated %5d  %.1f min\n", format(cu), rl$rule, nrow(e), as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  }
  rm(past); invisible(gc())
}
E <- rbindlist(res)
# targets: mean R1 perf next 12 months, and every R1 race in that window for concordance
tm <- rbindlist(lapply(as.list(CUTS), function(cu) h[r1 == TRUE & date >= cu & date < cu + 365,
  .(t_mean = mean(perf)), by = .(athlete_id, event_id)][, cut := cu]))
races <- rbindlist(lapply(as.list(CUTS), function(cu) h[r1 == TRUE & date >= cu & date < cu + 365 & !is.na(race_key),
  .(athlete_id, event_id, race_key, perf)][, cut := cu]))
s <- tm[E, on = c("athlete_id", "event_id", "cut"), nomatch = NULL]
s[, fit_set := year(cut) == 2024]
lv <- s[fit_set == TRUE, .(c0 = median(t_mean - E)), by = rule]
s <- lv[s, on = "rule"][, p := E + c0]
mae <- function(x) 100 * mean(abs(x$t_mean - x$p))
conc_tab <- function(rr) {   # per race, per rule: correct pairs and total pairs
  x <- races[E, on = c("athlete_id", "event_id", "cut"), nomatch = NULL, allow.cartesian = TRUE][rule == rr]
  x[, if (.N >= 2) { ii <- combn(.N, 2); d1 <- sign(E[ii[1, ]] - E[ii[2, ]]); d2 <- sign(perf[ii[1, ]] - perf[ii[2, ]])
                     .(ok = sum(d1 == d2 & d2 != 0), n = sum(d2 != 0), y = year(cut[1])) }, by = .(race_key, cut)]
}
base <- "rw1_win1"
ct <- rbindlist(lapply(RULES$rule, function(rr) conc_tab(rr)[, rule := rr]))
summ <- s[, .(mae_2024 = mae(.SD[fit_set == TRUE]), mae_2025 = mae(.SD[fit_set == FALSE])), by = rule]
cc <- ct[, .(conc_2024 = 100 * sum(ok[y == 2024]) / sum(n[y == 2024]), conc_2025 = 100 * sum(ok[y == 2025]) / sum(n[y == 2025])), by = rule]
summ <- cc[summ, on = "rule"][RULES[, .(rule, rw, win_w, cs)], on = "rule"]
cat("\nPer rule. Concordance % = within-race pairs in the right order (HIGHER is better). MAE % vs mean R1 mark next 12 months (LOWER is better).\n")
print(summ[order(-conc_2024)][, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
best <- summ[which.max(conc_2024), rule]
cat(sprintf("\nchosen on 2024 concordance: %s\n", best))
pb <- function(rr) {   # 2025 concordance gain vs base, bootstrap by race
  a <- ct[rule == base & y == 2025, .(race_key, cut, ok0 = ok, n0 = n)]; b <- ct[rule == rr & y == 2025, .(race_key, cut, ok1 = ok)]
  m <- merge(a, b, by = c("race_key", "cut")); set.seed(42)
  bs <- vapply(1:2000, function(k) { q <- m[sample(.N, .N, TRUE)]; (sum(q$ok1) - sum(q$ok0)) / sum(q$n0) }, 0)
  c(100 * (sum(m$ok1) - sum(m$ok0)) / sum(m$n0), 100 * quantile(bs, c(.025, .975)), nrow(m))
}
for (rr in unique(c(best, "rw0.5_win1", "rw1_win0", "rw1_win1_coastsemis"))) if (rr %in% RULES$rule) {
  g <- pb(rr); cat(sprintf("2025 concordance gain %-22s %+.3f pp [%+.3f, %+.3f] over %d R1 races\n", rr, g[1], g[2], g[3], g[4])) }
w <- E[cut == max(cut) & athlete_id %in% c("14856078", "14707010") & event_id == "AT-800Metres-W"]
w[, mark := exp(-E)][, who := fifelse(athlete_id == "14856078", "Werro", "Bol")]
cat("\nWerro vs Bol at the 2025-10-01 cut (800m W, version E rating as a time):\n")
print(dcast(w, rule ~ who, value.var = "mark")[, lapply(.SD, function(v) if (is.numeric(v)) sprintf("%d:%05.2f", as.integer(v %/% 60), v %% 60) else v)])
fwrite(summ, file.path(OUT, "flags", "tune_round_weights_summary.csv"))
cat("total time", format(Sys.time() - t0), "\n")
