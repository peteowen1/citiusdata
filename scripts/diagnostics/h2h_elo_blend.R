# Does a finishing-order Elo signal improve version E for 800/1500/5000 (both sexes)?
# Design and verdict: docs/reviews/h2h-elo-blend-2026-11-10.md
#
#   Elo  : per athlete-EVENT, online, from finishing places in R1-code races (all rounds),
#          K schedule as build_h2h_rating.R (K0 60, halves at 5 races, K per pairwise
#          comparison, scale 400). Walk-forward: a snapshot is taken BEFORE the first race
#          on or after each cut, so a cut sees only earlier races.
#   Blend: score = E + lambda * z, z = (Elo - mean) / sd across the Elo-rated athletes of
#          that event at that cut (lambda is in perf = -log(mark) units per SD of Elo).
#          Athletes with no prior placing get z = 0 (no adjustment).
#   Fit  : lambda on the four 2024 cuts; verdict on the four 2025 cuts (held out).
#   Targets (R1-code races in the 12 months after each cut, athletes who raced):
#          PRIMARY within-race concordance (pairs of finishers ordered correctly, resampled
#          by race); SECONDARY marks MAE on each athlete's mean R1 perf (level-corrected
#          per arm on 2024, resampled by athlete).
# Reads the parquet copy of the corpus (same build as athletics_corpus.rds, 2026-11-07) with
# a column/event filter, because the rds needs ~3 GB to load. Run via PowerShell Rscript.
t0 <- Sys.time()
suppressMessages({library(data.table); library(arrow)})
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
R1 <- c("OW", "GW", "GL", "DF", "A")
K0 <- as.numeric(Sys.getenv("H2H_K0", "60")); KH <- as.numeric(Sys.getenv("H2H_KHALF", "5")); SC <- 400
TAG <- if (K0 == 60 && KH == 5) "" else sprintf("_K%g_H%g", K0, KH)
LAM <- c(0, .001, .0025, .005, .0075, .01, .015, .02, .03, .04)
WERRO <- "14856078"; BOL <- "14707010"
nid <- function(x) sub("^0+", "", as.character(x))

# ---- load ----
co <- as.data.table(open_dataset(file.path(OUT, "athletics_corpus.parquet")) |>
  dplyr::filter(event_id %in% EV) |>
  dplyr::select(athlete_id, event_id, date, race_key, round, place, mark, perf, race_code) |>
  dplyr::collect())
co[, athlete_id := nid(athlete_id)][, r1 := toupper(trimws(race_code)) %in% R1]
cat(sprintf("loaded %s rows | events %d | dates %s..%s | R1-code rows %s\n",
            format(nrow(co), big.mark = ","), uniqueN(co$event_id), min(co$date, na.rm = TRUE),
            max(co$date, na.rm = TRUE), format(sum(co$r1), big.mark = ",")))
stopifnot(nrow(co) > 100000, uniqueN(co$event_id) == 6)

# ---- Elo walk-forward ----
g <- co[r1 == TRUE & is.finite(place) & place > 0 & !is.na(date) & !is.na(race_key) &
          !is.na(athlete_id) & athlete_id != ""]
g <- unique(g, by = c("athlete_id", "event_id", "race_key"))
g[, key := paste(athlete_id, event_id, sep = "|")]
g[, rid := paste(event_id, race_key, sep = "#")]
setorder(g, date, rid)
cuts <- as.Date(c("2024-01-01","2024-04-01","2024-07-01","2024-10-01",
                  "2025-01-01","2025-04-01","2025-07-01","2025-10-01"))
keys <- unique(g$key); ki <- match(g$key, keys)
Rt <- rep(1500, length(keys)); Nn <- numeric(length(keys))
races <- split(seq_len(nrow(g)), factor(g$rid, levels = unique(g$rid)))
rdate <- g$date[vapply(races, `[`, 1L, FUN.VALUE = 1L)]
snap <- list(); si <- 1L
take <- function(lab) { s <- data.table(kk = keys, elo = Rt, n = Nn)[n > 0]; setnames(s, "kk", "key"); s[, cut := lab]; s }
for (r in seq_along(races)) {
  while (si <= length(cuts) && rdate[r] >= cuts[si]) { snap[[si]] <- take(cuts[si]); si <- si + 1L }
  ii <- races[[r]]; n <- length(ii); if (n < 2) next
  k <- ki[ii]; cur <- Rt[k]; nn <- Nn[k]; pl <- g$place[ii]
  p <- 1 / (1 + 10^(-outer(cur, cur, "-") / SC)); diag(p) <- 0
  act <- vapply(seq_len(n), function(i) sum(pl[i] < pl[-i]) + 0.5 * sum(pl[i] == pl[-i]), 0)
  Rt[k] <- cur + (K0 / (1 + nn / KH)) / max(1, n - 1) * (act - rowSums(p)); Nn[k] <- nn + 1
}
while (si <= length(cuts)) { snap[[si]] <- take(cuts[si]); si <- si + 1L }
today <- take(as.Date("2026-11-06"))
elo <- rbindlist(snap)
elo[, c("athlete_id", "event_id") := tstrsplit(key, "|", fixed = TRUE)]
today[, c("athlete_id", "event_id") := tstrsplit(key, "|", fixed = TRUE)]
cat(sprintf("Elo: %s races, %s athlete-events rated; max |elo-1500| after 1 race %.1f (must be <= K0 %d)\n",
            format(length(races), big.mark = ","), format(length(keys), big.mark = ","),
            max(abs(today[n == 1, elo] - 1500)), K0))
stopifnot(max(abs(today[n == 1, elo] - 1500)) <= K0 * 1.001)
cat(sprintf("Elo walk-forward done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# ---- version E at each cut ----
fs <- list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE)
stopifnot(length(fs) == 8)
r <- rbindlist(lapply(fs, readRDS), fill = TRUE)
r[, athlete_id := nid(athlete_id)]; r[, cut := as.Date(cut, origin = "1970-01-01")]
r <- unique(r, by = c("athlete_id", "event_id", "cut"))
r[, a := wR_2 / (wR_2 + 0.5 * kR_2)][, E := a * R_2 + (1 - a) * long_365]
r <- r[is.finite(E) & event_id %in% EV]
s <- merge(r[, .(athlete_id, event_id, cut, E)], elo[, .(athlete_id, event_id, cut, elo, n)],
           by = c("athlete_id", "event_id", "cut"), all.x = TRUE)
cat(sprintf("E-rated rows %s; with an Elo (>=1 prior R1-code placing) %s (%.1f%%)\n",
            format(nrow(s), big.mark = ","), format(s[!is.na(elo), .N], big.mark = ","),
            100 * s[, mean(!is.na(elo))]))
s[, mu := mean(elo, na.rm = TRUE), by = .(event_id, cut)]
s[, sdv := sd(elo, na.rm = TRUE), by = .(event_id, cut)]
s[, z := fifelse(is.na(elo), 0, (elo - mu) / sdv)]
s[, fit_set := year(cut) == 2024]

# ---- targets ----
tg <- co[r1 == TRUE & is.finite(place) & place > 0 & !is.na(date)]
tg <- unique(tg, by = c("athlete_id", "event_id", "race_key"))
# (a) concordance pairs
pairs <- rbindlist(lapply(cuts, function(cu) {
  x <- tg[date >= cu & date < cu + 365 & !is.na(race_key)]
  x <- merge(x[, .(athlete_id, event_id, race_key, place)], s[cut == cu, .(athlete_id, event_id, E, z)],
             by = c("athlete_id", "event_id"))
  x[, rid := paste(event_id, race_key, sep = "#")]
  x <- x[rid %in% x[, .N, by = rid][N >= 2, rid]]
  p <- merge(x, x, by = "rid", allow.cartesian = TRUE, suffixes = c("_i", "_j"))[place_i < place_j]
  p[, .(cut = cu, event_id = event_id_i, rid, dE = E_i - E_j, dz = z_i - z_j)]
}))
pairs[, fit_set := year(cut) == 2024]
cat(sprintf("concordance pairs: 2024 %s, 2025 %s\n", format(pairs[fit_set == TRUE, .N], big.mark = ","),
            format(pairs[fit_set == FALSE, .N], big.mark = ","))); stopifnot(nrow(pairs) > 1000)
conc <- function(p, lam) { sc <- p$dE + lam * p$dz; mean((sc > 0) + 0.5 * (sc == 0)) }
gc <- data.table(lam = LAM, conc2024 = sapply(LAM, function(l) 100 * conc(pairs[fit_set == TRUE], l)))
cat("\n2024 fit, concordance % by lambda (higher is better):\n"); print(gc)
lam_c <- gc[which.max(conc2024), lam]
cat(sprintf("lambda chosen on 2024 (concordance): %g\n", lam_c))

# (b) marks target
nx <- rbindlist(lapply(cuts, function(cu)
  tg[date >= cu & date < cu + 365 & is.finite(perf), .(cut = cu, t_mean = mean(perf), nr = .N),
     by = .(athlete_id, event_id)]))
m <- merge(s, nx, by = c("athlete_id", "event_id", "cut"))
mae <- function(p, y) 100 * mean(abs(y - p))
fitm <- m[fit_set == TRUE]
gm <- data.table(lam = LAM, mae2024 = sapply(LAM, function(l) {
  q <- fitm$E + l * fitm$z; mae(q + median(fitm$t_mean - q), fitm$t_mean) }))
cat("\n2024 fit, marks MAE % by lambda (lower is better):\n"); print(gm)
lam_m <- gm[which.min(mae2024), lam]
cat(sprintf("lambda chosen on 2024 (marks): %g\n", lam_m))

# ---- 2025 verdict ----
bootc <- function(p, lam, B = 2000) {   # gain in pp, by race
  p <- copy(p); p[, c0 := (dE > 0) + 0.5 * (dE == 0)]; sc <- p$dE + lam * p$dz
  p[, c1 := (sc > 0) + 0.5 * (sc == 0)]
  a <- p[, .(d = sum(c1 - c0), n = .N, s0 = sum(c0), s1 = sum(c1)), by = rid]
  set.seed(42); bs <- vapply(1:B, function(k) { q <- a[sample(.N, .N, TRUE)]; sum(q$d) / sum(q$n) }, 0)
  c(n_races = nrow(a), n_pairs = sum(a$n), E = 100 * sum(a$s0) / sum(a$n), blend = 100 * sum(a$s1) / sum(a$n),
    gain = 100 * sum(a$d) / sum(a$n), lo = 100 * quantile(bs, .025), hi = 100 * quantile(bs, .975))
}
V <- pairs[fit_set == FALSE]
rows <- list(cbind(event = "ALL", as.data.frame(t(bootc(V, lam_c)))))
for (e in EV) rows[[length(rows) + 1]] <- cbind(event = e, as.data.frame(t(bootc(V[event_id == e], lam_c))))
tabc <- rbindlist(rows)
cat(sprintf("\n=== 2025 VERDICT, concordance (lambda %g, held out). E and blend = %% of finisher pairs ordered correctly (higher is better); gain = blend - E in pp, 95%% CI by race ===\n", lam_c))
print(tabc[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

c00 <- median(fitm$t_mean - fitm$E)
c0m <- median(fitm$t_mean - (fitm$E + lam_m * fitm$z))
M <- m[fit_set == FALSE]
M[, p0 := E + c00][, pb := E + lam_m * z + c0m]
bootm <- function(x, B = 2000) {
  a <- x[, .(d = sum(abs(t_mean - p0) - abs(t_mean - pb)), n = .N), by = athlete_id]
  set.seed(42); bs <- vapply(1:B, function(k) { q <- a[sample(.N, .N, TRUE)]; sum(q$d) / sum(q$n) }, 0)
  c(n_rows = nrow(x), n_athletes = nrow(a), E = mae(x$p0, x$t_mean), blend = mae(x$pb, x$t_mean),
    gain = 100 * sum(a$d) / sum(a$n), lo = 100 * quantile(bs, .025), hi = 100 * quantile(bs, .975))
}
rows <- list(cbind(event = "ALL", as.data.frame(t(bootm(M)))))
for (e in EV) rows[[length(rows) + 1]] <- cbind(event = e, as.data.frame(t(bootm(M[event_id == e]))))
tabm <- rbindlist(rows)
cat(sprintf("\n=== 2025 VERDICT, marks (lambda %g, level-corrected on 2024). E and blend = mean abs error of the athlete's mean R1 perf, in %% of time (lower is better); gain = E - blend (> 0 = blend helps), 95%% CI by athlete ===\n", lam_m))
print(tabm[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat(sprintf("\nsensitivity, marks gain at the concordance lambda %g: ", lam_c))
M[, pb := E + lam_c * z + median(fitm$t_mean - (fitm$E + lam_c * fitm$z))]
print(unname(round(bootm(M)[5:7], 3)))
fwrite(tabc, file.path(OUT, "flags", sprintf("h2h_elo_blend_concordance%s.csv", TAG)))
fwrite(tabm, file.path(OUT, "flags", sprintf("h2h_elo_blend_marks%s.csv", TAG)))

# ---- named check: Werro vs Bol, 800m W ----
EVW <- "AT-800Metres-W"
w <- s[cut == as.Date("2025-10-01") & event_id == EVW & athlete_id %in% c(WERRO, BOL)]
w[, mark_E := exp(-E)][, mark_blend := exp(-(E + lam_c * z))]
w[, who := fifelse(athlete_id == WERRO, "Werro", "Bol")]
cat("\n=== Werro vs Bol, 800m W, 2025-10-01 cut (marks in seconds; blend uses the concordance lambda, no level shift) ===\n")
print(w[, .(who, E = round(E, 4), elo = round(elo, 1), n_races = n, z = round(z, 2),
            mark_E = round(mark_E, 2), mark_blend = round(mark_blend, 2))])
td <- today[event_id == EVW & athlete_id %in% c(WERRO, BOL)][, who := fifelse(athlete_id == WERRO, "Werro", "Bol")]
cat("Elo on all data to 2026-11-06 (no E cached for today):\n"); print(td[, .(who, elo = round(elo, 1), n_races = n)])
pb <- 1 / (1 + 10^(-(td[who == "Werro", elo] - td[who == "Bol", elo]) / SC))
cat(sprintf("Elo-implied P(Werro beats Bol) today: %.3f\n", pb))
sh <- tg[event_id == EVW & athlete_id %in% c(WERRO, BOL) & date >= as.Date("2026-01-01")]
sh <- merge(sh[athlete_id == WERRO, .(race_key, date, round, pW = place, mW = mark)],
            sh[athlete_id == BOL, .(race_key, pB = place, mB = mark)], by = "race_key")
cat("Shared R1-code 800m W races in 2026 (pW = Werro place, pB = Bol place):\n"); print(sh[order(date)])
cat(sprintf("Werro ahead in %d of %d shared races\n", sh[, sum(pW < pB)], nrow(sh)))
cat("total time", format(Sys.time() - t0), "\n")

# ---- Bol: why no row at the 2025-10-01 cut, and her 800m W race list ----
cat("\nBol rows in the E cache for 800m W:", r[athlete_id == BOL & event_id == EVW, .N], "(0 = no 800m ability cached at any cut)\n")
bl <- tg[event_id == EVW & athlete_id == BOL][order(date)]
cat(sprintf("Bol R1-code 800m W placings: %d, first %s, last %s\n", nrow(bl), min(bl$date), max(bl$date)))
wl <- tg[event_id == EVW & athlete_id == WERRO][order(date)]
cat(sprintf("Werro R1-code 800m W placings: %d, first %s\n", nrow(wl), min(wl$date)))
