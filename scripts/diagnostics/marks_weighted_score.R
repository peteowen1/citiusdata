# Score the model by races we actually CARE about, not by races we happen to have.
#
# Pete: "can we weight MAE by wac class -- so OW, GL, GW is worth 10x E, F".
#
# Yes, and it is a different lever from the two tier knobs already in the lab,
# which is worth stating because all three sound alike:
#
#   w_static                 how much a historical MARK counts when estimating
#                            an athlete's ability. Swept as ^lambda; 0 won.
#   CITIUS_LAB_TIER_WEIGHT   how much a test row counts when FITTING parameters,
#                            so 27x more numerous T2 races cannot dominate.
#   this script              how much a test row counts when SCORING -- the
#                            objective itself.
#
# The third is the one that decides what "better" means. The project targets LA
# 2028, so a wrong prediction in an Olympic final costs more than a wrong one at
# a category F meet, and an unweighted mean says they are worth the same.
#
# WHY IT MIGHT CHANGE THE ANSWER, and not just the number: every parameter
# adopted today was chosen by an unweighted objective. If the championship races
# want different settings from the club races, we have been tuning for the wrong
# population and the whole day's conclusions need re-reading. That is the
# question this asks, and it is a bigger one than the weighting itself.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_weighted_score.R'
# Env: CITIUS_SCORE_WEIGHTS  "OW=10,GL=10,GW=10,DF=10,A=3,B=3,C=1,D=1,E=1,F=1"
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_2020"))
SPLIT <- as.Date(Sys.getenv("CITIUS_FIT_SPLIT", "2024-01-01"))
MINR  <- as.integer(Sys.getenv("CITIUS_MIN_RACES", "5"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

pairs <- readRDS(file.path(CACHE, "pairs.rds"))
pairs[, tactical := tactical & family %in% citius:::.CITIUS_TACTICAL_FAMILIES]
k     <- readRDS(file.path(CACHE, "keys.rds"))
test  <- readRDS(file.path(CACHE, "test_scored.rds"))
bm    <- readRDS(file.path(CACHE, "base_m.rds"))[, .(athlete_id, event_id, month, base_m)]
fit   <- as.list(readRDS(file.path(OUT, "marks_fit_params.rds")))
ep    <- as.data.table(readRDS(file.path(OUT, "event_params.rds")))
reg   <- as.data.table(citius_events())[, .(event_id, cv_prior)]
pairs <- merge(pairs, reg, by = "event_id", all.x = TRUE)
pairs[!is.finite(cv_prior) | cv_prior <= 0, cv_prior := citius:::.CITIUS_FALLBACK_CV]

# --- the WAC class of each scored race --------------------------------------
# `tier` on the result row is the World Athletics competition category (OW, GL,
# GW, DF, A-F). It is NOT `meet_tier`, the catalogue's T1/T2/T3, which is what
# the test set is already filtered on. A T1_elite meet still carries a WAC code,
# and those codes are what a reader means by "the Olympics" versus "a B meet".
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
tier_of <- unique(ch[, .(race_key, tier)], by = "race_key")
rm(ch); invisible(gc())
test <- merge(test, tier_of, by = "race_key", all.x = TRUE)
test[is.na(tier), tier := "unknown"]

cat("=== WAC classes present in the scored (T1_elite) test set, held out ===\n")
print(test[date >= SPLIT, .(rows = .N, races = uniqueN(race_key)), by = tier][order(-races)])

parse_w <- function(spec) {
  kv <- strsplit(trimws(strsplit(spec, ",")[[1]]), "=")
  stats::setNames(as.numeric(vapply(kv, `[`, "", 2)), vapply(kv, `[`, "", 1))
}
SCHEMES <- list(
  "unweighted"        = NULL,
  "championship 10x"  = parse_w("OW=10,GL=10,GW=10,DF=10,A=3,B=3,C=1,D=1,E=1,F=1"),
  "championship 3x"   = parse_w("OW=3,GL=3,GW=3,DF=3,A=2,B=2,C=1,D=1,E=1,F=1"),
  "championship only" = parse_w("OW=1,GL=1,GW=1,DF=1,A=0,B=0,C=0,D=0,E=0,F=0"))
if (nzchar(Sys.getenv("CITIUS_SCORE_WEIGHTS", "")))
  SCHEMES[["custom"]] <- parse_w(Sys.getenv("CITIUS_SCORE_WEIGHTS"))

i <- match(pairs$event_id, ep$event_id)
HLV <- fifelse(is.na(i), fit$hl,   ep$half_life[i])
RHV <- fifelse(is.na(i), fit$rhl,  ep$races_half_life[i])
CSV <- fifelse(is.na(i), fit$adj,  ep$context_scale[i])
TVV <- fifelse(is.na(i), fit$trim, ep$trim_tactical[i])

# `lam` is the precision-weight exponent, the one lever whose adopted value
# (0) was chosen on the unweighted objective and is worth re-checking here.
frame_at <- function(lam, khub = 2.5) {
  pp <- data.table::copy(pairs)
  data.table::setorder(pp, pid, age_days)
  pp[, .k := seq_len(.N) - 1L, by = pid]
  pp[, w := w_static^lam * 0.5^(age_days / HLV)]
  pp[, w := w * fifelse(is.finite(RHV) & RHV > 0, 0.5^(.k / RHV), 1)]
  pp[, p_use := perf_raw + CSV * (perf - perf_raw)]
  pp <- pp[!(tactical & !is.na(rk) & TVV > 0 & rk <= floor(grp_n * TVV))]
  pp[, mu1 := sum(w * p_use) / sum(w), by = pid][, n_g := .N, by = pid]
  pp[, dev := p_use - mu1][, cut := khub * cv_prior]
  pp[, w_rob := w][dev < -cut & n_g >= 3L, w_rob := w * (cut / abs(dev))]
  r <- pp[, .(ability_raw = sum(w_rob * p_use) / sum(w_rob), w_total = sum(w)), by = pid]
  m <- merge(k[, .(pid, athlete_id, event_id, month, sigma, sigma_between, prior_mu)], r, by = "pid")
  m[, kap := fit$shrink * (sigma^2 / sigma_between^2)]
  m[, pred := (1 - kap / (w_total + kap)) * ability_raw + (kap / (w_total + kap)) * prior_mu]
  merge(merge(test, m[, .(athlete_id, event_id, month, pred)],
              by = c("athlete_id", "event_id", "month")),
        bm, by = c("athlete_id", "event_id", "month"))
}
score <- function(d, wmap, label) {
  d <- copy(d)[date >= SPLIT]
  d[, sw := if (is.null(wmap)) 1 else {
    j <- match(tier, names(wmap)); fifelse(is.na(j), 1, unname(wmap[j])) }]
  d <- d[sw > 0]
  e <- d[, .(races = uniqueN(race_key), n = sum(sw),
             m = sum(sw * abs(pred - act)) / sum(sw),
             b = sum(sw * abs(base_m - act)) / sum(sw)), by = .(event_id, family)][races >= MINR]
  data.table(scheme = label, events = nrow(e), beat = sum(e$m < e$b),
             mae = round(100 * weighted.mean(e$m, e$n), 4),
             vs_last5 = round(100 * (weighted.mean(e$m, e$n) - weighted.mean(e$b, e$n)) /
                                weighted.mean(e$b, e$n), 2))
}
cat("\n=== does the adopted lambda = 0 still win once championships count more? ===\n")
out <- rbindlist(lapply(names(SCHEMES), function(nm)
  rbindlist(lapply(c(0, 0.5, 1), function(l) {
    r <- score(frame_at(l), SCHEMES[[nm]], nm); r[, lambda := l][]
  }))))
print(dcast(out, scheme ~ lambda, value.var = "vs_last5"))
cat("\n(cells are pooled error against last-5; more negative is better. Columns\n")
cat("are the precision-weight exponent, 0 being what was adopted today.)\n")
best <- out[, .SD[which.min(vs_last5)], by = scheme]
print(best[, .(scheme, best_lambda = lambda, events, beat, vs_last5)])
cat(if (all(best$best_lambda == best$best_lambda[1]))
  "\n=> every weighting picks the same lambda. Today's tuning was not steered by\n   which races the objective happened to count.\n"
  else
  "\n=> the weightings DISAGREE on lambda. Today's parameters were chosen by an\n   unweighted objective and need re-fitting against the one we actually want.\n")
fwrite(out, file.path(OUT, "marks_weighted_score.csv"))
say("wrote marks_weighted_score.csv")
