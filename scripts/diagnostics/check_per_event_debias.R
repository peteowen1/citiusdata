# Can a per-event level correction make predicted marks beat the last-5 baseline?
#
# THE CLAIM UNDER TEST (from a 2026-09-01 diagnostic): the model's marks loss to
# last-5 is not worse discrimination, it is a per-EVENT level offset. Centred
# globally the model loses; centred within event x sex it WINS. Fitting per-event
# offsets pre-2025 and applying them to 2025+ was reported to move the model from
# -2.98% to -10.09% against last-5.
#
# WHY IT NEEDS AN INDEPENDENT CHECK. An in-sample de-bias is guaranteed to look
# good -- subtracting each group's own mean error minimises that group's error by
# construction. The only question that matters is whether the offsets TRANSFER,
# so everything here is fitted on train and never refitted on test.
#
# FAIRNESS. The model is de-biased and so is last-5, with the identical
# procedure. De-biasing only the model would be rigging the comparison; last-5
# is close to unbiased already, so this should cost it little -- but it has to
# be offered the same treatment or the result is not a comparison.
#
# ANCHOR CHECKS, written before the output was looked at:
#   A1  IN-SAMPLE de-bias must improve the model. If it does not, the fitting
#       code is broken -- this is the one result that is true by construction.
#   A2  offsets must be STABLE train->test (positive correlation). A near-zero
#       correlation means the offsets are noise and any test gain is luck.
#   A3  last-5's own offsets must be SMALLER than the model's -- the whole claim
#       is that the model carries per-event bias that last-5 does not.
#   A4  train and test must be disjoint in time and share no race.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")

ARM   <- Sys.getenv("CITIUS_DEBIAS_ARM", "backtest_ctrl_now.rds")
SPLIT <- as.Date(Sys.getenv("CITIUS_DEBIAS_SPLIT", "2025-01-01"))
MIN_N <- as.integer(Sys.getenv("CITIUS_DEBIAS_MIN_N", "25"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | split %s | min group n %d", ARM, format(SPLIT), MIN_N)

b <- readRDS(file.path(OUT, ARM))
d <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                            a_mark = median_mark)],
           as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id))],
           by = c("race_id", "athlete_id"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
act <- unique(act, by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, orientation, sex, family)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

# ---- last-5 baseline, leakage-safe (same construction as score_arm.R) ------
hist <- deployed_history(OUT, events = unique(d$event_id),
                         from = min(d$date) - 3650, to = max(d$date))
hist <- hist[!is.na(perf) & !is.na(date)]
hist[, athlete_id := as.character(athlete_id)]
setorder(hist, athlete_id, event_id, date)
g <- c("athlete_id", "event_id")
hist[, k := seq_len(.N), by = g][, cs := cumsum(perf), by = g][, cs5 := shift(cs, 5L, fill = 0), by = g]
q <- d[, .(athlete_id, event_id, date = date - 1L, race_id)]
setkeyv(hist, c("athlete_id", "event_id", "date"))
m <- hist[q, on = .(athlete_id, event_id, date), roll = TRUE, mult = "last",
          .(race_id, athlete_id, k, cs, cs5)]
m[, l5 := (cs - cs5) / pmin(k, 5)]
d <- merge(d, unique(m[, .(race_id, athlete_id, l5)], by = c("race_id","athlete_id")),
           by = c("race_id", "athlete_id"))

d[, `:=`(act_perf = orientation * log(actual), a_perf = orientation * log(a_mark))]
d <- d[!is.na(act_perf) & !is.na(a_perf) & !is.na(l5)]
d[, grp := paste(event_id, sex, sep = "|")]
d[, `:=`(em = 100 * (a_perf - act_perf), eb = 100 * (l5 - act_perf))]

tr <- d[date <  SPLIT]; te <- d[date >= SPLIT]
cat("\n==== ANCHOR A4: disjoint split ====\n")
say("train %s rows (%s..%s) | test %s rows (%s..%s) | shared races %d",
    format(nrow(tr), big.mark=","), format(min(tr$date)), format(max(tr$date)),
    format(nrow(te), big.mark=","), format(min(te$date)), format(max(te$date)),
    length(intersect(tr$race_id, te$race_id)))

# Fit offsets on TRAIN only; groups below MIN_N get no offset (0), which is the
# honest fallback -- a mean over 3 rows is noise, and Pete's own framing was
# "when samples are large enough".
off <- tr[, .(n = .N, om = mean(em), ob = mean(eb)), by = grp]
off[, `:=`(use_m = fifelse(n >= MIN_N, om, 0), use_b = fifelse(n >= MIN_N, ob, 0))]
te <- merge(te, off[, .(grp, use_m, use_b, n_train = n)], by = "grp", all.x = TRUE)
te[is.na(use_m), `:=`(use_m = 0, use_b = 0, n_train = 0L)]

cat("\n==== ANCHOR A3: whose per-event offsets are bigger? ====\n")
big <- off[n >= MIN_N]
say("groups with n>=%d: %d | model offset sd %.3f pp | last-5 offset sd %.3f pp",
    MIN_N, nrow(big), sd(big$om), sd(big$ob))
say("A3 %s", if (sd(big$om) > sd(big$ob)) "PASS - model carries more per-event bias" else "FAIL - claim does not hold")

cat("\n==== ANCHOR A2: do offsets transfer? ====\n")
te_off <- te[, .(n = .N, om = mean(em), ob = mean(eb)), by = grp]
cmp <- merge(off[n >= MIN_N, .(grp, tr_m = om, tr_b = ob)], te_off[n >= 10, .(grp, te_m = om, te_b = ob)], by = "grp")
say("groups compared %d | model offset r(train,test) %.3f | last-5 r %.3f",
    nrow(cmp), cor(cmp$tr_m, cmp$te_m), cor(cmp$tr_b, cmp$te_b))
say("A2 %s", if (cor(cmp$tr_m, cmp$te_m) > 0.4) "PASS - offsets are stable enough to transfer" else "FAIL - offsets look like noise")

rel <- function(x, y) 100 * (mean(abs(x)) - mean(abs(y))) / mean(abs(y))
ctr <- function(v) v - mean(v, na.rm = TRUE)

blk <- function(dd, label) {
  if (!nrow(dd)) return(invisible())
  say("\n---- %s : %s rows, %s races, coverage n>=%d = %.1f%% ----", label,
      format(nrow(dd), big.mark=","), format(uniqueN(dd$race_id), big.mark=","),
      MIN_N, 100*mean(dd$n_train >= MIN_N))
  # status quo
  say("  %-34s model %8.4f  last5 %8.4f  rel %+7.2f%%", "raw MAE (status quo)",
      mean(abs(dd$em)), mean(abs(dd$eb)), rel(dd$em, dd$eb))
  say("  %-34s model %8.4f  last5 %8.4f  rel %+7.2f%%", "centred MAE (status quo)",
      mean(abs(ctr(dd$em))), mean(abs(ctr(dd$eb))), rel(ctr(dd$em), ctr(dd$eb)))
  # de-biased, offsets fitted on TRAIN, both predictors treated identically
  dm <- dd$em - dd$use_m; db <- dd$eb - dd$use_b
  say("  %-34s model %8.4f  last5 %8.4f  rel %+7.2f%%", "raw MAE (per-event de-biased)",
      mean(abs(dm)), mean(abs(db)), rel(dm, db))
  say("  %-34s model %8.4f  last5 %8.4f  rel %+7.2f%%", "centred MAE (per-event de-biased)",
      mean(abs(ctr(dm))), mean(abs(ctr(db))), rel(ctr(dm), ctr(db)))
  p <- t.test(abs(dm), abs(db), paired = TRUE)
  say("  paired raw |err| model vs last5 after de-bias: %+.4f pp, p = %.3g",
      mean(abs(dm)) - mean(abs(db)), p$p.value)
  say("  bias: model %+.3f -> %+.3f | last5 %+.3f -> %+.3f",
      mean(dd$em), mean(dm), mean(dd$eb), mean(db))
}

cat("\n==== ANCHOR A1: in-sample sanity (must improve, true by construction) ====\n")
tr2 <- merge(tr, off[, .(grp, use_m, use_b)], by = "grp")
say("train raw MAE  %.4f -> %.4f  (%s)", mean(abs(tr2$em)),
    mean(abs(tr2$em - tr2$use_m)),
    if (mean(abs(tr2$em - tr2$use_m)) < mean(abs(tr2$em))) "PASS" else "FAIL - fitting is broken")

cat("\n================ OUT-OF-SAMPLE TEST (the only result that counts) ================\n")
blk(te, "TEST, all tiers")
blk(te[meet_tier == "T1_elite"], "TEST, T1_elite")

cat("\n---- largest fitted model offsets (train, n>=MIN_N) ----\n")
print(head(off[n >= MIN_N][order(-abs(om)), .(grp, n, model_off = round(om,2), last5_off = round(ob,2))], 12))
