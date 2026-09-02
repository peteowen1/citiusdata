# Where does the sigma-context scale (0.785) hurt medal LOGLOSS?
#
# WHY LOGLOSS, NOT BRIER. Brier (squared error) said the sigma scale's medal
# effect was a statistical tie everywhere. Controlled comparison on the SAME
# T2 population (601 races) says otherwise for the higher-power metric:
#   tierctrl (no scale):  medal logloss +1.04%, p=0.40 -- no signal
#   sigctrl  (scale 0.785): medal logloss +6.49%, p=2.55e-05 -- REAL, significant
# T1 shows the same reversal, smaller: -1.27% p=0.09 (favourable, n.s.)
# without the scale vs +1.86% p=0.037 (unfavourable, borderline significant)
# with it. This is not noise -- Brier and logloss disagreeing on SIGNIFICANCE
# while agreeing on DIRECTION means the scale is making confident predictions
# MORE confidently wrong somewhere, which Brier's squared error underweights
# relative to logloss's log penalty.
#
# THE ARMS. sigctrl_full (sigma scale ONLY, no project_tier, no family_debias)
# vs tierctrl (deployed, no adjustments) -- isolates the scale, since these two
# differ in exactly that one flag. Read-only against existing artefacts.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_SLL_HOLDOUT", "2016-01-01"))
MIN_RACES <- 10L
say <- function(...) cat(sprintf(...), "\n", sep = "")

load_scored <- function(fn) {
  b <- readRDS(file.path(OUT, fn))
  pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                           p_gold, p_medal, median_mark)]
  outc <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                        hit, hit_medal, merged)]
  merge(pred, outc, by = c("race_id", "athlete_id"))[merged == FALSE]
}
sig <- load_scored("backtest_sigctrl_full.rds")
ctl <- load_scored("backtest_tierctrl.rds")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, sex, family, discipline)]
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]

prep <- function(dd) {
  dd <- merge(dd, act, by = c("race_id", "athlete_id"))
  dd <- merge(dd, reg, by = "event_id")
  dd <- merge(dd, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
  dd[date >= HOLDOUT]
}
sig <- prep(sig); ctl <- prep(ctl)

# Shared rows only, so the comparison is on IDENTICAL races both times.
key <- merge(sig[, .(race_id, athlete_id)], ctl[, .(race_id, athlete_id)], by = c("race_id","athlete_id"))
sig <- merge(sig, key, by = c("race_id","athlete_id")); ctl <- merge(ctl, key, by = c("race_id","athlete_id"))
stopifnot("shared-set sizes differ" = nrow(sig) == nrow(ctl))
say("shared population: %s rows, %s races", format(nrow(sig), big.mark=","), format(uniqueN(sig$race_id), big.mark=","))

EPS <- 1e-4
llf <- function(p, y) { p <- pmin(pmax(p, EPS), 1 - EPS); -(y * log(p) + (1 - y) * log(1 - p)) }
m <- merge(sig[, .(race_id, athlete_id, meet_tier, family, sex, discipline,
                   sig_ll = llf(p_medal, hit_medal), sig_br = (p_medal - hit_medal)^2)],
           ctl[, .(race_id, athlete_id,
                   ctl_ll = llf(p_medal, hit_medal), ctl_br = (p_medal - hit_medal)^2)],
           by = c("race_id", "athlete_id"))
stopifnot("arms are identical -- nothing to compare" = mean(abs(m$sig_ll - m$ctl_ll) > 1e-9) > 0.1)

run_cut <- function(dd, by_col, label, min_races = MIN_RACES) {
  cat(sprintf("\n---- %s ----\n", label))
  lv <- dd[!is.na(get(by_col)), unique(get(by_col))]
  rows <- list()
  for (v in lv) {
    sub <- dd[get(by_col) == v]
    nr <- uniqueN(sub$race_id)
    if (nr < min_races) next
    a <- sub[, .(v = mean(sig_ll)), by = race_id]; b <- sub[, .(v = mean(ctl_ll)), by = race_id]
    mm <- merge(a, b, by = "race_id")
    tt <- t.test(mm$v.x, mm$v.y, paired = TRUE)
    rows[[length(rows)+1L]] <- data.table(
      level = as.character(v), races = nr,
      sig_ll = round(mean(mm$v.x), 4), ctl_ll = round(mean(mm$v.y), 4),
      rel_pct = round(100*(mean(mm$v.x)-mean(mm$v.y))/mean(mm$v.y), 2),
      p = signif(tt$p.value, 3))
  }
  if (!length(rows)) { cat("  no level clears MIN_RACES\n"); return(invisible(NULL)) }
  res <- rbindlist(rows)[order(-rel_pct)]
  print(res, row.names = FALSE)
  invisible(res)
}

cat("\n================ medal LOGLOSS: sigma scale vs control, by tier ================\n")
run_cut(m, "meet_tier", "meet tier")

t2 <- m[meet_tier == "T2_strong"]
cat("\n================ T2 ONLY -- where the significant loss lives ================\n")
run_cut(t2, "family", "family (T2)")
t2[, fs := paste(family, sex, sep = "|")]
run_cut(t2, "fs", "family x sex (T2)", min_races = 6L)
run_cut(t2, "discipline", "discipline (T2)", min_races = 6L)

cat("\n================ T1 for comparison ================\n")
t1 <- m[meet_tier == "T1_elite"]
run_cut(t1, "family", "family (T1)")

cat("\n================ VERDICT ================\n")
say("If T2's loss concentrates in specific families/events, that is where the")
say("scale over-narrows the field most. If it is spread evenly, the scale is")
say("simply too aggressive for the T2 population as a whole -- fields at T2")
say("are wider/noisier than T1 finals, and 0.785 was sized only against T1.")
