# What is the REMAINING medal-Brier deficit made of, after narrowing sigma?
#
# Established so far (2026-09-01):
#   - the deficit is women's, concentrated in jump (jump|W +15.14%, p=0.00127)
#   - it is NOT explained by spread WIDTH (losing cells are better calibrated
#     for width than winning ones)
#   - narrowing sigma closes 44% of it and leaves +1.67%
#   - no level correction can touch it: a uniform per-race shift to `ability`
#     cannot change who beats whom
#
# So the remainder is about WHICH athletes are ranked where. Three cuts that
# can distinguish the candidate mechanisms, none of which the previous sweeps
# asked:
#   1. RELIABILITY -- is the model over- or under-confident on medals in the
#      losing cells? A Brier deficit can come from either, and the fix differs.
#   2. EVIDENCE DEPTH -- `shrinkage` and `w_total` are carried in the
#      predictions table. If the deficit concentrates on thin-evidence
#      athletes, it is a prior/seeding problem, not an ordering one. (Related:
#      debutants measured 1.55 sd below the value they were seeded at.)
#   3. WITHIN jump|W by discipline -- vertical (High Jump, Pole Vault:
#      progressive bar heights, a genuinely different outcome process) vs
#      horizontal (Long, Triple). PV|W already showed +14.49% medal on its own.
#
# Read-only against the existing arm. No simulation, so this is cheap.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_MD_HOLDOUT", "2025-01-01"))
ARM <- Sys.getenv("CITIUS_MD_ARM", "backtest_tierctrl.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
say("arm %s | holdout %s", ARM, format(HOLDOUT))

b <- readRDS(file.path(OUT, ARM))
pred <- as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                         p_gold, p_medal, median_mark, shrinkage, w_total)]
outc <- as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id),
                                      hit, hit_medal, merged)]
d <- merge(pred, outc, by = c("race_id", "athlete_id"))[merged == FALSE]
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, `:=`(athlete_id = as.character(athlete_id), competition_id = as.character(competition_id))]
act <- unique(ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
                 .(race_id = race_key, athlete_id, event_id, date, competition_id)],
              by = c("race_id", "athlete_id"))
d <- merge(d, act, by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events())[, .(event_id, sex, family, discipline)]
d <- merge(d, reg, by = "event_id")
cat_tbl <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat_tbl[, competition_id := as.character(competition_id)]
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]
d[, fs := paste(family, sex, sep = "|")]
LOSE <- c("jump|W", "sprint|W", "hurdles|W")
d[, cell := fifelse(fs %chin% LOSE, "LOSING cells (jump|W, sprint|W, hurdles|W)", "rest")]
say("population: %s rows, %s races", format(nrow(d), big.mark=","),
    format(uniqueN(d$race_id), big.mark=","))
stopifnot(nrow(d) > 0)

# ---- 1. RELIABILITY: over- or under-confident? -----------------------------
# Sum both sides before reading calibration: predicted medals and actual medals
# must both be counted, because a mismatch in the TOTAL is a different fault
# from a mismatch in the SHAPE, and the two are easy to confuse.
cat("\n================ 1. RELIABILITY of p_medal ================\n")
for (grp in unique(d$cell)) {
  s <- d[cell == grp]
  say("\n-- %s: %d rows, %d races", grp, nrow(s), uniqueN(s$race_id))
  say("   predicted medals %.1f vs actual medals %d (ratio %.3f)",
      sum(s$p_medal), sum(s$hit_medal), sum(s$p_medal) / max(sum(s$hit_medal), 1))
  s[, bin := cut(p_medal, c(0, .1, .2, .3, .5, .7, .9, 1), include.lowest = TRUE)]
  rel <- s[, .(n = .N, mean_pred = round(mean(p_medal), 3),
               observed = round(mean(hit_medal), 3)), by = bin][order(bin)]
  rel[, gap := round(mean_pred - observed, 3)]
  rel[, reads := fifelse(gap > 0.02, "OVER-confident",
                  fifelse(gap < -0.02, "UNDER-confident", "ok"))]
  print(rel, row.names = FALSE)
}

# ---- 2. EVIDENCE DEPTH -----------------------------------------------------
# Brier decomposed per race so the unit of replication stays the race.
byrace <- function(dd, pcol, y) dd[, .(v = mean((get(pcol) - get(y))^2), n = .N), by = race_id]
cat("\n================ 2. EVIDENCE DEPTH (shrinkage / w_total) ================\n")
if (all(c("shrinkage", "w_total") %in% names(d)) && d[, sum(is.finite(w_total))] > 0) {
  say("w_total coverage: %.1f%% finite | shrinkage coverage: %.1f%% finite",
      100 * mean(is.finite(d$w_total)), 100 * mean(is.finite(d$shrinkage)))
  dd <- d[is.finite(w_total)]
  dd[, w_bin := cut(w_total, quantile(w_total, 0:4/4, na.rm = TRUE),
                    include.lowest = TRUE, labels = c("Q1 thinnest", "Q2", "Q3", "Q4 deepest"))]
  # Per-athlete-row squared error, since evidence depth is an athlete property
  # not a race one; race id kept so the SE can be clustered.
  dd[, se_medal := (p_medal - hit_medal)^2]
  out <- dd[, .(rows = .N, mean_w = round(mean(w_total), 1),
                medal_sq_err = round(mean(se_medal), 4),
                pred_medals = round(sum(p_medal), 1), actual_medals = sum(hit_medal)),
            by = .(cell, w_bin)][order(cell, w_bin)]
  out[, over_pred_ratio := round(pred_medals / pmax(actual_medals, 1), 3)]
  print(out, row.names = FALSE)
  say("\nover_pred_ratio > 1 means the model hands out more medal probability than")
  say("the group actually won -- the seeding/prior failure mode, not an ordering one.")
} else say("shrinkage/w_total not populated in this arm; cut skipped (NOT a pass).")

# ---- 3. jump|W by discipline ----------------------------------------------
cat("\n================ 3. jump|W and the losing cells, by discipline ================\n")
jd <- d[fs %chin% LOSE]
out3 <- jd[, .(rows = .N, races = uniqueN(race_id),
               pred_medals = round(sum(p_medal), 1), actual_medals = sum(hit_medal),
               medal_sq_err = round(mean((p_medal - hit_medal)^2), 4)),
           by = .(fs, discipline)][races >= 5L]
out3[, over_pred_ratio := round(pred_medals / pmax(actual_medals, 1), 3)]
setorder(out3, -medal_sq_err)
print(out3, row.names = FALSE)

cat("\n================ VERDICT ================\n")
lose <- d[cell != "rest"]; rest <- d[cell == "rest"]
say("LOSING cells: predicted %.1f medals, actual %d (ratio %.3f)",
    sum(lose$p_medal), sum(lose$hit_medal), sum(lose$p_medal) / max(sum(lose$hit_medal), 1))
say("REST:         predicted %.1f medals, actual %d (ratio %.3f)",
    sum(rest$p_medal), sum(rest$hit_medal), sum(rest$p_medal) / max(sum(rest$hit_medal), 1))
say("")
say("Both ratios near 1.0 => the TOTAL probability is right and the deficit is")
say("purely about WHICH athletes get it: an ordering problem. A ratio away from")
say("1.0 in the losing cells only => a level/seeding problem after all, which")
say("would be fixable by a different route than reordering.")
