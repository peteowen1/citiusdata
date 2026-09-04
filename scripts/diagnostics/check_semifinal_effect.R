# Why do hurdles SEMI-FINALS show the model's worst deficit against season
# best - even though semis have the THINNEST within-athlete performance tail
# of any hurdles round (skew -0.201 vs heats -0.915)? That contradiction is
# what makes the finding unexplained: the usual story ("noisy round -> hard
# to predict") points the wrong way.
#
# INPUT NUMBERS BEING RE-EXAMINED (from check_hurdles_deficit.R):
#   semi-finals  edge -3.80 on 3,671 pairs (floor 0.83)  <- worst round
#   heats        edge -0.99
#   finals       edge -0.21 (inside its own floor)
#
# TWO DEFECTS THAT MUST BE RULED OUT BEFORE TRUSTING THOSE NUMBERS:
#   1. check_hurdles_deficit.R's "by depth" section bands rows by n_eff BEFORE
#      pairing, which restricts every pair to matched-experience athletes -
#      check_hurdles_deficit_v2.R exists because of this. Its "by round"
#      section has the SAME row-then-group-then-pair shape, but round (`rc`)
#      is verified below to be constant within a race_key, so grouping by
#      round before pairing never splits a real pair across groups the way
#      n_eff banding did. The round split is NOT the n_eff defect. Verified,
#      not assumed - see the stopifnot below.
#   2. check_hurdles_deficit.R scored `r_pre`, not `r_use`. This repo's own
#      history records that scoring the wrong one inverted three published
#      conclusions (2026-08-21). Every comparison in this script uses `r_use`.
#
# HYPOTHESIS 4 (field composition) IS TESTED FIRST, per instruction: semis are
# seeded from heats, so fields may be more evenly matched than heats or
# finals. A smaller rating gap makes ANY predictor's job harder - if that's
# what's happening, the "deficit" is not a defect, it's what a coin-flip field
# looks like. Everything else is only worth chasing if this comes back clean.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[is.finite(perf) & is.finite(place) & is.finite(r_pre)]
stopifnot("r_use is missing - the rule is score r_use, never r_pre" =
            "r_use" %chin% names(h))
h[!is.finite(r_use), r_use := r_pre]
h[, yr := year(date)]

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
stopifnot("registry join produced almost nothing" =
            h[!is.na(family), .N] > 0.9 * nrow(h))

# --- verify rc really is a race-level attribute -----------------------------
# If it were athlete-level (like n_eff), grouping rows by rc before pairing
# would be the exact defect v2 fixed. It isn't: every race has one round.
rc_check <- h[, .(n_rc = uniqueN(rc)), by = race_key]
stopifnot("rc is not race-level - round split would need the n_eff-style fix" =
            all(rc_check$n_rc == 1))
cat(sprintf("verified: rc is constant within race_key for all %s races.\n",
            format(nrow(rc_check), big.mark = ",")))

# --- walk-forward season best, strictly lagged ------------------------------
setorder(h, athlete_id, event_id, date, race_key)
h[, sb := shift(cummax(perf)), by = .(athlete_id, event_id, yr)]

# --- pair builder: FULL cartesian within race, never banded first ----------
build_pairs <- function(d) {
  if (nrow(d) < 20) return(NULL)
  a <- d[, .(rid = .GRP, i = seq_len(.N), athlete_id, place, r_use, sb, n_eff,
             rc, perf, event_id, date), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  won <- m$place.x < m$place.y
  m[, `:=`(cm  = fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won)),
           cs  = fifelse(sb.x    == sb.y,    0.5, as.numeric((sb.x    > sb.y)    == won)),
           gap = abs(r_use.x - r_use.y),
           rc  = rc.x)]   # rc.x == rc.y is guaranteed by the check above
  m[]
}

score <- function(m) {
  if (is.null(m) || !nrow(m)) return(NULL)
  data.table(pairs = nrow(m),
             model = round(100 * mean(m$cm), 2),
             season_best = round(100 * mean(m$cs), 2),
             edge = round(100 * (mean(m$cm) - mean(m$cs)), 2),
             floor = round(100 * sqrt(0.25 / nrow(m)), 2))
}

hd_all <- build_pairs(h[family == "hurdles" & is.finite(sb)])
stopifnot("no hurdles pairs built" = !is.null(hd_all) && nrow(hd_all) > 1000)

# =============================================================================
# SECTION A - HYPOTHESIS 4 FIRST: is the semi field just more evenly matched?
# =============================================================================
cat("\n================ A. GAP COMPOSITION BY ROUND (hurdles) ================\n")
cat("If semi pairs have systematically smaller |r_use gap|, a bigger deficit\n")
cat("there is MECHANICAL - closely matched fields are harder for everyone.\n\n")
gap_by_round <- hd_all[, .(pairs = .N,
                           mean_gap = round(mean(gap), 4),
                           median_gap = round(median(gap), 4),
                           p25 = round(quantile(gap, .25), 4),
                           p75 = round(quantile(gap, .75), 4)), by = rc][order(rc)]
print(gap_by_round)

# Quantile bins of gap, estimated from the pooled hurdles distribution (not
# hand-picked), so every round is judged against the SAME bin edges.
gap_breaks <- unique(quantile(hd_all$gap, seq(0, 1, 0.2)))
hd_all[, gap_bin := cut(gap, gap_breaks, include.lowest = TRUE)]

cat("\n-- edge by round WITHIN each gap quintile --\n")
cat("If the round effect survives inside equal-gap bins, field composition is\n")
cat("not the (whole) explanation - the model is doing worse than season best\n")
cat("even when matched on how hard the field is to predict.\n\n")
by_bin <- hd_all[, score(.SD), by = .(gap_bin, rc)][order(gap_bin, rc)]
print(by_bin)

# --- supporting composition checks: n_eff and field size by round ----------
cat("\n-- supporting checks: n_eff and field size by round (per athlete-row, not pair) --\n")
hd_rows <- h[family == "hurdles" & is.finite(r_use)]
hd_rows[, fs := .N, by = race_key]
print(hd_rows[, .(rows = .N, mean_n_eff = round(mean(n_eff), 2),
                  median_n_eff = round(median(n_eff), 2),
                  mean_field = round(mean(fs), 2),
                  median_field = as.numeric(median(fs))), by = rc][order(rc)])

# =============================================================================
# SECTION B - re-derive the round split correctly (r_use, full pairing)
# =============================================================================
cat("\n================ B. ROUND SPLIT, RE-DERIVED (r_use, full pairing) ================\n")
cat("overall (all years):\n")
print(hd_all[, score(.SD), by = rc][order(rc)])

for (target_yr in c(2025, 2026)) {
  # `target_yr`, NOT `yr` - `yr` is a COLUMN of h, so a same-named loop
  # variable is shadowed inside h[...] and `yr == yr` would compare the
  # column to itself, matching every row and silently pooling both windows.
  # v2's own header calls this out as a trap that was written into its first
  # draft anyway - repeating it here would be the same mistake twice.
  cat(sprintf("\n-- %d only (hypothesis 1: sample size / sign stability) --\n", target_yr))
  print(build_pairs(h[family == "hurdles" & yr == target_yr & is.finite(sb)])[
    , score(.SD), by = rc][order(rc)])
}

# =============================================================================
# SECTION C - HYPOTHESIS 2: is this a hurdles thing, or a semi-final thing?
# =============================================================================
cat("\n================ C. ROUND SPLIT, OTHER FAMILIES ================\n")
cat("If every family's semis look like hurdles semis, this is about semis in\n")
cat("general, not about hurdles specifically.\n\n")
for (fam in c("sprint", "middle", "distance")) {
  cat(sprintf("-- %s --\n", fam))
  fp <- build_pairs(h[family == fam & is.finite(sb)])
  print(fp[, score(.SD), by = rc][order(rc)])
}

# =============================================================================
# SECTION D - HYPOTHESIS 3: tactical qualifying (secured a spot, eased down)
# =============================================================================
cat("\n================ D. TACTICAL QUALIFYING ================\n")
cat("If athletes ease down once qualification is secure, the model should\n")
cat("look WRONG in the semi (correctly rates them, they under-run it) and\n")
cat("get VINDICATED in the final against the SAME rematched opponent.\n\n")

semi_d  <- h[family == "hurdles" & rc == "semi"  & is.finite(r_use)]
final_d <- h[family == "hurdles" & rc == "final" & is.finite(r_use)]

pair_key_of <- function(m) paste(pmin(m$athlete_id.x, m$athlete_id.y),
                                  pmax(m$athlete_id.x, m$athlete_id.y))

m_semi <- build_pairs(semi_d)
m_final <- build_pairs(final_d)
stopifnot("no semi pairs for tactical-qualifying join" = !is.null(m_semi) && nrow(m_semi) > 0)
stopifnot("no final pairs for tactical-qualifying join" = !is.null(m_final) && nrow(m_final) > 0)

m_semi[, `:=`(pair_key = pair_key_of(.SD), event_id = event_id.x, date_semi = date.x)]
m_final[, `:=`(pair_key = pair_key_of(.SD), event_id = event_id.x, date_final = date.x)]

link <- merge(
  m_semi[,  .(pair_key, event_id, date_semi,  cm_semi = cm,  cs_semi = cs)],
  m_final[, .(pair_key, event_id, date_final, cm_final = cm, cs_final = cs)],
  by = c("pair_key", "event_id"), allow.cartesian = TRUE)
# same meet: the final must follow the semi within a handful of days, and
# obviously not precede it
link <- link[date_final >= date_semi & date_final <= date_semi + 5]
# a semi pair can match several finals if the athlete_id pair recurs across
# meets/years by coincidence - keep only the NEAREST subsequent final
link[, gap_days := as.numeric(date_final - date_semi)]
setorder(link, pair_key, event_id, date_semi, gap_days)
link <- link[, .SD[1], by = .(pair_key, event_id, date_semi)]

cat(sprintf("rematched semi->final pairs (same two athletes, same event, within 5 days): %s\n",
            format(nrow(link), big.mark = ",")))

if (nrow(link) >= 30) {
  cat("\noverall: model concordance in the semi vs the SAME pair's rematch in the final\n")
  print(link[, .(pairs = .N,
                 model_in_semi  = round(100 * mean(cm_semi), 2),
                 model_in_final = round(100 * mean(cm_final), 2),
                 season_best_in_semi  = round(100 * mean(cs_semi), 2),
                 season_best_in_final = round(100 * mean(cs_final), 2),
                 floor = round(100 * sqrt(0.25 / .N), 2))])

  cat("\nthe key test: pairs where the model was WRONG in the semi (cm_semi == 0) -\n")
  cat("does the SAME matchup vindicate the model when it's rerun in the final?\n")
  wrong_in_semi <- link[cm_semi == 0]
  if (nrow(wrong_in_semi) >= 20) {
    print(wrong_in_semi[, .(pairs = .N,
                            model_in_final_given_wrong_in_semi = round(100 * mean(cm_final), 2),
                            floor = round(100 * sqrt(0.25 / .N), 2))])
  } else {
    cat(sprintf("only %d such pairs - too few to read.\n", nrow(wrong_in_semi)))
  }
} else {
  cat("fewer than 30 rematched pairs - hurdles semis rarely reconvene the same\n")
  cat("head-to-head in a final within 5 days, so this test has no power here.\n")
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================ SUMMARY ================\n")
cat("Read section A first: if the round effect does not survive inside\n")
cat("matched-gap bins, semis are simply a tighter field and the deficit is\n")
cat("mundane. If it DOES survive, read B (sample size), C (hurdles-specific\n")
cat("or semis-in-general) and D (tactical qualifying) in that order.\n")

f <- file.path(OUT, "semifinal_effect.json")
writeLines(jsonlite::toJSON(list(
  tag = TAG,
  gap_by_round = gap_by_round,
  gap_bin_by_round = by_bin,
  round_split_all_years = hd_all[, score(.SD), by = rc][order(rc)],
  tactical_qualifying = if (exists("link")) link[, .(pair_key, event_id, cm_semi, cm_final)] else NULL),
  dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
