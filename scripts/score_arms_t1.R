# RE-SCORE PAST EXPERIMENT ARMS ON T1 ONLY.
#
# WHY. Every knob in this project has been judged on the tier-WEIGHTED metric
# (majors 40, T1 12, T2 1, non-final x0.5). That metric already leans elite, but
# it is a soft weight over the whole corpus, and a knob that only bites when a
# field is tight - the ceiling, the cross-event blend - can be diluted by the
# millions of easy T2 pairs it still contains. A hard T1 cut asks a different
# question: does the knob help where the athletes are close together?
#
# READ THE POWER BEFORE READING THE RESULT. T1 is NOT a sharper instrument. On
# the sealed window the weighted metric is worth ~74,000 effective pairs (floor
# 0.159); a T1-only cut of the same year is ~20,000 (floor 0.35). T1 pools six
# seasons to get its floor down to ~0.13. So the amplification a hard cut buys -
# the model's edge over season best goes from +1.44 at T2 to +3.11 at T1, about
# 2.2x - is smaller than the noise it costs, about 4.3x. A knob that failed on
# the weighted metric will USUALLY fail here too, and more noisily.
#
# That makes this a targeted test, not a retrial: it can only overturn something
# that helps SPECIFICALLY at elite level while doing nothing or harm elsewhere.
# Anything that merely looks better here without clearing the floor is noise, and
# should be recorded as such rather than promoted.
#
# COMMON PAIRS ONLY. Arms differ in which rows they score - seeding flips
# athletes from unseen to seen, which changes the population. Comparing an arm's
# concordance over its own pairs against the base's over the base's pairs mixes
# a population change into what is supposed to be a model change. So every
# comparison below is an inner join on a pair identity and both arms are read on
# exactly the same pairs.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
YR   <- .env_int("ARMS_FROM_YEAR", "2021")
TIER <- Sys.getenv("TIER_ONLY", "T1_elite")

c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
keep <- c0[meet_tier == TIER, unique(competition_id)]
stopifnot("no competitions at that tier" = length(keep) > 0)
cat(sprintf("%s: %s competitions in the catalogue\n", TIER,
            format(length(keep), big.mark = ",")))

BAR <- "|"

# pair table for one arm, restricted to the tier
pairs_for <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) { cat(sprintf("  MISSING history for arm '%s'\n", tag)); return(NULL) }
  h <- setDT(read_parquet(f, col_select = c("race_key", "date", "athlete_id",
                                            "r_pre", "r_use", "place")))
  if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
  h[!is.finite(r_use), r_use := r_pre]
  h <- h[is.finite(r_use) & is.finite(place) & place > 0]
  h[, competition_id := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]]]
  h <- h[competition_id %chin% keep & year(as.Date(date)) >= YR]
  if (!nrow(h)) return(NULL)
  a <- h[, .(rid = .GRP, i = seq_len(.N), place, r_use, athlete_id), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  won <- m$place.x < m$place.y
  m[, c := fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won))]
  # PAIR IDENTITY. race_key is built upstream from competition_id, event and
  # round and does NOT depend on the arm, so it is stable across arms; the two
  # athlete ids are ordered so the same pair keys identically on both sides.
  m[, pid := paste(race_key.x, pmin(athlete_id.x, athlete_id.y),
                   pmax(athlete_id.x, athlete_id.y), sep = "|")]
  unique(m[, .(pid, c)], by = "pid")
}

# ARMS is "base:arm1,arm2,...;base2:arm3,arm4" - each group carries its own base
SPEC <- Sys.getenv("ARMS", "")
if (!nzchar(SPEC)) {
  cat("Set ARMS to 'base:arm1,arm2;base2:arm3'. Example:\n")
  cat("  ARMS='ceil_id:ceil_015,ceil_030;xev_id:xev_02,xev_05' Rscript ...\n")
  quit(status = 0)
}

out <- list()
for (grp in strsplit(SPEC, ";", fixed = TRUE)[[1]]) {
  bits <- strsplit(grp, ":", fixed = TRUE)[[1]]
  stopifnot("a group must look like base:arm1,arm2" = length(bits) == 2)
  base_tag <- trimws(bits[1]); arms <- trimws(strsplit(bits[2], ",", fixed = TRUE)[[1]])
  cat(sprintf("\n=== base %s ===\n", base_tag))
  b <- pairs_for(base_tag)
  if (is.null(b)) { cat("  base has no pairs at this tier - skipped\n"); next }
  cat(sprintf("  base: %s pairs, %.2f\n", format(nrow(b), big.mark = ","),
              100 * mean(b$c)))
  for (tg in arms) {
    x <- pairs_for(tg)
    if (is.null(x)) next
    j <- merge(x, b, by = "pid", suffixes = c(".arm", ".base"))
    n <- nrow(j)
    if (n < 500) { cat(sprintf("  %-12s only %d common pairs - skipped\n", tg, n)); next }
    d  <- 100 * (mean(j$c.arm) - mean(j$c.base))
    fl <- 100 * sqrt(0.25 / n)
    out[[length(out) + 1L]] <- data.table(
      base = base_tag, arm = tg, common = n,
      base_conc = round(100 * mean(j$c.base), 3),
      arm_conc  = round(100 * mean(j$c.arm), 3),
      delta = round(d, 3), floor = round(fl, 3),
      ratio = round(d / fl, 2))
    cat(sprintf("  %-12s %s common | base %.3f -> arm %.3f | %+.3f (floor %.3f, %.2fx)\n",
                tg, format(n, big.mark = ","), 100 * mean(j$c.base),
                100 * mean(j$c.arm), d, fl, d / fl))
  }
}

stopifnot("no arm produced a comparison" = length(out) > 0)
res <- rbindlist(out)
cat("\n=== all arms, largest T1 gain first ===\n")
print(res[order(-delta)])
cat("\nratio is delta / floor. Treat anything under 2x as noise - and note that\n")
cat("with this many arms, one or two will sit near 2x by chance alone. A result\n")
cat("here only means something if it ALSO holds on the weighted metric, or if\n")
cat("there is a mechanism explaining why it should be elite-specific.\n")
f <- file.path(D, "arms_t1.json")
writeLines(jsonlite::toJSON(res, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("wrote %s\n", basename(f)))
