# Adopt EW strength as the tiering basis in competition_catalogue.parquet.
#
# Run AFTER build_competition_catalogue.R + augment_catalogue_coverage.R and
# AFTER build_strength_ew.R. Order matters: this rewrites `strength` and then
# re-derives `meet_tier` from it.
#
# WHAT IT DOES
#   strength_pb  <- the career-best value, PRESERVED not discarded
#   strength     <- strength_ew where available, career-best elsewhere
#   meet_tier    <- re-derived, using the builder's rule verbatim
#
# Career-best is kept as `strength_pb` deliberately. The experiment doc's own
# rule is "add as a second column first, never overwrite `strength` in the same
# change", and the two bases disagree enough (r~0.925, median |diff| ~5) that
# anything comparing vintages needs both. It also means this is reversible by
# one assignment.
#
# WHY EW, given it scored WORSE. Career-best has lookahead: `a_q = max(pctl)`
# spans an athlete's whole career, so a 2019 meet was tiered using marks set in
# 2024, and meet_tier feeds the weights the model trains on. The A/B cannot
# adjudicate that, because a leaking baseline can win BECAUSE it leaks -- it
# knows things the honest arm does not. Career-best scored better (sealed 71.549
# vs 70.884; 74.28 vs 74.17 on the fixed 887-final majors panel) and that gap is
# not attributable to quality while one arm sees the future. Adopted on the leak,
# not on the metric. Pete's call, 2026-09-03, with the numbers in hand.
#
# EXPECT THE POOL TO MOVE. EW changes which meets are MEASURABLE, not just how
# they rank -- it needs prior races where career-best does not. Measured at
# adoption: ~10,100 tier changes, 2025 scored finals -3.0%, 2026 +15.9%. That is
# the change, not a bug, but it means any concordance compared across this
# boundary is not like-for-like. Re-baseline before reading one.
suppressMessages({library(arrow); library(data.table)})
source(here::here("citiusdata", "scripts", "_env.R"))
citius_version_guard(strict = TRUE)
D <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
EW  <- file.path(D, "strength_ew.parquet")
stopifnot("run build_strength_ew.R first" = file.exists(EW))

ct <- setDT(read_parquet(CAT)); ct[, competition_id := as.character(competition_id)]
ew <- setDT(read_parquet(EW)); ew[, competition_id := as.character(competition_id)]
before_tier <- copy(ct$meet_tier)
cat(sprintf("catalogue: %s meets\n", format(nrow(ct), big.mark = ",")))

# Preserve career-best before touching anything. Idempotent: a second run must
# not overwrite strength_pb with an already-EW `strength`.
#
# `.expect_pb_n` is captured HERE, before either branch runs, so the final
# assertion can check the real claim ("strength_pb still has as many non-NA
# values as it started with") rather than `any(!is.na(...))`, which only
# proves one row survived and would pass even after a partial overwrite.
.expect_pb_n <- if ("strength_pb" %in% names(ct)) sum(!is.na(ct$strength_pb)) else sum(!is.na(ct$strength))
if (!"strength_pb" %in% names(ct)) {
  ct[, strength_pb := strength]
  cat("preserved career-best strength as `strength_pb`\n")
} else {
  cat("`strength_pb` already present -- leaving it (this script is idempotent)\n")
}

# FULLY IDEMPOTENT, in two ways that both bit earlier today.
#
# 1. Drop any existing strength_ew/races_won_ew BEFORE merging. Merging onto a
#    table that already carries the column produces .x/.y suffixes rather than
#    an error -- the same name-collision that broke export_meet_events.R twice.
# 2. Reset `strength` from the preserved career-best first. Without it, a rerun
#    where a meet LOST its EW value would silently keep the previous run's EW
#    number instead of reverting to career-best, so the column would hold a
#    mixture no one could account for.
.stale <- intersect(c("strength_ew", "races_won_ew"), names(ct))
if (length(.stale)) ct[, (.stale) := NULL]   # guarded: absent on a first run
ct[, strength := strength_pb]

ct <- merge(ct, ew[, .(competition_id, strength_ew, races_won_ew)],
            by = "competition_id", all.x = TRUE)
n <- ct[!is.na(strength_ew), .N]
cat(sprintf("EW value available for %s of %s meets (%.1f%%)\n",
            format(n, big.mark = ","), format(nrow(ct), big.mark = ","), 100*n/nrow(ct)))
ct[!is.na(strength_ew), strength := strength_ew]

# Re-derive meet_tier. Verbatim from build_competition_catalogue.R:419-444 --
# including the unclassified QUANTILE, which is the thing a reproduction of this
# rule always gets wrong. Hardcoding it as a constant is what invalidated the
# first A/B: the real split moves with the strength distribution and a constant
# does not.
KNOWN_T1 <- c("olympics","world_champs","commonwealth","world_indoor",
              "diamond_league","world_other","indoor_tour","european_champs")
KNOWN_T2 <- c("continental","national_champs","ncaa","team_champs",
              "continental_tour","regional_games",
              "asian_games","african_games","panam_games","european_games")
KNOWN_T3 <- c("age_group","club_meet","ncaa_lower","team_champs_lower")
ct[, meet_tier := fcase(
  class %in% KNOWN_T1, "T1_elite",
  class %in% KNOWN_T2, "T2_strong",
  class %in% KNOWN_T3, "T3_development",
  class == "road_race" & !is.na(strength) & strength >= 75, "T1_elite",
  class == "road_race" & !is.na(strength) & strength >= 50, "T2_strong",
  class == "road_race", "T3_development",
  default = NA_character_)]
uq <- stats::quantile(ct[is.na(meet_tier)]$strength, 0.55, na.rm = TRUE)[[1]]
ct[is.na(meet_tier), meet_tier := fcase(
  is.na(strength), "T3_development",
  strength >= uq, "T2_strong",
  default = "T3_development")]
cat(sprintf("unclassified split at strength %.1f\n", uq))

cat(sprintf("\ntier changes: %s\n", format(sum(before_tier != ct$meet_tier), big.mark = ",")))
print(data.table(from = before_tier, to = ct$meet_tier)[from != to, .N, by = .(from, to)][order(-N)])
cat("\ntier counts now:\n")
print(ct[, .N, by = meet_tier][order(meet_tier)])
for (y in c(2025, 2026))
  cat(sprintf("%d finals in scored pool: %s\n", y,
      format(ct[meet_tier %chin% c("T1_elite","T2_strong") & year == y,
                sum(finals, na.rm = TRUE)], big.mark = ",")))

# Assert the VALUES, not that the script ran. Counting rows is not checking
# them -- and `all()` over a possibly-EMPTY set is the specific way that goes
# wrong: `all(logical(0))` is TRUE in R, so "strength did not adopt EW" would
# pass even if the merge above matched zero rows (e.g. a competition_id
# type/format mismatch between the two parquet files -- the exact column-
# mismatch failure mode this session hit four times elsewhere). Found by
# review 2026-09-04. The `n > 0` check makes that case fail loudly instead of
# passing silently; `n` is the count already printed above at line ~71.
stopifnot(
  "row count changed"        = nrow(ct) == length(before_tier),
  "duplicate ids"            = !any(duplicated(ct$competition_id)),
  "no meets adopted EW"      = n > 0,
  "career-best not preserved"= "strength_pb" %in% names(ct) && sum(!is.na(ct$strength_pb)) == .expect_pb_n,
  "strength did not adopt EW"= ct[!is.na(strength_ew), all(strength == strength_ew)],
  "a tier is missing"        = !any(is.na(ct$meet_tier)))

tmp <- paste0(CAT, ".tmp"); write_parquet(ct, tmp)
if (!file.rename(tmp, CAT)) stop("rename failed; new catalogue at ", tmp)
cat(sprintf("\nwrote competition_catalogue.parquet (%d columns)\n", ncol(ct)))
