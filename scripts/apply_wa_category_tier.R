# Tier unclassified meets by the World Athletics world-rankings category code.
#
# THE PROBLEM. `class` is a regex over `comp_name`. A meet named unusually falls
# to `unclassified`, and an unclassified meet is capped at T2 by a deliberate
# rule -- 89 meets once admitted on measured strength alone were all junk. The
# cap is right, but it also holds down meets that are unclassified only because
# nobody wrote a pattern: the Doha Diamond League appears as "Doha Meeting", the
# Birmingham DL as "Muller Grand Prix", Stockholm as "DN Galan", Paris as
# "Meeting AREVA", New York as "adidas Grand Prix", and the IAAF World Athletics
# Final (2001-2009, defunct) never got a rule at all.
#
# THE SIGNAL. Every result carries the feed's own competition category
# (source_athletics.R:210 -> `tier`, aggregated to `tier_codes`). It is the
# World Athletics world-rankings scale, and calibrating it against the meets we
# DO name shows a clean ordering:
#
#   code   meets   %T1-by-name   median athletes   median strength
#   OW        28       100.0          1,687             88.0     Olympics/Worlds
#   DF        11       100.0            258             94.6     DL Final
#   GW       323        71.5            221             91.2     DL / platinum road
#   GL       323        32.2            182             72.3     Gold label
#   A        564        20.2            136             77.3
#   B      1,272         2.3            181             58.2
#   C        735         4.2            162             77.8
#   D      1,470         0.5            190             58.8
#   E      1,006         4.4            192             43.9
#   F     10,503         1.5            141             39.3     the residual
#
# THE MAPPING (Pete's, 2026-09-03):  OW/DF/GW/GL -> T1,  A/B/C/D -> T2,
# E/F -> T3. Applied ONLY where our own class rule is silent -- a meet we can
# name keeps the tier its class gives it. 12,137 unclassified meets carry no
# code at all and keep the existing quantile treatment.
#
# PRECEDENCE: 2,363 meets carry several codes ("A/DF/F/GW"). Highest wins, so
# that meet is T1 on the DF rather than T3 on the F.
#
# THE COST, MEASURED BEFORE SHIPPING AND ACCEPTED KNOWINGLY. The promotions are
# well evidenced -- Valencia, the European Cross Country Championships, the
# historical Diamond Leagues. The DEMOTIONS are the larger half and are not:
# E/F -> T3 moves 1,947 meets carrying ~97,000 finals out of the scored pool,
# more than halving it (2025: 47,284 -> 18,625 finals). `F` is the RESIDUAL
# category, not a quality verdict -- 10,503 meets carry it, and a large open
# meet lands on F regardless of who turned up. The clearest casualty is
# Mt. SAC Relays 2024: 1,675 athletes, strength 77.8, coded F, demoted to T3.
#
# I recommended promotions-only with that evidence; Pete chose both. Reversible:
# `tier_pre_wa` preserves the tier each meet had before this ran.
suppressMessages({library(arrow); library(data.table)})
source(here::here("citiusdata", "scripts", "_env.R"))
citius_version_guard(strict = TRUE)
D <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
ct <- setDT(read_parquet(CAT))
ct[, competition_id := as.character(competition_id)]

KNOWN <- c("olympics","world_champs","commonwealth","world_indoor","diamond_league",
           "world_other","indoor_tour","european_champs","continental","national_champs",
           "ncaa","team_champs","continental_tour","regional_games","asian_games",
           "african_games","panam_games","european_games","age_group","club_meet",
           "ncaa_lower","team_champs_lower","road_race")
RANK <- c(OW = 1, DF = 2, GW = 3, GL = 4, A = 5, B = 6, C = 7, D = 8, E = 9, F = 10)
TIER <- c(OW = "T1_elite", DF = "T1_elite", GW = "T1_elite", GL = "T1_elite",
          A = "T2_strong", B = "T2_strong", C = "T2_strong", D = "T2_strong",
          E = "T3_development", F = "T3_development")

best_code <- function(s) vapply(strsplit(s, "/"), function(v) {
  v <- v[v %in% names(RANK)]
  if (!length(v)) NA_character_ else v[which.min(RANK[v])]
}, character(1))

# Idempotent: a rerun must compare against the ORIGINAL tier, not the one this
# script last wrote, or the "what moved" report is meaningless on the second run.
if (!"tier_pre_wa" %in% names(ct)) {
  ct[, tier_pre_wa := meet_tier]
  cat("preserved pre-mapping tier as `tier_pre_wa`\n")
} else {
  cat("`tier_pre_wa` already present -- rerun, comparing against the original\n")
  ct[, meet_tier := tier_pre_wa]
}
before <- copy(ct$tier_pre_wa)

ct[, wac := NA_character_]
ct[!is.na(tier_codes), wac := best_code(tier_codes)]
ct[, .apply := !class %chin% KNOWN & !is.na(wac)]
.n_wac <- ct[.apply == TRUE, .N]
cat(sprintf("unclassified meets: %s | with a WA code: %s | without (unchanged): %s\n",
            format(ct[!class %chin% KNOWN, .N], big.mark=","),
            format(.n_wac, big.mark=","),
            format(ct[!class %chin% KNOWN & is.na(wac), .N], big.mark=",")))

ct[.apply == TRUE, meet_tier := TIER[wac]]
if (!"tier_source" %in% names(ct)) ct[, tier_source := NA_character_]
ct[, tier_source := NA_character_]
ct[.apply == TRUE, tier_source := paste0("wac_", wac)]

cat(sprintf("\nmoved: %s\n", format(sum(before != ct$meet_tier), big.mark=",")))
# `before` has to be a COLUMN, not a free vector. Referencing a vector inside
# `by` evaluates it against the whole table while `i` has already subset the
# rows, so the lengths disagree and data.table errors -- which is the good case;
# with a recycling-compatible length it would silently mislabel every group.
ct[, .from := before]
print(ct[.from != meet_tier, .(meets = .N, finals = sum(finals, na.rm = TRUE)),
         by = .(from = .from, to = meet_tier, wac)][order(-meets)])
cat("\ntier counts:\n"); print(ct[, .N, by = meet_tier][order(meet_tier)])
cat("\nscored pool:\n")
for (y in c(2024, 2025, 2026)) {
  a <- sum(ct[before %chin% c("T1_elite","T2_strong") & year == y]$finals, na.rm=TRUE)
  b <- ct[meet_tier %chin% c("T1_elite","T2_strong") & year == y, sum(finals, na.rm=TRUE)]
  cat(sprintf("  %d finals: %s -> %s (%+d)\n", y, format(a, big.mark=","),
              format(b, big.mark=","), b - a))
}

stopifnot("row count changed"   = nrow(ct) == length(before),
          "duplicate ids"       = !any(duplicated(ct$competition_id)),
          # Uses the .from COLUMN, not the free `before` vector: inside `[`,
          # `before[class %chin% KNOWN]` evaluates `class` against the whole
          # table while meet_tier is already subset, so the lengths disagree.
          # Same trap as the reporting line above.
          "a known class moved" = ct[class %chin% KNOWN, all(meet_tier == .from)],
          # `all()` over a possibly-EMPTY set is TRUE in R, so "mapping not
          # applied" would pass even if .n_wac were 0 (e.g. `wac` extraction
          # silently returning nothing). Found by review 2026-09-04, same
          # shape as apply_strength_ew.R's identical gap.
          "no meets took a WA tier" = .n_wac > 0,
          "mapping not applied" = ct[.apply == TRUE, all(meet_tier == TIER[wac])],
          "a tier is missing"   = !any(is.na(ct$meet_tier)))
ct[, c(".apply", ".from") := NULL]
tmp <- paste0(CAT, ".tmp"); write_parquet(ct, tmp)
if (!file.rename(tmp, CAT)) stop("rename failed; new catalogue at ", tmp)
cat(sprintf("\nwrote competition_catalogue.parquet (%d columns)\n", ncol(ct)))
