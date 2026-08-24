# Stop dropping meets that World Athletics itself labels as top-category.
#
# THE ENGINE IS BLIND TO T3. form_ratings.R keeps only T1_elite and T2_strong and
# inner-joins, so a T3_development competition is not down-weighted - it is gone.
# 446 competitions carrying an OW/GW/DF/GL/A code sit in T3, which is 24,562
# scoreable results, 5,338 of them since 2025. Among them is the 2025 Valencia
# Marathon: 1,193 results, one of the fastest road races in the world.
#
# WHY VALENCIA SCORES AS WEAK. `strength` is the percentile of each race winner's
# mark averaged over the meet - an excellent measure for a championship-shaped
# meeting with a selected field, and the wrong one for a mass-participation race
# where the elite start alongside forty thousand club runners. The average is
# dragged down by exactly the people the elite race is not about. That is a real
# limitation of averaging, not a mis-set threshold, and no tuning of the cutoff
# fixes it - which is precisely the case where a published label beats a
# measurement.
#
# WHY A ONE-WAY FLOOR IS SAFE WHEN A CLASSIFIER IS NOT.
# build_competition_catalogue.R explains at length why `tier` cannot be the
# classifier: it is per-RESULT and varies within a meet - Weltklasse Zurich
# carries A, DF, F and GW across its own results. All true, and unchanged. But
# that note also records the direction of the error: Diamond League marks are
# "routinely labelled low tier". The codes are noisy DOWNWARDS. A real Diamond
# League meeting labelled F is common; a village meet labelled GL is not. So the
# PRESENCE of a top code carries information even though its absence does not,
# and it can raise a tier without being trusted to set one.
#
# THE FLOOR STOPS AT T2, DELIBERATELY. A first version lifted to T1 and broke two
# anchors - "no unclassified meet is T1" (141 found) and "no T1 meet sits below
# strength 40". Both anchors are right and neither was weakened. T1 is the
# population the model is JUDGED on, and the rule that a meet we cannot name has
# no business there still holds: a category code says a meet MATTERS, not that we
# know what it is. T2 is enough anyway, because being stuck at T3 is what made
# these results invisible; T1 versus T2 only changes metric weighting.
#
# RUNS LAST. An earlier attempt put this inside build_competition_catalogue.R,
# where it lifted 92 meets and missed the rest - augment_catalogue_coverage.R
# adds 25,286 competitions AFTER the base build, and those bypassed it entirely.
# Order in the chain: build -> coverage -> road_majors -> road_half_majors -> this.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
F_CAT <- file.path(D, "competition_catalogue.parquet")
stopifnot("run build_competition_catalogue.R and the augment chain first" =
            file.exists(F_CAT))

cg <- setDT(read_parquet(F_CAT))
cg[, competition_id := as.character(competition_id)]
n0 <- nrow(cg)
t0 <- cg[, .N, by = meet_tier][order(meet_tier)]
cat(sprintf("catalogue before: %s competitions\n", format(n0, big.mark = ",")))
print(t0)
stopifnot("catalogue looks truncated - expected the full augmented table" =
            n0 > 20000,
          "catalogue has no meet_tier column" = "meet_tier" %chin% names(cg))

# The code lives per RESULT, so read it from the corpus rather than trusting a
# summary column that may predate the augment steps.
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("competition_id","tier","scoreable","perf","date")))
c0[, competition_id := as.character(competition_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf)]
FLOOR_CODES <- c("OW", "GW", "DF", "GL", "A")
codes <- c0[tier %chin% FLOOR_CODES,
            .(coded_results = .N, last_coded = max(date)), by = competition_id]
stopifnot("no competition carries a top WA code - the corpus join is wrong" =
            nrow(codes) > 100)
cg <- merge(cg, codes, by = "competition_id", all.x = TRUE)
cg[is.na(coded_results), coded_results := 0L]

# Never lift a meet we positively identified as development. Knowledge from the
# meet's NAME outranks a code on one of its results - the same precedence the
# base builder already applies, and the reason a junior meeting held under a
# Diamond League banner stays where it is.
NEVER_LIFT <- c("age_group", "ncaa_lower", "team_champs_lower", "club_meet")
cg[, .liftable := meet_tier == "T3_development" & coded_results > 0 &
                  !(class %chin% NEVER_LIFT)]
lift <- cg[.liftable == TRUE]
cat(sprintf("\ncompetitions carrying OW/GW/DF/GL/A but tiered T3: %d\n",
            cg[meet_tier == "T3_development" & coded_results > 0, .N]))
cat(sprintf("of those, liftable (not a named development meet): %d\n", nrow(lift)))
cat(sprintf("blocked by class: %d\n",
            cg[meet_tier == "T3_development" & coded_results > 0 &
               class %chin% NEVER_LIFT, .N]))
cat("\n=== the ten biggest lifts ===\n")
print(lift[order(-coded_results)][seq_len(min(10L, .N)),
      .(comp_name = substr(comp_name, 1, 40), class,
        strength = round(strength, 1), coded_results, last_coded)])

cg[.liftable == TRUE, meet_tier := "T2_strong"]
.n_lift <- nrow(lift)
cg[, c(".liftable", "coded_results", "last_coded") := NULL]

# Assertions, not a printed count. A floor that lifts nothing is not wired up,
# and one that changes the row count has done something other than re-tier.
stopifnot("the WA-code floor lifted no meet at all - it is inert" = .n_lift > 0,
          "the floor changed the number of competitions" = nrow(cg) == n0,
          "the floor must never produce a T1 meet" =
            cg[, sum(meet_tier == "T1_elite")] == t0[meet_tier == "T1_elite", N],
          "a named development meet was lifted" =
            !any(cg$class %chin% NEVER_LIFT & cg$meet_tier != "T3_development"))
write_parquet(cg, F_CAT)
cat(sprintf("\nlifted %d competitions T3 -> T2. catalogue after:\n", .n_lift))
print(cg[, .N, by = meet_tier][order(meet_tier)])
cat(sprintf("wrote %s (%s competitions, row count unchanged)\n",
            basename(F_CAT), format(nrow(cg), big.mark = ",")))
cat("\nThese results were invisible to the engine before this ran. The next\n")
cat("form_ratings.R run is what actually puts them into the ratings.\n")
