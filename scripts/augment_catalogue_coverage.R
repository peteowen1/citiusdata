# General fix for the competition-catalogue coverage gap.
#
# ROOT CAUSE: build_competition_catalogue.R's population is
# championship_results.rds alone (the competition-route harvest, 7,054
# competitions). It has never seen athletics_history.rds (the career-route
# sweep), which build_athletics_corpus.R folds into athletics_corpus.parquet
# and alone accounts for 28,716 of the corpus's 32,076 distinct competitions.
# form_ratings.R INNER JOINS results to the catalogue and further restricts to
# meet_tier %in% c("T1_elite","T2_strong"), so every result whose competition
# has no catalogue row -- 78.0% of them -- is dropped before any tier is ever
# consulted.
#
# Measured 2026-08-17 (excluding ~2.5M corpus rows with NO competition_id at
# all -- competitionId 0 from the career-route feed, a SEPARATE defect, not
# fixable by any catalogue edit -- see source_athletics.R and
# docs/reference/harvesting.md):
#   corpus competitions with a real id:      32,075
#   already catalogued:                       7,054
#   uncatalogued:                            25,021 (78.0%)
#   name available for ALL of them via competition_name_lookup.parquet: 100%
#
# THIS SCRIPT DOES NOT HAND-LIST MEETS. It reruns build_competition_catalogue.R's
# own two decision rules -- name classification (cat_of(), reproduced below with
# two scale fixes explained inline) and measured field strength -- against the
# population the missing 78% belongs to, so the same auditable logic that
# already tiers the 7,054 known competitions extends to the rest. Full
# diagnosis, dry-run numbers and the road-racing decision are written up in
# docs/plans/catalogue-coverage-gap-2026-08-17.md -- read that first if this
# script's output looks wrong.
#
# THREE THINGS THIS SCRIPT HANDLES THAT THE MARATHON/HALF SCRIPTS DID NOT NEED TO:
#
#  1. comp_name is NA for 100% of career-route corpus rows (and 85% even of
#     competition-route rows -- confirmed by direct query, not assumed). Names
#     come from competition_name_lookup.parquet instead, exactly as
#     augment_catalogue_road_majors.R and augment_catalogue_road_half_majors.R
#     already do.
#
#  2. The career-route harvest encodes `round` in SHORT codes ("F","F1".."F9",
#     "H1".."H8","SF1","SF2","Q","Q1","Q2","CE","CE1"..) where the competition
#     route spells them out ("Final","Round 1 - Heat", ...).
#     build_competition_catalogue.R's grepl("final", round) finds the literal
#     word and matches ZERO of the 680,712 uncatalogued rows (confirmed).
#     Reused unmodified, every competition this script tries to add would
#     compute strength = NA regardless of how strong the field really was.
#     is_final_round() below recognises both vocabularies.
#     NOT FIXED HERE: form_ratings.R's own round classifier has the identical
#     blind spot (grepl("heat|round 1|qual", round) never matches "H1".."H8"),
#     so once these competitions reach the engine, career-route heat/semi/
#     qualifying rounds will misclassify as "final" for round-weighting.
#     form_ratings.R is off-limits while arms are running -- flagged for a
#     follow-up, not fixed here.
#
#  3. Two of build_competition_catalogue.R's classification patterns are loose
#     in a way that was invisible at 7,054 competitions and is NOT safe at
#     32,075: continental_tour's bare "|Meeting" alternative and team_champs's
#     bare "Cup$" alternative. Applied unmodified to the missing population
#     they classify 2,293 and 251 competitions respectively as KNOWN_T2 --
#     inspection found 97% of the continental_tour hits were generic club/open
#     meetings ("Watford Open Graded Meeting", "Open Meeting", "Avondmeeting")
#     and every team_champs hit inspected was a club/invitational cup
#     ("Xmas-Cup", "SK Cosma Summer Cup"). Same failure shape as the Diamond
#     League city-substring bug already fixed once in
#     build_competition_catalogue.R ("36% of the class was noise"). TIGHTENED
#     below (continental_tour 2,293 -> 35, team_champs 251 -> 65); every
#     displaced competition falls to unclassified, the safe default (never T1,
#     capped at T2 only by measured strength). build_competition_catalogue.R
#     itself is left untouched -- this is a local, documented deviation, not a
#     silent fork.
#
# ROAD RACING: deliberately not reopened. docs/incidents/
# road-coverage-and-the-strength-metric-2026-08-15.md measured that admitting
# mass-participation road racing into the form model's ordering corpus cost
# concordance even after the SEQ_MAXPLACE=12 cap, and held rather than shipped.
# Under the UNMODIFIED strength methodology used here, essentially no road_race
# competition clears the >=4-scored-finalists / >=5-scored-events bars from
# career-route PARTIAL field sampling, so this script adds ~0 road_race
# competitions to T1/T2 -- verified below by an anchor. The residual 10km-road
# gap needs the same treatment as the marathon/half-marathon majors: a short,
# explicit, auditable label-race list. Not attempted here.
#
# Usage (NOT run by the author of this script -- engine arms are running and
# the catalogue must not move mid-sequence):
#   powershell.exe -Command 'Rscript "C:/dev/citiusverse/citiusdata/scripts/augment_catalogue_coverage.R"'
# Then re-run form_ratings.R -- the catalogue is joined at engine time, not
# baked into the corpus.
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages({library(arrow); library(data.table)})
D <- here::here("citiusdata", "data")
CAT   <- file.path(D, "competition_catalogue.parquet")
CORP  <- file.path(D, "athletics_corpus.parquet")
LOOK  <- file.path(D, "competition_name_lookup.parquet")
stopifnot("run augment_catalogue_road_majors.R first - it builds the name lookup" =
            file.exists(LOOK))

cat0 <- setDT(read_parquet(CAT))
cat0[, competition_id := as.character(competition_id)]
cat(sprintf("catalogue before: %s competitions\n", format(nrow(cat0), big.mark = ",")))

nm <- setDT(read_parquet(LOOK))
nm[, competition_id := as.character(competition_id)]
cat(sprintf("competition names available: %s\n", format(nrow(nm), big.mark = ",")))

# ---- the population: everything in the corpus the catalogue has never seen -
corp <- setDT(read_parquet(CORP,
  col_select = c("athlete_id", "competition_id", "event_id", "date", "round",
                 "place", "perf", "source")))
corp[, competition_id := as.character(competition_id)]
corp[, athlete_id := as.character(athlete_id)]
n_before <- nrow(corp)
corp <- corp[!is.na(competition_id)]
cat(sprintf("corpus rows with a competition_id: %s of %s (%s have none -- a\n",
    format(nrow(corp), big.mark = ","), format(n_before, big.mark = ","),
    format(n_before - nrow(corp), big.mark = ",")))
cat("  separate defect in the career-route harvest; out of scope here)\n")

miss_ids <- setdiff(unique(corp$competition_id), cat0$competition_id)
cat(sprintf("uncatalogued competitions: %s of %s (%.1f%%)\n",
    format(length(miss_ids), big.mark = ","), format(uniqueN(corp$competition_id), big.mark = ","),
    100 * length(miss_ids) / uniqueN(corp$competition_id)))
if (!length(miss_ids)) { cat("nothing to add\n"); quit(status = 0) }

miss_nm <- nm[competition_id %chin% miss_ids, .(competition_id, comp_name = competition)]
n_no_name <- length(miss_ids) - nrow(miss_nm)
if (n_no_name > 0L) {
  cat(sprintf("%s missing competitions have NO name in the lookup -- they cannot\n", n_no_name))
  cat("  be classified and will be filed as unclassified with comp_name NA.\n")
  miss_nm <- rbind(miss_nm,
    data.table(competition_id = setdiff(miss_ids, miss_nm$competition_id),
               comp_name = NA_character_))
}

# ---- classification: build_competition_catalogue.R's cat_of(), with the two
#      scale fixes documented above. Everything else copied verbatim so a
#      diff against build_competition_catalogue.R shows exactly what changed. -
NOT_THE_EVENT <- paste(
  "Trials|Qualifier|Qualifying|Anniversary|Open Meeting|Selection|",
  "Warm.?up|Test Event|Festival|Classic -", sep = "")
NEVER_ELITE <- paste0(
  "Marathon|Half.?Marathon|10 ?[Kk]m?\\b|5 ?[Kk]m?\\b|Road Race|",
  "karusell|Bislettmila|Distanseserie|Distance challenge|Bislett Spring|",
  "Bislett Open|KM Oslo|Nasjonalt|Sommerstevne|Elite Series|Street Tour|",
  "Stabhochsprung|Kugelsto|m.odzie|youth|junior|U1[0-9]|U2[0-3]|",
  "Silesian Meeting|pre-programme|pre-event")
RULES <- list(
  list(class = "age_group",      pat = "U13|U14|U15|U16|U17|U18|U20|U23|Junior|Youth|Schools|Cadet|Minime"),
  list(class = "ncaa_lower", pat = paste0(
    "Division II|Division III|Div. II|Div. III|NAIA|NJCAA|Conference USA|",
    "Big Ten|SEC Outdoor|SEC Indoor|Pac-12|ACC Outdoor|ACC Indoor|",
    "Inter-University|Intervarsity|Students Open")),
  list(class = "ncaa", pat = "NCAA|Division I|Div. I|Collegiate|University Championships"),
  list(class = "olympics",       pat = "Olympic Games|XXX+ Olympic"),
  list(class = "world_champs",   pat = "World Athletics Championships|IAAF World Championships(?! in Athletics.*Indoor)|World Championships in Athletics"),
  list(class = "world_indoor",   pat = "World (Athletics )?Indoor Championships|IAAF World Indoor"),
  list(class = "world_other",    pat = "World Athletics (Relays|Cross Country|Race Walking|Road Running)|World Half Marathon|World Cross Country|World Race Walking|World Mountain"),
  list(class = "commonwealth",   pat = "Commonwealth Games"),
  list(class = "asian_games",    pat = "Asian Games"),
  list(class = "panam_games",    pat = "Pan American Games"),
  list(class = "african_games",  pat = "African Games|All-Africa Games"),
  list(class = "european_games", pat = "European Games"),
  list(class = "european_champs", pat = paste0(
    "European Athletics Championships$|European Athletics Championships |",
    "European Athletics Indoor Championships|European Championships$")),
  list(class = "continental",    pat = paste0(
    "European Athletics Championships|European Athletics Indoor Championships|",
    "African (Athletics )?Championships|Asian (Athletics )?Championships|",
    "Asian Indoor Athletics Championships|",
    "Pan American Athletics Championships|NACAC Championships|",
    "Oceania (Athletics )?Championships|South American (Athletics )?Championships|",
    "South American Indoor|Ibero.?American")),
  list(class = "road_race", pat = paste0(
    "Marathon|Half.?Marathon|\\b10 ?[Kk]m?\\b|\\b5 ?[Kk]m?\\b|",
    "Road Running|Road Race|Elite 10K|10K Elite|Great North Run|",
    "City Run|Corrida|Maraton")),
  list(class = "indoor_tour", pat = "World Indoor Tour|Indoor Tour Gold|Millrose|Mill\u00earose"),
  list(class = "regional_games", pat = paste0(
    "Mediterranean Games|Islamic Solidarity|",
    "Universiade|World University|Southeast Asian Games|Bolivarian|",
    "Central American|South American Games|GCC Games|Military Games|",
    "Military World|Gulf Games|Pacific Games|Maccabiah")),
  list(class = "diamond_league",
       pat = paste0("Weltklasse Z|Athletissima|Prefontaine Classic|Herculis|",
                    "Golden Gala|Bislett Games|Memorial Van Damme|",
                    "Anniversary Games|London Athletics Meet|Meeting de Paris|",
                    "Skolimowska Memorial|BAUHAUS.?galan|Diamond League|",
                    "Mohammed VI|Shanghai Golden Grand Prix|Qatar Athletic|",
                    "Bauhaus Galan|Dream Mile|Keqiao|Suzhou")),
  list(class = "club_meet", pat = paste0(
    "pre-programme|pre-event|Bislett Spring|Bislett Open|Bislett 600|",
    "Bislettmila|karusell|Distanseserie|Distance challenge|",
    "Lambertseter|Sommerstevne|Nasjonalt|KM Oslo|Street Tour|",
    "Boysen Memorial|Aspire Indoor Invitational|Challenge Games")),
  # TIGHTENED (scale fix #3, see header): bare "|Meeting" dropped. It matched
  # 2,400 of the missing competitions and 2,332 (97%) were generic club/open
  # meetings, not continental-tour fixtures. Verified the named alternatives
  # below still catch the real ones (35 remain: Motonet GP, Rieti, Zagreb,
  # Padova, the Kusocinski/Szewinska memorials, ...).
  list(class = "continental_tour", pat = "Continental Tour|Golden Spike|Kusoci|Szewi|Rieti|Zag|Hanzekovic|Padova|Turku|Motonet|Racers Grand Prix"),
  list(class = "national_champs", pat = "National Championships|Championships of|(USA|British|Jamaican|Kenyan|Australian|Japanese|Chinese|German|French|Italian|Spanish|Polish|South African|Canadian|Indian|Nigerian|Ethiopian|Dutch|Swedish|Norwegian|Finnish|Czech|Swiss|Belgian|Irish|Portuguese|Greek|Turkish|Brazilian|Mexican|Cuban|New Zealand) Championships"),
  list(class = "team_champs_lower", pat = paste0(
    "Second Division|Third Division|Second League|First League|1st League|",
    "2nd League|3rd League|Race Walking Cup|Throwing Cup")),
  # TIGHTENED (scale fix #3): bare "Cup$" dropped, "World Cup" added explicitly
  # so the genuine one (which would otherwise fall through to unclassified)
  # still matches. 251 raw hits -> 65; every displaced name inspected was a
  # club/invitational cup ("Xmas-Cup", "University Park Sykes & Sabock
  # Challenge Cup"), not a team championship.
  list(class = "team_champs",    pat = "Team Championships|European Athletics Team|Super League|First Division|World Cup")
)
cat_of <- function(x) {
  out <- rep(NA_character_, length(x))
  excluded <- grepl(NOT_THE_EVENT, x, ignore.case = TRUE, perl = TRUE)
  for (r in RULES) {
    senior <- r$class %in% c("olympics","world_champs","world_indoor","world_other",
                             "commonwealth","continental","regional_games")
    elite <- r$class %in% c("olympics","world_champs","world_indoor","commonwealth",
                            "continental","diamond_league")
    never <- grepl(NEVER_ELITE, x, ignore.case = TRUE, perl = TRUE)
    hit <- is.na(out) & grepl(r$pat, x, ignore.case = TRUE, perl = TRUE) &
      !(senior & excluded) & !(elite & never)
    out[hit] <- r$class
  }
  out[is.na(out)] <- "unclassified"
  out
}
miss_nm[, class := cat_of(comp_name)]
cat("\nname-based classification of the missing competitions:\n")
print(miss_nm[, .N, by = class][order(-N)])

# ---- measured field strength, over the FULL corpus (both routes, catalogued
#      and uncatalogued alike) so percentiles are computed on the richest
#      denominator available. Verbatim methodology from
#      build_competition_catalogue.R except is_final_round() (scale fix #2). -
corp[, era := 4L * (year(date) %/% 4L)]
corp[, n_era := .N, by = .(event_id, era)]
corp[, pctl := fifelse(n_era >= 200L,
                     frank(perf, na.last = "keep") / sum(!is.na(perf)), NA_real_),
   by = .(event_id, era)]
corp[is.na(pctl), pctl := frank(perf, na.last = "keep") / sum(!is.na(perf)), by = event_id]
ath_q <- corp[!is.na(pctl), .(a_q = max(pctl)), by = .(athlete_id, event_id)]

is_final_round <- function(rnd) {
  # competition route spells it out; career route uses "F"/"F1".."F9".
  literal <- grepl("final", rnd, ignore.case = TRUE) & !grepl("semi", rnd, ignore.case = TRUE)
  short <- grepl("^F[0-9]*$", rnd)
  literal | short
}
n_final_literal <- sum(grepl("final", corp$round, ignore.case = TRUE) &
                        !grepl("semi", corp$round, ignore.case = TRUE))
n_final_fixed <- sum(is_final_round(corp$round))
cat(sprintf("\nfinal-round rows: literal-only regex = %s | with short-code fix = %s\n",
    format(n_final_literal, big.mark = ","), format(n_final_fixed, big.mark = ",")))
stopifnot("the short-code fix must find MORE finals than the literal regex alone -- if not, round encoding has changed and this needs re-checking" =
            n_final_fixed > n_final_literal)

fin_rows <- corp[is_final_round(round)]
fin_rows <- merge(fin_rows, ath_q, by = c("athlete_id", "event_id"), all.x = TRUE)
ev_q <- fin_rows[!is.na(a_q), .(q = mean(a_q), n_ath = .N), by = .(competition_id, event_id, era)]
ev_q <- ev_q[n_ath >= 4]
ev_q[, n_meets := .N, by = .(event_id, era)]
ev_q <- ev_q[n_meets >= 3]
ev_q[, ev_pct := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
strength <- ev_q[, .(strength = round(mean(ev_pct), 1), races_won = .N), by = competition_id]
MIN_EVENTS_FOR_STRENGTH <- 5L
strength[races_won < MIN_EVENTS_FOR_STRENGTH, strength := NA_real_]
rm(fin_rows, ev_q, ath_q); invisible(gc())

# ---- per-competition summary, MISSING COMPETITIONS ONLY (never touches the
#      7,054 rows already in cat0) --------------------------------------------
miss_corp <- corp[competition_id %chin% miss_ids]
summ <- miss_corp[, .(
  first_date = min(date, na.rm = TRUE), last_date = max(date, na.rm = TRUE),
  year = year(min(date, na.rm = TRUE)), results = .N, athletes = uniqueN(athlete_id),
  events = uniqueN(event_id)
), by = competition_id]
cat_tbl <- merge(miss_nm, summ, by = "competition_id", all.x = TRUE)
cat_tbl <- merge(cat_tbl, strength, by = "competition_id", all.x = TRUE)

# ---- meet_tier: identical rule to build_competition_catalogue.R ------------
KNOWN_T1 <- c("olympics", "world_champs", "commonwealth", "world_indoor",
              "diamond_league", "world_other", "indoor_tour", "european_champs")
KNOWN_T2 <- c("continental", "national_champs", "ncaa", "team_champs",
              "continental_tour", "regional_games")
KNOWN_T3 <- c("age_group", "club_meet", "ncaa_lower", "team_champs_lower")
BY_STRENGTH <- c("road_race")
cat_tbl[, meet_tier := fcase(
  class %in% KNOWN_T1, "T1_elite",
  class %in% KNOWN_T2, "T2_strong",
  class %in% KNOWN_T3, "T3_development",
  class %in% BY_STRENGTH & !is.na(strength) & strength >= 75, "T1_elite",
  class %in% BY_STRENGTH & !is.na(strength) & strength >= 50, "T2_strong",
  class %in% BY_STRENGTH, "T3_development",
  default = NA_character_)]
uq <- stats::quantile(cat_tbl[is.na(meet_tier)]$strength, 0.55, na.rm = TRUE)
cat_tbl[is.na(meet_tier), meet_tier := fcase(
  is.na(strength), "T3_development",
  strength >= uq[[1]], "T2_strong",
  default = "T3_development")]
cat(sprintf("\nunclassified additions capped at T2, split at strength %.1f\n", uq[[1]]))

cat("\ntier distribution of the additions:\n")
print(cat_tbl[, .N, by = meet_tier][order(-N)])
cat("\nT1 additions by class:\n")
print(cat_tbl[meet_tier == "T1_elite", .N, by = class])
cat("\nT2 additions by class:\n")
print(cat_tbl[meet_tier == "T2_strong", .N, by = class])

# ---- ANCHOR CHECKS -----------------------------------------------------------
# Same discipline as build_competition_catalogue.R and the two road scripts:
# facts known before the data was touched. A failure means the METHOD is
# wrong, not the fact.
cat("\n=== ANCHOR CHECKS ===\n")
anchor <- function(label, ok, detail = "") {
  cat(sprintf("  %-62s %s%s\n", label, if (isTRUE(ok)) "PASS" else "**FAIL**",
              if (nzchar(detail)) paste0("  ", detail) else ""))
  isTRUE(ok)
}
t1 <- cat_tbl[meet_tier == "T1_elite"]
t2 <- cat_tbl[meet_tier == "T2_strong"]
ok1 <- anchor("no duplicate competition ids in the addition set",
              !anyDuplicated(cat_tbl$competition_id))
ok2 <- anchor("no added id is already in the catalogue",
              !any(cat_tbl$competition_id %chin% cat0$competition_id))
ok3 <- anchor("every added row carries a meet_tier",
              !any(is.na(cat_tbl$meet_tier)))
ok4 <- anchor("no T1 addition is unclassified, club_meet, age_group or road_race",
              !any(t1$class %in% c("unclassified", "club_meet", "age_group", "road_race")),
              paste(sort(unique(t1$class[t1$class %in% c("unclassified","club_meet","age_group","road_race")])), collapse=","))
ok5 <- anchor("T1 additions are a plausible count for global/DL-level meets (<=200)",
              nrow(t1) <= 200L, sprintf("%d found", nrow(t1)))
ok6 <- anchor("continental_tour additions stayed tightened (<=100, was 2,293 before the fix)",
              cat_tbl[class == "continental_tour", .N] <= 100L,
              sprintf("%d found", cat_tbl[class == "continental_tour", .N]))
ok7 <- anchor("team_champs additions stayed tightened (<=150, was 251 before the fix)",
              cat_tbl[class == "team_champs", .N] <= 150L,
              sprintf("%d found", cat_tbl[class == "team_champs", .N]))
ok8 <- anchor("road racing stays out of T1/T2 (see road-coverage-and-the-strength-metric-2026-08-15.md)",
              cat_tbl[class == "road_race" & meet_tier != "T3_development", .N] <= 50L,
              sprintf("%d road_race competitions reached T1/T2", cat_tbl[class == "road_race" & meet_tier != "T3_development", .N]))
ok9 <- anchor("no T1 meet sits below strength 40 where strength is known",
              !any(t1$strength < 40, na.rm = TRUE),
              sprintf("%d below", sum(t1$strength < 40, na.rm = TRUE)))
ok10 <- anchor("no NCAA D2/D3 or conference meet is above T3",
               !any(cat_tbl[class == "ncaa_lower"]$meet_tier != "T3_development"))
ok11 <- anchor("name coverage for the addition set is effectively complete (>=95%)",
               mean(!is.na(cat_tbl$comp_name)) >= 0.95,
               sprintf("%.1f%%", 100 * mean(!is.na(cat_tbl$comp_name))))
if (!all(ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11)) {
  cli::cli_abort(c(
    "x" = "An anchor failed. Fix the rule, do not special-case the exception.",
    "i" = "Catalogue was NOT written -- rerun after fixing."))
}

# ---- write: backup, tmp, atomic replace, matching the established pattern --
new <- cat_tbl[, .(competition_id, comp_name, year, results, athletes, events,
                    strength, races_won, class, meet_tier)]
for (cn in setdiff(names(cat0), names(new))) new[, (cn) := NA]
out <- rbind(cat0, new[, names(cat0), with = FALSE])
stopifnot("duplicate competition ids after rbind" = !anyDuplicated(out$competition_id),
          "row count did not grow by exactly the addition set" =
            nrow(out) == nrow(cat0) + nrow(new))

if (!file.exists(paste0(CAT, ".bak"))) file.copy(CAT, paste0(CAT, ".bak"))
# arrow memory-maps a parquet it has read, so writing back to the same path
# fails with "user-mapped section open". Write beside it, release, then replace.
tmp <- paste0(CAT, ".tmp")
n_after <- nrow(out); n_added <- nrow(new)
write_parquet(out, tmp)
rm(cat0, nm, corp, miss_corp, cat_tbl, new, out); invisible(gc())
ok <- file.copy(tmp, CAT, overwrite = TRUE)
stopifnot("could not replace the catalogue" = isTRUE(ok))
unlink(tmp)
cat(sprintf("\ncatalogue after: %s competitions (+%s), backup at competition_catalogue.parquet.bak\n",
            format(n_after, big.mark = ","), format(n_added, big.mark = ",")))
cat("This changes nothing until form_ratings.R is re-run -- the catalogue is\n")
cat("joined at engine time, not baked into the corpus.\n")
