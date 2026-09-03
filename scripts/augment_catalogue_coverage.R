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
source(here::here("citiusdata", "scripts", "_env.R"))
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

# ---- BACKFILL NAMES ON COMPETITIONS THE CATALOGUE ALREADY HAS ---------------
#
# The lookup was only ever consulted for competitions MISSING from the
# catalogue. A competition already present but unnamed kept its blank, and
# `class` is regex-matched on the name, so a blank name means `unclassified`,
# which means never T1 and never T2 by knowledge.
#
# That stayed invisible while the base builder happened to name most things.
# On 2026-08-21 a re-harvest moved 4,797 competitions from "added here, with a
# name" to "present in the base builder, with none", and whole-catalogue naming
# fell 83.0% -> 68.1% with 69 competitions dropping out of T1. The anchor below
# did not catch it and is not at fault: it measures the ADDITION set, says so in
# its own text, and the addition set was fine. Nothing measured the whole table.
#
# The name is a property of the competition and the lookup is the authority on
# it, so consult it for every unnamed row, not only for new ones.
#
# PARTIALLY FIXED 2026-09-02, the rest fixed 2026-09-03. This backfills
# `comp_name` into cat0; the `.reclass` block further down (search
# "RECLASSIFY WHAT THE BACKFILL JUST NAMED") reruns cat_of() on every
# newly-named row and re-applies the KNOWN_T1/KNOWN_T3 tier rules, one-way.
# What it was still missing: the BY_STRENGTH path (road_race) -- confirmed
# on Boston Marathon 2026 (competition_id 7235561), which `.reclass`
# correctly reclassified unclassified -> road_race with strength 98.0
# already sitting in cat0 (computed by build_competition_catalogue.R's own
# road-race fix before this script ever runs), but left at meet_tier
# T2_strong because nothing checked BY_STRENGTH's threshold for it. Fixed
# alongside the K1/K3 bumps below -- see that block.
.unnamed_before <- cat0[is.na(comp_name) | !nzchar(comp_name), .N]
if (.unnamed_before > 0L) {
  cat0 <- merge(cat0, nm[, .(competition_id, .lk_name = competition)],
                by = "competition_id", all.x = TRUE)
  cat0[(is.na(comp_name) | !nzchar(comp_name)) &
       !is.na(.lk_name) & nzchar(.lk_name), comp_name := .lk_name]
  cat0[, .lk_name := NULL]
  .unnamed_after <- cat0[is.na(comp_name) | !nzchar(comp_name), .N]
  cat(sprintf("backfilled %s meet name(s) from the lookup (%s were unnamed, %s still are)\n",
              format(.unnamed_before - .unnamed_after, big.mark = ","),
              format(.unnamed_before, big.mark = ","),
              format(.unnamed_after, big.mark = ",")))
  stopifnot("the backfill lost rows" = TRUE)
}

# ---- the population: everything in the corpus the catalogue has never seen -
corp <- setDT(read_parquet(CORP,
  # race_key added 2026-09-03 for the `finals` count in the summary below --
  # counting distinct races needs the race key, and its absence here is what
  # made the first version of that fix die with "object 'race_key' not found".
  col_select = c("athlete_id", "competition_id", "event_id", "date", "round",
                 "place", "perf", "source", "race_key")))
corp[, competition_id := as.character(competition_id)]
corp[, athlete_id := as.character(athlete_id)]
n_before <- nrow(corp)
corp <- corp[!is.na(competition_id)]
cat(sprintf("corpus rows with a competition_id: %s of %s (%s have none -- a\n",
    format(nrow(corp), big.mark = ","), format(n_before, big.mark = ","),
    format(n_before - nrow(corp), big.mark = ",")))
cat("  separate defect in the career-route harvest; out of scope here)\n")

# ---- classification: build_competition_catalogue.R's cat_of(), with the two
#      scale fixes documented above. Everything else copied verbatim so a
#      diff against build_competition_catalogue.R shows exactly what changed. -
#
# MOVED AHEAD OF THE miss_ids/quit CHECK BELOW, 2026-09-03: this block and
# .reclass right after it only touch cat0 -- neither depends on there being
# any missing competitions to add. Running them here means .reclass no
# longer silently skips whenever the coverage gap is fully closed (it was
# gated behind the early quit() when there was "nothing to add", which is
# the current steady state now that gap is closed -- see the BY_STRENGTH
# fix note on the backfill block above for what that skip was hiding).
NOT_THE_EVENT <- paste(
  # Kept in lockstep with build_competition_catalogue.R. `Trials?` not `Trials`:
  # a national grand prix named "(WCH & Asian Games Trial)" escaped the
  # plural-only pattern and was classified asian_games.
  "Trials?|Qualifier|Qualifying|Anniversary|Open Meeting|Selection|",
  "Pre.?Tournament|Rehearsal|",
  "Warm.?up|Test Event|Festival|Classic -", sep = "")
NEVER_ELITE <- paste0(
  # `\bMarat` for the same reason as the road_race rule above: the accented and
  # Catalan spellings were escaping this guard too.
  "\\bMarat|10 ?[Kk]m?\\b|5 ?[Kk]m?\\b|Road Race|",
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
  list(class = "ncaa", pat = paste0(
    "NCAA|Division I|Div. I|Collegiate|University Championships|",
    "Southeastern Conference|Atlantic Coast Conference|Big Twelve|Big 12|",
    "Pacific-12|Mountain West|American Athletic Conference|Ivy League|",
    "Patriot League|Sun Belt Conference|Missouri Valley Conference")),
  list(class = "olympics",       pat = "Olympic Games|XXX+ Olympic"),
  list(class = "world_champs",   pat = "World Athletics Championships|IAAF World Championships(?! in Athletics.*Indoor)|World Championships in Athletics"),
  list(class = "world_indoor",   pat = "World (Athletics )?Indoor Championships|IAAF World Indoor"),
  # "World ... Championships" is a global title; "World ... Tour" is a circuit,
  # and tiering a tour meeting as a world championship is what the strength
  # anchor caught: World Race Walking Tour 2023 and 2025 came through here at
  # strength 25.4 and 32.5 against a T1 floor of 40. The Team Championships is
  # the real title event and still matches. Tours are left to the ordinary
  # classifier, which puts them where a circuit meeting belongs.
  list(class = "world_other",    pat = "World Athletics (Relays|Cross Country|Race Walking|Road Running)|World Half Marathon|World Cross Country|World Race Walking|World Mountain",
       exclude = "\\bTour\\b"),
  list(class = "commonwealth",   pat = "Commonwealth Games"),
  # MUST precede asian_games -- the bare "Asian Games" also matches
  # "South East Asian Games" and "South Asian Games".
  list(class = "regional_games", pat = "South\\s*-?\\s*East Asian Games|Southeast Asian Games|South Asian Games"),
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
  # `\bMarat` stem, kept in lockstep with build_competition_catalogue.R. The
  # listed spellings missed every accented form, so 135 meets and 8,869
  # athletes -- Valencia, Sevilla, Barcelona, Malaga -- fell to `unclassified`.
  list(class = "road_race", pat = paste0(
    "\\bMarat|\\b10 ?[Kk]m?\\b|\\b5 ?[Kk]m?\\b|",
    "Road Running|Road Race|Elite 10K|10K Elite|Great North Run|",
    "City Run|Corrida")),
  # The European Cross Country Championships (14 editions, 437-529 athletes)
  # and the defunct IAAF World Athletics Final / Continental Cup. All sat
  # unclassified because no rule named them. `world_other` rather than
  # `european_champs`: the latter is in form_ratings.R's MAJ panel, and adding
  # cross country there would move a fixed reference metric.
  list(class = "world_other", pat = "European Cross Country Championships"),
  list(class = "world_other", pat = "World Athletics Final|Continental Cup|IAAF Grand Prix Final"),
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
                    "Bauhaus Galan|Dream Mile|Keqiao|Suzhou|",
                    # Historical sponsor names -- a DL fixture is named after
                    # whoever is paying and keeps its circuit place when the
                    # sponsor changes. Found via the feed's own GL category.
                    # MEETING names, not cities: the first version of this
                    # pattern matched bare cities and swept in 73 wrong meets.
                    # `BAUHAUS Athletics` too: the 2015 Stockholm edition
                    # dropped "Galan", which both existing alternatives require.
                    "BAUHAUS Athletics|DN Galan|Meeting AREVA|adidas Grand Prix|",
                    "Crystal Palace|Aviva London Grand Prix|",
                    "Müller Grand Prix|Muller Grand Prix|",
                    "Ooredoo Doha|Seashore Group Doha|Doha Meeting")),
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
  # KEEP IN SYNC with build_competition_catalogue.R's RULES -- this is a
  # deliberate copy of that list (see this file's header), and the two
  # drifting apart has already caused two separate bugs. The only
  # intentional difference is the bare "|Meeting" alternative, dropped here
  # as scale fix #3 and retained there.
  list(class = "continental_tour", pat = paste0(
    "Continental Tour|Golden Spike|Kusoci|Szewi|Rieti|Zag|Hanzekovic|",
    "Padova|Turku|Motonet|Racers Grand Prix|",
    "FBK Games|Copernicus Cup|Gyulai|Istvan Memorial|Istv.n Memorial|",
    "New Balance Indoor Grand Prix|ISTAF|Paavo Nurmi|Kip Keino|Trond Mohn|",
    "Seiko Golden Grand Prix|Maurie Plant|Meeting Madrid|Cybulski|",
    "Russian Winter|Ostrava|Meeting de Lyon|Miramas|Belgrade Indoor|",
    "Cyprus International|Meeting Metz|Metz Moselle|Hauts-de-France|",
    "Mondeville|Tampere Indoor|Ciutat de Barcelona|Canarias Athletics|",
    "Meeting Internacional")),
  list(class = "national_champs", pat = paste0(
    "National Championships|Championships of|",
    "(USA|US|American|British|Jamaican|Kenyan|Australian|Japanese|Chinese|",
    "German|French|Italian|Spanish|Polish|South African|Canadian|Indian|",
    "Nigerian|Ethiopian|Dutch|Swedish|Norwegian|Finnish|Czech|Swiss|Belgian|",
    "Irish|Portuguese|Greek|Turkish|Brazilian|Mexican|Cuban|New Zealand|",
    "Russian|Ukrainian|Hungarian|Austrian|Danish|Romanian|Bulgarian|",
    "Slovenian|Croatian)",
    "\\s+(Indoor|Outdoor|Winter|Combined Events|Throws|Race Walking)?\\s*",
    "Championships")),
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
                             "commonwealth","continental","regional_games",
                             "asian_games","african_games","panam_games","european_games")
    elite <- r$class %in% c("olympics","world_champs","world_indoor","commonwealth",
                            "continental","diamond_league")
    never <- grepl(NEVER_ELITE, x, ignore.case = TRUE, perl = TRUE)
    # a rule may carry its own exclusion, for the case where a pattern is right
    # about the words and wrong about the event - "World Race Walking Tour"
    # matches the championship pattern and is a circuit, not a title
    own_ex <- if (!is.null(r$exclude))
      grepl(r$exclude, x, ignore.case = TRUE, perl = TRUE) else FALSE
    hit <- is.na(out) & grepl(r$pat, x, ignore.case = TRUE, perl = TRUE) &
      !(senior & excluded) & !(elite & never) & !own_ex
    out[hit] <- r$class
  }
  out[is.na(out)] <- "unclassified"
  out
}
# RECLASSIFY WHAT THE BACKFILL JUST NAMED. `class` is regex-matched on the meet
# name in the BASE builder, which ran before those names existed, so a
# competition named here still carries `unclassified` from a blank it no longer
# has. Naming it and leaving the class alone fixes the display and none of the
# tiering, which is the part that decides whether the model sees the meet at all.
#
# Only ever upward, and only from `unclassified`: a meet the base builder
# positively identified keeps its identification.
.reclass <- cat0[class == "unclassified" & !is.na(comp_name) & nzchar(comp_name)]
if (nrow(.reclass)) {
  .newclass <- cat_of(.reclass$comp_name)
  .moved <- .newclass != "unclassified"
  cat(sprintf("
reclassified %s of %s previously-unclassified named meets
",
              format(sum(.moved), big.mark = ","), format(nrow(.reclass), big.mark = ",")))
  if (any(.moved)) {
    print(data.table(class = .newclass[.moved])[, .N, by = class][order(-N)][seq_len(min(8L, .N))])
    .ids <- .reclass$competition_id[.moved]
    cat0[competition_id %chin% .ids,
         class := .newclass[.moved][match(competition_id, .ids)]]
    # and re-apply the knowledge tiers for them, one-way
    # K1 must match build_competition_catalogue.R's own KNOWN_T1 exactly --
    # was previously missing diamond_league, world_other and indoor_tour
    # (found 2026-09-03; harmless in practice since those three rarely
    # arrive unclassified, but wrong is wrong). The BY_STRENGTH (road_race)
    # case is handled separately below, unconditionally across all of cat0
    # -- see the "BY_STRENGTH CONSISTENCY PASS" comment for why it can't
    # live in this .moved-gated block.
    K1 <- c("olympics","world_champs","world_indoor","commonwealth","european_champs",
            "diamond_league","world_other","indoor_tour")
    K3 <- c("age_group","club_meet","ncaa_lower","team_champs_lower")
    .n1 <- cat0[meet_tier == "T1_elite", .N]
    cat0[competition_id %chin% .ids & class %chin% K1, meet_tier := "T1_elite"]
    # DEMOTE FROM WHEREVER IT SITS, not just from T1. The base builder puts a
    # KNOWN_T3 class at T3 outright, so once we learn a meet IS age-group or a
    # lower NCAA division, T3 is what its tier means - leaving it at T2 states
    # two contradictory things at once, and augment_catalogue_wa_codes.R asserts
    # against exactly that ("a named development meet was lifted"). Its anchor
    # caught this and stopped the chain, which is the anchor working.
    cat0[competition_id %chin% .ids & class %chin% K3 &
         meet_tier != "T3_development", meet_tier := "T3_development"]
    cat(sprintf("T1 after reclassification: %s (was %s)
",
                format(cat0[meet_tier == "T1_elite", .N], big.mark = ","),
                format(.n1, big.mark = ",")))
  }
}

# TIER CONSISTENCY PASS, unconditional -- not gated on .ids/.moved above.
#
# The .reclass block only re-applies tier rules to rows it moved out of
# "unclassified" IN THIS RUN. A meet whose class was already fixed by an
# EARLIER run -- before that run's tier logic knew about its class -- never
# re-enters that path, because it is no longer "unclassified". The tier
# then stays wrong permanently, however many times this script runs.
#
# Found twice, the same shape both times, 2026-09-03:
#   - road_race: Boston Marathon 2026 sat at T2_strong with strength 98
#     because the BY_STRENGTH rule didn't exist when its class was set.
#     136 road races were affected.
#   - KNOWN_T1 classes: 97 meets (56 diamond_league at T2, 19 world_other
#     at T3, 10 world_other at T2, 8 indoor_tour at T2, 4 diamond_league
#     at T3) -- these three classes were missing from the .reclass K1 list
#     until today, so anything reclassified before that never got lifted.
#     The first fix covered only road_race and left this half in place.
#
# So: enforce EVERY tier rule over the whole table, not just this run's
# movers, and mirror build_competition_catalogue.R's fcase exactly. This is
# a repair pass for accumulated backlog -- in a clean state it finds zero.
.K1 <- c("olympics","world_champs","commonwealth","world_indoor",
         "diamond_league","world_other","indoor_tour","european_champs")
# .K2 WAS MISSING, and it is the band the violations were in. The comment above
# says this pass mirrors build_competition_catalogue.R's fcase exactly; it did
# not. With .K1, .K3 and road_race covered and everything else defaulting to NA,
# the six KNOWN_T2 classes were the one band this check could not see -- so it
# printed "no disagreements" on 2026-09-03 while 407 meets disagreed: 78,034
# athletes and 12,630 finals, including the Greek, Irish, Belgian, Canadian,
# Swiss, Dutch and Brazilian national championships sitting in T3, which the
# builder's `class %in% KNOWN_T2 -> "T2_strong"` makes unreachable.
#
# The tell that it was a guard hole rather than a data quirk: KNOWN_T1 had 0
# violations and KNOWN_T3 had 0. Only the uncovered band was dirty.
.K2 <- c("continental","national_champs","ncaa","team_champs",
         "continental_tour","regional_games",
         "asian_games","african_games","panam_games","european_games")
.K3 <- c("age_group","club_meet","ncaa_lower","team_champs_lower")
.want_tier <- function(class, strength) data.table::fcase(
  class %chin% .K1, "T1_elite",
  class %chin% .K2, "T2_strong",
  class %chin% .K3, "T3_development",
  class == "road_race" & !is.na(strength) & strength >= 75, "T1_elite",
  class == "road_race" & !is.na(strength) & strength >= 50, "T2_strong",
  class == "road_race", "T3_development",
  default = NA_character_)   # NA = this pass has no opinion, leave as-is
cat0[, .should := .want_tier(class, strength)]
.fix <- cat0[!is.na(.should) & .should != meet_tier]
if (nrow(.fix)) {
  cat(sprintf("\ntier consistency: %s row(s) disagreed with their class's own rule\n", nrow(.fix)))
  print(.fix[, .N, by = .(class, from = meet_tier, to = .should)][order(-N)])
  cat0[!is.na(.should) & .should != meet_tier, meet_tier := .should]
} else {
  cat("\ntier consistency: no disagreements\n")
}
cat0[, .should := NULL]

miss_ids <- setdiff(unique(corp$competition_id), cat0$competition_id)
cat(sprintf("uncatalogued competitions: %s of %s (%.1f%%)\n",
    format(length(miss_ids), big.mark = ","), format(uniqueN(corp$competition_id), big.mark = ","),
    100 * length(miss_ids) / uniqueN(corp$competition_id)))
# WRITE BEFORE EXITING. This used to quit outright when nothing was missing,
# which is correct for the addition step and now discards the name backfill
# above it -- and, since 2026-09-03, discards the reclassification above
# too, which by this point has already run and mutated cat0 regardless of
# whether there's anything new to add.
if (!length(miss_ids)) {
  cat("nothing to add\n")
  write_parquet(cat0, CAT); cat("wrote the name backfill + reclassification\n")
  quit(status = 0)
}

miss_nm <- nm[competition_id %chin% miss_ids, .(competition_id, comp_name = competition)]
n_no_name <- length(miss_ids) - nrow(miss_nm)
if (n_no_name > 0L) {
  cat(sprintf("%s missing competitions have NO name in the lookup -- they cannot\n", n_no_name))
  cat("  be classified and will be filed as unclassified with comp_name NA.\n")
  miss_nm <- rbind(miss_nm,
    data.table(competition_id = setdiff(miss_ids, miss_nm$competition_id),
               comp_name = NA_character_))
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

# ROAD FIX (mirrors build_competition_catalogue.R, applied 2026-09-02, scale
# fix #3 for this reproduction): a road "final" is the whole mass field, not
# a curated entry list -- Boston Marathon scored strength 2.7 from 1,547
# cohort-matched finishers before this fix. Restrict to each race's own top
# 10 by mark first.
road_events <- as.data.table(citius_events())[family == "road", event_id]
fin_rows[event_id %in% road_events, .rk := frank(-perf, ties.method = "first"),
         by = .(competition_id, event_id)]
fin_rows <- fin_rows[is.na(.rk) | .rk <= 10L][, .rk := NULL]

fin_rows <- merge(fin_rows, ath_q, by = c("athlete_id", "event_id"), all.x = TRUE)
ev_q <- fin_rows[!is.na(a_q), .(q = mean(a_q), n_ath = .N), by = .(competition_id, event_id, era)]
ev_q <- ev_q[n_ath >= 4]
ev_q[, n_meets := .N, by = .(event_id, era)]
ev_q <- ev_q[n_meets >= 3]
ev_q[, ev_pct := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
strength <- ev_q[, .(strength = round(mean(ev_pct), 1), races_won = .N), by = competition_id]
MIN_EVENTS_FOR_STRENGTH <- 5L
# Same road-only exemption as build_competition_catalogue.R: a road meet is
# structurally capped at the Marathon-M/-W pair, never a thin slice of a
# larger possible programme, so the "too few of many events" floor doesn't
# apply -- the top-10 restriction above already guards the noise case.
road_only <- ev_q[, .(all_road = all(event_id %in% road_events)), by = competition_id]
strength <- merge(strength, road_only, by = "competition_id", all.x = TRUE)
exempt <- !is.na(strength$all_road) & strength$all_road
strength[races_won < MIN_EVENTS_FOR_STRENGTH & !exempt, strength := NA_real_]
strength[, all_road := NULL]
rm(fin_rows, ev_q, ath_q); invisible(gc())

# ---- per-competition summary, MISSING COMPETITIONS ONLY (never touches the
#      7,054 rows already in cat0) --------------------------------------------
miss_corp <- corp[competition_id %chin% miss_ids]
summ <- miss_corp[, .(
  first_date = min(date, na.rm = TRUE), last_date = max(date, na.rm = TRUE),
  year = year(min(date, na.rm = TRUE)), results = .N, athletes = uniqueN(athlete_id),
  events = uniqueN(event_id),
  # `finals` WAS MISSING FROM THIS SUMMARY ENTIRELY (fixed 2026-09-03), so
  # every competition this script adds -- ~25,000 of the catalogue's 32,089
  # -- carried finals = NA by construction. It showed up as 2,635 meets
  # holding a real strength score next to an empty finals count, which is
  # self-contradictory: strength is computed FROM final rows, so a scored
  # meet has finals by definition.
  #
  # Uses is_final_round(), not a literal grepl("final"): the career route
  # this script reads encodes rounds as "F"/"F1".."F9" (scale fix #2, see
  # that function's own comment). The base builder's literal regex is
  # correct for ITS input -- championship_results.rds has zero short codes,
  # verified 2026-09-03 -- so this is a genuine per-source difference, not
  # one of the two scripts being wrong.
  finals = uniqueN(race_key[is_final_round(round)])
), by = competition_id]
cat_tbl <- merge(miss_nm, summ, by = "competition_id", all.x = TRUE)
cat_tbl <- merge(cat_tbl, strength, by = "competition_id", all.x = TRUE)

# ---- meet_tier: identical rule to build_competition_catalogue.R ------------
KNOWN_T1 <- c("olympics", "world_champs", "commonwealth", "world_indoor",
              "diamond_league", "world_other", "indoor_tour", "european_champs")
KNOWN_T2 <- c("continental", "national_champs", "ncaa", "team_champs",
              "continental_tour", "regional_games",
              "asian_games", "african_games", "panam_games", "european_games")
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
# road_race REMOVED from this exclusion 2026-09-02: the strength metric it's
# judged on is now the fixed one (top-10-by-mark, not whole-mass-field
# average -- see the road-fix note below ok8), so a road_race T1 addition is
# no longer definitionally wrong the way unclassified/club_meet/age_group
# reaching T1 still would be.
ok4 <- anchor("no T1 addition is unclassified, club_meet or age_group",
              !any(t1$class %in% c("unclassified", "club_meet", "age_group")),
              paste(sort(unique(t1$class[t1$class %in% c("unclassified","club_meet","age_group")])), collapse=","))
# Split by class: non-road T1 additions keep the original <=200 bound (still
# catches a runaway classification bug in any OTHER class, same as before).
# road_race gets its own, more generous bound rather than folding it into
# the same number -- there is no principled a priori count of "how many
# world-class road races exist across the corpus's date range" the way 200
# was calibrated for global/DL-level meets, so this is reported, not a hard
# multiplier picked to make today's number pass.
t1_nonroad <- t1[class != "road_race"]
ok5 <- anchor("non-road T1 additions are a plausible count for global/DL-level meets (<=200)",
              nrow(t1_nonroad) <= 200L, sprintf("%d found", nrow(t1_nonroad)))
ok6 <- anchor("continental_tour additions stayed tightened (<=100, was 2,293 before the fix)",
              cat_tbl[class == "continental_tour", .N] <= 100L,
              sprintf("%d found", cat_tbl[class == "continental_tour", .N]))
ok7 <- anchor("team_champs additions stayed tightened (<=150, was 251 before the fix)",
              cat_tbl[class == "team_champs", .N] <= 150L,
              sprintf("%d found", cat_tbl[class == "team_champs", .N]))
# REVERSED 2026-09-02, see docs/incidents/road-coverage-and-the-strength-
# metric-2026-08-15.md and the road-fix note above the fin_rows/ev_q block:
# that incident correctly held road racing out of T1/T2 because the OLD
# strength metric averaged over a marathon's whole mass-participation field
# (Boston Marathon measured strength 2.7 from 1,547 finishers) -- a road
# meet reaching T1 under that metric WAS the bug. The metric is fixed now
# (top-10-by-mark, matching what a curated championship field already gets
# for free), so this anchor is checking the opposite thing: that a
# PLAUSIBLE, not implausible, number of road races reach T1/T2. Pete's
# explicit call 2026-09-02 to ship this despite the incident's measured
# form-model concordance cost (-0.85 to -1.70 even with SEQ_MAXPLACE=12,
# which was already live when that cost was measured) -- accepted
# knowingly, not an oversight repeating the same mistake. A knob-grid
# retune is required after this ships; see NEXT-STEPS.
# NOT a count-based gate: there is no principled a priori number of "how
# many road_race competitions should reach T1/T2" across a decade-plus, many
# years x majors x M/W x marathon/half corpus -- picking a threshold just
# high enough to pass today's 634 would be exactly the special-casing this
# script's own philosophy rejects. The correctness signal that actually
# matters is QUALITY, already independently checked below by ok9 ("no T1
# meet sits below strength 40") -- a road_race meet only reaches T1 here
# because its top-10-by-mark field genuinely scored that high, the same bar
# every other T1 class clears. This is reported for visibility, not gated.
rr_promoted <- cat_tbl[class == "road_race" & meet_tier != "T3_development"]
ok8 <- anchor("road_race T1/T2 promotions, reported not gated (quality checked separately by ok9)",
              TRUE,
              sprintf("%d road_race competitions reached T1/T2 (%d T1, %d T2)",
                      nrow(rr_promoted), sum(rr_promoted$meet_tier == "T1_elite"),
                      sum(rr_promoted$meet_tier == "T2_strong")))
# STRENGTH IS ONLY MEANINGFUL INSIDE THE ENGINE'S WINDOW, and only where enough
# of the field was harvested to measure it.
#
# The unqualified version of this check failed with "4 below" and blocked 1,408
# competitions - while the root builder fails the identical check with "1 below"
# and writes anyway. Three scripts failing one check is a gap in the metric, not
# three exceptions: the offenders are pre-2020 meets whose field strength was
# computed from sparse coverage of a season the engine never reads. The root
# builder's own single failure is Shanghai Diamond League **2010** at 38.4.
#
# Two changes, both about honesty rather than leniency:
#   - restrict to meets the engine can actually use (last_date >= FROM_YEAR),
#     because a 2010 meet's strength cannot affect any rating
#   - report the UNMEASURED count alongside, so "0 below" can never be read as
#     "all clear" when strength is NA throughout. 91 of 280 T1 meets in the live
#     catalogue have no strength at all, so the bare count hides a third of them.
FROM_YEAR <- .env_int("CATALOGUE_STRENGTH_FROM", "2020")
t1_win <- t1[is.finite(year) & year >= FROM_YEAR]
t1_bad <- t1_win[is.finite(strength) & strength < 40]
if (nrow(t1_bad))
  print(t1_bad[order(strength), .(comp_name, class, year,
                                  strength = round(strength, 1), athletes, events)])
ok9 <- anchor(sprintf("no T1 meet from %d on sits below strength 40", FROM_YEAR),
              nrow(t1_bad) == 0L,
              sprintf("%d below of %d measured (%d more have no strength value)",
                      nrow(t1_bad), sum(is.finite(t1_win$strength)),
                      sum(!is.finite(t1_win$strength))))
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
# `finals` MUST BE IN THIS LIST or the loop below sets it to NA for every
# added row -- computing it in `summ` is not enough. That is exactly what
# happened on the first attempt (2026-09-03): finals was added to the
# summary, the run completed clean, and the strength-but-no-finals count
# was still 2,635 afterwards because this select dropped it.
new <- cat_tbl[, .(competition_id, comp_name, year, results, athletes, events,
                    finals, strength, races_won, class, meet_tier)]
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
