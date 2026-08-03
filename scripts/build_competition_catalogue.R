# What is actually in our data, one row per competition.
#
# Written 2026-07-31 after discovering the target population cannot be queried.
# The model exists to forecast major championship finals, and "major
# championship final" was not a field anywhere -- it lived only in conversation.
#
# The `tier` column cannot serve. It is per-RESULT, not per-competition, and it
# varies WITHIN a single meet: the 2025 Weltklasse Zurich carries A, DF, F and
# GW across its own results, which classify as high, mid, low and top. 117
# competitions hold more than one tier code. Diamond League marks -- set by
# exactly the athletes we care about -- are routinely labelled low tier and then
# given a -1.69% upward context adjustment as though run at a slow meet.
#
# So this script builds two things the feed does not provide:
#
#   class     what the meet IS, from its name, by explicit auditable rules.
#             Unmatched meets classify as "unclassified" and are REPORTED, never
#             guessed -- the same rule match_event() follows for events.
#
#   strength  how good the field actually was, measured rather than labelled:
#             the percentile of each race winner's mark within that event's own
#             distribution, averaged over the meet. This is what `tier` was
#             supposed to mean and does not.
#
# Usage:  Rscript scripts/build_competition_catalogue.R
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius"))
library(data.table)
OUT <- "C:/dev/citiusverse/citiusdata/data"

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
cat(sprintf("harvest: %s results | %s competitions\n",
            format(nrow(ch), big.mark = ","), format(uniqueN(ch$competition_id), big.mark = ",")))

# ---- classification by name, explicit and auditable -------------------------
# Ordered: the FIRST rule that matches wins, so put the specific before the
# general. Olympics before "games"; world champs before "world".
# A meet is classified by what it IS. Two things matter about the order:
# the EXCLUSIONS run first, because "Trials for World Athletics Championships"
# and "Commonwealth Games Anniversary Open Meeting" both contain the name of a
# major and are neither; and age-group meets run before their senior namesake,
# because "European Athletics U20 Championships" contains "European Athletics".
#
# Every one of these was a false positive in the first pass. They are listed
# rather than patched away so the next person can see what the names do.
NOT_THE_EVENT <- paste(
  "Trials|Qualifier|Qualifying|Anniversary|Open Meeting|Selection|",
  "Warm.?up|Test Event|Festival|Classic -", sep = "")

# Never an elite senior track meeting, whatever else the name contains. A
# marathon held in a Diamond League city is a marathon; a youth meeting under a
# DL banner is a youth meeting. These are applied to the elite classes only, so
# a road race still classifies correctly as a road race elsewhere.
NEVER_ELITE <- paste0(
  "Marathon|Half.?Marathon|10 ?[Kk]m?\\b|5 ?[Kk]m?\\b|Road Race|",
  "karusell|Bislettmila|Distanseserie|Distance challenge|Bislett Spring|",
  "Bislett Open|KM Oslo|Nasjonalt|Sommerstevne|Elite Series|Street Tour|",
  # `m.odzie` rather than a \\u escape: PCRE2 rejects \\u outright, and the
  # Polish "mlodziezowy" (youth) carries a character we should not have to encode.
  "Stabhochsprung|Kugelsto|m.odzie|youth|junior|U1[0-9]|U2[0-3]|",
  "Silesian Meeting|pre-programme|pre-event")

RULES <- list(
  list(class = "age_group",      pat = "U13|U14|U15|U16|U17|U18|U20|U23|Junior|Youth|Schools|Cadet|Minime"),
  # NCAA splits. Division I outdoor and indoor score 81-86 and are genuinely
  # world-class in sprints and jumps -- many Olympic finalists come through them.
  # Division II (65-71), Division III (50-56) and the conference meets (48-54)
  # are not the same competition and should not share a tier with them.
  list(class = "ncaa_lower", pat = paste0(
    "Division II|Division III|Div. II|Div. III|NAIA|NJCAA|Conference USA|",
    "Big Ten|SEC Outdoor|SEC Indoor|Pac-12|ACC Outdoor|ACC Indoor|",
    "Inter-University|Intervarsity|Students Open")),
  list(class = "ncaa", pat = "NCAA|Division I|Div. I|Collegiate|University Championships"),
  list(class = "olympics",       pat = "Olympic Games|XXX+ Olympic"),
  # The body was the IAAF until 2019, so London 2017 and Doha 2019 are "IAAF
  # World Championships in Athletics". Fixing this pattern in the HARVESTER and
  # not here left them harvested and unclassified -- the same omission twice.
  list(class = "world_champs",   pat = "World Athletics Championships|IAAF World Championships(?! in Athletics.*Indoor)|World Championships in Athletics"),
  list(class = "world_indoor",   pat = "World (Athletics )?Indoor Championships|IAAF World Indoor"),
  list(class = "world_other",    pat = "World Athletics (Relays|Cross Country|Race Walking|Road Running)|World Half Marathon|World Cross Country|World Race Walking|World Mountain"),
  list(class = "commonwealth",   pat = "Commonwealth Games"),
  list(class = "asian_games",    pat = "Asian Games"),
  list(class = "panam_games",    pat = "Pan American Games"),
  list(class = "african_games",  pat = "African Games|All-Africa Games"),
  list(class = "european_games", pat = "European Games"),
  # Continental championships, indoor ones included. Pan American Athletics
  # Championships, the Asian Indoor and the South American Indoor were all
  # sitting in `unclassified` and reaching T1 on strength alone -- they are
  # continental titles and should say so.
  # The European Championships, outdoor and indoor, score 83 and 86 -- above
  # the Commonwealth Games, which is already T1 -- because European depth in
  # most events is second only to a global final. Separated from the other
  # continental titles (African 73, Asian 75, South American 63), which are
  # correctly T2.
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

  # ROAD RACING is a meet type, not just an event family.
  #
  # `family` is per EVENT -- a marathon is family "road", and that has always
  # worked. `class` is per MEET, and every rule here was written for
  # championship-shaped meets, so "Shanghai Marathon" had no class at all and
  # reached T1 through the strength fallback. The event was classified; the
  # meeting was not.
  #
  # Elite road racing belongs in T1 (Pete, 2026-07-31) and the strength measure
  # sorts the Berlin marathon from a local half, so the class just needs to
  # exist for the tiering to be honest about what it is looking at.
  list(class = "road_race", pat = paste0(
    "Marathon|Half.?Marathon|\\b10 ?[Kk]m?\\b|\\b5 ?[Kk]m?\\b|",
    "Road Running|Road Race|Elite 10K|10K Elite|Great North Run|",
    "City Run|Corrida|Maraton")),

  # The World Indoor Tour is a real elite circuit with Gold/Silver/Bronze
  # levels, and its Gold meetings are the strongest indoor fields outside a
  # championship.
  list(class = "indoor_tour", pat = "World Indoor Tour|Indoor Tour Gold|Millrose|Mill\u00earose"),
  list(class = "regional_games", pat = paste0(
    "Mediterranean Games|Islamic Solidarity|",
    "Universiade|World University|Southeast Asian Games|Bolivarian|",
    "Central American|South American Games|GCC Games|Military Games|",
    "Military World|Gulf Games|Pacific Games|Maccabiah")),
  # Diamond League by MEETING name, not by city.
  #
  # The first version matched "Shanghai", "Doha", "Rabat", "Oslo", "Silesia",
  # "Xiamen", "Bislett" and "Weltklasse" anywhere in a name, which swept in 73
  # editions of things that merely happen in those places: the Xiamen, Shanghai,
  # Rabat and Oslo marathons, nine Norwegian club meets at Bislett stadium, a
  # German shot put meeting called "Kugelstossmeeting Weltklasse", and a YOUTH
  # meeting in Silesia carrying 39 finals at strength 18. All of them landed in
  # the elite evaluation population. 36% of the class was noise.
  #
  # A real DL meeting is a specific fixture. Named ones only, and never a road
  # race or an age-group meeting held under the same banner.
  list(class = "diamond_league",
       pat = paste0("Weltklasse Z|Athletissima|Prefontaine Classic|Herculis|",
                    "Golden Gala|Bislett Games|Memorial Van Damme|",
                    "Anniversary Games|London Athletics Meet|Meeting de Paris|",
                    "Skolimowska Memorial|BAUHAUS.?galan|Diamond League|",
                    "Mohammed VI|Shanghai Golden Grand Prix|Qatar Athletic|",
                    "Bauhaus Galan|Dream Mile|Keqiao|Suzhou")),
  # Named club-level and warm-up meets. Listed BEFORE continental_tour so a
  # "Bislett Spring" or a "pre-programme" cannot be swept up by a broader rule
  # and land in T1 next to the Olympics -- which is exactly what happened.
  list(class = "club_meet", pat = paste0(
    "pre-programme|pre-event|Bislett Spring|Bislett Open|Bislett 600|",
    "Bislettmila|karusell|Distanseserie|Distance challenge|",
    "Lambertseter|Sommerstevne|Nasjonalt|KM Oslo|Street Tour|",
    "Boysen Memorial|Aspire Indoor Invitational|Challenge Games")),
  list(class = "continental_tour", pat = "Continental Tour|Golden Spike|Kusoci|Szewi|Rieti|Zag|Hanzekovic|Padova|Turku|Motonet|Racers Grand Prix|Meeting"),
  list(class = "national_champs", pat = "National Championships|Championships of|(USA|British|Jamaican|Kenyan|Australian|Japanese|Chinese|German|French|Italian|Spanish|Polish|South African|Canadian|Indian|Nigerian|Ethiopian|Dutch|Swedish|Norwegian|Finnish|Czech|Swiss|Belgian|Irish|Portuguese|Greek|Turkish|Brazilian|Mexican|Cuban|New Zealand) Championships"),
  # Team championships split by division. The Super League and First Division
  # (71-75) are real; the Second Division (57), First League (33) and Third
  # Division (19) are not, and nor are the Pan American race walking cups
  # (37 and 23).
  list(class = "team_champs_lower", pat = paste0(
    "Second Division|Third Division|Second League|First League|1st League|",
    "2nd League|3rd League|Race Walking Cup|Throwing Cup")),
  list(class = "team_champs",    pat = "Team Championships|European Athletics Team|Cup$|Super League|First Division")
)
cat_of <- function(nm) {
  out <- rep(NA_character_, length(nm))
  # A meet whose name merely REFERENCES a major is not that major.
  excluded <- grepl(NOT_THE_EVENT, nm, ignore.case = TRUE, perl = TRUE)
  for (r in RULES) {
    senior <- r$class %in% c("olympics","world_champs","world_indoor","world_other",
                             "commonwealth","continental","regional_games")
    elite <- r$class %in% c("olympics","world_champs","world_indoor","commonwealth",
                            "continental","diamond_league")
    never <- grepl(NEVER_ELITE, nm, ignore.case = TRUE, perl = TRUE)
    hit <- is.na(out) & grepl(r$pat, nm, ignore.case = TRUE, perl = TRUE) &
      !(senior & excluded) & !(elite & never)
    out[hit] <- r$class
  }
  out[is.na(out)] <- "unclassified"
  out
}

# ---- measured field strength: WHO turned up, not how fast they ran ----------
# Two earlier attempts failed their anchor checks, and both failures were
# informative:
#
#   v1  percentile of the WINNER'S mark within the event, all-time.
#       Put the 1996 Olympics below NCAA regional heats -- it was measuring the
#       ERA, because the sport gets faster and our harvest skews recent.
#
#   v2  the same, within Olympic-quad eras. Still failed, because winning marks
#       are the wrong quantity: championship finals are TACTICAL and slow, which
#       this repo documents, while Diamond League races are paced for time. A
#       mark-based metric therefore rates the pinnacle of the sport below a
#       mid-season invitational.
#
# What actually makes a meet strong is the QUALITY OF THE ATHLETES IN IT, which
# is a property of the entrants rather than of how the race was run. Each
# athlete is scored by their own career-best percentile within their event and
# era; a meet is the mean of its finalists. Tactics, pacemakers, weather and
# altitude all drop out, because none of them change who lined up.
ch[, era := 4L * (year(date) %/% 4L)]
ch[, n_era := .N, by = .(event_id, era)]
ch[, pctl := fifelse(n_era >= 200L,
                     frank(perf, na.last = "keep") / sum(!is.na(perf)), NA_real_),
   by = .(event_id, era)]
ch[is.na(pctl), pctl := frank(perf, na.last = "keep") / sum(!is.na(perf)), by = event_id]
# An athlete's standing: the best they have ever been in that event.
ath_q <- ch[!is.na(pctl), .(a_q = max(pctl)), by = .(athlete_id, event_id)]
fin_rows <- ch[grepl("final", round, ignore.case = TRUE) &
                 !grepl("semi", round, ignore.case = TRUE)]
fin_rows <- merge(fin_rows, ath_q, by = c("athlete_id", "event_id"), all.x = TRUE)
# PER EVENT, then averaged -- because breadth is not weakness.
#
# The fourth attempt, and the first driven by a diagnosis rather than a guess.
# Averaging finalist quality across a whole meet penalises the Olympics for
# being big: it runs 43 events including race walks and combined, scoring 90.0,
# while a Diamond League meet runs 14 events of pure elite invitees and scores
# 98.0. The Olympics is not a weaker meet; it is a broader one.
#
# So each meet is scored WITHIN EACH EVENT against the other meets that held
# that event in the same era -- "was this the strongest 100m field of the year"
# -- and the meet's strength is the average of those per-event standings. A
# thin-depth event can then no longer drag a meet down, because it is only ever
# compared against other meets' versions of the same event.
ev_q <- fin_rows[!is.na(a_q), .(q = mean(a_q), n_ath = .N),
                 by = .(competition_id, event_id, era)]
ev_q <- ev_q[n_ath >= 4]
ev_q[, n_meets := .N, by = .(event_id, era)]
ev_q <- ev_q[n_meets >= 3]                       # need something to rank against
ev_q[, ev_pct := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
strength <- ev_q[, .(strength = round(mean(ev_pct), 1), s_raw = round(100 * mean(q), 1),
                     races_won = .N), by = competition_id]

# STRENGTH IS UNRELIABLE AT SMALL n, and it was quietly promoting exhibitions.
#
# "Whatgravity Challenge" scored 96, "Drake Relays Vault at Jordan Creek" 91,
# "Zurich Rock n Roll Running Series Madrid" 90, "Filothei Women Gala" 85. Every
# one is a one- or two-event specialist meet: with a handful of races the
# winners' percentiles come from a tiny sample and land wherever chance puts
# them. A pole vault exhibition reads 91 because the two vaulters who turned up
# happen to be decent vaulters.
#
# Below this many scored events the number is not evidence, so it is withheld
# rather than trusted -- the same reason fit_half_life() refuses a boundary
# optimum and match_event() refuses a fuzzy match.
MIN_EVENTS_FOR_STRENGTH <- 5L
thin <- strength[races_won < MIN_EVENTS_FOR_STRENGTH]
if (nrow(thin)) cli::cli_alert_info(
  "{nrow(thin)} competition{?s} have fewer than {MIN_EVENTS_FOR_STRENGTH} scored events; strength withheld.")
strength[races_won < MIN_EVENTS_FOR_STRENGTH, strength := NA_real_]

cat_tbl <- ch[, .(
  comp_name   = comp_name[1],
  first_date  = min(date, na.rm = TRUE),
  last_date   = max(date, na.rm = TRUE),
  year        = year(min(date, na.rm = TRUE)),
  country     = if ("venue_country" %in% names(ch)) venue_country[1] else NA_character_,
  results     = .N,
  athletes    = uniqueN(athlete_id),
  events      = uniqueN(event_id),
  races       = uniqueN(race_key),
  finals      = uniqueN(race_key[grepl("final", round, ignore.case = TRUE) &
                                   !grepl("semi", round, ignore.case = TRUE)]),
  tier_codes  = paste(sort(unique(na.omit(tier))), collapse = "/"),
  n_tier      = uniqueN(na.omit(tier))
), by = competition_id]
cat_tbl <- merge(cat_tbl, strength, by = "competition_id", all.x = TRUE)
cat_tbl[, class := cat_of(comp_name)]
# The whole point: one boolean the rest of the pipeline can filter on.
# ---- meet_tier: THREE bands, and the count is measured, not chosen ----------
# Fitted within athlete-event on 154,459 marks: centre each athlete on their own
# mean, then average the deviations by band. Equal-count bands so no band is
# starved of evidence.
#
#   bands   offsets                                     ordered?
#   2       -0.207  +0.211                              yes
#   3       -0.163  -0.102  +0.267                      yes
#   4       -0.176  -0.239  +0.068  +0.356              NO
#   5       -0.114  -0.387  +0.015  +0.121  +0.380      NO
#
# At four and five the weakest band comes out BETTER than the second weakest.
# A tier scale that is not monotonic is not measuring meet quality, so three is
# the most the data supports.
#
# Most of the signal is elite-versus-everything-else: T2->T1 separates at 17.4
# sigma, T3->T2 at only 3.0. The bottom split earns 0.06pp and should be
# collapsed without regret if it ever misbehaves.
#
# CAVEAT on those offsets: they come from a SINGLE within-athlete centring,
# which this repo documents as attenuating when exposure correlates with ability
# -- and it does, because good athletes go to good meets. The magnitudes are
# therefore too small, probably by about half. The ORDERING is unaffected, which
# is all the band count rests on. Re-estimate with fit_context_effect() before
# using these as model offsets.
# ---- meet_tier: KNOWLEDGE where we have it, measurement where we do not -----
#
# Four attempts to derive the tier purely from data all failed their anchors,
# each in a different way, and every failure was a size or coverage artefact:
#
#   winner's mark, all-time     measured the ERA -- the sport gets faster
#   winner's mark, per era      measured TACTICS -- championship finals run slow
#   mean finalist quality       measured HARVEST COVERAGE -- old careers are thin
#   per-event, then averaged    measured BREADTH -- averaging 43 events regresses
#                               to the middle while a 2-event meet does not
#
# The mistake was upstream of all four. We do not need a statistic to discover
# that the Olympic Games is the top tier of athletics -- that is knowledge, and
# forcing it through an estimator only gave the estimator a chance to be wrong.
#
# So: tier comes from CLASS wherever the class is known, and from measured
# strength only for the 504 meets we could not classify. Measurement is used
# where knowledge is absent, not as a substitute for it.
# world_other is the senior WORLD title in its discipline -- cross country,
# relays, race walking teams, half marathon. World Cross Country has the deepest
# distance fields on earth and has no business below a Diamond League meeting.
# indoor_tour is the World Indoor Tour Gold circuit, where world records are set.
KNOWN_T1 <- c("olympics", "world_champs", "commonwealth", "world_indoor",
              "diamond_league", "world_other", "indoor_tour", "european_champs")
KNOWN_T2 <- c("continental", "national_champs", "ncaa", "team_champs",
              "continental_tour", "regional_games")
KNOWN_T3 <- c("age_group", "club_meet", "ncaa_lower", "team_champs_lower")
# Road racing spans the Berlin marathon and a local 10K, so it is the one class
# where measured strength genuinely decides the tier rather than the label.
BY_STRENGTH <- c("road_race")
cat_tbl[, meet_tier := fcase(
  class %in% KNOWN_T1, "T1_elite",
  class %in% KNOWN_T2, "T2_strong",
  class %in% KNOWN_T3, "T3_development",
  class %in% BY_STRENGTH & !is.na(strength) & strength >= 75, "T1_elite",
  class %in% BY_STRENGTH & !is.na(strength) & strength >= 50, "T2_strong",
  class %in% BY_STRENGTH, "T3_development",
  # unclassified: fall back to the measured field strength, banded on its own
  # distribution among unclassified meets so the bands mean something.
  default = NA_character_)]
# An UNCLASSIFIED meet is never T1.
#
# 89 unidentified meets carrying 1,381 finals -- 23% of T1 -- were admitted on
# strength alone, and inspection found not one elite meeting among them: a Swiss
# national memorial, the Japan Inter-University Championships, a Conference USA
# indoor meet, the Africa Military Games, a "National Programme" support fixture.
#
# `match_event()` returns NA rather than guessing, because snapping an unknown
# event onto a neighbour corrupts histories undetectably. The same applies here:
# unclassified means we do not know what the meet IS, and a meet we cannot name
# has no business in the population the model is judged on. Strength was built
# to RANK meets we had identified, not to identify them.
uq <- stats::quantile(cat_tbl[is.na(meet_tier)]$strength, 0.55, na.rm = TRUE)
cat_tbl[is.na(meet_tier), meet_tier := fcase(
  is.na(strength), "T3_development",
  strength >= uq[[1]], "T2_strong",
  default = "T3_development")]
cat(sprintf("
unclassified meets capped at T2, split at strength %.1f
", uq[[1]]))

cat_tbl[, is_major := class %in% c("olympics", "world_champs", "commonwealth")]
cat_tbl[, is_global := class %in% c("olympics", "world_champs", "commonwealth",
                                    "world_indoor", "world_other", "continental")]
setorder(cat_tbl, -year, -results)

cat("\n=== what our data contains, by class ===\n")
print(cat_tbl[, .(comps = .N, results = sum(results), races = sum(races),
                  finals = sum(finals),
                  strength = round(median(strength, na.rm = TRUE), 1),
                  raw = round(median(s_raw, na.rm = TRUE), 1),
                  tier_codes_seen = uniqueN(unlist(strsplit(tier_codes, "/")))),
              by = class][order(-results)])

cat("\n=== THE TARGET POPULATION ===\n")
print(cat_tbl[is_major == TRUE, .(year, comp_name, races, finals, athletes,
                                  strength, tier_codes)][order(-year)])

cat("\n=== how badly does `tier` disagree with itself? ===\n")
cat(sprintf("competitions with >1 tier code: %d of %d (%.1f%%), holding %.1f%% of all results\n",
            nrow(cat_tbl[n_tier > 1]), nrow(cat_tbl), 100 * mean(cat_tbl$n_tier > 1),
            100 * sum(cat_tbl[n_tier > 1]$results) / sum(cat_tbl$results)))

cat("\n=== does the measured strength agree with the label? ===\n")
print(cat_tbl[!is.na(strength), .(comps = .N, median_strength = round(median(strength), 1),
                                  p10 = round(quantile(strength, .1), 1),
                                  p90 = round(quantile(strength, .9), 1)),
              by = class][order(-median_strength)])

cat("\n=== unclassified meets, largest first (these need rules) ===\n")
u <- cat_tbl[class == "unclassified"]
cat(sprintf("%d competitions, %.1f%% of results\n", nrow(u),
            100 * sum(u$results) / sum(cat_tbl$results)))
print(head(u[order(-results), .(year, comp_name, results, races, strength)], 15))

# ---- ANCHOR CHECKS ----------------------------------------------------------
# Facts known before the data was touched. If one fails the METHOD is wrong, not
# the fact -- these are not warnings to read past.
cat("
=== ANCHOR CHECKS ===
")
anchor <- function(label, ok, detail = "") {
  cat(sprintf("  %-52s %s%s
", label, if (isTRUE(ok)) "PASS" else "**FAIL**",
              if (nzchar(detail)) paste0("  ", detail) else ""))
  isTRUE(ok)
}
oly <- cat_tbl[class == "olympics" & !is.na(meet_tier)]
wch <- cat_tbl[class == "world_champs" & !is.na(meet_tier)]
dl  <- cat_tbl[class == "diamond_league" & !is.na(meet_tier)]
age <- cat_tbl[class == "age_group" & !is.na(meet_tier)]
ok1 <- anchor("every Olympic Games is T1", all(oly$meet_tier == "T1_elite"),
              paste(sort(unique(oly$meet_tier)), collapse = "/"))
ok2 <- anchor("every senior World Championships is T1", all(wch$meet_tier == "T1_elite"),
              paste(sort(unique(wch$meet_tier)), collapse = "/"))
ok3 <- anchor("most Diamond League is T1", mean(dl$meet_tier == "T1_elite") > 0.6,
              sprintf("%.0f%%", 100 * mean(dl$meet_tier == "T1_elite")))
ok4 <- anchor("no age-group meet is T1", !any(age$meet_tier == "T1_elite"),
              sprintf("%d of %d", sum(age$meet_tier == "T1_elite"), nrow(age)))

# NEGATIVE anchors. The set above only said what must be IN, which is why a
# youth meeting at strength 18 sat in T1 and every check passed. Naming what
# must be true catches under-inclusion; you also have to name what must be FALSE.
t1 <- cat_tbl[meet_tier == "T1_elite"]
ok5 <- anchor("no T1 meet is named as a youth/junior meeting",
              !any(grepl("m.odzie|youth|junior|U1[0-9]|U2[0-3]", t1$comp_name,
                         ignore.case = TRUE, perl = TRUE)),
              paste(utils::head(grep("m.odzie|youth|junior|U1[0-9]|U2[0-3]",
                    t1$comp_name, ignore.case = TRUE, perl = TRUE, value = TRUE), 2),
                    collapse = "; "))
ok6 <- anchor("no T1 meet sits below strength 40",
              !any(t1$strength < 40, na.rm = TRUE),
              sprintf("%d below", sum(t1$strength < 40, na.rm = TRUE)))
ok10 <- anchor("no NCAA D2/D3 or conference meet is above T3",
               !any(cat_tbl[class == "ncaa_lower"]$meet_tier != "T3_development"),
               sprintf("%d above", sum(cat_tbl[class == "ncaa_lower"]$meet_tier != "T3_development")))
ok11 <- anchor("world_other (senior world titles) is T1",
               all(cat_tbl[class == "world_other"]$meet_tier == "T1_elite"),
               paste(sort(unique(cat_tbl[class == "world_other"]$meet_tier)), collapse = "/"))
ok9 <- anchor("no unclassified meet is T1",
              !any(cat_tbl[meet_tier == "T1_elite"]$class == "unclassified"),
              sprintf("%d found", sum(cat_tbl[meet_tier == "T1_elite"]$class == "unclassified")))
ok8 <- anchor("no club or warm-up meet is T1",
              !any(cat_tbl[meet_tier == "T1_elite"]$class == "club_meet"),
              sprintf("%d found", sum(cat_tbl[meet_tier == "T1_elite"]$class == "club_meet")))
ok7 <- anchor("no Diamond League entry is a road race",
              !any(grepl("Marathon|Half|10 ?[Kk]m?\\b", cat_tbl[class == "diamond_league"]$comp_name,
                         ignore.case = TRUE, perl = TRUE)),
              sprintf("%d found", sum(grepl("Marathon|Half|10 ?[Kk]m?\\b",
                      cat_tbl[class == "diamond_league"]$comp_name,
                      ignore.case = TRUE, perl = TRUE))))
if (!all(ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8, ok9, ok10, ok11)) {
  cat("
An anchor failed. The tier metric is measuring something other than
")
  cat("meet quality -- fix the metric, do not special-case the exception.
")
  cat("Catalogue written anyway so the failure can be inspected.
")
}

arrow::write_parquet(cat_tbl, file.path(OUT, "competition_catalogue.parquet"))
cat("\nwrote competition_catalogue.parquet:", nrow(cat_tbl), "competitions\n")
