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

RULES <- list(
  list(class = "age_group",      pat = "U13|U14|U15|U16|U17|U18|U20|U23|Junior|Youth|Schools|Cadet|Minime"),
  list(class = "ncaa",           pat = "NCAA|NAIA|NJCAA|Division I|Division II|Division III|Big Ten|SEC Outdoor|Pac-12|ACC Outdoor"),
  list(class = "olympics",       pat = "Olympic Games|XXX+ Olympic"),
  # The body was the IAAF until 2019, so London 2017 and Doha 2019 are "IAAF
  # World Championships in Athletics". Fixing this pattern in the HARVESTER and
  # not here left them harvested and unclassified -- the same omission twice.
  list(class = "world_champs",   pat = "World Athletics Championships|IAAF World Championships(?! in Athletics.*Indoor)|World Championships in Athletics"),
  list(class = "world_indoor",   pat = "World (Athletics )?Indoor Championships|IAAF World Indoor"),
  list(class = "world_other",    pat = "World Athletics (Relays|Cross Country|Race Walking|Road Running)|World Half Marathon|World Cross Country|World Race Walking|World Mountain"),
  list(class = "commonwealth",   pat = "Commonwealth Games"),
  list(class = "continental",    pat = "European Athletics Championships|European Athletics Indoor Championships|African (Athletics )?Championships|Asian (Athletics )?Championships|Pan American Games|NACAC Championships|Oceania (Athletics )?Championships|South American Championships|Ibero.?American"),
  list(class = "regional_games", pat = "Asian Games|African Games|Mediterranean Games|Islamic Solidarity|Universiade|World University|Southeast Asian Games|Bolivarian|Central American"),
  list(class = "diamond_league", pat = "Weltklasse|Prefontaine|Athletissima|Bislett|Herculis|Golden Gala|Anniversary Games|Diamond League|Meeting de Paris|Memorial Van Damme|Shanghai|Doha|Rabat|Silesia|Skolimowska|Bauhaus|Xiamen|Suzhou|Oslo"),
  list(class = "continental_tour", pat = "Continental Tour|Golden Spike|Kusoci|Szewi|Rieti|Zag|Hanzekovic|Padova|Turku|Motonet|Meeting"),
  list(class = "national_champs", pat = "National Championships|Championships of|(USA|British|Jamaican|Kenyan|Australian|Japanese|Chinese|German|French|Italian|Spanish|Polish|South African|Canadian|Indian|Nigerian|Ethiopian|Dutch|Swedish|Norwegian|Finnish|Czech|Swiss|Belgian|Irish|Portuguese|Greek|Turkish|Brazilian|Mexican|Cuban|New Zealand) Championships"),
  list(class = "team_champs",    pat = "Team Championships|European Athletics Team|Cup$|Super League|First League|First Division")
)
cat_of <- function(nm) {
  out <- rep(NA_character_, length(nm))
  # A meet whose name merely REFERENCES a major is not that major.
  excluded <- grepl(NOT_THE_EVENT, nm, ignore.case = TRUE, perl = TRUE)
  for (r in RULES) {
    senior <- r$class %in% c("olympics","world_champs","world_indoor","world_other",
                             "commonwealth","continental","regional_games")
    hit <- is.na(out) & grepl(r$pat, nm, ignore.case = TRUE, perl = TRUE) &
      !(senior & excluded)
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
KNOWN_T1 <- c("olympics", "world_champs", "commonwealth", "world_indoor",
              "diamond_league")
KNOWN_T2 <- c("continental", "national_champs", "ncaa", "team_champs",
              "continental_tour", "regional_games", "world_other")
KNOWN_T3 <- c("age_group")
cat_tbl[, meet_tier := fcase(
  class %in% KNOWN_T1, "T1_elite",
  class %in% KNOWN_T2, "T2_strong",
  class %in% KNOWN_T3, "T3_development",
  # unclassified: fall back to the measured field strength, banded on its own
  # distribution among unclassified meets so the bands mean something.
  default = NA_character_)]
uq <- stats::quantile(cat_tbl[is.na(meet_tier)]$strength, c(0.80, 0.45), na.rm = TRUE)
cat_tbl[is.na(meet_tier), meet_tier := fcase(
  is.na(strength), "T3_development",
  strength >= uq[[1]], "T1_elite",
  strength >= uq[[2]], "T2_strong",
  default = "T3_development")]
cat(sprintf("
unclassified meets banded on strength at %.1f / %.1f
", uq[[1]], uq[[2]]))

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
if (!all(ok1, ok2, ok3, ok4)) {
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
