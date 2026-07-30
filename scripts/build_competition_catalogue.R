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
  list(class = "world_champs",   pat = "World Athletics Championships"),
  list(class = "world_indoor",   pat = "World (Athletics )?Indoor Championships"),
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

# ---- measured field strength -------------------------------------------------
# Percentile of the winning mark within the event's own distribution across the
# whole harvest. Consistent by construction, and independent of any label.
ch[, perf_rank := frank(perf, na.last = "keep") / sum(!is.na(perf)), by = event_id]
win <- ch[place == 1L & !is.na(perf), .(competition_id, event_id, perf_rank)]
strength <- win[, .(strength = round(100 * mean(perf_rank, na.rm = TRUE), 1),
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
cat_tbl[, is_major := class %in% c("olympics", "world_champs", "commonwealth")]
cat_tbl[, is_global := class %in% c("olympics", "world_champs", "commonwealth",
                                    "world_indoor", "world_other", "continental")]
setorder(cat_tbl, -year, -results)

cat("\n=== what our data contains, by class ===\n")
print(cat_tbl[, .(comps = .N, results = sum(results), races = sum(races),
                  finals = sum(finals),
                  strength = round(median(strength, na.rm = TRUE), 1),
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

arrow::write_parquet(cat_tbl, file.path(OUT, "competition_catalogue.parquet"))
cat("\nwrote competition_catalogue.parquet:", nrow(cat_tbl), "competitions\n")
