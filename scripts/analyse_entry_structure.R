# What does one entry actually buy? Field structure and the entry-share null.
#
# The dominance work so far used `expected share = 1 / competing_nations`, which
# assumes a nation sending 86 athletes and one sending 3 are equally likely to
# win. They are not. The right null is entry-based:
#
#   E[golds for nation n in sport s]  =  E_s * a_ns / A_s
#
# where E_s is the sport's gold events, a_ns the nation's competitors and A_s
# the total. It is exact under "every competitor is equally likely to win any
# given gold" and needs no assumption about field sizes per event.
#
# It also settles a structural question that cuts both ways. A team sport is a
# narrow field -- one of twelve teams -- so the per-EVENT chance is good. An
# individual sport is a wide field but offers many events and several entries
# per nation in each. Which wins?

library(data.table)
DATA <- here::here("citiusdata", "data")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
source(here::here("citiusdata", "scripts", "games_reference.R"))

sp   <- as.data.table(readRDS(file.path(DATA, "sport_medal_tables.rds")))
part <- as.data.table(readRDS(file.path(DATA, "sport_participation.rds")))
tot  <- fread(file.path(DATA, "sport_participation_totals.csv"))
sp[, canon := canonical_nation(nation)]
sp[, adj := sport_adjudication(sport)]

# ---------------------------------------------------------------------------
# 0. Which sport-editions have usable participation data
# ---------------------------------------------------------------------------
# A list is usable only if every listed nation carries a count AND the number of
# nations listed matches the infobox. Without both, a share computed from it is
# a share of an unknown denominator.
q <- part[, .(n_listed = .N, n_counted = sum(!is.na(competitors)),
              athletes = sum(competitors, na.rm = TRUE)),
          by = .(games, year, sport)]
q <- merge(q, tot[, .(games, year, sport, infobox_nations)],
           by = c("games", "year", "sport"), all.x = TRUE)
q[, usable := n_counted == n_listed & athletes > 0 &
              !is.na(infobox_nations) & n_listed == infobox_nations]

golds <- sp[, .(golds = sum(gold), medal_nations = uniqueN(canon)),
            by = .(games, year, sport, adj)]
fs <- merge(q[usable == TRUE], golds, by = c("games", "year", "sport"))
cat(sprintf("Usable sport-editions with participation AND medals: %d\n", nrow(fs)))
cat(sprintf("  Commonwealth %d, Summer Olympics %d\n",
            nrow(fs[games == "commonwealth"]), nrow(fs[games == "olympics_summer"])))
cat("  NOTE: coverage is thin and non-random -- Wikipedia lists per-nation\n")
cat("  squad sizes for some sport-editions and not others. Everything below is\n")
cat("  descriptive of the covered set, not a population estimate.\n")

# ---------------------------------------------------------------------------
# 1. What one entry buys, by sport
# ---------------------------------------------------------------------------
cat("\n\n================ WHAT ONE ENTRY BUYS ================\n")
fs[, golds_per_athlete := golds / athletes]
fs[, athletes_per_nation := athletes / n_listed]
fs[, golds_per_nation_entry := golds * athletes_per_nation / athletes]  # = golds / n_listed
fs[, p_gold_per_event_avg_nation := (athletes_per_nation / athletes)]

cat("\nGlasgow 2026, every sport with usable data:\n")
print(fs[games == "commonwealth" & year == 2026][order(-golds_per_athlete),
  .(sport, adj, events = golds, athletes, nations = n_listed,
    sq = round(athletes_per_nation, 1),
    golds_per_athlete = round(golds_per_athlete, 4),
    golds_per_entering_nation = round(golds_per_nation_entry, 3))])

cat("\nPooled across all usable sport-editions, by adjudication class:\n")
print(fs[!is.na(adj), .(
  sport_editions = .N,
  median_events = as.double(median(golds)),
  median_athletes = as.double(median(athletes)),
  median_squad = round(median(athletes_per_nation), 1),
  golds_per_athlete = round(median(golds_per_athlete), 4),
  golds_per_entering_nation = round(median(golds / n_listed), 3)
), by = adj][order(-golds_per_athlete)])

cat("\nThe same, pooled by whether the sport is a TEAM event:\n")
fs[, team_sport := sport %in% c("Netball", "Basketball", "3x3 basketball", "Hockey",
                                "Field hockey", "Football", "Volleyball",
                                "Beach volleyball", "Handball", "Water polo",
                                "Rugby sevens", "Cricket", "Polo", "Baseball",
                                "Softball", "Tug of war", "Lacrosse")]
print(fs[, .(sport_editions = .N,
             median_events = as.double(median(golds)),
             median_athletes = as.double(median(athletes)),
             golds_per_athlete = round(median(golds_per_athlete), 4),
             golds_per_entering_nation = round(median(golds / n_listed), 3)),
         by = .(team_sport)][order(team_sport)])

cat("\n--- The trade-off, stated per event and per Games ---\n")
g26 <- fs[games == "commonwealth" & year == 2026]
for (s in c("Netball", "Swimming", "Athletics", "Artistic gymnastics", "Judo")) {
  r <- g26[sport == s]
  if (!nrow(r)) r <- fs[games == "commonwealth" & year == 2022 & sport == s]
  if (!nrow(r)) next
  cat(sprintf("%-20s %2d event(s), %3d athletes from %2d nations. Average nation:\n",
              s, r$golds, r$athletes, r$n_listed))
  cat(sprintf("%-20s   %.1f%% chance of any given gold, %.2f expected golds overall\n", "",
              100 * r$athletes_per_nation / r$athletes,
              r$golds * r$athletes_per_nation / r$athletes))
}

# ---------------------------------------------------------------------------
# 2. Entry-adjusted dominance
# ---------------------------------------------------------------------------
cat("\n\n================ ENTRY-ADJUSTED DOMINANCE ================\n")
cat("Actual golds against what a nation's SHARE OF ENTRANTS would buy.\n")
cat("A ratio of 1.0 means the nation won exactly its entry share.\n")

pn <- merge(part, fs[, .(games, year, sport, athletes, golds, adj)],
            by = c("games", "year", "sport"))
pn[, entry_share := competitors / athletes]
pn[, expected_golds := golds * entry_share]
pn <- merge(pn, sp[, .(games, year, sport, canon, actual_golds = gold)],
            by = c("games", "year", "sport", "canon"), all.x = TRUE)
pn[is.na(actual_golds), actual_golds := 0L]

cat("\nGlasgow 2026 by sport -- Australia, and the best in each sport:\n")
a26 <- pn[games == "commonwealth" & year == 2026]
print(a26[canon == "Australia"][order(-actual_golds),
  .(sport, adj, entrants = competitors, of = athletes,
    entry_pct = round(100 * entry_share, 1),
    expected = round(expected_golds, 1), actual = actual_golds,
    ratio = round(actual_golds / expected_golds, 1))])

cat("\nGlasgow 2026, biggest over-performance against entry share (>= 3 golds):\n")
print(head(a26[actual_golds >= 3][order(-actual_golds / expected_golds),
  .(nation = canon, sport, entrants = competitors,
    entry_pct = round(100 * entry_share, 1),
    expected = round(expected_golds, 1), actual = actual_golds,
    ratio = round(actual_golds / expected_golds, 1))], 12))

cat("\nAustralia's Glasgow 2026 total, entry-adjusted:\n")
aus <- a26[canon == "Australia"]
cat(sprintf("  entrants   : %d of %d across the %d covered sports (%.1f%%)\n",
            sum(aus$competitors), sum(unique(a26[, .(sport, athletes)])$athletes),
            nrow(aus),
            100 * sum(aus$competitors) / sum(unique(a26[, .(sport, athletes)])$athletes)))
cat(sprintf("  expected golds on entry share : %.1f\n", sum(aus$expected_golds)))
cat(sprintf("  actual golds                  : %d\n", sum(aus$actual_golds)))
cat(sprintf("  ratio                         : %.2fx\n",
            sum(aus$actual_golds) / sum(aus$expected_golds)))

cat("\nAll-time, best entry-adjusted single-sport performances (>= 8 golds, >= 20 nations):\n")
print(head(pn[actual_golds >= 8 & golds >= 8][
  , ratio := actual_golds / expected_golds][order(-ratio),
  .(games, year, sport, nation = canon, entrants = competitors, of = athletes,
    entry_pct = round(100 * entry_share, 1),
    expected = round(expected_golds, 1), actual = actual_golds,
    ratio = round(ratio, 1))], 15))

# ---------------------------------------------------------------------------
# 3. Does hosting buy entry?
# ---------------------------------------------------------------------------
cat("\n\n================ DOES HOSTING BUY ENTRY? ================\n")
cat("If the home gold bump is access rather than performance, the host should\n")
cat("send a bigger SHARE of the entrants at home than at adjacent editions.\n")
cat("This is testable without any medal data at all.\n")

pn[, host := unlist(lapply(paste0(games, "_", year),
       function(k) if (is.null(hosts_map[[k]])) NA_character_ else hosts_map[[k]]))]
pn[, host_nat := host_nation(host)]
hosts <- unique(pn[!is.na(host_nat), .(games, year, host_nat)])

rows <- list()
for (i in seq_len(nrow(hosts))) {
  g <- hosts$games[i]; y <- hosts$year[i]; n <- hosts$host_nat[i]
  eds <- sort(unique(pn[games == g, year])); pos <- match(y, eds)
  if (is.na(pos)) next
  nb <- eds[c(pos - 1, pos + 1)]; nb <- nb[!is.na(nb)]
  if (!length(nb)) next
  # Compare only sports covered at BOTH the host edition and the neighbour.
  for (yy in nb) {
    common <- intersect(pn[games == g & year == y, unique(sport)],
                        pn[games == g & year == yy, unique(sport)])
    if (!length(common)) next
    hs <- pn[games == g & year == y  & sport %in% common & canon == n, sum(competitors)] /
          pn[games == g & year == y  & sport %in% common, sum(competitors)]
    bs <- pn[games == g & year == yy & sport %in% common & canon == n, sum(competitors)] /
          pn[games == g & year == yy & sport %in% common, sum(competitors)]
    if (!is.finite(hs) || !is.finite(bs)) next
    # A zero away share almost always means the nation was ABSENT, not that it
    # entered nobody -- the Soviet Union at Los Angeles 1984, the United States
    # at Moscow 1980, Germany at London 1948. Scoring those as a real zero turns
    # the other side's boycott into a home-entry effect.
    rows[[length(rows) + 1]] <- data.table(
      games = g, host_year = y, nation = n, ref_year = yy,
      sports = length(common),
      entry_share_home = 100 * hs, entry_share_away = 100 * bs,
      away_absent = bs == 0,
      lift_pp = 100 * (hs - bs))
  }
}
ent <- rbindlist(rows)
if (nrow(ent)) {
  band <- function(label, dd) {
    if (nrow(dd) < 5) return(NULL)
    tt <- t.test(dd$entry_share_home, dd$entry_share_away, paired = TRUE)
    wt <- suppressWarnings(wilcox.test(dd$entry_share_home, dd$entry_share_away,
                                       paired = TRUE))
    data.table(subset = label, n = nrow(dd),
               home = round(mean(dd$entry_share_home), 2),
               away = round(mean(dd$entry_share_away), 2),
               lift_pp = round(mean(dd$lift_pp), 2),
               median_lift = round(median(dd$lift_pp), 2),
               ratio = round(mean(dd$entry_share_home) / mean(dd$entry_share_away), 2),
               lo = round(tt$conf.int[1], 2), hi = round(tt$conf.int[2], 2),
               p_t = signif(tt$p.value, 3), p_wilcox = signif(wt$p.value, 3))
  }
  cat(sprintf("\n%d host-edition/neighbour pairs with comparable sports.\n", nrow(ent)))
  cat(sprintf("%d of them have a zero away share (the nation was almost certainly absent).\n",
              sum(ent$away_absent)))
  print(rbindlist(list(
    band("all pairs", ent),
    band("away share > 0 (nation demonstrably present)", ent[away_absent == FALSE]),
    band("present, 1920 onwards", ent[away_absent == FALSE & host_year >= 1920]),
    band("present, 1960 onwards", ent[away_absent == FALSE & host_year >= 1960]),
    band("present, >= 4 comparable sports", ent[away_absent == FALSE & sports >= 4])
  ), fill = TRUE))

  cat("\nLargest lifts among demonstrably-present pairs:\n")
  print(head(ent[away_absent == FALSE][order(-lift_pp),
    .(games, host_year, nation, ref_year, sports,
      home = round(entry_share_home, 2), away = round(entry_share_away, 2),
      lift_pp = round(lift_pp, 2))], 15))
} else {
  cat("\nNot enough overlapping coverage to test this.\n")
}

saveRDS(list(field_structure = fs, per_nation = pn, entry_lift = ent),
        file.path(DATA, "entry_structure_results.rds"))
cat("\nSaved entry_structure_results.rds\n")
