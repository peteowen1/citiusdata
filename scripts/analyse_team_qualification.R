# Do hosts WIN more in team sports at home, or just TURN UP more?
#
# Team sports gave hosts +8.11 pp of their golds at home against +6.56 pp for
# individual sports. Two mechanisms produce that and they mean opposite things:
#
#   QUALIFICATION. Hosts are seeded straight into team tournaments they might
#   never have earned a place in. A nation that is absent away and present at
#   home gains its entire share from access, with nothing to do with playing
#   better.
#
#   PERFORMANCE. Among tournaments the host was in either way, it wins more of
#   them at home.
#
# The share gain decomposes exactly into the two:
#
#   E[share | home] - E[share | away]
#     = P(in | home) * E[share | in, home] - P(in | away) * E[share | in, away]
#
# so the first term can be held fixed to isolate the second.

library(data.table)
DATA <- "C:/dev/citiusverse/citiusdata/data"
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
source("C:/dev/citiusverse/citiusdata/scripts/games_reference.R")

med   <- as.data.table(readRDS(file.path(DATA, "multisport_medal_tables.rds")))
sport <- as.data.table(readRDS(file.path(DATA, "sport_medal_tables_with_podiums.rds")))
tp    <- as.data.table(readRDS(file.path(DATA, "team_participation.rds")))
tmeta <- fread(file.path(DATA, "team_participation_meta.csv"))
med[, canon := canonical_nation(nation)]
sport[, canon := canonical_nation(nation)]

TEAM <- sport_subjectivity()[team_sport == TRUE, sport]
sport[, sport := sport_family(sport)]
tp[, sport := sport_family(sport)]
tmeta[, sport := sport_family(sport)]
TEAM <- unique(sport_family(TEAM))

# Gold share per nation per team sport-edition.
tot <- sport[sport %in% TEAM, .(sport_golds = sum(gold)), by = .(games, year, sport)]
shr <- merge(sport[sport %in% TEAM, .(gold = sum(gold)), by = .(games, year, sport, canon)],
             tot, by = c("games", "year", "sport"))
shr[, share := 100 * gold / sport_golds]

has_list <- unique(tmeta[n_teams > 0, .(games, year, sport)])
has_list[, listed := TRUE]
present  <- unique(med[, .(games, year, canon)]); present[, at_games := TRUE]
in_team  <- unique(tp[, .(games, year, sport, canon)]); in_team[, in_tourn := TRUE]

hosts <- unique(sport[, .(games, year)])
hosts[, host := unlist(lapply(paste0(games, "_", year),
  function(k) if (is.null(hosts_map[[k]])) NA_character_ else hosts_map[[k]]))]
hosts[, host_nat := host_nation(host)]
hosts <- hosts[!is.na(host_nat)]

rows <- list()
for (i in seq_len(nrow(hosts))) {
  g <- hosts$games[i]; y <- hosts$year[i]; n <- hosts$host_nat[i]
  eds <- sort(unique(tot[games == g, year])); pos <- match(y, eds)
  if (is.na(pos)) next
  nb <- eds[c(pos - 1, pos + 1)]; nb <- nb[!is.na(nb)]
  if (!length(nb)) next

  for (s in tot[games == g & year == y, unique(sport)]) {
    # Both the host edition and at least one neighbour must have a team list,
    # otherwise "not in the tournament" cannot be told from "not harvested".
    if (!nrow(has_list[games == g & year == y & sport == s])) next
    nb_ok <- nb[vapply(nb, function(z)
      nrow(has_list[games == g & year == z & sport == s]) > 0, logical(1))]
    # And the nation must have attended those Games at all.
    nb_ok <- nb_ok[vapply(nb_ok, function(z)
      nrow(present[games == g & year == z & canon == n]) > 0, logical(1))]
    if (!length(nb_ok)) next

    in_home <- nrow(in_team[games == g & year == y & sport == s & canon == n]) > 0
    in_away <- vapply(nb_ok, function(z)
      nrow(in_team[games == g & year == z & sport == s & canon == n]) > 0, logical(1))

    sh <- function(z) {
      v <- shr[games == g & year == z & sport == s & canon == n, share]
      if (length(v)) v[1] else 0
    }
    rows[[length(rows) + 1]] <- data.table(
      games = g, year = y, nation = n, sport = s,
      sport_golds = tot[games == g & year == y & sport == s, sport_golds],
      in_home = in_home, frac_in_away = mean(in_away),
      share_home = sh(y), share_away = mean(vapply(nb_ok, sh, numeric(1))),
      n_neighbours = length(nb_ok))
  }
}
d <- rbindlist(rows)
d[, share_gain := share_home - share_away]

cat(sprintf("=== sample ===\n%d host-edition x team-sport rows, %d sports, %d host editions\n",
            nrow(d), uniqueN(d$sport), uniqueN(d[, .(games, year)])))
print(d[, .(n = .N), by = games][order(-n)])

# ---------------------------------------------------------------------------
# 1. Qualification: do hosts turn up more at home?
# ---------------------------------------------------------------------------
cat("\n================ DO HOSTS TURN UP MORE AT HOME? ================\n")
cat(sprintf("\nHost is in the tournament at home ......... %.1f%% of %d\n",
            100 * mean(d$in_home), nrow(d)))
cat(sprintf("Host is in the tournament away ........... %.1f%%\n",
            100 * mean(d$frac_in_away)))
tt <- t.test(as.numeric(d$in_home), d$frac_in_away, paired = TRUE)
cat(sprintf("difference: %+.1f pp (95%% CI %+.1f to %+.1f, p = %.4g)\n",
            100 * mean(d$in_home - d$frac_in_away),
            100 * tt$conf.int[1], 100 * tt$conf.int[2], tt$p.value))

cat("\nby sport:\n")
print(d[, .(n = .N,
            home_pct = round(100 * mean(in_home)),
            away_pct = round(100 * mean(frac_in_away)),
            gap = round(100 * mean(in_home - frac_in_away))),
        by = sport][order(-n)])

# ---------------------------------------------------------------------------
# 2. Split the share gain into access and performance
# ---------------------------------------------------------------------------
cat("\n================ ACCESS vs PERFORMANCE ================\n")
cat(sprintf("\nOverall share gain in team sports: %+.2f pp (%.2f%% home vs %.2f%% away)\n",
            mean(d$share_gain), mean(d$share_home), mean(d$share_away)))

both <- d[in_home == TRUE & frac_in_away == 1]
cat(sprintf("\nRestricted to tournaments the host was in BOTH at home and away (%d rows):\n",
            nrow(both)))
if (nrow(both) >= 10) {
  tt2 <- t.test(both$share_home, both$share_away, paired = TRUE)
  cat(sprintf("  %.2f%% away -> %.2f%% home = %+.2f pp (95%% CI %+.2f to %+.2f, p = %.4g)\n",
              mean(both$share_away), mean(both$share_home), mean(both$share_gain),
              tt2$conf.int[1], tt2$conf.int[2], tt2$p.value))
  cat(sprintf("  That is %.0f%% of the unconditional %+.2f pp.\n",
              100 * mean(both$share_gain) / mean(d$share_gain), mean(d$share_gain)))
}

gained <- d[in_home == TRUE & frac_in_away < 1]
cat(sprintf("\nTournaments the host entered at home but not (always) away: %d rows\n",
            nrow(gained)))
if (nrow(gained)) {
  cat(sprintf("  their mean share gain: %+.2f pp\n", mean(gained$share_gain)))
  cat(sprintf("  they are %.0f%% of rows and carry %.0f%% of the total gain\n",
              100 * nrow(gained) / nrow(d),
              100 * sum(gained$share_gain) / sum(d$share_gain)))
}

cat("\n--- the decomposition ---\n")
p_home <- mean(d$in_home); p_away <- mean(d$frac_in_away)
s_home <- d[in_home == TRUE, mean(share_home)]
s_away <- d[frac_in_away > 0, sum(share_away * frac_in_away) / sum(frac_in_away)]
cat(sprintf("  P(in tournament)  home %.3f  away %.3f\n", p_home, p_away))
cat(sprintf("  share when in     home %.2f%%  away %.2f%%\n", s_home, s_away))
access_term <- (p_home - p_away) * s_away
perf_term   <- p_home * (s_home - s_away)
cat(sprintf("  access  (turning up more)  : %+.2f pp\n", access_term))
cat(sprintf("  performance (winning more) : %+.2f pp\n", perf_term))
cat(sprintf("  sum                        : %+.2f pp  (observed %+.2f pp)\n",
            access_term + perf_term, mean(d$share_gain)))

# ---------------------------------------------------------------------------
# 3. Compare with individual sports on the same footing
# ---------------------------------------------------------------------------
cat("\n================ AGAINST INDIVIDUAL SPORTS ================\n")
panel <- as.data.table(readRDS(file.path(DATA, "host_sport_panel.rds")))
iv <- panel[team_sport == FALSE]
cat(sprintf("individual sports        : %+.2f pp (n = %d)\n",
            mean(iv$effect_pp), nrow(iv)))
cat(sprintf("team sports, all         : %+.2f pp (n = %d)\n",
            mean(d$share_gain), nrow(d)))
if (nrow(both) >= 10) {
  cat(sprintf("team sports, host in both: %+.2f pp (n = %d)\n",
              mean(both$share_gain), nrow(both)))
  tt3 <- t.test(both$share_gain, iv$effect_pp)
  cat(sprintf("  team(in both) minus individual: %+.2f pp (95%% CI %+.2f to %+.2f, p = %.4g)\n",
              mean(both$share_gain) - mean(iv$effect_pp),
              tt3$conf.int[1], tt3$conf.int[2], tt3$p.value))
}

saveRDS(d, file.path(DATA, "team_qualification_panel.rds"))
cat("\nSaved team_qualification_panel.rds\n")
