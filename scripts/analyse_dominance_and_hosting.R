# Dominance rankings and the host effect on measured vs judged events.
#
# Two questions:
#   1. Which single-edition national performances are the most dominant?
#   2. Does hosting help more in judged events than in measured ones?
#
# Question 2 is the one that can go wrong quietly, so the design is spelled out
# here rather than in a comment next to the regression:
#
# A host nation's medal haul at home cannot be compared to the all-time average
# of every other nation -- nations host when they are rich and strong, and
# strength trends over decades. The comparison used is WITHIN NATION AND WITHIN
# SERIES: a host edition against that same nation's share at the editions
# immediately before and after. That differences out both the nation's standing
# level and any local trend through the host year.
#
# Shares, never counts: programme sizes have quadrupled, so a count comparison
# measures the calendar.
#
# The estimand is not the home advantage itself -- it is the DIFFERENCE between
# the judged and measured home advantages. A home advantage common to both
# (bigger team, home crowd, no travel, home training) is real but says nothing
# about officials. Only the gap between the two does.

library(data.table)

DATA <- here::here("citiusdata", "data")
source(here::here("citiusdata", "scripts", "games_reference.R"))
# canonical_nation(), nation_iso3(), sport_adjudication() and sport_family()
# come from the package, not from a copy in this directory -- a second copy
# masks the package's and the two drift apart silently.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))

med   <- as.data.table(readRDS(file.path(DATA, "multisport_medal_tables.rds")))
SPORT_FILE <- if (file.exists(file.path(DATA, "sport_medal_tables_with_podiums.rds"))) {
  "sport_medal_tables_with_podiums.rds"
} else "sport_medal_tables.rds"
sport <- as.data.table(readRDS(file.path(DATA, SPORT_FILE)))
econ  <- as.data.table(readRDS(file.path(DATA, "country_economic_history.rds")))

med[,   canon := canonical_nation(nation)]
sport[, canon := canonical_nation(nation)]

# ---------------------------------------------------------------------------
# 0. Cross-checks. Nothing below is trustworthy if these fail.
# ---------------------------------------------------------------------------
cat("================ CROSS-CHECKS ================\n")

dupe <- med[, .N, by = .(games, year, canon)][N > 1]
cat("A. one row per nation per edition ..... ", if (!nrow(dupe)) "PASS" else "FAIL", "\n", sep = "")
if (nrow(dupe)) print(dupe)

shr <- med[, .(s = sum(gold) / total_golds_in_games[1]), by = .(games, year)]
cat("B. gold shares sum to 1 per edition ... ",
    sprintf("%d/%d editions", sum(abs(shr$s - 1) < 1e-9), nrow(shr)), "\n", sep = "")
print(shr[abs(s - 1) > 1e-9])

# Do the sport-level tables reproduce the overall medal table? This is the
# check that decides whether the sport split can carry any weight at all.
sp_tot <- sport[, .(sport_golds = sum(gold)), by = .(games, year)]
ov_tot <- unique(med[, .(games, year, overall_golds = total_golds_in_games)])
recon  <- merge(sp_tot, ov_tot, by = c("games", "year"))
recon[, pct := round(100 * sport_golds / overall_golds, 1)]
cat("\nC. sport-level coverage of each edition's golds:\n")
print(recon[order(games, year)][, .(games, year, sport_golds, overall_golds, pct)])

cat("\nD. sports with no adjudication class assigned:\n")
sport[, adj := sport_adjudication(sport)]
unc <- sport[is.na(adj), .(golds = sum(gold), editions = uniqueN(paste(games, year))),
             by = sport][order(-golds)]
if (nrow(unc)) print(unc) else cat("   none -- every sport classified\n")

# Coverage is measured against the edition's OFFICIAL gold total, not against
# what we happened to harvest. Measuring against the harvest would score an
# edition 100% covered while a whole sport was missing from it.
#
# Two distinct gaps sit behind this, both real and neither a parser bug:
#
#  - "Aquatics" is the combined article Wikipedia uses for older editions with
#    no separate swimming page. It pools swimming (measured) with diving and
#    synchronised swimming (judged) into one per-nation total and cannot be
#    split, so those editions carry unclassifiable golds.
#  - Team sports often have no NOC medal table at all -- netball, rugby sevens,
#    hockey, basketball, polo and tug of war list a podium, not counts -- and
#    Gymnastics at the 2006 Commonwealth Games has no medal table on Wikipedia
#    in any form, only per-apparatus results.
#
# Because class shares are computed WITHIN a class, a missing sport distorts
# only its own class. The team-sport gap therefore lands almost entirely on
# "opponent", which is why the headline comparison below is measured vs judged
# and the opponent figures are reported but not leaned on.
MIN_CLASS_COVERAGE <- 0.95
official <- unique(med[, .(games, year, official = total_golds_in_games)])
ed_cov <- sport[, .(classified = sum(gold[!is.na(adj)]), harvested = sum(gold)),
                by = .(games, year)]
ed_cov <- merge(ed_cov, official, by = c("games", "year"))
ed_cov[, coverage := classified / official]
usable <- ed_cov[coverage >= MIN_CLASS_COVERAGE, .(games, year)]
cat(sprintf("\nE. editions with >=%.0f%% of OFFICIAL golds classified: %d/%d\n",
            100 * MIN_CLASS_COVERAGE, nrow(usable), nrow(ed_cov)))
cat("   excluded:\n")
print(ed_cov[coverage < MIN_CLASS_COVERAGE][order(coverage)][,
      .(games, year, classified, official, coverage = round(coverage, 3))])
sport_all <- copy(sport)
sport <- merge(sport, usable, by = c("games", "year"))

# ---------------------------------------------------------------------------
# 1. Most dominant single-edition performances
# ---------------------------------------------------------------------------
cat("\n\n================ DOMINANCE ================\n")

med[, comp_nations := as.numeric(competing_nations)]
med[, gold_pct := 100 * gold / total_golds_in_games]
med[, exp_pct  := 100 / comp_nations]
med[, multiple := gold_pct / exp_pct]

# Logit shift: how far the nation's gold probability sits above the uniform
# expectation, on a scale that does not saturate near 100%.
lg <- function(p) log(p / (1 - p))
med[, p_act := pmin(pmax(gold / total_golds_in_games, 1e-6), 1 - 1e-6)]
med[, p_exp := 1 / comp_nations]
med[, logit_shift := lg(p_act) - lg(p_exp)]

MIN_GOLDS <- 5L
med[, boycott := is_boycott_edition(games, year)]

# --- anchor checks, written before looking at any ranking -------------------
# Facts that must hold whatever the method does. If one fails the method is
# wrong; it is not an exception to explain.
cat("\n--- anchor checks ---\n")
anchor <- function(label, ok) {
  cat(sprintf("  %-58s %s\n", label, if (isTRUE(ok)) "PASS" else "FAIL"))
  isTRUE(ok)
}
a1 <- anchor("Australia tops Commonwealth 2026",
             med[games == "commonwealth" & year == 2026][order(-gold)][1, canon] == "Australia")
a2 <- anchor("St Louis 1904 is the most lopsided Summer Olympics",
             med[games == "olympics_summer"][order(-gold_pct)][1, canon] == "United States" &&
             med[games == "olympics_summer"][order(-gold_pct)][1, year] == 1904)
a3 <- anchor("USSR 1980 is top-5 of Summer Olympics gold share",
             which(med[games == "olympics_summer"][order(-gold_pct), canon] == "Soviet Union" &
                   med[games == "olympics_summer"][order(-gold_pct), year] == 1980)[1] <= 5)
a4 <- anchor("China tops the 2022 Asian Games",
             med[games == "asian_games" & year == 2022][order(-gold)][1, canon] == "China")
# The USA has topped every Pan American Games since 1955 with one exception:
# Havana 1991, where host Cuba won 140 golds to the USA's 130. Encoding the
# exception rather than the rule -- the first draft of this anchor asserted the
# rule, failed, and was right to.
pa_win <- med[games == "panam_games" & year >= 1955][order(year, -gold, -silver)][,
              .SD[1], by = year]
a5 <- anchor("USA tops every Pan Am since 1955 except Cuba at home in 1991",
             all(pa_win[year != 1991, canon] == "United States") &&
             pa_win[year == 1991, canon] == "Cuba")
a6 <- anchor("every nation's gold share is between 0 and 1",
             med[, all(gold_share >= 0 & gold_share <= 1)])
if (!all(a1, a2, a3, a4, a5, a6)) {
  cli::cli_abort("An anchor check failed. The method is wrong; do not read the rankings below.")
}

cat("\nGold share is the primary measure: it is the fraction of the golds\n")
cat("actually on offer that one nation took, which is what dominance means.\n")
cat("The field-size multiple below answers a DIFFERENT question -- how far\n")
cat("above a uniform-random field -- and it systematically favours large\n")
cat("fields, because a 206-nation uniform baseline is 0.49%. Read it as\n")
cat("'unlikely given the number of competitors', not as 'more dominant'.\n")
cat("[B] marks an edition thinned by a boycott.\n")

cat("\n--- Top 20 by raw gold share (all series pooled) ---\n")
print(head(med[gold >= MIN_GOLDS][order(-gold_pct),
    .(games, year, nation = canon, host = host_nation(host), golds = gold,
      of = total_golds_in_games, gold_pct = round(gold_pct, 1),
      nations = comp_nations, B = fifelse(boycott, "[B]", ""))], 20))

cat("\n--- Top 20 by raw gold share, boycott-affected editions removed ---\n")
print(head(med[gold >= MIN_GOLDS & !boycott][order(-gold_pct),
    .(games, year, nation = canon, host = host_nation(host), golds = gold,
      of = total_golds_in_games, gold_pct = round(gold_pct, 1),
      nations = comp_nations)], 20))

cat("\n--- Top 20 by field-size-adjusted multiple (share / uniform share) ---\n")
print(head(med[gold >= MIN_GOLDS][order(-multiple),
    .(games, year, nation = canon, golds = gold, of = total_golds_in_games,
      gold_pct = round(gold_pct, 1), nations = comp_nations,
      multiple = round(multiple, 1), B = fifelse(boycott, "[B]", ""))], 20))

cat("\n--- Most dominant performance in each series (by gold share) ---\n")
print(med[gold >= MIN_GOLDS][order(games, -gold_pct)][, .SD[1], by = games][,
    .(games, year, nation = canon, golds = gold, of = total_golds_in_games,
      gold_pct = round(gold_pct, 1), nations = comp_nations)])

cat("\n--- Commonwealth Games: top 15 editions by gold share ---\n")
print(head(med[games == "commonwealth" & gold >= MIN_GOLDS][order(-gold_pct),
    .(year, nation = canon, host = host_nation(host), golds = gold,
      of = total_golds_in_games, gold_pct = round(gold_pct, 1),
      nations = comp_nations)], 15))

cat("\n--- Australia at every Commonwealth Games ---\n")
print(med[games == "commonwealth" & canon == "Australia"][order(year),
    .(year, host = host_nation(host), golds = gold, of = total_golds_in_games,
      gold_pct = round(gold_pct, 1), medals = total,
      medal_pct = round(100 * total / total_medals_in_games, 1),
      home = host_nation(host) == "Australia")])

# ---------------------------------------------------------------------------
# 2. Host effect, measured vs judged
# ---------------------------------------------------------------------------
cat("\n\n================ HOST EFFECT ================\n")

sport[, host := unlist(lapply(paste0(games, "_", year),
                              function(k) if (is.null(hosts_map[[k]])) NA_character_ else hosts_map[[k]]))]
sport[, host_nat := host_nation(host)]

#' Share of the class's golds taken by each nation in each edition.
#'
#' The class column is renamed up front rather than reached with `get()` inside
#' `[`, which breaks data.table's fast column path.
class_share <- function(dt, class_col = "adj") {
  d <- dt[!is.na(dt[[class_col]])]
  d[, cls := d[[class_col]]]
  tot  <- d[, .(class_golds = sum(gold)), by = .(games, year, cls)]
  by_n <- d[, .(golds = sum(gold)), by = .(games, year, cls, canon)]
  m <- merge(by_n, tot, by = c("games", "year", "cls"))
  m[, share := 100 * golds / class_golds]
  m[]
}

# Which nations were actually present at each edition. Needed because a nation
# missing from a neighbour edition has two very different explanations, and
# they pull the estimate opposite ways: it competed and won nothing (a real
# zero), or it did not compete at all (no information).
#
# This matters more than it sounds. The Soviet Union hosted Moscow 1980 and
# then boycotted Los Angeles 1984; the United States boycotted 1980 and hosted
# 1984. Scoring the absent neighbour as a zero would credit each with a colossal
# home advantage that is entirely the other side's boycott.
present <- unique(med[, .(games, year, canon, present = TRUE)])

#' Host effect: host-edition share minus the mean of the adjacent editions.
#'
#' Adjacent means the nation's share at the previous and next edition of the
#' same series, which differences out both the nation's standing level and any
#' local trend running through the host year. A neighbour counts only if the
#' nation appears in that edition's medal table; otherwise it is dropped as
#' unknown rather than read as zero.
#'
#' Limitation, stated rather than hidden: appearing in a medal table proves
#' participation, but NOT appearing does not prove absence — a nation that
#' competed and won nothing looks identical to one that stayed home. The rule
#' is conservative for the host nations studied here, which are almost always
#' medal-winners, and it is the boycott cases it is there to protect.
host_effect <- function(shares, sport_dt, min_class_golds = 8L) {
  hosts <- unique(sport_dt[!is.na(host_nat), .(games, year, host_nat)])
  out <- list()
  for (i in seq_len(nrow(hosts))) {
    g <- hosts$games[i]; y <- hosts$year[i]; n <- hosts$host_nat[i]
    eds <- sort(unique(shares[games == g, year]))
    pos <- match(y, eds)
    if (is.na(pos)) next
    nb <- eds[c(pos - 1, pos + 1)]
    nb <- nb[!is.na(nb)]
    if (!length(nb)) next

    for (cl in unique(shares$cls)) {
      tot_at_host <- shares[games == g & year == y & cls == cl, class_golds][1]
      if (is.na(tot_at_host) || tot_at_host < min_class_golds) next

      share_at <- function(yy) {
        if (!nrow(shares[games == g & year == yy & cls == cl])) return(NA_real_)
        if (!nrow(present[games == g & year == yy & canon == n])) return(NA_real_)
        v <- shares[games == g & year == yy & cls == cl & canon == n, share]
        if (length(v)) v[1] else 0     # present, but won nothing in this class
      }

      h  <- share_at(y)
      bs <- vapply(nb, share_at, numeric(1))
      kept <- nb[!is.na(bs)]; bs <- bs[!is.na(bs)]
      if (is.na(h) || !length(bs)) next

      out[[length(out) + 1]] <- data.table(
        games = g, year = y, nation = n, cls = cl,
        host_share = h, base_share = mean(bs),
        n_neighbours = length(bs),
        neighbours = paste(kept, collapse = "/"),
        class_golds = tot_at_host, effect_pp = h - mean(bs)
      )
    }
  }
  rbindlist(out)
}

shares <- class_share(sport)
he <- host_effect(shares, sport)

cat("\n--- Host effect by adjudication class (percentage points of class golds) ---\n")
summ <- he[, .(host_editions = .N,
               mean_effect_pp = round(mean(effect_pp), 2),
               median_effect_pp = round(median(effect_pp), 2),
               se = round(sd(effect_pp) / sqrt(.N), 2),
               mean_base = round(mean(base_share), 2),
               mean_host = round(mean(host_share), 2)), by = cls]
summ[, t := round(mean_effect_pp / se, 2)]
print(summ[order(-mean_effect_pp)])

cat("\n--- The estimand: judged effect MINUS measured effect, paired by host edition ---\n")
w <- dcast(he, games + year + nation ~ cls, value.var = "effect_pp")
if (all(c("judged", "measured") %in% names(w))) {
  p <- w[!is.na(judged) & !is.na(measured)]
  p[, diff := judged - measured]
  tt <- t.test(p$judged, p$measured, paired = TRUE)
  cat(sprintf("paired host editions: %d\n", nrow(p)))
  cat(sprintf("mean judged effect  : %+.2f pp\n", mean(p$judged)))
  cat(sprintf("mean measured effect: %+.2f pp\n", mean(p$measured)))
  cat(sprintf("difference          : %+.2f pp  (95%% CI %+.2f to %+.2f, p = %.4f)\n",
              mean(p$diff), tt$conf.int[1], tt$conf.int[2], tt$p.value))
  # A percentage-point gain means more where the baseline share is small, so
  # report the ratio alongside it. Both are computed on the SAME paired set as
  # the test above, not on the full unpaired table.
  cat("\nBaseline -> host, on the paired set:\n")
  pl <- melt(p, id.vars = c("games", "year", "nation"),
             measure.vars = c("judged", "measured"),
             variable.name = "cls", value.name = "effect_pp")
  base_host <- he[cls %in% c("judged", "measured")]
  base_host <- merge(base_host, p[, .(games, year, nation)],
                     by = c("games", "year", "nation"))
  print(base_host[, .(base = round(mean(base_share), 2),
                      host = round(mean(host_share), 2),
                      ratio = round(mean(host_share) / mean(base_share), 2)),
                  by = cls])
  cat("\nPer host edition, largest judged-minus-measured gaps:\n")
  print(head(p[order(-diff), .(games, year, nation,
                               judged = round(judged, 1),
                               measured = round(measured, 1),
                               diff = round(diff, 1))], 12))
}

cat("\n--- Same split by series ---\n")
print(he[, .(n = .N, mean_pp = round(mean(effect_pp), 2)), by = .(games, cls)][order(games, cls)])

# ---------------------------------------------------------------------------
# 3. Sensitivity: the two sports whose class is genuinely arguable
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 2b. Is it judging, or is it entry?
# ---------------------------------------------------------------------------
cat("\n\n================ WHAT ELSE COULD CAUSE THIS ================\n")
cat("A judged-minus-measured gap is consistent with officials favouring the\n")
cat("host, but it is equally consistent with ACCESS: hosts get automatic entry\n")
cat("and full team quotas, and that is worth far more in a sport where entry is\n")
cat("restricted than in athletics, where anyone with the standard runs. Three\n")
cat("checks that pull the two apart.\n")

cat("\n(i) Class size. A judged class has far fewer golds, so one extra gold\n")
cat("    moves its share much further. That inflates VARIANCE, not the mean --\n")
cat("    but it is why the judged estimate has the wider interval.\n")
csz <- sport[, .(golds = sum(gold)), by = .(games, year, adj)][!is.na(adj)]
print(csz[, .(median_golds_in_class = median(golds),
              pp_per_gold = round(100 / median(golds), 2)), by = adj][order(adj)])

cat("\n(i-b) And size is not only a variance story. Regress the class effect on\n")
cat("      the class's gold count directly: if smaller classes gain MORE on\n")
cat("      average, the judged-vs-measured gap may be size, not judging --\n")
cat("      because judged classes are the small ones.\n")
he_sz <- he[!is.na(effect_pp) & class_golds > 0]
fit_sz <- lm(effect_pp ~ log(class_golds), data = he_sz)
print(round(summary(fit_sz)$coefficients, 3))
cat(sprintf("\n  median class golds: judged %d, opponent %d, measured %d\n",
            as.integer(median(he[cls == "judged", class_golds])),
            as.integer(median(he[cls == "opponent", class_golds])),
            as.integer(median(he[cls == "measured", class_golds]))))
cat("  So a judged-vs-measured comparison is also a small-vs-large comparison.\n")
cat("  The sport-level regression in analyse_host_regression.R is the proper\n")
cat("  test, because there size enters as a continuous control.\n")

cat("\n(ii) If it were purely entry quotas, the most restricted-entry class --\n")
cat("     team sports, where a host qualifies automatically for a place it\n")
cat("     might never earn -- should show the largest effect, not judged.\n")
print(he[, .(host_editions = .N, mean_pp = round(mean(effect_pp), 2)),
         by = .(games, cls)][order(games, -mean_pp)])

cat("\n(iii) Timing. A pure home-ground effect is symmetric: the host year is\n")
cat("      above BOTH neighbours by similar amounts. A funding ramp is not --\n")
cat("      the edition after hosting stays elevated because the athletes are\n")
cat("      still there.\n")
ramp <- list()
for (cl in c("measured", "judged", "opponent")) {
  hs <- unique(sport[!is.na(host_nat), .(games, year, host_nat)])
  for (i in seq_len(nrow(hs))) {
    g <- hs$games[i]; y <- hs$year[i]; n <- hs$host_nat[i]
    eds <- sort(unique(shares[games == g, year])); pos <- match(y, eds)
    if (is.na(pos) || pos < 2 || pos >= length(eds)) next
    gv <- function(yy) {
      if (!nrow(present[games == g & year == yy & canon == n])) return(NA_real_)
      v <- shares[games == g & year == yy & cls == cl & canon == n, share]
      if (!nrow(shares[games == g & year == yy & cls == cl])) return(NA_real_)
      if (length(v)) v[1] else 0
    }
    prev <- gv(eds[pos - 1]); host <- gv(y); nxt <- gv(eds[pos + 1])
    if (anyNA(c(prev, host, nxt))) next
    ramp[[length(ramp) + 1]] <- data.table(cls = cl, games = g, year = y,
                                           nation = n, prev, host, nxt)
  }
}
ramp <- rbindlist(ramp)
if (nrow(ramp)) {
  print(ramp[, .(n = .N,
                 before = round(mean(prev), 2),
                 at_home = round(mean(host), 2),
                 after = round(mean(nxt), 2),
                 gain_vs_before = round(mean(host - prev), 2),
                 retained_after = round(mean(nxt - prev), 2)), by = cls][order(cls)])
  cat("\n  'retained_after' is the edition-after share minus the edition-before\n")
  cat("  share. If it is near zero the bump is the Games itself; if it is a\n")
  cat("  large fraction of the home gain, the nation simply got better.\n")
}

cat("\n(iv) Robustness of the headline to the editions most likely to distort it.\n")
subset_test <- function(label, keep) {
  # `he[which(keep)]`, not `he[keep]`: a bare symbol in `i` is resolved against
  # the table's own columns first, so the day `he` gains a column called `keep`
  # this would silently filter on that instead of the argument.
  h <- he[which(keep)]
  w <- dcast(h, games + year + nation ~ cls, value.var = "effect_pp")
  if (!all(c("judged", "measured") %in% names(w))) return(NULL)
  pp <- w[!is.na(judged) & !is.na(measured)]
  if (nrow(pp) < 5) return(NULL)
  tt <- t.test(pp$judged, pp$measured, paired = TRUE)
  wt <- suppressWarnings(wilcox.test(pp$judged, pp$measured, paired = TRUE))
  data.table(subset = label, n = nrow(pp),
             diff = round(mean(pp$judged - pp$measured), 2),
             median_diff = round(median(pp$judged - pp$measured), 2),
             lo = round(tt$conf.int[1], 2), hi = round(tt$conf.int[2], 2),
             p_t = signif(tt$p.value, 3), p_wilcox = signif(wt$p.value, 3))
}
he[, boycott := is_boycott_edition(games, year)]
print(rbindlist(list(
  subset_test("all host editions", rep(TRUE, nrow(he))),
  subset_test("excluding boycott editions", !he$boycott),
  subset_test("1920 onwards", he$year >= 1920),
  subset_test("1960 onwards", he$year >= 1960),
  subset_test("Commonwealth only", he$games == "commonwealth"),
  subset_test("Summer Olympics only", he$games == "olympics_summer")
), fill = TRUE))

cat("\n\n================ SENSITIVITY ================\n")
cat("Every boundary in the taxonomy that a reasonable person could draw\n")
cat("elsewhere is moved, one at a time, and the headline re-estimated. If the\n")
cat("sign survives all of them it is a finding; if it flips, it was a\n")
cat("restatement of one line in a lookup table.\n\n")

run_variant <- function(label, mutate_fn) {
  s <- copy(sport)
  s[, adj_v := adj]
  mutate_fn(s)
  h <- host_effect(class_share(s, "adj_v"), s)
  w <- dcast(h, games + year + nation ~ cls, value.var = "effect_pp")
  if (!all(c("judged", "measured") %in% names(w))) return(NULL)
  pp <- w[!is.na(judged) & !is.na(measured)]
  if (nrow(pp) < 3) return(NULL)
  tt <- t.test(pp$judged, pp$measured, paired = TRUE)
  data.table(variant = label, n = nrow(pp),
             judged = round(mean(pp$judged), 2),
             measured = round(mean(pp$measured), 2),
             diff = round(mean(pp$judged - pp$measured), 2),
             lo = round(tt$conf.int[1], 2), hi = round(tt$conf.int[2], 2),
             p = signif(tt$p.value, 3))
}

variants <- rbindlist(list(
  run_variant("baseline", function(s) invisible(NULL)),
  run_variant("boxing -> opponent, judo -> judged", function(s) {
    s[sport == "Boxing", adj_v := "opponent"]
    s[sport == "Judo",   adj_v := "judged"]
  }),
  run_variant("equestrian -> measured", function(s) {
    s[sport %in% c("Equestrian events", "Equestrian"), adj_v := "measured"]
  }),
  run_variant("equestrian dropped", function(s) {
    s[sport %in% c("Equestrian events", "Equestrian"), adj_v := NA_character_]
  }),
  run_variant("boxing dropped", function(s) {
    s[sport == "Boxing", adj_v := NA_character_]
  }),
  run_variant("gymnastics dropped", function(s) {
    s[sport %in% c("Gymnastics", "Artistic gymnastics", "Rhythmic gymnastics"),
      adj_v := NA_character_]
  })
), fill = TRUE)
print(variants)

he2 <- host_effect(class_share(
  { s <- copy(sport); s[, adj_v := adj]
    s[sport == "Boxing", adj_v := "opponent"]; s[sport == "Judo", adj_v := "judged"]; s },
  "adj_v"), sport)

# ---------------------------------------------------------------------------
# 4. Glasgow 2026 by class
# ---------------------------------------------------------------------------
cat("\n\n================ GLASGOW 2026 BY CLASS ================\n")
g26 <- sport[games == "commonwealth" & year == 2026]
if (nrow(g26)) {
  cat("\nGolds available per class:\n")
  print(g26[, .(golds = sum(gold), sports = uniqueN(sport)), by = adj][order(-golds)])
  cat("\nTop nations by class:\n")
  tot <- g26[, .(cg = sum(gold)), by = adj]
  bn  <- g26[, .(golds = sum(gold)), by = .(adj, canon)]
  bn  <- merge(bn, tot, by = "adj")[, share := round(100 * golds / cg, 1)]
  for (a in c("measured", "judged", "opponent")) {
    cat("\n", a, ":\n", sep = "")
    print(head(bn[adj == a][order(-golds), .(nation = canon, golds, share)], 8))
  }
  cat("\nAustralia and Scotland (the host) by sport:\n")
  print(g26[canon %in% c("Australia", "Scotland")][order(canon, -gold),
            .(nation = canon, sport, adj, gold, silver, bronze)])
}

# ---------------------------------------------------------------------------
# 5. Why Australia's 2026 number is what it is: programme mix vs performance
# ---------------------------------------------------------------------------
# A nation's overall gold share is the sum over sports of
#   (that sport's share of the programme) x (the nation's share within it).
# So a share can move because the nation got better, or because the programme
# changed shape underneath it. Glasgow 2026 cut the programme from 280 golds to
# 216 and dropped whole sports, so the two have to be separated before any
# claim about Australia "improving" is made.
cat("\n\n================ PROGRAMME MIX vs PERFORMANCE ================\n")
sm <- sport_all[games == "commonwealth" & year %in% c(2022, 2026)]
# Group to families first. Without this, "Cycling" (2022) and "Track cycling"
# (2026) look like one sport dropped and another added, and the decomposition
# blames the programme for a renaming.
sm[, sport := sport_family(sport)]
tot_y <- sm[, .(prog = sum(gold)), by = year]
w <- merge(sm[, .(sport_golds = sum(gold)), by = .(year, sport)], tot_y, by = "year")
w[, weight := sport_golds / prog]
aus <- sm[canon == "Australia", .(aus_gold = sum(gold)), by = .(year, sport)]
stopifnot(!anyDuplicated(w[, .(year, sport)]), !anyDuplicated(aus[, .(year, sport)]))
mix <- merge(w, aus, by = c("year", "sport"), all.x = TRUE)
mix[is.na(aus_gold), aus_gold := 0]
mix[, rate := aus_gold / sport_golds]

cat("\nAustralia by sport, 2022 vs 2026:\n")
cmp <- dcast(mix, sport ~ year, value.var = c("sport_golds", "weight", "aus_gold", "rate"))
setnames(cmp, gsub("_20", "_", names(cmp)))
print(cmp[order(-weight_26, na.last = TRUE)][, .(
  sport,
  golds_22 = sport_golds_22, share_of_prog_22 = round(100 * weight_22, 1),
  aus_22 = aus_gold_22, aus_rate_22 = round(100 * rate_22, 1),
  golds_26 = sport_golds_26, share_of_prog_26 = round(100 * weight_26, 1),
  aus_26 = aus_gold_26, aus_rate_26 = round(100 * rate_26, 1))])

w22 <- mix[year == 2022]; w26 <- mix[year == 2026]
common <- intersect(w22$sport, w26$sport)
actual22 <- sum(w22$weight * w22$rate)
actual26 <- sum(w26$weight * w26$rate)
# Shift-share on the sports present in both years, renormalised so the weights
# in each counterfactual sum to one.
c22 <- w22[sport %in% common]; c26 <- w26[sport %in% common]
c22[, wn := weight / sum(weight)]; c26[, wn := weight / sum(weight)]
setkey(c22, sport); setkey(c26, sport)
base_22 <- sum(c22$wn * c22$rate)
mix_only <- sum(c26$wn * c22[c26$sport, rate])   # 2026 programme, 2022 form
perf_only <- sum(c22$wn * c26[c22$sport, rate])  # 2022 programme, 2026 form
both <- sum(c26$wn * c26$rate)

cat(sprintf("\nAustralia's actual gold share: 2022 %.1f%%  ->  2026 %.1f%%\n",
            100 * actual22, 100 * actual26))
cat("\nOn the sports contested in BOTH years, renormalised:\n")
cat(sprintf("  2022 programme, 2022 form ......................... %.1f%%\n", 100 * base_22))
cat(sprintf("  2026 programme, 2022 form  (mix effect alone) ..... %.1f%%  (%+.1f pp)\n",
            100 * mix_only, 100 * (mix_only - base_22)))
cat(sprintf("  2022 programme, 2026 form  (form effect alone) .... %.1f%%  (%+.1f pp)\n",
            100 * perf_only, 100 * (perf_only - base_22)))
cat(sprintf("  2026 programme, 2026 form ......................... %.1f%%  (%+.1f pp)\n",
            100 * both, 100 * (both - base_22)))
cat("\nSports dropped from the programme between 2022 and 2026, and what\n")
cat("Australia had been taking from them:\n")
dropped <- w22[!sport %in% common][order(-aus_gold)]
print(dropped[, .(sport, golds_2022 = sport_golds, aus_2022 = aus_gold,
                  aus_rate = round(100 * rate, 1))])
cat(sprintf("\nDropped sports were %d of the 2022 programme's %d golds (%.0f%%);\n",
            sum(dropped$sport_golds), tot_y[year == 2022, prog],
            100 * sum(dropped$sport_golds) / tot_y[year == 2022, prog]))
cat(sprintf("Australia had taken %d of them (%.0f%%), against its %.0f%% overall.\n",
            sum(dropped$aus_gold),
            100 * sum(dropped$aus_gold) / sum(dropped$sport_golds),
            100 * actual22))

saveRDS(list(host_effect = he, host_effect_swapped = he2, shares = shares,
             variants = variants, mix = mix),
        file.path(DATA, "host_effect_results.rds"))
cat("\nSaved host_effect_results.rds\n")
