# How many golds does hosting actually buy, and where do they come from?
#
# Everything so far has been in percentage points of a sport's golds, which is
# the right unit for a regression and a poor one for understanding. This
# converts the whole picture to EXPECTED GOLDS: what an average host would have
# won had it not been hosting, what it actually won, and how the difference
# splits by every factor measured.
#
# The counterfactual is the same within-nation one used throughout: the host's
# share of each sport's golds at the editions immediately before and after,
# applied to the host edition's own programme. So "golds without hosting" means
# "the golds its away-form share of this exact programme would have produced",
# not "the golds it won last time" -- programme sizes change.
#
#   expected_without = sum over sports of  share_away * golds_in_sport
#   actual           = sum over sports of  share_home * golds_in_sport
#   host advantage   = actual - expected_without
#
# Every split below partitions that same difference, so the parts always add
# back to the whole. Where a factor cannot be measured for some sports, the
# unattributed remainder is reported rather than spread around.

library(data.table)
DATA <- "C:/dev/citiusverse/citiusdata/data"
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
source("C:/dev/citiusverse/citiusdata/scripts/games_reference.R")

panel <- as.data.table(readRDS(file.path(DATA, "host_sport_panel.rds")))
tq    <- as.data.table(readRDS(file.path(DATA, "team_qualification_panel.rds")))
med   <- as.data.table(readRDS(file.path(DATA, "multisport_medal_tables.rds")))
med[, canon := canonical_nation(nation)]

# Golds, not points. share_* are percentages of that sport's golds.
panel[, golds_actual   := host_share / 100 * sport_golds]
panel[, golds_expected := base_share / 100 * sport_golds]
panel[, golds_gained   := golds_actual - golds_expected]

n_ed <- uniqueN(panel[, .(games, year)])
cat("================ THE HEADLINE, IN GOLDS ================\n")
cat(sprintf("\n%d host editions, %d host-edition x sport rows, %d sports, five series.\n",
            n_ed, nrow(panel), uniqueN(panel$sport)))
cat("Covers only sports with sport-level medal data, so these are per-host-edition\n")
cat("totals over the covered programme, not over every event contested.\n")

tot_exp <- sum(panel$golds_expected); tot_act <- sum(panel$golds_actual)
cat(sprintf("\n  Golds an average host would win WITHOUT hosting ... %6.2f\n", tot_exp / n_ed))
cat(sprintf("  Golds an average host actually wins .............. %6.2f\n", tot_act / n_ed))
cat(sprintf("  Hosting is worth ................................. %6.2f golds (%+.0f%%)\n",
            (tot_act - tot_exp) / n_ed, 100 * (tot_act / tot_exp - 1)))

# Programme size differs hugely across series, so also give it per 100 golds.
cat(sprintf("\n  Per 100 golds of covered programme: %.2f without hosting -> %.2f with (%+.2f)\n",
            100 * tot_exp / sum(panel$sport_golds),
            100 * tot_act / sum(panel$sport_golds),
            100 * (tot_act - tot_exp) / sum(panel$sport_golds)))

split_by <- function(dt, by, label) {
  s <- dt[, .(sports = uniqueN(sport), rows = .N,
              golds_in_scope = sum(sport_golds) / n_ed,
              without = sum(golds_expected) / n_ed,
              actual = sum(golds_actual) / n_ed,
              gained = sum(golds_gained) / n_ed), by = by]
  s[, share_of_gain := round(100 * gained / sum(gained))]
  # The RATE matters as much as the total. A category can be a small share of
  # the extra golds simply by being a small share of the programme -- judged
  # sports are 26 golds against measured's 96 -- so give gain per 100 golds
  # available, which is comparable across categories of any size.
  s[, gain_per_100_available := round(100 * gained / golds_in_scope, 2)]
  setorderv(s, "gained", -1)
  cat("\n---", label, "---\n")
  print(s[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
  invisible(s)   # else the returned table prints a second, unrounded time
}

# ---------------------------------------------------------------------------
# 1. Team versus individual
# ---------------------------------------------------------------------------
cat("\n\n================ WHERE THE EXTRA GOLDS COME FROM ================\n")
split_by(panel, "team_sport", "team versus individual sports")

# ---------------------------------------------------------------------------
# 2. By subjectivity
# ---------------------------------------------------------------------------
panel[, subj_band := cut(subjectivity, c(-Inf, 0.10, 0.25, 0.50, Inf),
        labels = c("measured (0.00-0.10)", "refereed (0.10-0.25)",
                   "mixed (0.25-0.50)", "judged (0.50-1.00)"))]
split_by(panel, "subj_band", "by how official-dependent the sport is")

# ---------------------------------------------------------------------------
# 3. By sport size -- the factor that actually held up in the regression
# ---------------------------------------------------------------------------
panel[, size_band := cut(sport_golds, c(0, 4, 10, 25, Inf),
        labels = c("tiny (1-4 golds)", "small (5-10)", "medium (11-25)", "large (26+)"))]
split_by(panel, "size_band", "by how many golds the sport offers")

# ---------------------------------------------------------------------------
# 4. The biggest single contributors
# ---------------------------------------------------------------------------
cat("\n--- the ten sports that give hosts the most extra gold ---\n")
sp <- panel[, .(rows = .N, golds_in_sport = sum(sport_golds) / n_ed,
                without = sum(golds_expected) / n_ed,
                actual = sum(golds_actual) / n_ed,
                gained = sum(golds_gained) / n_ed,
                subjectivity = round(mean(subjectivity), 2)), by = sport]
sp[, share_of_gain := round(100 * gained / sum(gained), 1)]
print(head(sp[order(-gained)][, lapply(.SD, function(x)
  if (is.numeric(x)) round(x, 3) else x)], 10))

# ---------------------------------------------------------------------------
# 5. Access versus performance
# ---------------------------------------------------------------------------
cat("\n\n================ ACCESS VERSUS PERFORMANCE ================\n")
cat("Access = golds from being in a competition the host would otherwise have\n")
cat("missed. Performance = golds from winning more of what it was in anyway.\n")

# Team sports: measured directly from the qualification panel.
tq[, golds_actual   := share_home / 100 * sport_golds]
tq[, golds_expected := share_away / 100 * sport_golds]
tq[, golds_gained   := golds_actual - golds_expected]
n_tq <- uniqueN(tq[, .(games, year)])
p_home <- mean(tq$in_home); p_away <- mean(tq$frac_in_away)
# Ratio of means, so the decomposition closes exactly -- see the derivation in
# analyse_team_qualification.R.
s_home <- mean(tq$share_home) / p_home
s_away <- mean(tq$share_away) / p_away
frac_access <- ((p_home - p_away) * s_away) /
               ((p_home - p_away) * s_away + p_home * (s_home - s_away))

team_gain <- panel[team_sport == TRUE, sum(golds_gained)] / n_ed
cat(sprintf("\nTEAM SPORTS (%d rows, %d host editions, qualification measured directly)\n",
            nrow(tq), n_tq))
cat(sprintf("  in the tournament: %.1f%% at home vs %.1f%% away\n",
            100 * p_home, 100 * p_away))
cat(sprintf("  gain per host edition ....... %.3f golds\n", team_gain))
cat(sprintf("    of which access ........... %.3f (%.0f%%)\n",
            team_gain * frac_access, 100 * frac_access))
cat(sprintf("    of which performance ...... %.3f (%.0f%%)\n",
            team_gain * (1 - frac_access), 100 * (1 - frac_access)))

# Individual sports: access shows up as entry lift, which is measurable on a
# sub-sample. Scale the observed entry coefficient by the mean lift.
ind <- panel[team_sport == FALSE]
ind_gain <- sum(ind$golds_gained) / n_ed
ie <- ind[!is.na(entry_lift_pp)]
ENTRY_COEF <- 0.48   # pp of a sport's golds per pp of its field entered
cat(sprintf("\nINDIVIDUAL SPORTS (%d rows; entry lift measurable on %d)\n",
            nrow(ind), nrow(ie)))
cat(sprintf("  gain per host edition ....... %.3f golds\n", ind_gain))
if (nrow(ie) >= 30) {
  # Attributable to entry = coef * lift, in pp of each sport's golds.
  ie[, access_pp := ENTRY_COEF * entry_lift_pp]
  ie[, access_golds := pmax(0, pmin(access_pp, effect_pp)) / 100 * sport_golds]
  frac_ind_access <- sum(ie$access_golds) / sum(ie$golds_gained)
  cat(sprintf("  mean entry lift ............. %+.2f pp of the field\n",
              mean(ie$entry_lift_pp)))
  cat(sprintf("  on the entry sub-sample, access explains %.0f%% of the gain\n",
              100 * frac_ind_access))
  cat(sprintf("    access ...... %.3f golds (%.0f%%)\n",
              ind_gain * frac_ind_access, 100 * frac_ind_access))
  cat(sprintf("    performance . %.3f golds (%.0f%%)\n",
              ind_gain * (1 - frac_ind_access), 100 * (1 - frac_ind_access)))
} else frac_ind_access <- NA_real_

# ---------------------------------------------------------------------------
# 6. The whole thing on one page
# ---------------------------------------------------------------------------
cat("\n\n================ FULL ATTRIBUTION, PER HOST EDITION ================\n")
total_gain <- (tot_act - tot_exp) / n_ed
attrib <- data.table(
  component = c(
    "Baseline: golds without hosting",
    "  + individual sports, access (bigger team at home)",
    "  + individual sports, performance",
    "  + team sports, access (automatic qualification)",
    "  + team sports, performance",
    "= golds actually won"),
  golds = c(
    tot_exp / n_ed,
    if (is.na(frac_ind_access)) NA_real_ else ind_gain * frac_ind_access,
    if (is.na(frac_ind_access)) NA_real_ else ind_gain * (1 - frac_ind_access),
    team_gain * frac_access,
    team_gain * (1 - frac_access),
    tot_act / n_ed))
attrib[, pct_of_gain := c(NA, round(100 * golds[2:5] / total_gain), NA)]
print(attrib[, .(component, golds = round(golds, 2), pct_of_gain)])

cat(sprintf("\nHosting is worth %.2f golds to an average host over the covered\n", total_gain))
cat(sprintf("programme -- a %.0f%% uplift on the %.2f it would otherwise expect.\n",
            100 * total_gain / (tot_exp / n_ed), tot_exp / n_ed))

cat("\n--- the same, scaled to a modern 300-gold Games ---\n")
scale300 <- 300 / (sum(panel$sport_golds) / n_ed)
cat(sprintf("  without hosting %.1f golds -> with hosting %.1f (+%.1f)\n",
            tot_exp / n_ed * scale300, tot_act / n_ed * scale300,
            total_gain * scale300))

cat("\n--- how much of this is boycotts? ---\n")
panel[, boycott := is_boycott_edition(games, year)]
clean <- panel[boycott == FALSE]
n_clean <- uniqueN(clean[, .(games, year)])
cat(sprintf("  boycott-affected host editions in the panel: %d of %d\n",
            n_ed - n_clean, n_ed))
cat(sprintf("  excluding them: %.2f golds without hosting -> %.2f with (+%.2f, %+.0f%%)\n",
            sum(clean$golds_expected) / n_clean, sum(clean$golds_actual) / n_clean,
            (sum(clean$golds_actual) - sum(clean$golds_expected)) / n_clean,
            100 * (sum(clean$golds_actual) / sum(clean$golds_expected) - 1)))
cat(sprintf("  against %+.2f golds (%+.0f%%) on the full panel\n",
            total_gain, 100 * total_gain / (tot_exp / n_ed)))

cat("\n--- caveats that bound all of the above ---\n")
cat("* Covers sports with sport-level medal data only; median edition coverage\n")
cat("  is 97% of official golds but Pan American and African editions are lower.\n")
cat("* The individual-sport access share is estimated from the entry sub-sample\n")
cat("  and applied to all individual sports; team-sport access is measured\n")
cat("  directly from qualification, which is the firmer of the two.\n")
cat("* Boycott-thinned host editions are in the panel. They inflate a host's\n")
cat("  home share for reasons that have nothing to do with hosting.\n")

saveRDS(list(panel = panel, attribution = attrib,
             team_access_frac = frac_access, ind_access_frac = frac_ind_access),
        file.path(DATA, "host_attribution.rds"))
fwrite(attrib, file.path(DATA, "host_attribution.csv"))
cat("\nSaved host_attribution.\n")
