# Do we hold the T1 and T2 competitions we ought to, and is anything sitting in
# T3 that is really a strong meet?
#
# TWO QUESTIONS, AND THE SECOND ONE HAS A KNOWN TRAP. "This T3 meet has an elite
# mark in it" is usually one exceptional teenager in a weak field, not a meet
# that deserves promotion. A single best mark cannot tell those apart; DEPTH
# can. So this counts how many DISTINCT ATHLETES clear an elite bar, and reports
# the one-athlete cases separately rather than mixing them in.
#
# THE ERA CONFOUND, WHICH HAS ALREADY BITTEN THIS PROJECT ONCE. Meet strength was
# previously measured against an all-time mark distribution, and because the
# sport gets faster every old meet scored low - which is how the Olympics ended
# up classified as second tier. Percentiles here are therefore computed WITHIN a
# recent window only, so a 2015 meet is judged against 2015-era marks and not
# against 2026 ones.
#
# ANCHORS ARE ASSERTED, NOT EYEBALLED. Written before running it: the Olympics
# and the World Championships are top tier by definition, and if the tiering says
# otherwise the method is wrong rather than the meet unusual.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D     <- here::here("citiusdata", "data")
FROMY <- .env_int("TIER_FROM_YEAR", "2023")   # the window percentiles are taken in
QBAR  <- .env_num("TIER_ELITE_Q",   "0.99")   # what counts as an elite mark
MINN  <- .env_int("TIER_MIN_EVENT_N", "2000") # events too thin to percentile honestly

cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cg[, competition_id := as.character(competition_id)]
cached <- sub("[.]rds$", "", list.files(file.path(D, "ath_comp_cache"), pattern = "[.]rds$"))
cg[, fetched := competition_id %chin% cached]

# ---- ANCHORS ---------------------------------------------------------------
# Asserted before any of the analysis below is looked at.
# YOUTH OLYMPIC GAMES ARE NOT THE OLYMPICS. Singapore 2010, Nanjing 2014 and
# Buenos Aires 2018 are age-group events and correctly sit in development tier;
# the first version of this anchor matched them on "Olympic Games" and fired.
# Worth being precise about which thing was wrong: the SELECTOR was, not the
# tiering. Narrowing an anchor because it matched meets it never meant to cover
# is legitimate; narrowing one because a meet it did mean to cover came out
# awkwardly is the rationalisation the anchor exists to prevent.
.olympic <- cg[grepl("Olympic Games", comp_name, ignore.case = TRUE) &
               !grepl("Youth|U20|U18|Junior", comp_name, ignore.case = TRUE) &
               results > 500]
.worlds  <- cg[grepl("World (Athletics )?Championships", comp_name, ignore.case = TRUE) &
               !grepl("U20|U18|Junior|Youth|Indoor|Relay", comp_name, ignore.case = TRUE) &
               results > 500]
cat(sprintf("ANCHOR: %d Olympic and %d senior World Championship competitions found\n",
            nrow(.olympic), nrow(.worlds)))
stopifnot("no Olympic competitions matched - the anchor cannot test anything" = nrow(.olympic) > 0,
          "no World Championship competitions matched" = nrow(.worlds) > 0)
.bad <- rbind(.olympic, .worlds)[meet_tier != "T1_elite"]
if (nrow(.bad)) {
  cat("\nANCHOR FAILED - these are top-tier meets by definition:\n")
  print(.bad[, .(competition_id, year, meet_tier, results, comp_name)])
}
stopifnot("an Olympics or World Championships is not classified T1 - the TIERING is wrong, not the meet" =
            nrow(.bad) == 0)
cat("ANCHOR PASSED: every Olympics and senior World Championships is T1_elite\n\n")

# ---- 1. tier coverage ------------------------------------------------------
cat("=== do we hold what we should, by tier? ===\n")
cov <- cg[, .(competitions = .N, results = sum(results, na.rm = TRUE),
              fetched = sum(fetched), fetched_pct = round(100 * mean(fetched), 1),
              named_pct = round(100 * mean(!is.na(comp_name) & nzchar(comp_name)), 1)),
          by = meet_tier][order(meet_tier)]
print(cov)
cat("\nfetched = we have pulled it from the competition endpoint, which is what\n")
cat("carries eventName (age divisions) and the full field.\n")

gap <- cg[meet_tier %chin% c("T1_elite", "T2_strong") & fetched == FALSE]
cat(sprintf("\nT1/T2 competitions NOT fetched: %s, holding %s corpus rows\n",
            format(nrow(gap), big.mark = ","), format(sum(gap$results, na.rm = TRUE), big.mark = ",")))
if (nrow(gap)) {
  print(gap[order(-results)][1:min(10, .N), .(competition_id, year, meet_tier, results, comp_name)])
  g <- gap[, .(competition_id, rank_value = results, results, year)]
  setorder(g, -rank_value)
  fwrite(g, file.path(D, "tier_gap_targets.csv"))
  cat(sprintf("wrote tier_gap_targets.csv (%s competitions)\n", format(nrow(g), big.mark = ",")))
}

# ---- 2. elite marks sitting in T3 -----------------------------------------
cat(sprintf("\n=== elite marks in T3 meets, %d onward ===\n", FROMY))
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("race_key", "competition_id", "event_id",
                                        "athlete_id", "perf", "date", "mark", "scoreable")))
c0 <- c0[scoreable == TRUE & is.finite(perf) & year(date) >= FROMY]
c0[, competition_id := as.character(competition_id)]
cat(sprintf("marks in window: %s across %s events\n",
            format(nrow(c0), big.mark = ","), format(uniqueN(c0$event_id), big.mark = ",")))
stopifnot("window is empty" = nrow(c0) > 10000)

# events with enough marks in the window to percentile honestly
n_ev <- c0[, .N, by = event_id][N >= MINN]
c0 <- c0[event_id %chin% n_ev$event_id]
c0[, bar := stats::quantile(perf, QBAR, na.rm = TRUE), by = event_id]
c0[, elite := perf >= bar]
cat(sprintf("events kept (>= %s marks): %s | elite bar = %.0fth percentile within event\n",
            format(MINN, big.mark = ","), format(nrow(n_ev), big.mark = ","), 100 * QBAR))

t3 <- merge(c0[elite == TRUE], cg[, .(competition_id, meet_tier, comp_name, year, results)],
            by = "competition_id")[meet_tier == "T3_development"]
s <- t3[, .(elite_marks = .N, elite_athletes = uniqueN(athlete_id),
            elite_events = uniqueN(event_id)),
        by = .(competition_id, comp_name, year, results)]
setorder(s, -elite_athletes, -elite_marks)
cat(sprintf("\nT3 competitions containing at least one elite mark: %s\n",
            format(nrow(s), big.mark = ",")))

cat("\n--- DEPTH: many different athletes clearing the bar. These are the\n")
cat("    promotion candidates, because one athlete cannot produce this. ---\n")
print(s[elite_athletes >= 5][1:min(12, .N)])

cat("\n--- ONE ATHLETE ONLY: exactly the 'high school kid miles better than the\n")
cat("    field' case. NOT evidence about the meet. ---\n")
one <- s[elite_athletes == 1][order(-elite_marks)]
cat(sprintf("    %s such competitions (%.1f%% of those with any elite mark)\n",
            format(nrow(one), big.mark = ","), 100 * nrow(one) / max(nrow(s), 1)))
print(one[1:min(5, .N)])

f <- file.path(D, "tier_promotion_candidates.csv")
fwrite(s[elite_athletes >= 5], f)
cat(sprintf("\nwrote %s (%s candidates with 5+ distinct elite athletes)\n",
            basename(f), format(s[elite_athletes >= 5, .N], big.mark = ",")))
cat("\nThis is a SHORTLIST, not a reclassification. Promoting a meet changes the\n")
cat("40/12/1 scoring weights, so it belongs in the catalogue chain with its own\n")
cat("evidence, not applied from here.\n")
