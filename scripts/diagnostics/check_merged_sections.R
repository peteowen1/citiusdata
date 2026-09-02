# race_key is competition|event|round|date, which has no section identifier - so
# parallel sections of the same round on the same day collapse into one "race".
# Concordance is pairwise WITHIN a race, so those pairs compare athletes who
# never met, and the race shock is estimated across sections that had different
# conditions.
#
# A duplicated finishing place is NOT the same thing as a merged race. High
# jump and pole vault are decided on countback (fewer failures, then fewer
# attempts at the winning height), so two athletes legitimately share a place
# with the SAME mark - that is a genuine tie, not two sections stitched
# together under one race_key. A merged race only exists when a place is
# duplicated AND the tied rows have DIFFERENT marks: same place, different
# performance, which is only possible if the rows never actually raced each
# other. This is the idiom used everywhere else in this repo that removes
# merged races before scoring (check_blend_concordance.R, build_reharvest_
# targets.R, score_by_event.R, ...) - this script used to disagree with all of
# them by counting on place alone, and it showed: jump events read 31.46% "in
# merged races", roughly 9x the field-wide rate, because countback ties in HJ/
# PV were being counted as merges. The corrected number below is what those
# other scripts, and the runbook, actually mean by "merged".
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select=c("race_key","event_id","place","seen","perf","r_use","date")))
h <- h[seen == TRUE & is.finite(place) & place > 0 & place <= 12]

# BROAD count: a duplicated finishing position inside one race_key, regardless
# of whether the tied rows share a mark. Kept only to show the gap against the
# corrected count below - do not headline this, it includes legitimate ties.
r <- h[, .(n = .N, dup_broad = .N - uniqueN(place)), by = race_key]

# CORRECTED count: same place AND different marks. round(perf, 9) matches the
# idiom in check_blend_concordance.R so this and the concordance script can't
# quietly drift apart on floating-point equality.
.dup <- h[, .(ath = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
          ath > 1 & marks > 1, unique(race_key)]
r[, merged := race_key %chin% .dup]

cat(sprintf("scored races: %s | duplicate finishing place (broad, includes ties): %s (%.2f%%)\n",
            format(nrow(r), big.mark=","), format(sum(r$dup_broad>0), big.mark=","),
            100*mean(r$dup_broad>0)))
cat(sprintf("merged races (corrected: place duplicated AND marks differ): %s (%.2f%%)\n",
            format(sum(r$merged), big.mark=","), 100*mean(r$merged)))
cat(sprintf("scored rows inside merged races: %s of %s (%.2f%%)\n",
            format(sum(r[merged==TRUE, n]), big.mark=","), format(sum(r$n), big.mark=","),
            100*sum(r[merged==TRUE, n])/sum(r$n)))

# how many CONCORDANCE PAIRS come from them - the quantity that matters, since a
# merged race contributes pairs quadratically in its size
r[, pairs := n*(n-1)/2]
cat(sprintf("\nconcordance pairs from merged races (corrected): %s of %s (%.2f%%)\n",
            format(sum(r[merged==TRUE, pairs]), big.mark=","), format(sum(r$pairs), big.mark=","),
            100*sum(r[merged==TRUE, pairs])/sum(r$pairs)))
cat("Quadratic in race size, so a few large merged races carry more weight than\n")
cat("their row count suggests - which is why pairs is the number to quote.\n")
cat(sprintf("(for comparison, the broad/ties-included definition would claim %s pairs, %.2f%%)\n",
            format(sum(r[dup_broad>0, pairs]), big.mark=","),
            100*sum(r[dup_broad>0, pairs])/sum(r$pairs)))

# guard: if the corrected rate ever climbs back near the broad rate, the
# marks-differ filter has drifted (e.g. perf rounding changed, or a countback
# event started storing distinct marks for tied places) - stop rather than
# silently reporting a re-inflated headline.
merged_rate <- mean(r$merged)
if (merged_rate > 0.10) {
  stop(sprintf(
    "merged-race rate is %.2f%% of scored races - over the 10%% sanity guard. ",
    100*merged_rate),
    "Historically this sits under 1% (918 races / 0.55% on the pre-reharvest ",
    "corpus, per docs/plans/post-reharvest-runbook.md). A jump this large means ",
    "the marks-must-differ definition has drifted back toward counting ",
    "legitimate countback ties (HJ/PV) as merges - check round(perf, 9) and the ",
    "place/seen filters above before trusting this number.")
}

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h2 <- merge(h, r[, .(race_key, merged, dup_broad)], by="race_key")
h2 <- merge(h2, reg, by="event_id")
cat("\nper family (corrected definition; broad/ties-included shown alongside):\n")
print(h2[, .(rows=.N,
             pct_merged=round(100*mean(merged),2),
             pct_dup_broad=round(100*mean(dup_broad>0),2)),
          by=family][order(-pct_merged)])
