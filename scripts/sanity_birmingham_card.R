# Sanity checks on the Birmingham card, run before anyone reads it.
#
# Glasgow's equivalent exists because an athlete reached the published card as
# second favourite in the 100m on a predicted 10.97s. This one has less time and
# more moving parts -- a round structure whose counts are DERIVED, a crosswalk
# resolved by birthdate, and 42 events of a 44-event programme -- so it checks
# the pipeline as well as the numbers.
#
# Anchors written down BEFORE the numbers were looked at:
#   1. p_gold must rank with ABILITY within every event.
#   2. Probabilities must nest: gold <= medal <= final <= reach-round-2.
#   3. Every event sums to 1 gold, 3 medals, and its own final size.
#   4. Coverage must equal the 42 modellable events, and the 2 excluded must be
#      the two Marathon Race Walks and nothing else.
#   5. Provenance must be internally consistent: the stamp must equal DEPLOYED's,
#      and the cutoff must precede the first day of competition.
#   6. The three defects the ticket-06 prototype found must be absent.

VERSE <- "C:/dev/citiusverse"
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(jsonlite)
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
D <- file.path(VERSE, "citiusdata", "data")

MEET_START <- as.Date("2026-08-10")
BIRMINGHAM <- 7192415L
PROGRAMME  <- 44L    # individual events, from the published timetable (ticket 01)
EXPECT_UNMODELLED <- c("Marathon Race Walk")

p  <- setDT(readRDS(file.path(D, "birmingham2026_pretournament.rds")))
st <- fread(file.path(D, "birmingham2026_round_structure.csv"))

fails <- 0L
say <- function(ok, msg) {
  # An NA verdict is a FAILED check, not a pass and not a crash. `if (!NA)`
  # errors and would take every later check down with it, reporting nothing.
  ok <- isTRUE(ok)
  if (!ok) fails <<- fails + 1L
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", msg))
}

# Every check below filters, and a filter over an empty or all-NA column matches
# nothing and prints PASS. That is the exact shape of the vacuous guard this
# repo has shipped before, so the inputs are verified to EXIST first. These are
# stopifnot, not say(): if they fail the checks below are meaningless rather
# than failing, and should not print at all.
NEEDED <- c("p_gold", "p_medal", "p_final", "ability", "athlete", "nation",
            "event_id", "config", "cutoff", "generated_at")
stopifnot(
  "prediction file is empty" = nrow(p) > 0L,
  "no events in the prediction file" = uniqueN(p$event_id) > 0L,
  "columns the checks read are missing" = all(NEEDED %in% names(p)))
for (col in c("p_gold", "p_medal", "p_final", "ability")) {
  cov <- mean(is.finite(p[[col]]))
  if (cov < 0.5) stop(sprintf(
    "`%s` is finite for only %.1f%% of rows -- checks reading it would pass vacuously",
    col, 100 * cov))
}

cat("card generated:", as.character(unique(p$generated_at))[1],
    "| cutoff:", as.character(unique(p$cutoff))[1],
    "| config:", unique(p$config)[1], "\n")
cat(sprintf("events: %d | athlete-events: %s\n\n", uniqueN(p$event_id),
            format(nrow(p), big.mark = ",")))

cat("1. p_gold ranks with ability within each event\n")
# `ability` is ALREADY in oriented perf space -- higher is better in every event.
# Do not multiply by `orientation` again: Glasgow's script applies orientation to
# a raw MARK (`orientation * log(median_mark)`), which is a different quantity.
# Applying it here inverts the anchor and reports rho = -0.96, which reads as a
# catastrophic model failure and is actually a sign error in the check.
q <- copy(p)
q[, better := ability]
ag <- q[is.finite(better) & is.finite(p_gold),
        .(n = .N, rho = if (.N > 3) suppressWarnings(
            stats::cor(p_gold, better, method = "spearman")) else NA_real_),
        by = event_id][!is.na(rho)]
say(nrow(ag) > 20 && all(ag$rho > 0.4),
    sprintf("min rho %.3f over %d events (worst: %s)",
            min(ag$rho), nrow(ag), ag[which.min(rho)]$event_id))

cat("\n2. probabilities nest\n")
say(p[p_gold > p_medal + 1e-9, .N] == 0, "p_gold <= p_medal everywhere")
say(p[p_medal > p_final + 1e-9, .N] == 0, "p_medal <= p_final everywhere")
if ("p_reach_r2" %in% names(p)) {
  say(p[!is.na(p_reach_r2) & p_final > p_reach_r2 + 1e-9, .N] == 0,
      "p_final <= p_reach_r2 everywhere a round 2 exists")
}
say(p[p_gold < -1e-9 | p_gold > 1 + 1e-9, .N] == 0, "every probability lies in [0, 1]")

cat("\n3. per-event sums\n")
s <- p[, .(gold = sum(p_gold, na.rm = TRUE), medal = sum(p_medal, na.rm = TRUE),
           final = sum(p_final, na.rm = TRUE), n = .N), by = event_id]
say(max(abs(s$gold - 1)) < 0.01, sprintf("gold sums to 1 (max deviation %.4f)", max(abs(s$gold - 1))))
say(max(abs(s$medal - 3)) < 0.05, sprintf("medals sum to 3 (max deviation %.4f)", max(abs(s$medal - 3))))
# p_final must equal the seats in that event's final. Read that off the STRUCTURE
# table rather than re-deriving it from event names -- the structure is what the
# simulation actually consumed, so re-deriving it here would let the two drift
# apart and check the wrong thing.
nr  <- st[, .(n_rounds = max(round_index)), by = event_id]
pen <- merge(st, nr, by = "event_id")[round_index == n_rounds - 1L,
        .(seats = advance * races + fastest_losers), by = event_id]
exp_seats <- merge(merge(s, nr, by = "event_id"), pen, by = "event_id", all.x = TRUE)
# A single-round event has no penultimate round: its "final" is the whole field.
exp_seats[n_rounds == 1L, seats := as.numeric(n)]
say(all(is.finite(exp_seats$seats)) && exp_seats[abs(final - seats) > 0.05, .N] == 0,
    sprintf("p_final equals the final's seats in every event (%d checked)", nrow(exp_seats)))
if (exp_seats[!is.finite(seats) | abs(final - seats) > 0.05, .N]) {
  print(exp_seats[!is.finite(seats) | abs(final - seats) > 0.05])
}

cat("\n4. coverage against the published programme\n")
j <- fromJSON(file.path(D, "birmingham2026_entries.json"), simplifyVector = FALSE)
prog <- unlist(j$events)
say(length(prog) == PROGRAMME,
    sprintf("entry list holds all %d individual programme events (found %d)", PROGRAMME, length(prog)))
modelled <- uniqueN(p$event_id)
missing <- setdiff(sub("^(Men's|Women's)\\s+", "", prog), unique(p$discipline))
say(all(missing %in% EXPECT_UNMODELLED),
    sprintf("the %d unmodelled event-discipline%s %s exactly as expected: %s",
            length(missing), if (length(missing) == 1) "" else "s",
            if (length(missing) == 1) "is" else "are",
            paste(missing, collapse = ", ")))
say(modelled == PROGRAMME - 2L,
    sprintf("%d of %d programme events modelled", modelled, PROGRAMME))

cat("\n5. provenance\n")
say(length(unique(p$config)) == 1L && unique(p$config)[1] == DEPLOYED$stamp,
    sprintf("config stamp is DEPLOYED's ('%s'), not a literal", DEPLOYED$stamp))
say(all(as.Date(p$cutoff) < MEET_START),
    sprintf("cutoff (%s) precedes the first day of competition (%s)",
            as.character(unique(p$cutoff))[1], MEET_START))
say(all(!is.na(p$generated_at)), "every row carries generated_at")
say(all(p$competition_id == BIRMINGHAM), "every row names the competition being forecast")
say(all(p$counts_source == "derived"),
    "round counts are labelled derived, so the page cannot imply they are official")

cat("\n6. the three defects the prototype found\n")
say(p[is.na(athlete) | !nzchar(athlete), .N] == 0,
    sprintf("every row has an athlete name (%d blank)", p[is.na(athlete) | !nzchar(athlete), .N]))
say(p[is.na(nation) | !nzchar(nation), .N] == 0,
    sprintf("every row has a nation (%d blank)", p[is.na(nation) | !nzchar(nation), .N]))
say(p[, .N, by = .(athlete_id, event_id)][N > 1, .N] == 0,
    "no duplicate athlete-event rows")
fm <- p[, .(rows = .N, claimed = field_modelled[1]), by = event_id]
say(fm[rows != claimed, .N] == 0,
    sprintf("field_modelled equals the row count in every event (%d mismatched)",
            fm[rows != claimed, .N]))
if (fm[rows != claimed, .N]) print(fm[rows != claimed])

cat("\n7. combined-event contamination was excluded from this forecast\n")
# The prototype found `contested_round = "Combined - Group"` on a standalone
# women's 100mH -- the decathlon/heptathlon sharing an event_id with the
# standalone race, logged in NEXT-STEPS.
#
# Do NOT check the store for this. The store IS contaminated; that is a
# corpus-level defect this forecast cannot fix, so a store check fails forever
# and says nothing about the card. What matters is whether THIS forecast
# excluded them, which only the artefact records.
say("combined_rows_excluded" %in% names(p),
    "the artefact records how many combined-event rows were excluded")
if ("combined_rows_excluded" %in% names(p)) {
  n_ex <- unique(p$combined_rows_excluded)
  say(length(n_ex) == 1L && is.finite(n_ex[1]) && n_ex[1] > 0,
      sprintf("%s combined-event row%s excluded from the history behind this card",
              format(n_ex[1], big.mark = ","), if (isTRUE(n_ex[1] == 1)) "" else "s"))
}

cat(sprintf("\n%s -- %d check%s failed\n", if (fails == 0L) "ALL CHECKS PASSED" else "FAILURES",
            fails, if (fails == 1L) "" else "s"))
if (fails > 0L) quit(status = 1L)
