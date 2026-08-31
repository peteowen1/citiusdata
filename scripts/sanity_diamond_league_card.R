# Sanity checks on a Diamond-League-shaped card (Brussels/Budapest), run
# before anyone reads it. Adapted from sanity_birmingham_card.R for a
# straight-final, no-rounds shape (no p_reach_r2, no round-structure table).
#
# Anchors written down BEFORE the numbers were looked at:
#   1. p_gold must rank with ability within every event.
#   2. Probabilities must nest: gold <= medal.
#   3. Every event sums to 1 gold and 3 medals.
#   4. Provenance must be internally consistent, and field_type must be
#      stamped as the unofficial third-party list it actually is -- NOT
#      "official_entry_list", which would misrepresent the provenance to
#      anyone reading the site.
#   5. No duplicate athlete-event rows; every row has an athlete and nation;
#      the stamped field_modelled must equal the rows actually present.
#   6. EVERY qualified entrant is accounted for -- on the card, or named in
#      <meet>_unmodelled_entrants.csv with a reason. A finalist can legitimately
#      be unmodellable (no history in the event, so estimate_ability() emits no
#      row for them), but they must never just vanish: this anchor is the
#      backstop for exactly that silent omission.
#
# Usage:  Rscript scripts/sanity_diamond_league_card.R <meet_id>

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
D <- file.path(VERSE, "citiusdata", "data")

args <- commandArgs(trailingOnly = TRUE)
MEET <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_DL_MEET", "brussels2026")

p <- setDT(readRDS(file.path(D, paste0(MEET, "_pretournament.rds"))))

fails <- 0L
say <- function(ok, msg) {
  ok <- isTRUE(ok)
  if (!ok) fails <<- fails + 1L
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", msg))
}

# field_modelled/field_entrants/field_unmodelled and athlete_id are in this
# list because the checks that read them are `x != y` filters and `by=` groups:
# on a MISSING column those become a zero-row filter or an error, and the first
# of those passes VACUOUSLY. Same reasoning sanity_birmingham_card.R spells out
# for its own counts_source/field_modelled entries.
NEEDED <- c("p_gold", "p_medal", "ability", "athlete", "nation", "event_id",
            "athlete_id", "config", "cutoff", "generated_at",
            "field_type", "field_source",
            "field_modelled", "field_entrants", "field_unmodelled")
stopifnot(
  "prediction file is empty" = nrow(p) > 0L,
  "no events in the prediction file" = uniqueN(p$event_id) > 0L,
  "columns the checks read are missing" = all(NEEDED %in% names(p)))
for (col in c("p_gold", "p_medal")) {
  n_bad <- sum(!is.finite(p[[col]]))
  if (n_bad > 0L) stop(sprintf("`%s` is NA or non-finite on %d row(s)", col, n_bad))
}
cov_ab <- mean(is.finite(p$ability))
if (cov_ab < 0.5) stop(sprintf("`ability` is finite for only %.1f%% of rows", 100 * cov_ab))

cat("card generated:", as.character(unique(p$generated_at))[1],
    "| cutoff:", as.character(unique(p$cutoff))[1],
    "| config:", unique(p$config)[1], "\n")
cat(sprintf("events: %d | athlete-events: %s\n\n", uniqueN(p$event_id), format(nrow(p), big.mark = ",")))

cat("1. p_gold ranks with ability within each event\n")
ag <- p[is.finite(ability) & is.finite(p_gold),
        .(n = .N, rho = if (.N > 3) suppressWarnings(stats::cor(p_gold, ability, method = "spearman")) else NA_real_),
        by = event_id][!is.na(rho)]
say(nrow(ag) > 5 && all(ag$rho > 0.4),
    sprintf("min rho %.3f over %d events (worst: %s)", min(ag$rho), nrow(ag), ag[which.min(rho)]$event_id))

cat("\n2. probabilities nest\n")
say(p[p_gold > p_medal + 1e-9, .N] == 0, "p_gold <= p_medal everywhere")
say(p[p_gold < -1e-9 | p_gold > 1 + 1e-9, .N] == 0, "every probability lies in [0, 1]")

cat("\n3. per-event sums\n")
s <- p[, .(gold = sum(p_gold, na.rm = TRUE), medal = sum(p_medal, na.rm = TRUE), n = .N), by = event_id]
say(max(abs(s$gold - 1)) < 0.01, sprintf("gold sums to 1 (max deviation %.4f)", max(abs(s$gold - 1))))
say(max(abs(s$medal - 3)) < 0.05, sprintf("medals sum to 3 (max deviation %.4f)", max(abs(s$medal - 3))))
say(all(s$n >= 3), sprintf("every scored event has >= 3 entrants (min %d)", min(s$n)))

cat("\n4. provenance\n")
say(length(unique(p$config)) == 1L && unique(p$config)[1] == DEPLOYED$stamp,
    sprintf("config stamp is DEPLOYED's ('%s'), not a literal", DEPLOYED$stamp))
say(all(!is.na(p$generated_at)), "every row carries generated_at")
say(all(p$field_type == "third_party_qualifier_list_unofficial"),
    "field_type is honestly stamped as an UNOFFICIAL third-party list, not official_entry_list")
say(all(!is.na(p$field_source) & nzchar(p$field_source)),
    "every row carries a field_source note (where the entry list actually came from)")

cat("\n5. row integrity\n")
say(p[is.na(athlete) | !nzchar(athlete), .N] == 0, "every row has an athlete name")
say(p[is.na(nation) | !nzchar(nation), .N] == 0, "every row has a nation")
say(p[, .N, by = .(athlete_id, event_id)][N > 1, .N] == 0, "no duplicate athlete-event rows")
# field_modelled is stamped from nrow(proj) -- the field the simulation was
# handed -- while the rows here are what came back out of medal_probs(). They
# are derived independently, so this is a real cross-check that the number the
# page will print matches the field actually forecast, not a restatement of it.
fm <- p[, .(rows = .N, claimed = field_modelled[1]), by = event_id]
say(nrow(fm) > 0L && fm[rows != claimed, .N] == 0,
    sprintf("field_modelled equals the row count in every event (%d mismatched)",
            fm[rows != claimed, .N]))
if (fm[rows != claimed, .N]) print(fm[rows != claimed])

cat("\n6. every qualified entrant is accounted for\n")
# THE BACKSTOP for the silent-omission failure this card shape is prone to:
# estimate_ability() emits nothing for an (athlete, event) pair with no
# history, so such an entrant never reaches the field and would drop off the
# card with nothing erroring. Checks 1-5 all validate rows that MADE IT IN, so
# none of them can see an absence. This one compares the card against the
# entrant list it was built from.
#
# stopifnot, not say(): a missing companion file would make every check below
# vacuous rather than failing, so it must not print a verdict at all.
ids_f <- file.path(D, paste0(MEET, "_athlete_ids.csv"))
unm_f <- file.path(D, paste0(MEET, "_unmodelled_entrants.csv"))
stopifnot(
  "entrant id file is missing - cannot verify the card covers the field" = file.exists(ids_f),
  "unmodelled-entrant file is missing - predict_diamond_league_final.R must write it" = file.exists(unm_f))
ids_all <- fread(ids_f)
unm <- fread(unm_f)
stopifnot(
  "id file lacks the columns this check reads" = all(c("event_id", "athlete_id") %in% names(ids_all)),
  "unmodelled file lacks a reason column" = "reason" %in% names(unm))
ids_all[, athlete_id := as.character(athlete_id)]
if (nrow(unm)) unm[, athlete_id := as.character(athlete_id)]

ent_keys  <- unique(ids_all[!is.na(athlete_id), paste(event_id, athlete_id)])
card_keys <- unique(p[, paste(event_id, athlete_id)])
unm_keys  <- if (nrow(unm)) unique(unm[!is.na(athlete_id), paste(event_id, athlete_id)]) else character(0)
lost <- setdiff(ent_keys, union(card_keys, unm_keys))
# `length(ent_keys) > 0` is the anti-vacuity guard: on an empty id file the
# setdiff is empty too and this would otherwise report PASS having checked
# nothing.
say(length(ent_keys) > 0L && length(lost) == 0L,
    sprintf("all %d resolved entrants are on the card or listed as unmodelled (%d unaccounted for)",
            length(ent_keys), length(lost)))
if (length(lost)) print(utils::head(lost, 20))

n_unexp <- if (nrow(unm)) sum(unm$reason == "unexplained", na.rm = TRUE) else 0L
say(n_unexp == 0L,
    sprintf("no unmodelled entrant has an 'unexplained' reason (%d do)", n_unexp))

# Independent arithmetic cross-check: the per-event counts stamped on the card
# must reconcile with the rows actually present. Catches a stamping bug that the
# set comparison above would miss.
fe <- p[, .(rows = .N, entrants = field_entrants[1], unmodelled = field_unmodelled[1]), by = event_id]
say(nrow(fe) > 0L && fe[entrants != rows + unmodelled, .N] == 0L,
    sprintf("field_entrants == rows + field_unmodelled in every event (%d mismatched)",
            fe[entrants != rows + unmodelled, .N]))
if (fe[entrants != rows + unmodelled, .N]) print(fe[entrants != rows + unmodelled])

if (nrow(unm)) {
  cat(sprintf("  (%d entrant%s not forecast:)\n", nrow(unm), if (nrow(unm) == 1L) "" else "s"))
  print(unm[, .N, by = reason][order(-N)])
}

cat(sprintf("\n%s -- %d check%s failed\n", if (fails == 0L) "ALL CHECKS PASSED" else "FAILURES",
            fails, if (fails == 1L) "" else "s"))
if (fails > 0L) quit(status = 1L)
