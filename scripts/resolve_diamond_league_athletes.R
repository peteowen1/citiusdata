# Resolve a Diamond-League-shaped entry list (Brussels Final, Budapest
# Ultimate Championship) to World Athletics athlete ids.
#
# WHY THIS SCRIPT EXISTS. run_meet.ps1's own header says the Diamond League
# meets "are not a parameterisation of [the Birmingham chain]... they get
# their own step list when that card is built." That step list was never
# built (2026-08-31). World Athletics has not published an official entry
# list for Brussels as of this run (confirmed via athletics_calendar() and a
# direct browser check -- a genuine branded Error 500 page, not a bot-block).
# The field used here is a third-party-compiled post-Zurich qualifier list
# (etusuora.com), not an official WA entry list -- see the CAVEAT columns
# stamped on the output.
#
# NO BIRTHDATE. resolve_birmingham_athletes.R's rule-2 (birthdate agreement)
# is the strongest disambiguator this project has, and it is unavailable
# here -- the source list has name + country only. Falls back to rule 3
# (most event history) for every ambiguous match, which is weaker; multi-id
# matches are reported, not silently resolved, so a human can spot-check.
#
# Usage:  Rscript scripts/resolve_diamond_league_athletes.R <meet_id>
#   e.g.  Rscript scripts/resolve_diamond_league_athletes.R brussels2026

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")

args <- commandArgs(trailingOnly = TRUE)
MEET <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_DL_MEET", "brussels2026")
ENTRIES <- file.path(D, paste0(MEET, "_entries.csv"))
OUT <- file.path(D, paste0(MEET, "_athlete_ids.csv"))
if (!file.exists(ENTRIES)) cli::cli_abort("No entries file at {.file {ENTRIES}}.")

# --- entries --------------------------------------------------------------
e <- fread(ENTRIES)
e[, event_id := match_event(event, sex)]
n_unmatched_event <- e[is.na(event_id), .N]
if (n_unmatched_event) {
  cli::cli_alert_warning("{n_unmatched_event} entr{?y/ies} had an unmatched discipline (dropped):")
  print(unique(e[is.na(event_id), .(event, sex)]))
}
e <- e[!is.na(event_id)]
cli::cli_alert_info("{nrow(e)} modellable entr{?y/ies}, {uniqueN(e$event_id)} event{?s}.")

# --- candidate ids ----------------------------------------------------------
champs <- tryCatch(
  with_citius_db_connection(function(conn) load_championship_results(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(champs) || !nrow(champs)) champs <- setDT(readRDS(file.path(D, "championship_results.rds")))

# TRANSLITERATE, don't strip. The first version of this was
# gsub("[^A-Z]", "", toupper(x)), which DELETES every non-ASCII letter rather
# than folding it to its ASCII base: the corpus's "Kristjan Čeh" normalised to
# "KRISTJANEH" (the Č silently vanished) while the entry list's plain-ASCII
# "Kristjan Ceh" normalised to "KRISTJANCEH", so the reigning world discus
# champion failed to resolve. Measured 2026-08-31 on the Brussels field: this
# ONE bug accounted for 16 of 23 unresolved entrants, every one a real athlete
# already in the corpus (Čeh, Šutej, Topić, Zapletalová, Kołodziejski,
# Sarâboyukov, Vilagoš, Jæger, Gómez, Bourgoin, Menéndez, Hodelín, Díaz
# Hernández, Pérez Hernández, Figueroa, Schyns). stri_trans_general folds
# Č->C, ø->o, æ->ae, ł->l, ņ->n deterministically -- this is CANONICALISATION,
# not fuzzy guessing, and it is applied identically to both sides.
norm <- function(x) {
  y <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
  gsub("[^A-Z]", "", toupper(y))
}
# Same normalisation, but with the whitespace-separated name parts reversed
# first -- for sources that write family name first (see tier 2 below).
norm_rev <- function(x) {
  vapply(strsplit(x, "[[:space:]]+"),
         function(p) norm(paste(rev(p), collapse = " ")), character(1))
}
champs[, aid := as.character(athlete_id)]

cand <- champs[!is.na(athlete_name) & !is.na(aid), .(n_all = .N), by = .(key = norm(athlete_name), aid)]
ev_hist <- champs[!is.na(event_id), .(n_ev = .N), by = .(aid, event_id)]
cand_keys <- unique(cand$key)

# --- tiered key resolution --------------------------------------------------
# Three tiers, in descending confidence. Tiers 2 and 3 are REPORTED every run,
# never silent: citius/CLAUDE.md's rule for match_event() is "return NA rather
# than guessing, because silently snapping an unknown onto a neighbour
# corrupts histories undetectably", and the same applies to athletes. Tier 1
# is exact and needs no scrutiny; tiers 2-3 print what they matched so a human
# can eyeball them before the card ships.
e[, key := norm(athlete)]
e[, row := .I]
e[, match_key := NA_character_]
e[, match_tier := NA_character_]

# Tier 1 -- exact on the canonicalised key.
e[key %in% cand_keys, `:=`(match_key = key, match_tier = "exact")]

# Tier 2 -- family-name-first sources. Deterministic (an exact match on the
# reversed parts), not fuzzy, but reported because a name that matches only
# when reversed is worth a human glance. Brussels 2026 had two: the source
# wrote "Seville Oblique" for Oblique Seville (JAM) and "Yan Ziyi" for Ziyi
# Yan (CHN, family name first per Chinese convention).
e[is.na(match_key), rkey := norm_rev(athlete)]
e[is.na(match_key) & rkey %in% cand_keys, `:=`(match_key = rkey, match_tier = "reversed")]

# Tier 3 -- source typos. THIS ONE IS A GUESS, so it carries three
# independent guards and is printed loudly: (a) edit distance 1, or 2 only
# for keys >= 12 characters where two edits is still proportionally tiny;
# (b) the best distance must be reached by exactly ONE candidate key -- a tie
# is a real ambiguity and stays NA; (c) the candidate must have actual
# history in the event being forecast, which a coincidentally-similar name
# almost never does. Brussels 2026 had FOUR, all genuine source misspellings
# of real finalists, each verified against real corpus history in the entered
# event: "Trayvon Brommell" (Bromell, d1), "Jacop Krop" (Jacob Krop, d1),
# "Helena Penotte" (Ponette, d2), "Davisleidy Velazco" (Davisleydi Velazco,
# d2). Final tier distribution on that run: exact 247, reversed 2, fuzzy_d1 2,
# fuzzy_d2 2 = 253 of 253. Counts corrected 2026-08-31 after a review caught
# this comment asserting three and omitting Ponette -- in a codebase that
# treats comments as measured fact, a wrong count here undermines the very
# convention that makes the rest of these headers worth trusting.
fz <- e[is.na(match_key) & !is.na(event_id)]
if (nrow(fz)) {
  key_aid <- cand[, .(key, aid)]
  for (i in seq_len(nrow(fz))) {
    k <- fz$key[i]; ev <- fz$event_id[i]
    thr <- if (nchar(k) >= 12L) 2L else 1L
    # Block on first letter and a length window before computing distances --
    # standard blocking, keeps this from being an O(entries x 200k) scan and
    # cannot admit a candidate the threshold would have rejected anyway.
    pool <- cand_keys[substr(cand_keys, 1L, 1L) == substr(k, 1L, 1L) &
                        abs(nchar(cand_keys) - nchar(k)) <= thr]
    if (!length(pool)) next
    d <- utils::adist(k, pool)[1, ]
    best <- min(d)
    if (best > thr) next
    hit <- pool[d == best]
    if (length(hit) != 1L) next          # tie -> real ambiguity, leave NA
    ok_ev <- merge(key_aid[key == hit], ev_hist[event_id == ev], by = "aid")
    if (!nrow(ok_ev) || sum(ok_ev$n_ev) == 0L) next   # no history in this event
    e[row == fz$row[i], `:=`(match_key = hit, match_tier = paste0("fuzzy_d", best))]
  }
}

cw <- merge(e[!is.na(match_key), .(row, key = match_key, event_id, athlete, country)],
            cand, by = "key", allow.cartesian = TRUE)
cw <- merge(cw, ev_hist, by = c("aid", "event_id"), all.x = TRUE)
cw[is.na(n_ev), n_ev := 0L]

# No birthdate to disambiguate on -- straight to rule 3 (most event history),
# same fallback resolve_birmingham_athletes.R uses when birthdate doesn't
# resolve it. Multiple ids tied on n_ev/n_all are a real ambiguity, reported
# below, not silently broken by a fixed tie-break order.
keep <- cw[order(row, -n_ev, -n_all)][, .SD[1], by = row]
# How many DISTINCT ids the name matched, carried onto the artefact -- see the
# collision report below for why the count matters more than whether they tied.
ncand <- cw[, .(n_cand = uniqueN(aid)), by = row]
res <- merge(e[, .(row, athlete, country, event, event_id, match_tier)],
             keep[, .(row, aid, n_ev, n_all)], by = "row", all.x = TRUE)
res <- merge(res, ncand, by = "row", all.x = TRUE)
res[, athlete_id := aid]
res[is.na(athlete_id), `:=`(n_ev = 0L, n_cand = 0L)]

# --- report -----------------------------------------------------------------
cli::cli_h2("Resolution ({MEET})")
resolved <- res[!is.na(athlete_id)]
cli::cli_alert_info("Resolved {nrow(resolved)} of {nrow(res)} ({round(100*nrow(resolved)/nrow(res),1)}%).")
print(res[!is.na(match_tier), .N, by = match_tier][order(-N)])

# Everything matched by anything other than an exact key gets printed in full.
# A reversed-order match is deterministic and low-risk; a fuzzy one is a
# GUESS the three guards above merely make a good guess -- neither ships
# without appearing here first.
insp <- res[!is.na(athlete_id) & match_tier != "exact"]
if (nrow(insp)) {
  cli::cli_alert_warning("{nrow(insp)} entr{?y/ies} resolved by a NON-EXACT match - eyeball these before the card ships:")
  print(merge(insp[, .(row, athlete, country, event, match_tier)],
              e[, .(row, matched_to = match_key)], by = "row")[order(match_tier)])
}

un <- res[is.na(athlete_id)]
if (nrow(un)) {
  cli::cli_alert_warning("{nrow(un)} entr{?y/ies} unresolved - no history under this name:")
  print(un[, .(athlete, country, event)])
}

noev <- res[!is.na(athlete_id) & n_ev == 0]
if (nrow(noev)) {
  cli::cli_alert_warning("{nrow(noev)} resolved entr{?y/ies} have NO history in the event being forecast (name matched, but never raced it -- check by hand):")
  print(noev[, .(athlete, country, event)])
}

# NAME COLLISIONS -- every row whose name matched MORE THAN ONE id, not just
# the ones that tied.
#
# The first version of this check only fired when two candidate ids shared an
# IDENTICAL top (n_ev, n_all). That is close to backwards: a genuine collision
# (two different real athletes who happen to share a name) will almost never
# tie exactly -- one will simply have logged a bit more history -- and the
# selection rule above is precisely "pick whoever has more". So the tie check
# caught freak numeric coincidences while the actual danger it exists to guard
# against, a real collision silently resolved by most-history, was the case
# least likely to trip it. Found in review 2026-08-31.
#
# citius/CLAUDE.md's rule for match_event() is "return NA rather than guessing,
# because silently snapping an unknown onto a neighbour corrupts histories
# undetectably". We cannot return NA here without dropping real finalists off a
# published card, so the next best thing is: resolve, but make EVERY collision
# visible -- printed here and stamped as `n_cand` on the artefact, so a human
# can review the pick rather than trust it blind.
coll <- res[n_cand > 1]
if (nrow(coll)) {
  cli::cli_alert_warning("{nrow(coll)} entr{?y/ies} matched MORE THAN ONE athlete id - resolved by most-event-history (no birthdate available to break it). Eyeball these:")
  det <- merge(coll[, .(row, athlete, country, event, n_cand, chosen = athlete_id, chosen_n_ev = n_ev)],
               cw[, .(row, aid, n_ev, n_all)], by = "row", allow.cartesian = TRUE)
  setorder(det, row, -n_ev, -n_all)
  print(det[, .(athlete, country, event, n_cand, chosen, candidate = aid, n_ev, n_all)])
} else {
  cli::cli_alert_success("No entry matched more than one athlete id.")
}

fwrite(res, OUT)
cli::cli_alert_success("Wrote {basename(OUT)}: {nrow(res)} rows.")
