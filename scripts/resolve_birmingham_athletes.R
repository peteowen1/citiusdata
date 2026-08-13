# Birmingham 2026 -- resolve entry-list names to World Athletics athlete ids.
#
# Why this is not just the Glasgow lookup. `predict_glasgow_entries.R` builds a
# normalised-name -> id map and takes `athlete_id[1]` -- FIRST ID WINS. On the
# Birmingham field that silently mis-resolves: 27 of 1,404 entries (1.9%) land
# on a normalised key held by more than one id, one of them on FOUR ids, and the
# arbitrary pick is often the wrong one. Adam KELLY's two ids hold 57 rows and 7;
# Jack HIGGINS' three hold 47, 28 and 18. Taking the first is a coin toss over an
# athlete's entire history, and a wrong pick does not error -- it produces a
# confident forecast from someone else's marks.
#
# This is the same failure class as the crosswalk merge already logged in
# NEXT-STEPS ("Guy BROOKS" and "George Brooks" -> one person_id, sigma 99x the
# event median). There the fix was said to need "a cross-source guard on
# conflicting given names". Here we have something stronger and cheaper:
# BIRTHDATE, carried by both sides.
#
# Rules, in order:
#   1. Candidates are ids whose normalised name matches the entrant's.
#   2. If any candidate's birthdate matches the entry list's DoB, keep ONLY
#      those. Several ids may match -- that is one person split across ids, and
#      the right answer is the UNION of their history, not a choice between them.
#   3. If no birthdate matches (or none is recorded), fall back to the candidate
#      with the most history IN THE EVENT BEING FORECAST, which is strictly
#      better than first-by-arbitrary-order.
#   4. Every entry records which rule resolved it, so the card can be audited
#      and the fallbacks counted rather than assumed rare.

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(jsonlite)
D   <- file.path(VERSE, "citiusdata", "data")
OUT <- file.path(D, "birmingham2026_athlete_ids.csv")

# --- entries ------------------------------------------------------------------
j   <- fromJSON(file.path(D, "birmingham2026_entries.json"), simplifyVector = FALSE)
evs <- unlist(j$events)
nz  <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)
e <- rbindlist(lapply(j$rows, function(r) data.table(
  event = evs[r[[1]] + 1], nation = nz(r[[2]]), athlete = nz(r[[3]]),
  dob = nz(r[[4]]), pb = nz(r[[5]]), sb = nz(r[[6]]))), fill = TRUE)
e[, sex := fifelse(grepl("^Women", event), "W", "M")]
e[, discipline := sub("^(Men's|Women's)\\s+", "", event)]
e[, event_id := match_event(discipline, sex)]
e <- e[!is.na(event_id)]
e[, dob_d := as.Date(dob, "%d %b %Y")]
cli::cli_alert_info("{nrow(e)} modellable entr{?y/ies}, {uniqueN(e$event_id)} event{?s}.")

# --- candidate ids ------------------------------------------------------------
champs <- setDT(readRDS(file.path(D, "championship_results.rds")))
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
champs[, aid := as.character(athlete_id)]
champs[, bd  := as.Date(birthdate)]

# One row per (name-key, id): its birthdate and how much history it holds.
cand <- champs[!is.na(athlete_name) & !is.na(aid),
               .(bd = bd[which(!is.na(bd))[1]], n_all = .N), by = .(key = norm(athlete_name), aid)]
# One string, not two: cli_alert_info()'s second positional argument is `id`,
# not more message, so a split message silently drops its second half.
bd_cov <- mean(!is.na(cand$bd))
cli::cli_alert_info(
  "{format(nrow(cand), big.mark = ',')} name-id candidates; birthdate present on {round(100*bd_cov)}%.")
# GATE, not just a report. If `birthdate` is ever renamed or reformatted upstream
# in championship_results.rds, every entrant silently falls through to the
# most-event-history fallback -- which is the coin-toss-over-a-whole-career
# failure this script exists to eliminate. Reported-but-ungated is not enough
# during a hand-run meet where nobody is obliged to read the console.
# Measured 68.1% on the 2026-07-31 corpus; 30% is a floor, not a target.
if (!is.finite(bd_cov) || bd_cov < 0.30) {
  cli::cli_abort(c(
    "Birthdate coverage is {round(100*bd_cov, 1)}% - too low to disambiguate on.",
    i = "Check `birthdate` in championship_results.rds. Without it every entry
         falls back to most-event-history, which is what this script replaced."))
}

# History per (id, event), for the rule-3 fallback.
ev_hist <- champs[!is.na(event_id), .(n_ev = .N), by = .(aid, event_id)]

e[, key := norm(athlete)]
cw <- merge(e[, .(row = .I, key, event_id, athlete, nation, dob_d)], cand,
            by = "key", allow.cartesian = TRUE)
cw <- merge(cw, ev_hist, by = c("aid", "event_id"), all.x = TRUE)
cw[is.na(n_ev), n_ev := 0L]

# Rule 2: birthdate agreement.
cw[, bd_match := !is.na(bd) & !is.na(dob_d) & bd == dob_d]
cw[, any_bd := any(bd_match), by = row]

keep <- rbind(
  cw[any_bd == TRUE  & bd_match == TRUE][, rule := "birthdate"],
  cw[any_bd == FALSE][order(row, -n_ev, -n_all)][, .SD[1], by = row][, rule := "most_event_history"],
  fill = TRUE)
setorder(keep, row)

res <- keep[, .(ids = paste(unique(aid), collapse = "|"), n_ids = uniqueN(aid),
                rule = rule[1], n_ev = sum(n_ev)), by = row]
res <- merge(e[, .(row = .I, athlete, nation, dob, event, event_id)], res, by = "row", all.x = TRUE)
res[is.na(n_ids), `:=`(n_ids = 0L, rule = "unresolved", n_ev = 0L)]

# --- report -------------------------------------------------------------------
cli::cli_h2("Resolution")
print(res[, .(entries = .N), by = rule][order(-entries)])
cli::cli_alert_info("Resolved {res[n_ids > 0, .N]} of {nrow(res)} ({round(100*res[n_ids>0,.N]/nrow(res),1)}%).")
merged <- res[n_ids > 1]
cli::cli_alert_info(
  "{nrow(merged)} entr{?y/ies} resolve to MULTIPLE ids and take the UNION of their history (same athlete split across ids).")
if (nrow(merged)) print(merged[, .(athlete, nation, event, n_ids, rule)])

fb <- res[rule == "most_event_history" & n_ids > 0]
cli::cli_alert_warning(
  "{nrow(fb)} entr{?y/ies} had no birthdate agreement and fell back to most-event-history.")

un <- res[n_ids == 0]
if (nrow(un)) {
  cli::cli_alert_warning("{nrow(un)} entr{?y/ies} unresolved - no history under this name:")
  print(un[, .(athlete, nation, event)])
}

# A resolved entrant with ZERO history in the event being forecast is evidence
# of a bad match, not merely a thin one -- flag it rather than let the field
# prior quietly carry them.
noev <- res[n_ids > 0 & n_ev == 0]
cli::cli_alert_warning("{nrow(noev)} resolved entr{?y/ies} have NO history in the event being forecast.")

fwrite(res, OUT)
cli::cli_alert_success("Wrote {basename(OUT)}: {nrow(res)} rows.")
