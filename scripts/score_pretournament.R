# How the PRE-TOURNAMENT predictions actually did at Glasgow 2026.
#
# Strictly out of sample: every prediction was generated from a history cut at
# 2026-07-23, the day before the Games opened, so nothing that happened in
# Glasgow informs any number here.
#
# Scored against two benchmarks, because "was the model any good" is meaningless
# without something to be good relative to:
#
#   uniform  - every entrant equally likely. The floor.
#   PB rank  - back the athlete with the best personal best on the entry list.
#              This is what an informed person with the start lists and no model
#              would do, and it is a genuinely strong benchmark in athletics.
#
# Usage:  Rscript scripts/score_pretournament.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

pred <- setDT(readRDS(file.path(OUT, "glasgow2026_pretournament.rds")))
say("Predictions cut at %s, generated %s.", as.character(pred$cutoff[1]),
    format(pred$generated_at[1], "%Y-%m-%d %H:%M"))
say("%s entrant-rows across %d events (%s).", format(nrow(pred), big.mark = ","),
    uniqueN(pred$event_id), paste(unique(pred$sport), collapse = " + "))

# ---- outcomes ---------------------------------------------------------------
# Both sports are keyed onto athlete_key(): the predictions carry a name-derived
# id, athletics results carry World Athletics numeric ids, and the swimming CRS
# scrape carries its own. Sorting name tokens makes the key invariant to
# given-first vs surname-first, which the three sources disagree about.
ath <- setDT(readRDS(file.path(OUT, "glasgow2026_results.rds")))
ath <- ath[!is.na(place) & place > 0L & round == "Final",
           .(sport = "Athletics", event_id, k = athlete_key(athlete_name),
             athlete_name, place)]

swm <- setDT(parse_crs_export(file.path(OUT, "glasgow2026_swimming.json")))
swm <- swm[!is.na(place) & place > 0L &
             grepl("final", round, ignore.case = TRUE) &
             !grepl("semi", round, ignore.case = TRUE),
           .(sport = "Swimming", event_id, k = athlete_key(athlete_name),
             athlete_name, place)]

fin <- rbind(ath, swm)
# One row per athlete per final. Relay legs and any duplicated feed rows would
# otherwise let one person hold two places in the same race.
fin <- unique(fin, by = c("event_id", "k"))
say("\nFinals complete: %d athletics, %d swimming.",
    uniqueN(ath$event_id), uniqueN(swm$event_id))

pred[, k := athlete_key(athlete_name)]
ev <- intersect(unique(fin$event_id), unique(pred$event_id))
say("Predicted and decided: %d event%s.", length(ev), if (length(ev) == 1) "" else "s")

# Score only finals whose winner we had in the field at all. Anything else
# measures entry-list coverage, not the model, and is reported separately.
# Coverage must be checked PER EVENT. Testing `k %in% pred$k` globally passes any
# winner who appears anywhere in the prediction set, including in a different
# event -- so a 5000m won by someone we only rated over 10000m counted as
# covered, then scored with a rank of NA.
win <- fin[place == 1L]
covered <- win[pred[, .(event_id, k)], on = .(event_id, k), nomatch = NULL]$event_id
missed <- setdiff(ev, covered)
if (length(missed)) {
  say("\n%d final%s skipped - winner absent from our field: %s", length(missed),
      if (length(missed) == 1) "" else "s", paste(missed, collapse = ", "))
}
ev <- intersect(ev, covered)

# ---- the PB benchmark -------------------------------------------------------
# Best personal best on the entry list, resolved onto the same key.
pbrank <- tryCatch({
  j <- jsonlite::fromJSON(file.path(OUT, "glasgow2026_entries.json"), simplifyVector = FALSE)
  evs <- unlist(j$events)
  nz <- function(z) if (is.null(z) || !length(z)) NA_character_ else as.character(z)
  e <- rbindlist(lapply(j$rows, function(r) data.table(
    event = evs[r[[1]] + 1], athlete = nz(r[[3]]), pb = nz(r[[5]]))), fill = TRUE)
  e[, sex := fifelse(grepl("^Women", event), "W", fifelse(grepl("^Men", event), "M", NA_character_))]
  e[, event_id := match_event(sub("^(Men's|Women's|Mixed)\\s+", "", event), sex)]
  e <- e[!is.na(event_id) & !is.na(pb)]
  e[, k := athlete_key(athlete)]
  # PBs are marks, so they need the event's orientation to rank: lower is better
  # for a time, higher for a throw. Ranking raw would invert every field event.
  e[citius_events(), on = "event_id", orientation := i.orientation]
  e[, pbn := suppressWarnings(parse_mark(pb))]
  e <- e[!is.na(pbn) & pbn > 0 & !is.na(orientation)]
  e[, score := orientation * log(pbn)]
  e[order(event_id, -score), .(k = k[1]), by = event_id]
}, error = function(err) { say("PB benchmark unavailable: %s", conditionMessage(err)); NULL })

# ---- per-event table --------------------------------------------------------
rows <- rbindlist(lapply(ev, function(e) {
  p <- pred[event_id == e][order(-p_gold)]
  o <- fin[event_id == e]
  w <- o[place == 1L]$k[1]
  medals <- o[place <= 3L]$k
  rk <- which(p$k == w)
  data.table(
    sport = p$sport[1], event_id = e, field = nrow(p),
    our_pick = p$athlete_name[1], our_p = p$p_gold[1],
    winner = o[place == 1L]$athlete_name[1],
    p_on_winner = p[k == w]$p_gold[1],
    winner_rank = if (length(rk)) rk[1] else NA_integer_,
    called = identical(p$k[1], w),
    medals_hit = sum(head(p$k, 3) %in% medals),
    # NA, not FALSE, when this event has no PB benchmark at all. The entry-list
    # JSON is athletics-only, so scoring its absence as a miss reported
    # "PB favourite won 0 of 13" for swimming -- a benchmark that was never
    # computed, presented as one the model beat.
    pb_called = {
      pk <- if (!is.null(pbrank)) pbrank[event_id == e]$k[1] else NA_character_
      if (is.null(pk) || is.na(pk)) NA else identical(pk, w)
    })
}), fill = TRUE)

setorder(rows, sport, -our_p)
say("\n== Every decided final, pre-tournament ==")
print(rows[, .(sport, event = sub("^(AT|SW)-", "", event_id), field,
               our_pick, our_p = round(our_p, 3), winner,
               p_win = round(p_on_winner, 3), rank = winner_rank,
               hit = called)], nrows = 60)

# ---- headline ---------------------------------------------------------------
say("\n== How it did ==")
for (s in c("Athletics", "Swimming", "ALL")) {
  d <- if (s == "ALL") rows else rows[sport == s]
  if (!nrow(d)) next
  p <- pred[event_id %in% d$event_id]
  o <- fin[event_id %in% d$event_id]
  sc <- score_predictions(
    p[, .(race_id = event_id, athlete_id = k, p_gold)],
    o[, .(race_id = event_id, athlete_id = k, hit = place == 1L)], "p_gold")
  say("\n%s - %d final%s", s, nrow(d), if (nrow(d) == 1) "" else "s")
  say("  favourite won        : %d of %d (%.0f%%)", sum(d$called), nrow(d),
      100 * mean(d$called))
  if (!all(is.na(d$pb_called)))
    say("  PB favourite won     : %d of %d (%.0f%%)  <- the no-model benchmark",
        sum(d$pb_called, na.rm = TRUE), sum(!is.na(d$pb_called)),
        100 * mean(d$pb_called, na.rm = TRUE))
  # Numerator and denominator must agree: mean(..., na.rm = TRUE) silently drops
  # NA ranks from the denominator while sum() counts them out of nrow(), which
  # reported 6 of 10 as 75%.
  say("  winner in our top 3  : %d of %d (%.0f%%)",
      sum(d$winner_rank <= 3, na.rm = TRUE), nrow(d),
      100 * sum(d$winner_rank <= 3, na.rm = TRUE) / nrow(d))
  say("  median rank of winner: %.0f of %.0f entrants",
      median(d$winner_rank, na.rm = TRUE), median(d$field))
  say("  mean prob on winner  : %.3f (uniform would give %.3f)",
      mean(d$p_on_winner, na.rm = TRUE), mean(1 / d$field))
  say("  gold Brier %.4f vs %.4f baseline -> skill %+.3f",
      sc$overall$brier, sc$overall$brier_baseline, sc$overall$brier_skill)
  say("  medal picks correct  : %d of %d top-3 slots (%.0f%%)",
      sum(d$medals_hit), 3 * nrow(d), 100 * sum(d$medals_hit) / (3 * nrow(d)))
}

saveRDS(rows, file.path(OUT, "glasgow2026_pretournament_scored.rds"))
say("\nwrote glasgow2026_pretournament_scored.rds")
