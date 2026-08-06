# Birmingham 2026 -- the round structure `simulate_rounds()` needs.
#
# `simulate_rounds()` takes a `structure` list per event:
#   list(list(races = 3, advance = 2, fastest_losers = 2), list(races = 1))
# It cannot infer any of it. This builds that list for all 42 modellable events.
#
# TWO SOURCES, AND THEY ARE NOT EQUALLY SOLID. Keep them apart:
#
#  * SHAPE -- how many rounds, and of what kind -- is read off the OFFICIAL
#    published timetable and is authoritative. Every event's shape below was
#    parsed from the session list, not assumed.
#  * COUNTS -- how many races per round, how many advance automatically, how
#    many fastest losers -- are DERIVED. No progression table exists in the
#    European Athletics regulations or the World Athletics rules: heat counts
#    are set by the Technical Delegates and published with the start lists,
#    typically one to two days before the meet.
#
# So `counts_source` is "derived" everywhere and the card must say so. When the
# start lists publish, override the derived rows and re-run: the shape will not
# change, only the counts, and only the counts affect advancement probability.
#
# The derivation, stated plainly so it can be argued with:
#   - Track finals seat 8. The 1500m and steeplechase final seats 14 -- MEASURED
#     off past European Championships in the corpus, not assumed to be 12.
#   - Semi-finals are 3 races of 8 (24 through), EXCEPT the 800m, which runs 2
#     semis of 8 (16 through). Also measured: assuming a flat 3 semis would push
#     8 extra athletes through round one of the 800m and overstate every
#     advancement probability in it.
#   - Round one seats 8 per race, so races = ceil(field / 8), and it advances
#     enough to fill the next round: automatic qualifiers per race first, the
#     remainder as fastest losers.
#   - Field events run two qualifying groups to a final of 12. The real rule is
#     a threshold ("achieve X, else the best 12"), which `.advancers()` cannot
#     represent. Ticket 13 measured top-12-by-place as a close proxy: three Rome
#     2024 field finals all landed at exactly 12. Modelled as 1 automatic per
#     group plus 10 fastest, which is as near to "best 12 overall" as the
#     available shape allows.
#   - Straight finals and combined events are one round of one race.

VERSE <- "C:/dev/citiusverse"
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table)); library(jsonlite)
D   <- file.path(VERSE, "citiusdata", "data")
OUT <- file.path(D, "birmingham2026_round_structure.csv")

# --- field sizes from the entry list -----------------------------------------
j   <- fromJSON(file.path(D, "birmingham2026_entries.json"), simplifyVector = FALSE)
evs <- unlist(j$events)
e <- rbindlist(lapply(j$rows, function(r) data.table(
  event = evs[r[[1]] + 1], athlete = as.character(r[[3]]))), fill = TRUE)
e[, sex := fifelse(grepl("^Women", event), "W", "M")]
e[, discipline := sub("^(Men's|Women's)\\s+", "", event)]
e[, event_id := match_event(discipline, sex)]
e <- e[!is.na(event_id)]
fld <- e[, .(field = .N), by = .(event_id, discipline, sex)]

# --- shape, read off the official timetable -----------------------------------
# Three-round track events. Men's 400m is labelled "Qualifying > Semi-Final >
# Final" on the timetable rather than "Round One"; same shape, different word.
THREE <- c("100 Metres", "200 Metres", "400 Metres", "800 Metres",
           "100 Metres Hurdles", "110 Metres Hurdles", "400 Metres Hurdles")
TWO   <- c("1500 Metres", "3000 Metres Steeplechase")
FIELD <- c("High Jump", "Pole Vault", "Long Jump", "Triple Jump",
           "Shot Put", "Discus Throw", "Hammer Throw", "Javelin Throw")

shape_of <- function(disc) {
  if (disc %in% THREE) "track3"
  else if (disc %in% TWO) "track2"
  else if (disc %in% FIELD) "field2"
  else "single"          # 5000, 10,000, marathon, race walks, combined events
}
fld[, shape := vapply(discipline, shape_of, character(1))]

cli::cli_h2("Shape, from the published timetable")
print(fld[, .(events = .N), by = shape][order(-events)])
stopifnot("every modellable event must have a shape" = !any(is.na(fld$shape)))

# --- counts, derived ----------------------------------------------------------
LANES <- 8L
split_adv <- function(n_races, target) {
  # Fill `target` places from `n_races` races: as many automatic per race as fit,
  # remainder on time. Automatic qualifiers are what make a heat a race rather
  # than a time trial, so prefer them, but never more than the target allows.
  auto <- max(1L, target %/% n_races)
  while (auto * n_races > target && auto > 1L) auto <- auto - 1L
  list(advance = auto, fastest_losers = max(0L, target - auto * n_races))
}

rows <- list()
for (i in seq_len(nrow(fld))) {
  f <- fld[i]; st <- list()
  if (f$shape == "track3") {
    # MEASURED, not assumed. Pooled over the three European Championships in
    # championship_results.rds, per edition: the 100m runs 3 semis of 8 (24
    # through) to a final of 8, but the 800m runs only 2 semis of 8 (16
    # through). A flat "3 semis" would push 8 extra athletes through round one
    # in the 800m and overstate every advancement probability in it.
    semis <- if (f$discipline == "800 Metres") 2L else 3L
    to_semis <- semis * LANES
    r1 <- max(1L, ceiling(f$field / LANES))
    a1 <- split_adv(r1, min(to_semis, f$field))
    a2 <- split_adv(semis, 8L)                              # 2 auto + 2 fastest
    st <- list(
      list(round = "Round One",  races = r1,    advance = a1$advance, fastest_losers = a1$fastest_losers),
      list(round = "Semi-Final", races = semis, advance = a2$advance, fastest_losers = a2$fastest_losers),
      list(round = "Final",      races = 1L,    advance = NA_integer_, fastest_losers = NA_integer_))
  } else if (f$shape == "track2") {
    # Distance heats are not lane-bound. Final size is measured, not assumed:
    # the European 1500m final seats 14, not the 12 a track final suggests.
    r1 <- max(1L, ceiling(f$field / 15L))
    a1 <- split_adv(r1, min(14L, f$field))
    st <- list(
      list(round = "Round One", races = r1, advance = a1$advance, fastest_losers = a1$fastest_losers),
      list(round = "Final",     races = 1L, advance = NA_integer_, fastest_losers = NA_integer_))
  } else if (f$shape == "field2") {
    st <- list(
      list(round = "Qualifying", races = 2L, advance = 1L, fastest_losers = 10L),
      list(round = "Final",      races = 1L, advance = NA_integer_, fastest_losers = NA_integer_))
  } else {
    st <- list(list(round = "Final", races = 1L, advance = NA_integer_, fastest_losers = NA_integer_))
  }
  for (k in seq_along(st)) rows[[length(rows) + 1L]] <- data.table(
    event_id = f$event_id, discipline = f$discipline, sex = f$sex, field = f$field,
    shape = f$shape, round_index = k, round = st[[k]]$round, races = st[[k]]$races,
    advance = st[[k]]$advance, fastest_losers = st[[k]]$fastest_losers)
}
s <- rbindlist(rows)
s[, `:=`(shape_source = "official_timetable", counts_source = "derived")]

# --- assertions ---------------------------------------------------------------
# A structure that lets fewer athletes through than the next round seats, or
# more, produces advancement probabilities that cannot be right. Check it.
chk <- s[!is.na(advance), .(through = advance * races + fastest_losers), by = .(event_id, round_index)]
nxt <- s[, .(event_id, round_index, races, seats = fifelse(round == "Final", 8L, races * LANES))]
cmp <- merge(chk, nxt[, .(event_id, round_index = round_index - 1L, next_races = races)],
             by = c("event_id", "round_index"))
bad <- s[!is.na(advance) & (advance < 1L | fastest_losers < 0L)]
stopifnot("every non-final round must advance at least one per race" = !nrow(bad))
stopifnot("qualifiers must never exceed the field" =
  !nrow(merge(chk, fld[, .(event_id, field)], by = "event_id")[through > field]))

cli::cli_h2("Structure")
cli::cli_alert_success("{uniqueN(s$event_id)} events, {nrow(s)} rounds.")
print(s[round_index == 1, .(events = .N), by = .(shape, races, advance, fastest_losers)][order(shape)])

fwrite(s, OUT)
cli::cli_alert_success("Wrote {basename(OUT)}.")
cli::cli_alert_warning(
  "COUNTS ARE DERIVED. Override from the official start lists when they publish (~1-2 days out) and re-run; the shape will not change.")
print(s[shape == "track3" & round_index == 1,
        .(discipline, sex, field, races, advance, fastest_losers)][order(discipline, sex)])
