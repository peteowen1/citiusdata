# Pure podium-parsing logic shared by harvest_team_podiums.R and
# harvest_team_participation.R -- and, deliberately, by the citius test suite
# (tests/testthat/test-podium-helpers.R), which previously tested a hand-copied
# twin of these rules while the real ones lived here untested. Everything in
# this file is pure: no I/O, no globals, all inputs passed in.
#
# Both rules were wrong on the first attempt and failed silently, which is why
# they are under test at all: shortest-prefix matching assigned "India" inside
# "Indonesia", and the all-missing-only placing fallback missed exactly the
# podium rows.

# Known nations sorted for longest-prefix matching: longest first, so
# "South Africa" wins over any shorter prefix, and "American Samoa" over
# "Samoa".
pp_nation_prefixes <- function(known) {
  known <- unique(known[!is.na(known) & nzchar(known)])
  known[order(-nchar(known))]
}

# The nation a podium/standings cell starts with, or NA -- never a guess.
# Podium cells run the nation straight into the squad with no separator
# ("South AfricaJames MurphyZain Davids...").
#
# `canonicalise`: optional function tried on the WHOLE cleaned cell first.
# Standings tables use abbreviations that are not prefixes of the canonical
# name (the 1992 Unified Team appears as "CIS"), so prefix matching alone
# silently loses them. harvest_team_participation.R passes canonical_nation;
# harvest_team_podiums.R historically did not, and keeps that behaviour.
pp_leading_nation <- function(txt, known_by_len, canonicalise = NULL) {
  s <- trimws(gsub("\u00a0", " ", as.character(txt)))
  s <- gsub("\\[[^]]*\\]", "", s)
  s <- gsub("\\s*\\((H|h)\\)\\s*$", "", s)     # host marker
  s <- gsub("\\s*\\*+$", "", s)
  if (!nzchar(s)) return(NA_character_)
  if (!is.null(canonicalise)) {
    full <- canonicalise(s)
    if (!is.na(full) && full %in% known_by_len) return(full)
  }
  hit <- known_by_len[startsWith(s, known_by_len)]
  if (length(hit)) hit[1] else NA_character_
}

# Fill missing placings from row order, PER ROW. A final-standings table
# renders the top three places as medal icons, so they parse to NA while
# places 4 onward are numeric; falling back to row order only when EVERY
# position is NA missed exactly the podium rows.
pp_fill_missing_places <- function(pos) {
  pos[is.na(pos)] <- seq_along(pos)[is.na(pos)]
  pos
}

# TRUE when a Gold column holds NOC medal COUNTS rather than squad listings.
# Both table kinds carry Gold/Silver/Bronze headers; without this the podium
# parser would re-parse tables the main harvester already handled.
pp_is_noc_count_column <- function(v) {
  v <- trimws(gsub("\\[.*?\\]", "", as.character(v)))
  v <- v[nzchar(v)]
  length(v) > 0 && mean(grepl("^[0-9]+$", v)) >= 0.8
}
