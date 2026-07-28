# Export Glasgow 2026 data for the In The Game blog (inthegame.blog/games).
#
# Re-runnable end to end: harvests whatever the feed has, rebuilds every
# artefact from scratch, uploads to R2. A later run supersedes an earlier one.
#
# There is no CI for this yet (citiusdata#2) — it is run by hand through the
# Games. That makes staleness the default failure mode rather than an edge case,
# so every artefact carries `generated_at` and the site renders an "as at" stamp
# off it. Do not remove that column to tidy the schema.
#
# The one thing to understand before editing: **expected and actual medals must
# be counted over the same set of events**, or the two columns sitting next to
# each other on the hub are not comparable and the page lies. Swimming has no
# predictions at all (citiusdata#3) and relays are unmodelled (citius#1), so
# both are excluded from the model-scope table and reported separately under
# `scope == "official"`. See the medal section below.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(jsonlite); library(arrow)

OUT     <- here::here("citiusdata", "data")
BLOG    <- here::here("citiusdata", "blog")
GLASGOW <- 7187518L
# Predictions were locked from data strictly before the first day of competition.
# Anything at or after this date is an outcome, never evidence — see citiusdata#1.
GAMES_START <- as.Date("2026-07-27")
BUCKET  <- "inthegame-data"
PREFIX  <- "games"
NOW     <- Sys.time()

dir.create(BLOG, recursive = TRUE, showWarnings = FALSE)

# --- athletics results -------------------------------------------------------
# Prefer a live harvest; fall back to the last saved copy so a feed outage
# degrades to stale-but-labelled rather than to an empty page.
ath <- tryCatch(setDT(harvest_competitions(GLASGOW)), error = function(e) NULL)
# `harvest_ok` is published to the site. Without it the freshness banner tracks
# when this SCRIPT ran, not how old the DATA is: run it during a feed outage and
# the page shows a green "as at just now" over yesterday's results, which is the
# exact failure the banner exists to prevent.
harvest_ok <- !(is.null(ath) || !nrow(ath))
if (!harvest_ok) {
  cli::cli_alert_warning("Live harvest failed or empty; falling back to saved results.")
  ath <- setDT(readRDS(file.path(OUT, "glasgow2026_results.rds")))
} else {
  saveRDS(ath, file.path(OUT, "glasgow2026_results.rds"))
}
cli::cli_alert_info("Athletics: {nrow(ath)} result{?s}, {uniqueN(ath$race_key)} race{?s}.")

# --- swimming results --------------------------------------------------------
# Glasgow swimming ran 24-26 Jul and is FINISHED. This is an archive, not a
# feed: it refreshes only when someone re-runs the CRS browser recipe.
swim <- tryCatch(setDT(parse_crs_export(file.path(OUT, "glasgow2026_swimming.json"))),
                 error = function(e) NULL)
# Catching `error` is NOT enough: parse_crs_export() returns an EMPTY table
# without erroring when the export has no rows (a truncated or malformed manual
# CRS scrape). Both paths collapse to zero swimming rows, and the site claims
# "across athletics and swimming" in its medal-table caption — so a whole
# finished sport can vanish while the page insists it is included. Publish the
# count and let the page tell the truth.
if (is.null(swim)) {
  cli::cli_alert_warning("Swimming export failed to parse; continuing with athletics only.")
  swim <- data.table()
}
swim_rows <- nrow(swim)
if (!swim_rows) {
  cli::cli_alert_warning("Swimming contributed ZERO rows - the medal table will exclude it.")
}

# --- unify -------------------------------------------------------------------
# The two feeds carry different columns by nature: athletics has wind and no
# nation, swimming has reaction time and lane. Keep the union, fill NA, and do
# not pretend a missing column is a zero.
KEEP <- c("sport", "event_id", "discipline", "sex", "round", "race_key", "date",
          "athlete_id", "athlete_name", "nation", "place", "mark_string", "mark",
          "perf", "wind", "reaction_time")

norm_results <- function(d) {
  if (!nrow(d)) return(NULL)
  d <- copy(d)
  if (!"sex" %in% names(d) && "sex_code" %in% names(d)) d[, sex := sex_code]
  if (!"nation" %in% names(d)) {
    d[, nation := if ("country" %in% names(d)) country else NA_character_]
  }
  for (nm in setdiff(KEEP, names(d))) d[, (nm) := NA]
  d[, athlete_id := as.character(athlete_id)]
  d[, ..KEEP]
}

results <- rbindlist(list(norm_results(ath), norm_results(swim)), fill = TRUE)

# A final is the medal race. Semifinals match "final" as a substring, which is
# how a scoring script quietly scores the wrong races.
results[, is_final := grepl("final", round, ignore.case = TRUE) &
                      !grepl("semi", round, ignore.case = TRUE)]
# Relays are team events that citius does not model (citius#1). They resolve to
# NA event_id by design; matching on the discipline name too catches any that
# slip through a future registry change.
results[, is_relay := is.na(event_id) | grepl("relay", discipline, ignore.case = TRUE)]

# --- nation lookup -----------------------------------------------------------
# The World Athletics competition feed carries no athlete nationality, so the
# official entry list is the only route to one for athletics. Swimming carries
# it natively from the CRS scrape.
entries <- fromJSON(file.path(OUT, "glasgow2026_entries.json"), simplifyVector = FALSE)
ev_names <- unlist(entries$events)
nz <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)
ent <- rbindlist(lapply(entries$rows, function(r) data.table(
  event = ev_names[r[[1]] + 1], nation = nz(r[[2]]), athlete = nz(r[[3]]))), fill = TRUE)
normkey <- function(x) gsub("[^A-Z]", "", toupper(x))
nat_lookup <- unique(ent[, .(key = normkey(athlete), nation)])[, .(nation = nation[1]), by = key]

add_nation <- function(d, name_col) {
  d[, .k := normkey(get(name_col))]
  d[nat_lookup, nation_fill := i.nation, on = c(.k = "key")]
  if (!"nation" %in% names(d)) d[, nation := NA_character_]
  d[is.na(nation), nation := nation_fill]
  d[, c(".k", "nation_fill") := NULL]
  d[]
}

results <- add_nation(results, "athlete_name")
miss <- results[is_final == TRUE & !is.na(place) & place <= 3L & is.na(nation), .N]
if (miss) cli::cli_alert_warning("{miss} medallist{?s} still have no nation.")

# --- predictions -------------------------------------------------------------
# TWO prediction sets, and conflating them would destroy the report card.
#
#   forecast - the locked pre-Games entry-list run. Made before a single race
#              was run, so it is the only thing honest to SCORE. Pinned by
#              filename, never "newest file": selecting by mtime picks up the
#              live re-runs below and silently scores the model on races it has
#              already watched.
#   live     - in-Games re-predictions for events still in progress, conditioned
#              on completed heats (p_reach_r2/p_reach_r3/p_final). Legitimate
#              and more accurate, but it is NOT a forecast and is never scored.
pick_newest <- function(pattern) {
  f <- list.files(OUT, pattern = pattern, full.names = TRUE)
  if (!length(f)) return(NULL)
  f[which.max(file.info(f)$mtime)]
}

locked <- pick_newest("^glasgow2026_entrylist_predictions_.*parquet$")
if (is.null(locked)) cli::cli_abort("No locked entry-list prediction file found.")
fc <- setDT(read_parquet(locked))
# pick_newest() matches on filename and mtime, never content. A stray file that
# matches the glob with a newer mtime would sail through and silently zero the
# whole "Against the model" comparison — which renders on the site as an absent
# section, reading as "nothing to show" rather than "something is broken".
if (!nrow(fc)) cli::cli_abort("Locked forecast {.file {basename(locked)}} has ZERO rows.")
fc[, `:=`(athlete_id = as.character(athlete_id), basis = "forecast",
          source_file = basename(locked))]

live_file <- pick_newest("^glasgow2026_live_predictions_.*parquet$")
lv <- NULL
if (!is.null(live_file)) {
  lv <- setDT(read_parquet(live_file))
  lv[, `:=`(athlete_id = as.character(athlete_id), basis = "live",
            source_file = basename(live_file))]
  # The live run carries athlete_name and no nation; align it onto the forecast
  # schema so one table can hold both without a page having to know which is which.
  if (!"athlete" %in% names(lv) && "athlete_name" %in% names(lv)) {
    lv[, athlete := athlete_name][, athlete_name := NULL]
  }
  lv <- add_nation(lv, "athlete")
  lv <- merge(lv, citius_events()[, .(event_id, discipline, sex)], by = "event_id", all.x = TRUE)
}

# Swimming (citiusdata#3). The meet finished on 26 Jul, so these are NOT a
# forecast: ability is estimated from World Aquatics history dated before the
# Games and scored after the fact. Tagged "retrospective" and kept out of both
# the model-scope medal table and the report card's headline, because the claim
# those make is "we published this before a race was run" and this did not exist
# until two days after the meet ended. It is published because a forward-test on
# an independent feed is worth showing — labelled as what it is.
swim_file <- pick_newest("^glasgow2026_swimming_predictions.*parquet$")
rt <- NULL
if (!is.null(swim_file)) {
  rt <- setDT(read_parquet(swim_file))
  rt[, `:=`(athlete_id = as.character(athlete_id), basis = "retrospective",
            source_file = basename(swim_file))]
  if (!"athlete" %in% names(rt) && "athlete_name" %in% names(rt)) {
    rt[, athlete := athlete_name][, athlete_name := NULL]
  }
  # The swimming feed carries nation on the RESULTS, not the predictions, so the
  # lookup is built from the scraped results rather than the athletics entry list.
  # Read off `results` (already normalised, swimming's `country` mapped to
  # `nation`), not the raw `swim` parse, which still calls the column `country`.
  swim_nat <- unique(results[sport == "Swimming" & !is.na(nation),
                             .(key = normkey(athlete_name), nation)]
                     )[, .(nation = nation[1]), by = key]
  rt[, .k := normkey(athlete)]
  rt[swim_nat, nation := i.nation, on = c(.k = "key")]
  rt[, .k := NULL]
  rt <- merge(rt, citius_events()[, .(event_id, discipline, sex)],
              by = "event_id", all.x = TRUE)
  cli::cli_alert_info("Swimming retrospective: {nrow(rt)} row{?s} across {uniqueN(rt$event_id)} event{?s}.")
}

pred <- rbindlist(list(fc, lv, rt), fill = TRUE)
cli::cli_alert_info(
  "Predictions: forecast {nrow(fc)} row{?s}/{uniqueN(fc$event_id)} event{?s} from {.file {basename(locked)}}; live {if (is.null(lv)) 0L else nrow(lv)} row{?s}.")

# The locked forecast must never have seen a Glasgow result (citiusdata#1).
if (!is.null(fc$generated_at) && any(as.Date(fc$generated_at) >= GAMES_START)) {
  cli::cli_alert_warning(
    "Locked forecast was generated on/after the first day of competition - verify it excluded competition {GLASGOW} before publishing a report card.")
}

# --- medal tables ------------------------------------------------------------
# TWO scopes, emitted as one table with a `scope` column so a page cannot
# accidentally put them in the same column and imply they are comparable.
#
#   official - every medal awarded, relays and swimming included. What a
#              newspaper prints. Has no expected counterpart.
#   model    - only events we predicted: individual athletics finals. Expected
#              and actual are counted over exactly the same events, and
#              `expected_todate` is restricted to finals already run, so the
#              two numbers on the hub answer the same question.
medallists <- results[is_final == TRUE & !is.na(place) & place >= 1L & place <= 3L]

official <- medallists[, .(gold   = sum(place == 1L),
                           silver = sum(place == 2L),
                           bronze = sum(place == 3L)), by = nation]
official[, `:=`(scope = "official", expected_todate = NA_real_,
                expected_full = NA_real_, expected_medals_full = NA_real_)]

# Expected always comes from the LOCKED forecast. Using the live re-predictions
# here would mean the "expected" column already knew the heat results.
predicted_events <- unique(fc$event_id)
done_events <- unique(medallists[!is_relay & event_id %in% predicted_events]$event_id)

model_actual <- medallists[!is_relay & event_id %in% predicted_events,
                           .(gold   = sum(place == 1L),
                             silver = sum(place == 2L),
                             bronze = sum(place == 3L)), by = nation]
model_exp <- fc[, .(expected_full = sum(p_gold),
                    expected_medals_full = sum(p_medal)), by = nation]
model_exp_todate <- fc[event_id %in% done_events, .(expected_todate = sum(p_gold)), by = nation]

model <- Reduce(function(a, b) merge(a, b, by = "nation", all = TRUE),
                list(model_actual, model_exp, model_exp_todate))
for (nm in c("gold", "silver", "bronze")) model[is.na(get(nm)), (nm) := 0L]
model[, scope := "model"]

medals <- rbindlist(list(official, model), fill = TRUE)
medals[, `:=`(events_scored = length(done_events),
              events_predicted = length(predicted_events),
              generated_at = NOW)]
setorder(medals, scope, -gold, -silver, -bronze)

# --- scorecard ---------------------------------------------------------------
# Everything score_glasgow2026.R prints, returned as data instead of cat().
card <- list(generated_at = format(NOW, "%Y-%m-%dT%H:%M:%S%z"),
             games = "Commonwealth Games 2026", venue = "Glasgow",
             prediction_file = basename(locked),
             scored_basis = "forecast",
             # Freshness of the DATA, as distinct from freshness of this run.
             # `generated_at` alone cannot tell the site whether the numbers are
             # current: it is Sys.time() whether the feed answered or not.
             harvest_ok = harvest_ok,
             swim_rows = swim_rows,
             results_through = as.character(max(results$date, na.rm = TRUE)))

finals <- results[is_final == TRUE & !is.na(place) & place > 0L & !is_relay &
                    event_id %in% predicted_events]
if (nrow(finals)) {
  # Scored against the LOCKED forecast only — never the live re-predictions.
  p <- fc[, .(race_id = event_id, athlete_id, p_gold, p_medal)]
  o <- finals[, .(race_id = event_id, athlete_id,
                  hit = place == 1L, hit_medal = place <= 3L)]
  # Only score races whose actual winner we predicted at all; otherwise this
  # measures entry-list coverage rather than the model.
  keep <- o[, .(ok = any(hit)), by = race_id][ok == TRUE]$race_id
  card$races_scored <- length(keep)
  card$races_skipped_winner_absent <- length(setdiff(unique(finals$event_id), keep))

  if (length(keep)) {
    g <- score_predictions(p[race_id %in% keep], o[race_id %in% keep], "p_gold")
    m <- score_predictions(p[race_id %in% keep],
                           o[race_id %in% keep, .(race_id, athlete_id, hit = hit_medal)],
                           "p_medal")
    card$gold  <- g$overall[c("brier", "brier_baseline", "brier_skill", "n_races")]
    card$medal <- m$overall[c("brier", "brier_baseline", "brier_skill")]
    card$reliability <- g$reliability

    top <- p[race_id %in% keep][order(race_id, -p_gold)][, .SD[1], by = race_id]
    top <- merge(top, o[hit == TRUE, .(race_id, winner = athlete_id)], by = "race_id")
    card$favourite_won <- sum(top$athlete_id == top$winner)
    card$favourite_of  <- nrow(top)

    # Where the winner ranked in our list. "Favourite won" is a harsh binary in a
    # 12-strong field; rank shows whether a miss was a near-miss.
    rk <- p[race_id %in% keep][order(race_id, -p_gold)][,
            .(rank = which(athlete_id == o[race_id == .BY$race_id & hit == TRUE]$athlete_id[1]),
              field = .N), by = race_id]
    card$winner_rank <- rk

    # Probability sitting on athletes who never contested the final is a FIELD
    # error, not a model error. Report it rather than renormalise it away.
    contested <- unique(results[is_final == TRUE & event_id %in% keep, .(race_id = event_id, athlete_id)])
    ghost <- p[race_id %in% keep][!contested, on = .(race_id, athlete_id)]
    ghost[, eliminated := athlete_id %in% unique(results$athlete_id)]
    card$nonstarter_gold_mass <- list(
      eliminated_earlier = sum(ghost[eliminated == TRUE]$p_gold),
      never_at_games     = sum(ghost[eliminated == FALSE]$p_gold))
  }
} else {
  card$races_scored <- 0L
}

# --- swimming retrospective, scored SEPARATELY -------------------------------
# Its own key in the scorecard, never folded into card$gold. The headline claim
# is about a published pre-Games forecast; this is a post-hoc forward-test on a
# different sport and a different feed, and averaging the two would quietly
# launder one into the other.
if (!is.null(rt) && nrow(rt)) {
  sf <- results[sport == "Swimming" & is_final == TRUE & !is.na(place) &
                  place > 0L & !is_relay & event_id %in% unique(rt$event_id)]
  if (nrow(sf)) {
    # The two swimming feeds do NOT share an athlete id: predictions carry World
    # Aquatics UUIDs, the CRS results carry their own. They are joined on
    # athlete_key(), which sorts name tokens so "Hannah STERRY" and
    # "STERRY Hannah" agree - the same key the upstream script uses. Matching is
    # partial by nature (many Commonwealth swimmers have no World Aquatics
    # history at all), which is why races whose WINNER is unmatched are dropped
    # below rather than counted as misses.
    ps <- rt[, .(race_id = event_id, athlete_id = athlete_key(athlete), p_gold, p_medal)]
    os <- sf[, .(race_id = event_id, athlete_id = athlete_key(athlete_name),
                 hit = place == 1L, hit_medal = place <= 3L)]
    ps <- ps[!is.na(athlete_id)]; os <- os[!is.na(athlete_id)]
    # A race counts only if the actual winner is in the predicted field. Without
    # this, the score measures how many swimmers World Aquatics has a history
    # for, not how good the model is.
    # A race counts only if the actual winner is in the predicted field.
    # Otherwise the score measures how many swimmers World Aquatics happens to
    # have a history for, not how good the model is.
    keep_s <- merge(os[hit == TRUE, .(race_id, athlete_id)],
                    ps[, .(race_id, athlete_id)],
                    by = c("race_id", "athlete_id"))$race_id
    card$swimming_retro <- list(
      basis = "retrospective",
      note = "Model re-run on data available before the meet; not a published forecast.",
      races_scored = length(keep_s),
      races_skipped_winner_unrated = length(setdiff(unique(sf$event_id), keep_s)))
    if (length(keep_s)) {
      gs <- score_predictions(ps[race_id %in% keep_s], os[race_id %in% keep_s], "p_gold")
      ms <- score_predictions(ps[race_id %in% keep_s],
                              os[race_id %in% keep_s, .(race_id, athlete_id, hit = hit_medal)],
                              "p_medal")
      card$swimming_retro$gold  <- gs$overall[c("brier", "brier_baseline", "brier_skill", "n_races")]
      card$swimming_retro$medal <- ms$overall[c("brier", "brier_baseline", "brier_skill")]
      tops <- ps[race_id %in% keep_s][order(race_id, -p_gold)][, .SD[1], by = race_id]
      tops <- merge(tops, os[hit == TRUE, .(race_id, winner = athlete_id)], by = "race_id")
      card$swimming_retro$favourite_won <- sum(tops$athlete_id == tops$winner)
      card$swimming_retro$favourite_of  <- nrow(tops)
    }
  }
}

# --- events registry ---------------------------------------------------------
reg <- citius_events()[, .(event_id, sport, discipline, sex, family, tactical, technical)]
status <- results[, .(results = .N,
                      races = uniqueN(race_key),
                      final_done = any(is_final & !is.na(place)),
                      last_date = max(date, na.rm = TRUE)), by = event_id]
# Every event we predicted OR have results for. Restricting to events with
# results would hide the 26 athletics events still to come, which is most of the
# schedule and exactly what a reader visiting mid-Games wants to see.
ids <- union(status[!is.na(event_id)]$event_id, predicted_events)
events <- merge(reg[event_id %in% ids], status[!is.na(event_id)], by = "event_id", all.x = TRUE)
events[is.na(results), `:=`(results = 0L, races = 0L, final_done = FALSE)]
events[, `:=`(predicted = event_id %in% predicted_events, generated_at = NOW)]

# --- athlete history ---------------------------------------------------------
# Career marks for the athletes actually at these Games, so the site can show
# what a result means against a career rather than in isolation. Scoped to
# Glasgow participants on purpose: the full 308k-row history is the wrong thing
# to put behind a per-athlete page, and shipping it would put megabytes on the
# wire for a page that reads one athlete at a time.
HIST_COLS <- c("athlete_id", "athlete_name", "date", "event_id", "discipline",
               "sex_code", "round", "mark_string", "mark", "perf", "place",
               "wind", "tier", "venue_city", "age", "comp_name")
who <- unique(c(as.character(results$athlete_id), as.character(pred$athlete_id)))
who <- who[!is.na(who)]
hist <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
hist <- hist[as.character(athlete_id) %in% who]
for (nm in setdiff(HIST_COLS, names(hist))) hist[, (nm) := NA]
hist <- hist[, ..HIST_COLS]
setnames(hist, "sex_code", "sex")
hist[, `:=`(athlete_id = as.character(athlete_id), generated_at = NOW)]
setorder(hist, athlete_id, -date)
cli::cli_alert_info(
  "Athlete history: {nrow(hist)} row{?s} for {uniqueN(hist$athlete_id)} of {length(who)} Glasgow athlete{?s}.")

# --- write + upload ----------------------------------------------------------
pred[, generated_at_export := NOW]
results[, generated_at := NOW]

artefacts <- list(
  "cg2026-predictions.parquet" = pred,
  "cg2026-results.parquet"     = results,
  "cg2026-medals.parquet"      = medals,
  "cg2026-athlete-history.parquet" = hist,
  "events.parquet"             = events)

for (nm in names(artefacts)) {
  write_parquet(artefacts[[nm]], file.path(BLOG, nm))
  cli::cli_alert_success("{nm}: {nrow(artefacts[[nm]])} row{?s}")
}
write_json(card, file.path(BLOG, "cg2026-scorecard.json"), auto_unbox = TRUE,
           digits = 6, pretty = TRUE, na = "null")

upload <- function(f) {
  key <- sprintf("%s/%s/%s", BUCKET, PREFIX, f)
  # shQuote is not optional: the cache-control value contains a space, and
  # system2() does no quoting on Windows, so it arrives as two arguments and
  # wrangler reports a baffling error about --file instead.
  args <- c("r2", "object", "put", shQuote(key), "--file", shQuote(file.path(BLOG, f)),
            "--cache-control", shQuote("public, max-age=300"), "--remote")
  st <- suppressWarnings(system2("wrangler", args, stdout = TRUE, stderr = TRUE))
  ok <- is.null(attr(st, "status")) || attr(st, "status") == 0
  if (ok) cli::cli_alert_success("uploaded {key}")
  else cli::cli_alert_danger("FAILED {key}: {paste(tail(st, 3), collapse = ' ')}")
  ok
}

if (!nzchar(Sys.getenv("CITIUS_SKIP_UPLOAD"))) {
  # The scorecard carries the freshness stamp the whole site trusts, so it goes
  # LAST and only if every data artefact landed. Uploading it unconditionally
  # publishes a brand-new "as at just now" banner over a medal table that failed
  # to upload and is a run behind — fresh-looking and wrong, with nothing on the
  # page contradicting it.
  ok <- vapply(names(artefacts), upload, logical(1))
  if (!all(ok)) {
    cli::cli_abort(c(
      "{sum(!ok)} data upload{?s} failed - scorecard NOT uploaded.",
      i = "R2 still serves the previous run's scorecard, so the site stays
           self-consistent. Re-run once the cause is fixed."))
  }
  if (!upload("cg2026-scorecard.json")) cli::cli_abort("Scorecard upload failed.")
} else {
  cli::cli_alert_info("CITIUS_SKIP_UPLOAD set - wrote to {.file {BLOG}} only.")
}
