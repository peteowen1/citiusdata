# Glasgow 2026: PRE-TOURNAMENT predictions for every event, both sports.
#
# "Pre-tournament" is taken literally: ability is estimated only from results
# dated before the Games opened, and the field is the published entry list --
# not the athletes who turned out to make the final, which would be hindsight.
#
# The existing prediction files are not pre-tournament. The athletics one was
# generated on 27 July with athletics already underway; the swimming one on
# 28 July, by which point most swimming had finished.
#
# Usage:  Rscript scripts/predict_glasgow_pretournament.R
VERSE <- "C:/dev/citiusverse"
suppressMessages({library(citius); library(data.table)})
D <- file.path(VERSE, "citiusdata", "data")
# Model inputs come from DEPLOYED, never from literals here. See _deployed.R.
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
`%||%` <- function(a, b) if (is.null(a)) b else a
say <- function(...) cat(sprintf(...), "\n", sep = "")
N_SIMS <- 20000L
CUT <- as.Date("2026-07-23")          # the Games opened on the 24th
say("cut-off: %s -- nothing on or after this date informs any estimate", CUT)

xw <- setDT(arrow::read_parquet(file.path(D, "athlete_crosswalk_swimming.parquet")))
person <- unique(xw[source == "crs_glasgow2026" & !is.na(athlete_name),
                    .(athlete_name, person_id)])

# ---- swimming --------------------------------------------------------------
say("\n=== swimming ===")
g <- glasgow_swimming(D)[!is.na(event_id)]
sw <- setDT(readRDS(file.path(D, "swimming_corpus.rds")))
cal_sw <- readRDS(file.path(D, "calibration_swimming.rds"))
# Leak guard, asserted rather than assumed: no Glasgow swim may reach the model.
sw <- sw[is.na(date) | date < CUT]
stopifnot("history must not contain the Games" = !any(sw$date >= CUT, na.rm = TRUE))

# The FIELD is everyone entered, taken from the heats -- using finalists would
# be hindsight, since who reached the final is a result.
field_sw <- unique(g[, .(athlete_name, country, event_id)])
field_sw <- merge(field_sw, person, by = "athlete_name", all.x = TRUE)
# Every entrant carries a person_id whether or not it reaches any history, so
# counting non-NA person_ids measures nothing. Count entrants who actually
# appear in the pre-cut history.
have <- unique(sw$athlete_id)
u <- unique(field_sw, by = "athlete_name")
say("entrants %s | with pre-Games history %s (%.0f%%)",
    format(nrow(u), big.mark = ","),
    format(sum(u$person_id %in% have), big.mark = ","),
    100 * mean(u$person_id %in% have))

ab_sw <- estimate_ability(sw[!is.na(perf)], as_of = CUT, half_life = 180,
                          calibration = cal_sw)
# A spread no athlete could have means a merged identity, not a wild swimmer.
ab_sw <- drop_impossible_sigma(ab_sw)
ab_sw <- temper_unevidenced(ab_sw)
say("  tempered %d athlete-event(s) with no usable evidence", attr(ab_sw, "tempered") %||% 0L)
if (!is.null(attr(ab_sw, "dropped"))) {
  d <- attr(ab_sw, "dropped")
  say("  dropped %d athlete-event(s) with an impossible spread (merged identities):", nrow(d))
  print(d)
}

sim_event <- function(field, ab, ev, cal, label) {
  f <- field[event_id == ev & !is.na(person_id)]
  if (!nrow(f)) return(NULL)
  # size = the whole entry field. project_field defaults to 8, which selects a
  # FINAL -- pre-tournament the question is who wins out of everyone entered,
  # and picking the eight best beforehand would assume the answer.
  proj <- tryCatch(project_field(ab[athlete_id %in% f$person_id],
                                 event = ev, as_of = CUT, size = nrow(f)),
                   error = function(e) NULL)
  if (is.null(proj) || !nrow(proj)) return(NULL)
  sim <- tryCatch(simulate_event(proj, n_sims = N_SIMS, calibration = cal),
                  error = function(e) NULL)
  if (is.null(sim)) return(NULL)
  # simulate_event returns the raw simulation matrices; medal_probs() reduces
  # them to per-athlete probabilities.
  s <- tryCatch(medal_probs(sim), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(NULL)
  s <- merge(s, unique(f[, .(person_id, athlete_name, country)]),
             by.x = "athlete_id", by.y = "person_id", all.x = TRUE)
  s[, `:=`(event_id = ev, sport = label)]
  s[]
}

res_sw <- rbindlist(lapply(unique(field_sw$event_id),
                           function(e) sim_event(field_sw, ab_sw, e, cal_sw, "Swimming")),
                    fill = TRUE)
say("simulated %d of %d swimming events", uniqueN(res_sw$event_id),
    uniqueN(field_sw$event_id))

# ---- athletics -------------------------------------------------------------
say("\n=== athletics ===")
res_at <- data.table()
f_ent <- file.path(D, "glasgow2026_entries.json")
f_hist <- file.path(D, "championship_results.rds")
if (file.exists(f_ent) && file.exists(f_hist)) {
  j <- jsonlite::fromJSON(f_ent, simplifyVector = FALSE)
  ent <- rbindlist(lapply(j$rows, function(r) data.table(
    event = j$events[[r[[1]] + 1L]], country = r[[2]], athlete_name = r[[3]],
    dob = r[[4]])), fill = TRUE)
  at <- setDT(readRDS(f_hist))
  at <- at[is.na(date) | date < CUT]
  # Athletics history stays on the competition harvest here, NOT the corpus:
  # entrants are joined to history through the crosswalk's `person_id`, which is
  # built from this feed. Swapping the history without rebuilding the crosswalk
  # would drop coverage silently rather than error.
  cal_at <- deployed_calibration(D)

  xwa <- setDT(arrow::read_parquet(file.path(D, "athlete_crosswalk_athletics.parquet")))
  pa <- unique(xwa[source == "crs_glasgow2026" & !is.na(athlete_name),
                   .(athlete_name, person_id)])
  # The crosswalk's person_id for athletics is keyed on the World Athletics id
  # where one was matched, so join history through it.
  hist_person <- unique(xwa[source == "worldathletics", .(athlete_id, person_id)])
  at[hist_person, on = "athlete_id", person_id := i.person_id]

  ent[, event_id := match_event(sub("^(Men's|Women's)\\s+", "", event),
                                fifelse(grepl("^Women", event), "W", "M"))]
  fa <- unique(ent[!is.na(event_id), .(athlete_name, country, event_id)])
  fa <- merge(fa, pa, by = "athlete_name", all.x = TRUE)
  have_a <- unique(at[!is.na(person_id)]$person_id)
  ua <- unique(fa, by = "athlete_name")
  say("entrants %s | with pre-Games history %s (%.0f%%)",
      format(nrow(ua), big.mark = ","),
      format(sum(ua$person_id %in% have_a), big.mark = ","),
      100 * mean(ua$person_id %in% have_a))

  ab_at <- deployed_ability(at[!is.na(perf) & !is.na(person_id),
                               .(athlete_id = person_id, event_id, date, perf,
                                 round, competition_id)],
                            as_of = CUT, calibration = cal_at)
  # Same two guards as swimming. Athletics history comes through a different
  # crosswalk but the failure modes are identical -- a merged identity gives an
  # impossible spread, and an athlete whose evidence has decayed to nothing is
  # credited for the not-knowing.
  ab_at <- drop_impossible_sigma(ab_at)
  if (!is.null(attr(ab_at, "dropped"))) {
    say("  dropped %d athlete-event(s) with an impossible spread:", nrow(attr(ab_at, "dropped")))
    print(attr(ab_at, "dropped"))
  }
  ab_at <- temper_unevidenced(ab_at)
  say("  tempered %d athlete-event(s) with no usable evidence", attr(ab_at, "tempered") %||% 0L)
  res_at <- rbindlist(lapply(unique(fa$event_id),
                             function(e) sim_event(fa, ab_at, e, cal_at, "Athletics")),
                      fill = TRUE)
  say("simulated %d of %d athletics events", uniqueN(res_at$event_id),
      uniqueN(fa$event_id))
}

all <- rbindlist(list(res_sw, res_at), fill = TRUE)
all[, generated_at := Sys.time()][, cutoff := CUT]
# Output name carries the configuration tag, so re-running under a promoted
# model cannot overwrite the artefact an already-published score was measured
# on. The 2026-07-29 run scored 13 of 21 finals at gold skill +0.375; that claim
# is only checkable while the file behind it still exists.
TAG <- Sys.getenv("CITIUS_PRED_TAG", "")
base <- paste0("glasgow2026_pretournament", if (nzchar(TAG)) paste0("_", TAG) else "")
all[, config := if (nzchar(TAG)) TAG else "2026-07-29 baseline"]
saveRDS(all, file.path(D, paste0(base, ".rds")))
arrow::write_parquet(all, file.path(D, paste0(base, ".parquet")))
say("\n%s rows | %s events | wrote %s.{rds,parquet}",
    format(nrow(all), big.mark = ","), uniqueN(all$event_id), base)
