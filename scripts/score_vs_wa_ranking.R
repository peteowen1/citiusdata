# DOES THE MODEL ORDER A CHAMPIONSHIP FIELD BETTER THAN WORLD ATHLETICS' OWN
# RANKING, USING THE RANKING AS IT STOOD BEFORE THE MEET?
#
# WHY THIS ONE MATTERS MORE THAN THE OTHERS. Every benchmark this model has been
# scored against - season best, personal best, last race, mean of last three -
# is computed by us, from our corpus, with our cleaning, our event mapping and
# our race keys. A shared upstream defect would flatter the model against all of
# them at once and none would notice. The WA ranking is built by someone else,
# from their own database, by a published points system. It is the first
# genuinely external check here, and the first that can disagree for reasons
# that are not our own bugs.
#
# LEAKAGE. The undated `currentWorldRankings` on the athlete profile CANNOT be
# used for this: it is a rolling 12-18 month window scraped today, so for a 2024
# race it already contains the race being predicted, and the ranking would
# "predict" it near-perfectly. That is why this reads wa_rankings_dated.parquet,
# harvested with an explicit rankDate BEFORE each meet, and asserts that below
# rather than trusting the filename.
#
# WHAT A LOSS WOULD MEAN. WA's ranking is a points average over a rolling window
# with placing bonuses - closer to a season-form table than to a rating. If it
# beats the model on championship fields, the gap is a target, not a curiosity:
# it is exactly the population LA 2028 projections are made on.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

# WA url slug -> our event_id. WRITTEN OUT, NOT DERIVED. The two naming schemes
# are unrelated ("110mh" against "AT-110MetresHurdles-M"), so any rule that
# looked like it worked would be a coincidence that breaks on the next event. A
# slug that maps to nothing drops that event silently, so the map is asserted
# complete against the file below.
MAP <- data.table(
  event_slug = c("100m","200m","400m","800m","1500m","5000m","10000m","marathon",
                 "110mh","100mh","400mh","high-jump","pole-vault","long-jump",
                 "triple-jump","shot-put","discus-throw","hammer-throw",
                 "javelin-throw"),
  stem = c("100Metres","200Metres","400Metres","800Metres","1500Metres",
           "5000Metres","10000Metres","Marathon","110MetresHurdles",
           "100MetresHurdles","400MetresHurdles","HighJump","PoleVault",
           "LongJump","TripleJump","ShotPut","DiscusThrow","HammerThrow",
           "JavelinThrow"))

w <- setDT(read_parquet(file.path(D, "wa_rankings_dated.parquet")))
w[, athlete_id := as.character(athlete_id)]
w <- merge(w, MAP, by = "event_slug", all.x = TRUE)
# AN UNMAPPED SLUG IS A FAULT, NOT A NOTE. The first version of this file
# printed a note and carried on - and javelin, which had been harvested and was
# sitting in the file, was dropped from the benchmark while the run reported a
# healthy pooled figure and an event table that simply had no javelin row in it.
# A slug present in the data with no entry in the map means the map is
# incomplete, and the only safe response is to stop.
miss <- w[is.na(stem), unique(event_slug)]
stopifnot("a harvested event slug has no entry in the map - complete the map" =
            length(miss) == 0)
w[, event_id := sprintf("AT-%s-%s", stem, fifelse(sex == "men", "M", "W"))]
w[, rank_date := as.Date(rank_date)]

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
h[, athlete_id := as.character(athlete_id)]
h <- h[is.finite(r_use) & is.finite(place) & place > 0]
h[, date := as.Date(date)]

# THE EVENT_ID THE MAP BUILDS MUST EXIST IN THE HISTORY. If a stem is misspelt
# the join returns nothing for that event and the benchmark quietly covers
# fewer events than it reports - the exact failure this file is trying to avoid.
bad <- setdiff(unique(w$event_id), unique(h$event_id))
stopifnot("the slug map builds event_ids that do not exist in the history" =
            length(bad) == 0)

# --- the meets ---------------------------------------------------------------
# Named windows rather than a tier filter: this is a comparison against a
# ranking harvested for these two meets specifically, and a tier filter would
# quietly pull in meets no ranking was fetched for.
MEETS <- data.table(
  meet = c("Paris 2024", "Tokyo 2025"),
  from = as.Date(c("2024-08-01", "2025-09-13")),
  to   = as.Date(c("2024-08-11", "2025-09-21")),
  rank_date = as.Date(c("2024-07-23", "2025-09-02")))

# THE RANKING MUST PREDATE THE RACE. Asserted, not assumed - a ranking dated
# during or after the meet contains the result it is being scored on, and would
# produce a WA figure far above anything real.
stopifnot("a ranking is dated on or after its meet - that is leakage" =
            MEETS[, all(rank_date < from)])

score_meet <- function(i) {
  M <- MEETS[i]
  d <- h[date >= M$from & date <= M$to]
  wk <- w[rank_date == M$rank_date, .(athlete_id, event_id, wa_place)]
  d <- merge(d, wk, by = c("athlete_id", "event_id"), all.x = TRUE)

  cov <- d[, .(rows = .N, ranked = sum(!is.na(wa_place)))]
  cat(sprintf("\n%s (%s to %s), ranking of %s\n", M$meet, M$from, M$to, M$rank_date))
  cat(sprintf("  %s scored rows, %s carry a WA ranking (%.1f%%)\n",
              format(cov$rows, big.mark = ","), format(cov$ranked, big.mark = ","),
              100 * cov$ranked / max(cov$rows, 1)))
  if (cov$ranked < 100) { cat("  too few ranked rows to score\n"); return(NULL) }

  # PAIRS WHERE BOTH ATHLETES CARRY BOTH PREDICTORS. Scoring the model on all
  # pairs and WA only on ranked ones would compare two methods on two different
  # populations, and the model would win on the strength of the easier field.
  # This is the same restriction that made the season-best comparison honest.
  p <- d[!is.na(wa_place)]
  a <- p[, .(rid = .GRP, i = seq_len(.N), place, r_use, wa_place,
             event_id, rc), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (!nrow(m)) return(NULL)
  won <- m$place.x < m$place.y
  # higher rating is better; LOWER wa_place is better
  cm <- fifelse(m$r_use.x    == m$r_use.y,    0.5, as.numeric((m$r_use.x > m$r_use.y) == won))
  cw <- fifelse(m$wa_place.x == m$wa_place.y, 0.5, as.numeric((m$wa_place.x < m$wa_place.y) == won))
  m[, `:=`(cm = cm, cw = cw)]

  n <- nrow(m)
  cat(sprintf("  %s pairs | model %.2f | WA ranking %.2f | edge %+.2f (floor %.2f)\n",
              format(n, big.mark = ","), 100 * mean(cm), 100 * mean(cw),
              100 * (mean(cm) - mean(cw)), 100 * sqrt(0.25 / n)))
  m[, meet := M$meet]
  m[]
}

cat("=== model against World Athletics' own pre-meet ranking ===\n")
res <- rbindlist(lapply(seq_len(nrow(MEETS)), score_meet), fill = TRUE)
stopifnot("neither meet produced any pairs" = nrow(res) > 0)

cat("\n=== pooled ===\n")
n <- nrow(res)
cat(sprintf("%s pairs | model %.2f | WA %.2f | edge %+.2f (floor %.2f)\n",
            format(n, big.mark = ","), 100 * mean(res$cm), 100 * mean(res$cw),
            100 * (mean(res$cm) - mean(res$cw)), 100 * sqrt(0.25 / n)))

cat("\n=== by round ===\n")
res[, rnd := fifelse(grepl("final", rc.x, ignore.case = TRUE) &
                     !grepl("semi", rc.x, ignore.case = TRUE), "final",
                     fifelse(grepl("semi", rc.x, ignore.case = TRUE), "semi", "heat"))]
print(res[, .(pairs = .N, model = round(100 * mean(cm), 2),
              wa = round(100 * mean(cw), 2),
              edge = round(100 * (mean(cm) - mean(cw)), 2),
              floor = round(100 * sqrt(0.25 / .N), 2)), by = rnd][order(-pairs)])

# READ THIS TABLE WITH THE COUNT OF EVENTS IN MIND. `floor` is ONE standard
# error, and 25 events are listed. On pure noise, roughly 8 of them land beyond
# 1 floor and 1 or 2 beyond 2 - so an event sitting at -2.5 against a floor of
# 2.0 is 1.2 standard errors, which is nothing, and reading it as "the model
# loses this event" is the multiple-comparison mistake. Nothing here is an
# event-level finding without a second window to confirm it; the pooled figure,
# at roughly 9 floors, is the result. The table is for direction and pattern
# only - e.g. whether the losses cluster in field events, which they appear to.
cat("\n=== by event, worst edge first - see the note above on reading it ===\n")
print(res[, .(pairs = .N, model = round(100 * mean(cm), 2),
              wa = round(100 * mean(cw), 2),
              edge = round(100 * (mean(cm) - mean(cw)), 2),
              floor = round(100 * sqrt(0.25 / .N), 2)),
          by = .(event_id = event_id.x)][pairs >= 200][order(edge)])

f <- file.path(D, "wa_benchmark.json")
writeLines(jsonlite::toJSON(list(
  tag = TAG, pairs = n,
  model = round(100 * mean(res$cm), 3), wa = round(100 * mean(res$cw), 3),
  by_meet = res[, .(pairs = .N, model = round(100 * mean(cm), 2),
                    wa = round(100 * mean(cw), 2)), by = meet],
  by_round = res[, .(pairs = .N, model = round(100 * mean(cm), 2),
                     wa = round(100 * mean(cw), 2)), by = rnd]),
  dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
# REPORT THE DEPTH FROM THE DATA. An earlier version of this line asserted 400
# from memory when the harvest fetched 2 pages of 100 - the kind of
# stated-rather-than-measured number that has been wrong in this project before.
cat(sprintf("\nThe ranking runs %d deep per event and sex, so an athlete outside\n",
            w[, max(wa_place)]))
cat("it carries no place, and their pairs are dropped. That removes the weakest\n")
cat("athletes from BOTH sides equally, which makes this a HARDER population than\n")
cat("the full championship field, not an easier one - and it is why coverage\n")
cat("sits near 45% of scored rows rather than near 100%.\n")
