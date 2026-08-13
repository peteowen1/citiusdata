# Assemble the swimming corpus from all sources, on ONE identity per person and
# with overlapping performances counted once.
#
# WHY: the same swim can legitimately appear in more than one feed. A British
# swimmer's international race is in World Aquatics AND, if the meet was ranked,
# in Swim England. Counting it twice inflates w_total, which REDUCES shrinkage --
# so the athletes we cover best would be the least regressed to the mean, which
# is exactly backwards.
#
# IDENTITY: the four feeds use four disjoint id namespaces. The crosswalk's
# person_id is the single identity; without it one swimmer is several people
# holding partial careers, and empirical Bayes shrinks each of the fragments.
#
# Usage:  Rscript scripts/build_swimming_corpus.R
VERSE <- here::here()
suppressMessages({library(citius); library(data.table)})
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

xw <- setDT(arrow::read_parquet(file.path(D, "athlete_crosswalk_swimming.parquet")))
id2person <- unique(xw[!is.na(athlete_id), .(source, athlete_id, person_id)])

parts <- list()

f <- file.path(D, "swim_athlete_history.rds")
if (file.exists(f)) {
  wa <- setDT(readRDS(f))
  wa[, `:=`(source = "worldaquatics", is_best = FALSE)]
  parts$wa <- wa
  say("worldaquatics: %s rows", format(nrow(wa), big.mark = ","))
}

# The MEET harvest, which the career harvest cannot replace. Athlete pages give
# a swimmer's results but no competition id, no race grouping and no meet start
# date -- so whole fields, race effects and any competition-level backtest all
# depend on this source specifically. The same swims appear in both routes and
# are merged below.
f <- file.path(D, "swimming_history.rds")
if (file.exists(f)) {
  wm <- setDT(readRDS(f))
  wm[, `:=`(source = "worldaquatics", is_best = FALSE)]
  parts$wm <- wm
  say("worldaquatics (meets): %s rows, %s competitions",
      format(nrow(wm), big.mark = ","), format(uniqueN(wm$competition_id), big.mark = ","))
}

f <- file.path(D, "swimengland_rankings.rds")
if (file.exists(f)) {
  se <- setDT(readRDS(f))
  se[, source := "swimengland"]
  parts$se <- se
  say("swimengland:   %s rows (ranked lists, is_best)", format(nrow(se), big.mark = ","))
}

d <- file.path(D, "swimcloud_cache")
if (dir.exists(d) && length(list.files(d))) {
  cache_files <- list.files(d, full.names = TRUE)
  raw <- lapply(cache_files, function(p) tryCatch(readRDS(p), error = function(e) NULL))
  bad <- cache_files[vapply(raw, is.null, logical(1))]
  if (length(bad)) {
    say("swimcloud cache: %d corrupt file%s dropped silently otherwise: %s%s",
        length(bad), if (length(bad) == 1L) "" else "s",
        paste(basename(head(bad, 10)), collapse = ", "),
        if (length(bad) > 10) sprintf(" and %d more", length(bad) - 10) else "")
  }
  sc <- rbindlist(raw, fill = TRUE)
  if (nrow(sc)) {
    sc[, source := "swimcloud"]
    parts$sc <- sc
    say("swimcloud:     %s rows", format(nrow(sc), big.mark = ","))
  }
}

keep <- c("source", "athlete_id", "athlete_name", "event_id", "discipline",
          "date", "mark", "mark_string", "place", "round", "comp_name",
          # The source's own competition id must survive. Substituting the
          # competition NAME turned Swim England's 17,317 ranked meets into
          # pseudo-competitions and the backtest reported 5,508 "competitions
          # with finals" against a true 43.
          "competition_id", "comp_start", "tier",
          "course", "is_best", "race_key")
# Fill missing columns with a TYPED NA. A bare NA is logical, and if every
# source lacks a column the result is a logical column that then refuses a
# character or Date assignment later.
na_for <- list(source = NA_character_, athlete_id = NA_character_,
               athlete_name = NA_character_, event_id = NA_character_,
               discipline = NA_character_, date = as.Date(NA),
               mark = NA_real_, mark_string = NA_character_, place = NA_integer_,
               round = NA_character_, comp_name = NA_character_,
               competition_id = NA_character_, comp_start = as.Date(NA),
               tier = NA_character_, course = NA_character_,
               is_best = NA, race_key = NA_character_)
all <- rbindlist(lapply(parts, function(p) {
  for (m in setdiff(keep, names(p))) p[[m]] <- na_for[[m]]
  p[, ..keep]
}), fill = TRUE)
say("\ncombined: %s rows", format(nrow(all), big.mark = ","))

# ---- one identity ----------------------------------------------------------
all[id2person, on = .(source, athlete_id), person_id := i.person_id]
say("person_id resolved: %.1f%% (unresolved rows keep their source id and stay\n  separate people, which is the safe failure)",
    100 * mean(!is.na(all$person_id)))
all[is.na(person_id), person_id := paste(source, athlete_id, sep = "|")]

# ---- one row per performance -----------------------------------------------
# Match on WHAT HAPPENED -- person, date, event, mark -- not on ids, because the
# whole problem is that the ids differ between feeds. Course is part of the key:
# the same swimmer can record the same time on the same day in two pools only in
# the sense that a short-course and long-course swim are different performances.
before <- nrow(all)
all[, mark_r := round(mark, 2)]
# When the same swim arrives by two routes, KEEP THE RICHEST ROW. The meet
# harvest carries competition_id, comp_start and race_key; the athlete harvest
# carries none of them. Sorting only by is_best kept whichever happened to come
# first and silently discarded the race structure.
all[, richness := (!is.na(race_key)) + (!is.na(competition_id)) +
                  (!is.na(comp_start)) + (is.na(is_best) | !is_best)]
setorder(all, person_id, date, event_id, mark_r, -richness)
all <- unique(all, by = c("person_id", "date", "event_id", "mark_r", "course"))
say("deduped: %s -> %s rows (%s duplicate performance%s removed)",
    format(before, big.mark = ","), format(nrow(all), big.mark = ","),
    format(before - nrow(all), big.mark = ","),
    if (before - nrow(all) == 1) "" else "s")
all[, c('mark_r', 'richness') := NULL]

# ---- put every mark on a long-course footing -------------------------------
# 53% of the corpus is short course, which is measurably faster. Leaving the two
# mixed injects a systematic ~3% error, several times the 0.73% within-athlete
# spread, and it would land disproportionately on British swimmers because they
# are the ones with short-course records.
#
# The offset applied here is OUR measurement (measure_course_offset.R, 188,603
# within-athlete pairs), per event, not the conversion the source publishes.
all[citius_events(), on = "event_id", orientation := i.orientation]
all[!is.na(mark) & mark > 0 & !is.na(orientation), perf := to_perf(mark, orientation)]
off <- setDT(readRDS(file.path(D, "course_offset.rds")))[n >= 30, .(event_id, offset)]
all[off, on = "event_id", course_offset := i.offset]
all[, perf_lc := perf]
adj <- !is.na(all$perf) & all$course %in% c("SCM", "SCY") & !is.na(all$course_offset)
all[adj, perf_lc := perf - course_offset]
say("\ncourse-adjusted %s short-course row%s onto a long-course footing",
    format(sum(adj), big.mark = ","), if (sum(adj) == 1) "" else "s")
say("  short-course rows with no measured offset (left unadjusted, flagged): %s",
    format(sum(all$course %in% c("SCM", "SCY") & is.na(all$course_offset)), big.mark = ","))
all[, course_adjusted := adj]

say("\nby source after dedupe:")
print(all[, .(rows = .N, people = uniqueN(person_id)), by = source][order(-rows)])
say("\npeople: %s | events: %s | %s..%s",
    format(uniqueN(all$person_id), big.mark = ","), uniqueN(all$event_id),
    min(all$date, na.rm = TRUE), max(all$date, na.rm = TRUE))
say("results per person: median %s, 90th pct %s",
    median(all[, .N, by = person_id]$N),
    round(quantile(all[, .N, by = person_id]$N, 0.9)))

# Emit the canonical column names the models expect, so this is a drop-in for
# the single-source history. The source's own id is kept alongside for tracing a
# row back to where it came from.
all[, source_athlete_id := athlete_id]
all[, athlete_id := person_id]
# Fall back to the meet name only where a source gives no id of its own.
all[, competition_id := as.character(competition_id)]
all[is.na(competition_id) | !nzchar(competition_id),
    competition_id := paste(source, comp_name, sep = "|")]
all[is.na(comp_start), comp_start := min(date, na.rm = TRUE), by = competition_id]

# A competition is SCOREABLE only if it has real races -- a field, a round, and
# finishing places. Ranked lists have none of those: they are one best per
# swimmer per season and can only ever be history. Marking this explicitly stops
# a downstream consumer from mistaking a ranking for a meet.
all[, scoreable := !is.na(race_key) & !is.na(round) & (is.na(is_best) | !is_best)]
say("scoreable rows (real races): %s of %s across %s competition%s",
    format(sum(all$scoreable), big.mark = ","), format(nrow(all), big.mark = ","),
    format(uniqueN(all[scoreable == TRUE]$competition_id), big.mark = ","),
    if (uniqueN(all[scoreable == TRUE]$competition_id) == 1) "" else "s")
# perf is the LONG-COURSE-equivalent performance; the raw one stays as perf_raw
# so the adjustment can be audited or undone.
all[, perf_raw := perf]
all[, perf := perf_lc]
# Races are only identifiable where a source gives whole fields. Swim England
# ranked lists have no race, so they keep NA and contribute to ability only --
# decompose_races() drops NA race_key rows, which is the correct behaviour.
say("rows carrying a race_key (whole fields, usable for race effects): %s of %s",
    format(sum(!is.na(all$race_key)), big.mark = ","),
    format(nrow(all), big.mark = ","))

# Write atomically: every arm and backtest reads this path directly, so a
# crash or interrupt mid-save must never leave a partial file where a full
# corpus used to be. Write to a sibling temp file, then rename over the
# target -- the temp write either finishes cleanly or the old file survives.
rds_path <- file.path(D, "swimming_corpus.rds")
tmp <- paste0(rds_path, ".tmp")
saveRDS(all, tmp)
if (file.exists(rds_path)) file.remove(rds_path)
file.rename(tmp, rds_path)

pq_path <- file.path(D, "swimming_corpus.parquet")
tmp <- paste0(pq_path, ".tmp")
arrow::write_parquet(all, tmp)
if (file.exists(pq_path)) file.remove(pq_path)
file.rename(tmp, pq_path)
say("\nwrote swimming_corpus.{rds,parquet}")
