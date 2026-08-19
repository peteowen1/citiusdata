# Turn the raw profile cache into tables.
#
# harvest_athlete_profiles.R deliberately caches raw JSON and extracts nothing,
# so this can be rerun and extended without refetching anything. If a field
# turns out to matter that nobody parsed, edit here and rerun - it is a local
# read, not another day of scraping.
#
# Emits five parquets:
#   athlete_meta          one row per athlete: country, sex, birthdate, seasons
#   athlete_wa_rankings   World Athletics' own ranking, by event group
#   athlete_pbs           personal bests, 17 fields
#   athlete_sbs           season bests, same shape
#   athlete_honours       titles and medals
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
CACHE <- file.path(D, "ath_profile_cache")
f <- list.files(CACHE, pattern = "[.]rds$", full.names = TRUE)
cat(sprintf("profile files: %s\n", format(length(f), big.mark = ",")))
if (!length(f)) { cat("nothing cached yet\n"); quit(status = 0) }

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
sc <- function(x) { v <- x %||% NA; if (length(v) != 1) NA else v }

meta <- vector("list", length(f)); wr <- list(); pb <- list(); sb <- list(); ho <- list()
nmiss <- 0L
# A 404 sentinel is counted in nmiss and reported. A genuine readRDS failure -
# a truncated file from a harvest killed mid-write, which the harvester's own
# header says is expected - had NO counter, so N corrupt files silently dropped
# N athletes and the coverage percentages below still looked healthy, because
# they are computed only over what parsed.
nbad <- 0L; badf <- character(0)
for (i in seq_along(f)) {
  a <- tryCatch(readRDS(f[i]), error = function(e) NULL)
  if (is.null(a)) {
    nbad <- nbad + 1L
    if (length(badf) < 5) badf <- c(badf, basename(f[i]))
    next
  }
  if (isTRUE(a$.missing)) { nmiss <- nmiss + 1L; next }
  id <- as.character(sc(a$id))
  seasons <- unlist(a$activeSeasons %||% list())
  meta[[i]] <- data.table(
    athlete_id = id,
    first_name = as.character(sc(a$firstname)),
    last_name  = as.character(sc(a$lastname)),
    country    = as.character(sc(a$country)),
    sex        = as.character(sc(a$sex)),
    birthdate  = as.Date(substr(as.character(sc(a$birthdate)), 1, 10)),
    birth_year_only = isTRUE(sc(a$birthdateOnlyYear)),
    rep_id     = as.character(sc(a$athleteRepresentativeId)),
    n_seasons  = length(seasons),
    first_season = if (length(seasons)) min(seasons) else NA_integer_,
    last_season  = if (length(seasons)) max(seasons) else NA_integer_)
  # world rankings: eventGroup + place, one row each
  if (length(a$currentWorldRankings)) wr[[length(wr) + 1L]] <- rbindlist(lapply(
    a$currentWorldRankings, function(r) data.table(
      athlete_id = id, event_group = as.character(sc(r$eventGroup)),
      wa_place = suppressWarnings(as.integer(sc(r$place))))), fill = TRUE)
  # bests: the same 17 fields in both blocks
  flat <- function(lst, tag) {
    if (!length(lst)) return(NULL)
    rbindlist(lapply(lst, function(r) data.table(
      athlete_id = id, kind = tag,
      date = as.Date(substr(as.character(sc(r$date)), 1, 10)),
      discipline = as.character(sc(r$discipline)),
      discipline_code = as.character(sc(r$disciplineCode)),
      mark = as.character(sc(r$mark)),
      performance_value = suppressWarnings(as.numeric(sc(r$performanceValue))),
      is_technical = isTRUE(sc(r$isTechnical)),
      location = paste(unlist(r$location %||% list()), collapse = " / "),
      legal = isTRUE(sc(r$legal)),
      result_score = suppressWarnings(as.numeric(sc(r$resultScore))),
      wind = suppressWarnings(as.numeric(sc(r$wind))),
      place = as.character(sc(r$place)),
      competition_id = as.character(sc(r$competitionId)),
      event_id_wa = as.character(sc(r$eventId)))), fill = TRUE)
  }
  x <- flat(a$personalbests, "pb"); if (!is.null(x)) pb[[length(pb) + 1L]] <- x
  y <- flat(a$seasonsbests,  "sb"); if (!is.null(y)) sb[[length(sb) + 1L]] <- y
  if (length(a$honours)) ho[[length(ho) + 1L]] <- rbindlist(lapply(
    a$honours, function(r) data.table(
      athlete_id = id,
      honour = paste(utils::head(unlist(r), 4), collapse = " | "))), fill = TRUE)
  if (i %% 2000L == 0L) cat(sprintf("  parsed %s/%s\n",
      format(i, big.mark = ","), format(length(f), big.mark = ",")))
}

if (nbad > 0) {
  cat(sprintf("UNREADABLE cache files: %d of %d (%s%s)\n", nbad, length(f),
              paste(badf, collapse = ", "), if (nbad > length(badf)) ", ..." else ""))
  cat("  These athletes are missing from every table below, and the coverage\n")
  cat("  figures are computed only over what parsed - so they will look fine.\n")
  cat("  Re-harvest them, or delete the truncated files so they are re-fetched.\n")
}
w <- function(l, nm) {
  if (!length(l)) { cat(sprintf("  %-22s EMPTY\n", nm)); return(invisible()) }
  x <- rbindlist(l, fill = TRUE)
  write_parquet(x, file.path(D, paste0(nm, ".parquet")))
  cat(sprintf("  %-22s %s rows\n", nm, format(nrow(x), big.mark = ",")))
  invisible(x)
}
cat(sprintf("\n404 sentinels skipped: %d\n", nmiss))
m <- w(Filter(Negate(is.null), meta), "athlete_meta")
w(wr, "athlete_wa_rankings"); w(pb, "athlete_pbs")
w(sb, "athlete_sbs");         w(ho, "athlete_honours")
if (!is.null(m)) {
  cat(sprintf("\ncountry present on %.1f%% | birthdate on %.1f%%\n",
      100 * mean(!is.na(m$country)), 100 * mean(!is.na(m$birthdate))))
  cat("\ntop countries:\n"); print(utils::head(sort(table(m$country), decreasing = TRUE), 10))
}
