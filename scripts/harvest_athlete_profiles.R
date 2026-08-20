# Harvest EVERY field the World Athletics athlete endpoint exposes.
#
# `athletics_athlete_profile()` has always fetched this endpoint and kept six
# fields - id, names, country, sex, birthdate - discarding the rest. The rest
# turns out to be substantial:
#
#   currentWorldRankings   eventGroup + place. World Athletics' OWN ranking,
#                          built by a different method (rolling window, placing
#                          bonuses) from races we may not even hold. The only
#                          genuinely independent rating available to us.
#   personalbests          17 fields per event: date, discipline, mark,
#                          resultScore, wind, legal, location, competitionId...
#   seasonsbests           same shape, current season
#   honours                titles and medals
#   activeSeasons          which years the athlete competed
#   country                the nationality the corpus has never carried, which
#                          is what makes home advantage testable at all
#
# THIS SCRIPT CACHES THE RAW PARSED JSON, one file per athlete, and extracts
# nothing. Extraction is `assemble_athlete_profiles.R`, reading the cache. That
# split is the point: deciding later that we want a field we did not think to
# parse costs a re-read, not a re-scrape of 87,125 athletes.
#
# Resumable and idempotent: existing files are skipped, so it can be killed and
# restarted freely. A 404 writes a sentinel rather than nothing, or every future
# run would retry the same dead ids forever.
#
#   CITIUS_MAX_PROFILES   how many to fetch this run (0 = no limit)
#   CITIUS_THROTTLE       min seconds between requests (default 0.25 = 4/s,
#                         the citius_get_json() default). Raise this to go
#                         gentler after a rate-limit backoff.
#
# Rate: citius_get_json throttles to 1/CITIUS_THROTTLE requests/second with
# retries and an identifying user agent. Do NOT parallelise this - httr2
# throttles per request object, so N processes would issue N times the rate
# at a third-party service.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table)); suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
ns <- asNamespace("citius")

OUT   <- here::here("citiusdata", "data")
CACHE <- file.path(OUT, "ath_profile_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
MAX <- .env_int("CITIUS_MAX_PROFILES", "0")
THROTTLE <- .env_num("CITIUS_THROTTLE", "0.25")
cat(sprintf("throttle: %.2f s/request (%.2f req/s)\n", THROTTLE, 1 / THROTTLE))

# --- who to fetch: every athlete we have ever seen ---------------------------
ids <- character(0)
ac <- list.files(file.path(OUT, "ath_athlete_cache"), pattern = "[.]rds$")
if (length(ac)) ids <- c(ids, sub("[.]rds$", "", ac))
cs <- list.files(file.path(OUT, "athletics_careers_store"), pattern = "parquet$",
                 recursive = TRUE, full.names = TRUE)
if (length(cs)) {
  for (f in cs) {
    x <- tryCatch(setDT(read_parquet(f, col_select = "athlete_id")), error = function(e) NULL)
    if (!is.null(x)) ids <- c(ids, as.character(x$athlete_id))
  }
}
evs <- setdiff(sub("^event_id=", "", list.dirs(file.path(OUT, "athletics_corpus_store"),
               recursive = FALSE, full.names = FALSE)), "__unmatched__")
for (EV in evs) {
  f <- file.path(OUT, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) next
  x <- tryCatch(setDT(read_parquet(f, col_select = "athlete_id")), error = function(e) NULL)
  if (!is.null(x)) ids <- c(ids, as.character(x$athlete_id))
}
ids <- unique(ids[!is.na(ids) & nzchar(ids) & ids != "NA"])
ids <- ids[grepl("^[0-9]+$", ids)]
cat(sprintf("athletes known: %s\n", format(length(ids), big.mark = ",")))

# --- PRIORITY ORDER ----------------------------------------------------------
# 340,043 athletes at the measured 96/min is ~59 hours, so the order matters far
# more than it would on a short sweep: the run must be stoppable at any point
# without having wasted the time. Championship athletes first (2,783, about half
# an hour), then all T1 elite, then everyone in the scored window, then the long
# tail of athletes who appear only in old or minor races.
prio <- function() {
  hf <- file.path(OUT, "seqv3_history_final.parquet")
  if (!file.exists(hf)) return(list())
  h <- setDT(read_parquet(hf, col_select = c("athlete_id","race_key","date","place","rc")))
  h[, athlete_id := as.character(athlete_id)]
  cf <- file.path(OUT, "competition_catalogue.parquet")
  cg <- setDT(read_parquet(cf)); cg[, competition_id := as.character(competition_id)]
  h[, competition_id := tstrsplit(race_key, "[|]", keep = 1L)[[1]]]
  h <- merge(h, cg[, .(competition_id, meet_tier, class)], by = "competition_id", all.x = TRUE)
  MAJ <- c("olympics","world_champs","european_champs","commonwealth")
  list(majors  = unique(h[class %chin% MAJ & rc == "final", athlete_id]),
       t1      = unique(h[meet_tier == "T1_elite", athlete_id]),
       scored  = unique(h[year(date) >= 2025 & place <= 12, athlete_id]),
       corpus  = unique(h$athlete_id))
}
pr <- tryCatch(prio(), error = function(e) list())
rank_of <- setNames(rep(5L, length(ids)), ids)
for (i in seq_along(pr)) {
  k <- intersect(pr[[i]], ids)
  rank_of[k] <- pmin(rank_of[k], i)
}
ids <- ids[order(rank_of[ids])]
if (length(pr)) cat(sprintf("priority: majors %s, T1 %s, scored %s, corpus %s, other %s
",
    format(sum(rank_of == 1L), big.mark=","), format(sum(rank_of == 2L), big.mark=","),
    format(sum(rank_of == 3L), big.mark=","), format(sum(rank_of == 4L), big.mark=","),
    format(sum(rank_of == 5L), big.mark=",")))

have <- sub("[.]rds$", "", list.files(CACHE, pattern = "[.]rds$"))
todo <- ids[!ids %chin% have]      # keeps priority order; setdiff() would not
cat(sprintf("already cached: %s | to fetch: %s\n",
            format(length(have), big.mark = ","), format(length(todo), big.mark = ",")))
if (!length(todo)) { cat("nothing to do.\n"); quit(status = 0) }
if (MAX > 0L && length(todo) > MAX) todo <- head(todo, MAX)

est <- length(todo) * 0.25 / 60
cat(sprintf("fetching %s this run (~%.0f min at 4 req/s)\n\n",
            format(length(todo), big.mark = ","), est))

t0 <- Sys.time(); ok <- 0L; miss <- 0L; err <- 0L
for (i in seq_along(todo)) {
  id <- todo[i]
  f <- file.path(CACHE, paste0(id, ".rds"))
  a <- tryCatch(ns$citius_get_json(paste0(ns$athletics_base_url(), "/athletes/", id),
                                    throttle = THROTTLE),
                error = function(e) structure(list(), class = "citius_fetch_error"))
  if (inherits(a, "citius_fetch_error")) { err <- err + 1L; next }
  # A 404 is cached as a sentinel, not skipped: without it every future run
  # re-requests the same dead ids and the sweep never converges.
  if (is.null(a)) { saveRDS(list(.missing = TRUE, .id = id), f); miss <- miss + 1L }
  else            { saveRDS(a, f); ok <- ok + 1L }
  if (i %% 250L == 0L) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("  %s/%s | ok %s missing %s errors %s | %.1f min | %.0f/min | eta %.0f min\n",
        format(i, big.mark = ","), format(length(todo), big.mark = ","),
        format(ok, big.mark = ","), miss, err, el, i / el,
        (length(todo) - i) / (i / el)))
  }
}
el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cat(sprintf("\ndone: %s fetched, %s missing (404), %s errors in %.1f min (%.0f/min)\n",
            format(ok, big.mark = ","), miss, err, el, length(todo) / el))
cat(sprintf("cache now holds %s profiles\n",
            format(length(list.files(CACHE, pattern = "[.]rds$")), big.mark = ",")))
cat("run again to continue; assemble_athlete_profiles.R turns the cache into tables\n")
