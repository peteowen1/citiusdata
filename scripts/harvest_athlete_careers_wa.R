# Harvest whole athlete careers DIRECTLY from World Athletics.
#
# WHY THIS EXISTS. harvest_athlete_histories.R fetches careers from the
# community mirror (worldathletics.nimarion.de), which has returned HTTP 500 on
# every route since 2026-09-12 -- verified again on 2026-09-15, both /athletes
# and /competitions. That blocks the career sweep entirely, and the career sweep
# is the high-value one: measured on our own data, the career route carries a
# median of 5 races per athlete-event in the 12-year window against the
# competition route's 2, mean 11.27 against 4.38.
#
# WHY IT MATTERS. estimate_ability() groups by athlete x event, and the median
# pair has TWO races. With w_total = recency x precision that is thin enough
# that an ability is mostly prior rather than evidence -- 25.9% of athletes have
# exactly one race and 40% have two or fewer. Thin history is the mechanism
# behind both the Ingebrigtsen case and, per harvest_athlete_histories.R's own
# header, "very probably the root of the open calibration problem".
#
# THE QUERY, and the two things that make it return nothing.
# getSingleCompetitorResultsDate(id, resultsByYear, resultsByYearOrderBy):
#   * `id` is the SAME id we store as athlete_id (confirmed: activeYears comes
#     back correct for our ids; getCompetitorAAId(iaafId=ours) returns NULL,
#     so ours is already the aaId).
#   * `resultsByYear` MUST be an explicit year. Omitting it echoes a default
#     year back in `parameters` and returns ZERO rows.
#   * `resultsByYearOrderBy` MUST be "date". The default is "discipline", which
#     also returns zero rows. Both failures are HTTP 200 with an empty list, so
#     they look like "this athlete has no results" rather than a bad call.
#
# It is one request per athlete-YEAR, not per athlete, so `activeYears` (free in
# the same response) decides which years are worth asking for. Years outside the
# window are never requested.
#
# Captures EVERY field the row type exposes. The competition route's career rows
# have no competitionId at all, which is why 98.8% of them carry no race_key;
# these do, plus `category` (the tier) and `race`.
#
# ONLY WRITES ITS OWN CACHE. It does not append to championship_results.rds,
# rebuild the corpus or touch any training input.
#
# Usage:
#   Rscript citiusdata/scripts/harvest_athlete_careers_wa.R
#     CITIUS_CAREER_MAX=500        athletes this run (resumable)
#     CITIUS_CAREER_FROM=2014      earliest year to request
#     CITIUS_CAREER_HOURS=8        wall-clock budget
#     CITIUS_CAREER_MIN_FREE_MB=800
#     CITIUS_CAREER_PAUSE=0.05     seconds between requests

VERSE <- here::here()
suppressMessages({library(data.table); library(httr2); library(jsonlite); library(cli)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
source(file.path(VERSE, "citiusdata", "scripts", "_env.R"))

D     <- file.path(VERSE, "citiusdata", "data")
CACHE <- file.path(D, "wa_career_cache")
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
MAXN   <- .env_int("CITIUS_CAREER_MAX", "500")
YFROM  <- .env_int("CITIUS_CAREER_FROM", "2014")
HOURS  <- as.numeric(Sys.getenv("CITIUS_CAREER_HOURS", "8"))
FLOOR  <- .env_int("CITIUS_CAREER_MIN_FREE_MB", "800")
PAUSE  <- as.numeric(Sys.getenv("CITIUS_CAREER_PAUSE", "0.05"))
JOURNAL <- file.path(CACHE, "_journal.csv")
T0 <- Sys.time()
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

free_mb <- function() {
  x <- suppressWarnings(tryCatch(
    system2("powershell", c("-NoProfile","-Command",
      "(Get-Counter '\\Memory\\Available MBytes').CounterSamples[0].CookedValue"),
      stdout = TRUE, stderr = FALSE), error = function(e) NA))
  v <- suppressWarnings(as.numeric(x[length(x)]))
  if (length(v) && is.finite(v)) v else Inf
}

# --- endpoint, read fresh -----------------------------------------------------
ep <- local({
  o <- capture.output(v <- source(file.path(VERSE, "citiusdata", "scripts",
                                            "discover_wa_endpoint.R"))$value)
  v
})
if (is.na(ep$status) || ep$status != 200L)
  cli_abort("Endpoint discovery returned HTTP {ep$status}; nothing can be fetched.")
say("endpoint %s", ep$edge)

FIELDS <- paste("date competition venue indoor disciplineCode disciplineNameUrlSlug",
                "typeNameUrlSlug discipline country category race place mark wind",
                "notLegal resultScore remark competitionId eventId")
Q_YEARS <- 'query($id: Int){ getSingleCompetitorResultsDate(id: $id){ activeYears } }'
Q_ROWS  <- sprintf('query($id: Int, $y: Int){ getSingleCompetitorResultsDate(id: $id, resultsByYear: $y, resultsByYearOrderBy: "date"){ resultsByDate { %s } } }', FIELDS)

gql <- function(query, variables) {
  resp <- tryCatch(
    request(ep$url) |>
      req_headers(accept = "*/*", `content-type` = "application/json",
                  `x-api-key` = ep$key, `x-amz-user-agent` = "aws-amplify/3.0.7") |>
      req_body_raw(toJSON(list(query = query, variables = variables), auto_unbox = TRUE),
                   type = "application/json") |>
      req_timeout(45) |> req_retry(max_tries = 3) |>
      req_error(is_error = function(r) FALSE) |> req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200L) return(NULL)
  tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE), error = function(e) NULL)
}

# --- who to fetch, most valuable first ---------------------------------------
say("reading athletes ...")
ch <- as.data.table(with_citius_db_connection(function(cn) DBI::dbGetQuery(cn,
  "SELECT athlete_id, competition_id, round, place, COUNT(*) n
   FROM championship_results GROUP BY 1,2,3,4"), read_only = TRUE))
ch[, aid := suppressWarnings(as.integer(athlete_id))]
ch <- ch[!is.na(aid)]

cat_f <- file.path(D, "competition_catalogue.parquet")
elite <- if (file.exists(cat_f)) {
  ct <- as.data.table(arrow::read_parquet(cat_f))[, .(competition_id, meet_tier)]
  as.character(ct[meet_tier %chin% c("T1_elite", "T2_strong")]$competition_id)
} else character(0)
fin <- unique(ch[!is.na(place) & as.character(competition_id) %chin% elite &
                   grepl("final", round, ignore.case = TRUE) &
                   !grepl("semi", round, ignore.case = TRUE)]$aid)
held <- ch[, .(n_held = sum(n)), by = aid]
held[, priority := fifelse(aid %in% fin, 1L, fifelse(n_held <= 3L, 2L, 3L))]
setorder(held, priority, -n_held)
say("%s athletes: %s elite-finalists, %s thin, %s other",
    format(nrow(held), big.mark=","), format(sum(held$priority==1L), big.mark=","),
    format(sum(held$priority==2L), big.mark=","), format(sum(held$priority==3L), big.mark=","))

done <- sub("\\.rds$", "", list.files(CACHE, pattern = "^[0-9]+\\.rds$"))
todo <- held[!as.character(aid) %chin% done]
say("%s remaining (%.0f%%); fetching up to %s this run",
    format(nrow(todo), big.mark=","), 100*nrow(todo)/nrow(held), format(MAXN, big.mark=","))
if (!nrow(todo)) { say("nothing to do"); quit(status = 0) }
todo <- head(todo, MAXN)

if (!file.exists(JOURNAL))
  fwrite(data.table(time=character(), athlete_id=integer(), years=integer(),
                    rows=integer(), status=character()), JOURNAL)

ok <- 0L; empty <- 0L; failed <- 0L; n_rows <- 0L; n_req <- 0L
for (i in seq_len(nrow(todo))) {
  if (as.numeric(difftime(Sys.time(), T0, units = "hours")) > HOURS) {
    say("budget of %.1f h reached", HOURS); break
  }
  fm <- free_mb()
  if (fm < FLOOR) {
    say("  low memory (%.0f MB) -- waiting", fm); Sys.sleep(60)
    if (free_mb() < FLOOR) { say("STOPPING: still %.0f MB", free_mb()); break }
  }
  aid <- todo$aid[i]
  yj <- gql(Q_YEARS, list(id = aid)); n_req <- n_req + 1L
  yrs <- suppressWarnings(as.integer(unlist(yj$data$getSingleCompetitorResultsDate$activeYears)))
  yrs <- sort(yrs[!is.na(yrs) & yrs >= YFROM], decreasing = TRUE)
  if (!length(yrs)) {
    # No active years in range IS an answer; cache it so the athlete is not
    # retried forever. A FAILED request is different and is left uncached.
    if (is.null(yj)) { failed <- failed + 1L
      fwrite(data.table(time=format(Sys.time()), athlete_id=aid, years=0L, rows=0L,
                        status="failed"), JOURNAL, append = TRUE)
    } else { saveRDS(data.table(), file.path(CACHE, paste0(aid, ".rds"))); empty <- empty + 1L
      fwrite(data.table(time=format(Sys.time()), athlete_id=aid, years=0L, rows=0L,
                        status="empty"), JOURNAL, append = TRUE) }
    Sys.sleep(PAUSE); next
  }
  parts <- list(); bad <- FALSE
  for (y in yrs) {
    rj <- gql(Q_ROWS, list(id = aid, y = y)); n_req <- n_req + 1L
    if (is.null(rj)) { bad <- TRUE; break }
    rows <- rj$data$getSingleCompetitorResultsDate$resultsByDate
    if (length(rows)) {
      dt <- rbindlist(lapply(rows, function(x)
        as.data.table(lapply(x, function(v) if (is.null(v)) NA else v))), fill = TRUE)
      dt[, `:=`(athlete_id = aid, year = y)]
      parts[[length(parts) + 1L]] <- dt
    }
    Sys.sleep(PAUSE)
  }
  if (bad) {
    failed <- failed + 1L
    fwrite(data.table(time=format(Sys.time()), athlete_id=aid, years=length(yrs), rows=0L,
                      status="failed"), JOURNAL, append = TRUE)
    next          # NOT cached, so it is retried next run
  }
  out <- if (length(parts)) rbindlist(parts, use.names = TRUE, fill = TRUE) else data.table()
  saveRDS(out, file.path(CACHE, paste0(aid, ".rds")))
  n_rows <- n_rows + nrow(out)
  if (nrow(out)) ok <- ok + 1L else empty <- empty + 1L
  fwrite(data.table(time=format(Sys.time()), athlete_id=aid, years=length(yrs),
                    rows=nrow(out), status=if (nrow(out)) "ok" else "empty"),
         JOURNAL, append = TRUE)
  if (i %% 25 == 0)
    say("  %d/%d | %s rows | %s requests | %.2fs/req", i, nrow(todo),
        format(n_rows, big.mark=","), format(n_req, big.mark=","),
        as.numeric(difftime(Sys.time(), T0, units="secs"))/max(1,n_req))
}

say("")
say("=== CAREER SWEEP SUMMARY ===")
say("elapsed        %.2f h", as.numeric(difftime(Sys.time(), T0, units = "hours")))
say("athletes ok    %d", ok)
say("empty          %d", empty)
say("failed         %d", failed)
say("results        %s", format(n_rows, big.mark = ","))
say("requests       %s (%.2fs each)", format(n_req, big.mark = ","),
    as.numeric(difftime(Sys.time(), T0, units="secs"))/max(1,n_req))
say("cache holds    %s athlete files",
    format(length(list.files(CACHE, pattern="^[0-9]+\\.rds$")), big.mark = ","))
say("NOTHING was appended to any training input.")
