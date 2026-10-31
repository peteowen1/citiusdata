# Harvest every completed meet that is not yet in our data, and STAGE it.
#
# WHY THIS EXISTS. Audited 2026-09-14: nothing in citiusdata fetches on a
# schedule. Every sibling verse has a daily scrape; the absence here is why the
# corpus sat 9 days behind, the catalogue 18, and August 2026 held 4,813 corpus
# rows against July's 117,988. check_harvest_freshness.R made that visible;
# this closes the half of the gap that can be closed safely.
#
# WHAT IT DELIBERATELY DOES NOT DO. It does not touch
# championship_results.rds, athletics_corpus, the catalogue or any store. It
# fetches results and stages them, nothing more.
#
# That line is where it is because append_meet_to_championship_results.R's own
# header draws it: "quietly adding a meet a calibration was not fitted on is
# how a model ends up evaluated on its own training data." Fetching is additive
# and reversible -- a raw results file that is wrong can be deleted. Appending
# to a training input is neither, and stays a deliberate act.
#
# So this turns "nobody has fetched Budapest" into "Budapest is fetched and
# waiting", which changes the manual job from a multi-step discovery-and-
# harvest into a single append.
#
# NO CREDENTIAL IS NEEDED, and that is what makes it schedulable. The WA edge
# and client key rotate without notice -- 4881 retired 2026-09-09, 4883 gone by
# 09-12, 4888 then, 4892 by 09-14 -- but neither is a secret: both are compiled
# into the public Next.js bundle every visitor downloads. discover_wa_endpoint.R
# reads them fresh each run, which is why a stored GitHub secret for this was
# always going to be wrong within days.
#
# Usage:
#   Rscript citiusdata/scripts/harvest_pending_meets.R
#   CITIUS_HARVEST_PUBLISH=1 Rscript ...    # also upload to the staging release
#   CITIUS_HARVEST_MEETS=budapest2026 ...   # force specific meets

suppressMessages({
  devtools::load_all(here::here("citius"), quiet = TRUE)
  library(data.table); library(cli)
})
D <- here::here("citiusdata", "data")
S <- here::here("citiusdata", "scripts")
TAG <- Sys.getenv("CITIUS_HARVEST_TAG", "pending-harvest")
REPO <- "peteowen1/citiusdata"
PUBLISH <- nzchar(Sys.getenv("CITIUS_HARVEST_PUBLISH", ""))

forced <- strsplit(Sys.getenv("CITIUS_HARVEST_MEETS", ""), ",")[[1]]
forced <- trimws(forced[nzchar(forced)])

if (length(forced)) {
  cal <- fread(file.path(D, "athletics_calendar.csv"))
  pending <- cal[meet_id %in% forced]
} else {
  # DISCOVERY IS THE WA CALENDAR, NOT OUR OWN CATALOGUE. This used to look for
  # completed meets in athletics_calendar.csv that were absent from the
  # catalogue. Both are blind to anything nobody has added: the csv is hand-
  # maintained and the catalogue is built FROM the harvested history, so once
  # the corpus stopped (2026-10-11) the catalogue stopped at the same date and
  # every run printed "Nothing pending" while 105 meets since then (of 118 the
  # WA calendar lists with results and we do not hold) sat unfetched. Freshness
  # went red on 10-22; this job stayed green. The WA calendar is the one source
  # that knows a meet happened without us having to have seen it first.
  LOOKBACK <- as.integer(Sys.getenv("CITIUS_HARVEST_LOOKBACK_DAYS", "21"))
  if (is.na(LOOKBACK) || LOOKBACK < 1L) {
    cli_abort("CITIUS_HARVEST_LOOKBACK_DAYS must be a positive whole number of days.")
  }

  # "Held" is the union of the corpus and the catalogue. The catalogue is built
  # from championship_results.rds, so a meet appended today is in the corpus
  # before it is in the catalogue; checking either alone would re-stage it.
  # A MISSING INPUT IS AN ERROR, not an empty set: an empty `have` would make
  # every meet in the window look pending, and an unreadable one would hide
  # what is already held.
  suppressMessages(library(arrow))
  read_ids <- function(f, label) {
    if (!file.exists(f)) cli_abort("{.file {basename(f)}} is absent; cannot tell which meets are already held.")
    ids <- tryCatch(
      as.character(as.data.table(arrow::read_parquet(f, col_select = "competition_id"))$competition_id),
      error = function(e) cli_abort("Could not read competition_id from {.file {basename(f)}}: {conditionMessage(e)}"))
    ids <- unique(ids[!is.na(ids)])
    if (!length(ids)) cli_abort("{label} holds no competition ids; refusing to treat that as 'nothing held'.")
    cli_alert_info("{label}: {length(ids)} competition id{?s} held")
    ids
  }
  have <- union(
    read_ids(file.path(D, "athletics_corpus.parquet"), "athletics_corpus"),
    read_ids(file.path(D, "competition_catalogue.parquet"), "competition_catalogue"))

  from <- Sys.Date() - LOOKBACK
  cli_alert_info("Fetching the WA calendar {as.character(from)} .. {as.character(Sys.Date())}")
  # A FAILED OR PARTIAL CALENDAR FETCH MUST NOT READ AS "NOTHING PENDING". An
  # error, a WAF interstitial (HTTP 200 with no payload, attr fetch_ok FALSE),
  # a pagination that did not finish (attr complete FALSE) and a zero-row answer
  # for a 21-day window are all indistinguishable from a quiet fortnight if
  # they are allowed to fall through to the check below. Each stops the run
  # non-zero instead, so the workflow goes red.
  wa <- tryCatch(athletics_calendar_all(start_date = from, end_date = Sys.Date()),
                 error = function(e) cli_abort("WA calendar fetch failed: {conditionMessage(e)}"))
  if (is.null(wa) || !nrow(wa)) {
    cli_abort("WA calendar returned no meets for {as.character(from)} .. {as.character(Sys.Date())}; treating that as a failed fetch, not a quiet fortnight.")
  }
  if (!isTRUE(attr(wa, "complete"))) {
    cli_abort("WA calendar pagination did not complete; the list is truncated, so 'nothing pending' cannot be trusted.")
  }
  if (identical(attr(wa, "fetch_ok"), FALSE)) {
    cli_abort("WA calendar fetch unconfirmed (WAF/interstitial?); refusing to trust it.")
  }
  wa <- as.data.table(wa)
  need <- c("competition_id", "has_results")
  if (length(miss <- setdiff(need, names(wa)))) {
    cli_abort("WA calendar is missing column{?s} {.field {miss}}; the harvest cannot tell which meets have results.")
  }
  cli_alert_info("WA calendar: {nrow(wa)} meet{?s}, {sum(wa$has_results %in% TRUE)} with results")

  wa[, cid := as.character(competition_id)]
  done <- unique(wa[has_results %in% TRUE & !is.na(cid)], by = "cid")
  done <- done[!cid %in% have]
  # Meets already staged in the release but not yet appended are not "held",
  # yet re-fetching them every day would re-stage the whole backlog (~118 meets,
  # ~18 min) on each run until someone appends. Skip them, as backfill_harvest.R
  # does. A listing that fails is not fatal: re-fetching is slow but safe.
  staged_ids <- tryCatch({
    a <- vb_list_assets(REPO, TAG)$name
    sub("^meet_([0-9]+)_raw_results\\.rds$", "\\1", grep("^meet_[0-9]+_raw_results\\.rds$", a, value = TRUE))
  },
  vb_error_absent = function(e) character(0),
  error = function(e) {
    cli_alert_warning("Could not list {TAG} ({conditionMessage(e)}); already-staged meets will be re-fetched.")
    character(0)
  })
  if (length(staged_ids)) {
    cli_alert_info("{TAG}: {length(staged_ids)} meet{?s} already staged, skipped")
    done <- done[!cid %in% staged_ids]
  }
  # meet_<id> is the name append_meet_to_championship_results.R looks for first
  # (RAW_F), so a staged calendar meet is picked up by its competition id with
  # no renaming step.
  pending <- done[, .(meet_id = paste0("meet_", cid), wa_competition_id = cid,
                      name = if ("name" %in% names(done)) name else NA_character_)]
}

if (!nrow(pending)) {
  cli_alert_success("Nothing pending -- every completed meet in the WA calendar window is already held.")
  quit(status = 0)
}
cli_alert_info("{nrow(pending)} meet{?s} pending: {.val {pending$meet_id}}")

# Shell out per meet rather than reimplementing the fetch. harvest_wa_results.R
# is the tested path and carries the round-naming and race-key handling; a
# second copy here would drift from it silently.
staged <- character(0)
failed <- character(0)
for (i in seq_len(nrow(pending))) {
  m <- pending[i]
  comp <- suppressWarnings(as.integer(m$wa_competition_id))
  if (is.na(comp)) {
    cli_alert_warning("{m$meet_id}: no wa_competition_id, skipped.")
    next
  }
  cli_h2("{m$meet_id} (competition {comp})")
  out_f <- file.path(D, paste0(m$meet_id, "_raw_results.rds"))
  ok <- tryCatch({
    withr::with_envvar(
      c(CITIUS_COMP = as.character(comp), CITIUS_MEET = m$meet_id),
      source(file.path(S, "harvest_wa_results.R"), local = new.env()))
    TRUE
  }, error = function(e) { cli_alert_danger("{m$meet_id}: {conditionMessage(e)}"); FALSE })

  if (isTRUE(ok) && file.exists(out_f)) {
    n <- tryCatch(nrow(readRDS(out_f)), error = function(e) NA_integer_)
    cli_alert_success("{m$meet_id}: staged {n} row{?s} -> {.path {basename(out_f)}}")
    staged <- c(staged, out_f)
  } else {
    failed <- c(failed, m$meet_id)
  }
}

cli_h1("Summary")
cli_alert_info("staged {length(staged)}, failed {length(failed)}")
if (length(failed)) cli_alert_danger("failed: {.val {failed}}")

if (PUBLISH && length(staged)) {
  # Through vb_publish rather than piggyback directly, so the staging release
  # gets manifest-last atomicity and sha256 sidecars like every other verse's
  # data bus. carry_forward keeps previously staged meets in the manifest
  # instead of a partial run erasing them.
  cli_h2("Publishing to {TAG}")
  vb_publish(staged, repo = REPO, tag = TAG, carry_forward = TRUE)
  cli_alert_success("Published {length(staged)} file{?s} to {TAG}.")
} else if (length(staged)) {
  cli_alert_info("Staged locally only. Set {.envvar CITIUS_HARVEST_PUBLISH} to upload.")
}

# Non-zero only if EVERY meet failed: a partial harvest is real progress and
# should not read as a broken run.
if (length(failed) && !length(staged)) quit(status = 1)
quit(status = 0)
