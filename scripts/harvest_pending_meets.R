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

cal <- fread(file.path(D, "athletics_calendar.csv"))
cal[, date_end := as.Date(date_end)]

forced <- strsplit(Sys.getenv("CITIUS_HARVEST_MEETS", ""), ",")[[1]]
forced <- trimws(forced[nzchar(forced)])

if (length(forced)) {
  pending <- cal[meet_id %in% forced]
} else {
  done <- cal[!is.na(date_end) & date_end < Sys.Date()]
  # "Not yet in our data" means absent from the catalogue, which is built FROM
  # championship_results.rds -- so catalogue membership is the honest proxy for
  # "this meet has been appended". Checking the corpus directly would be the
  # same answer one rebuild later.
  f_ctl <- file.path(D, "competition_catalogue.parquet")
  if (!file.exists(f_ctl)) {
    cli_abort("No competition_catalogue.parquet; cannot tell which meets are already in.")
  }
  suppressMessages(library(arrow))
  have <- as.character(as.data.table(
    arrow::read_parquet(f_ctl, col_select = "competition_id"))$competition_id)
  done[, cid := as.character(wa_competition_id)]
  pending <- done[!cid %in% have]
}

if (!nrow(pending)) {
  cli_alert_success("Nothing pending -- every completed meet is already in the catalogue.")
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
