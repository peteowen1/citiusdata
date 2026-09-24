# Is our data actually current, and which completed meets are missing?
#
# WHY THIS EXISTS. Audited 2026-09-14: citiusdata has NO scheduled data
# refresh. Every sibling verse has one (bouncerdata runs three dailies,
# pannadata one) and the absence here is the direct cause of the corpus sitting
# 9 days behind, the competition catalogue 18 days behind, and August/September
# 2026 being nearly unharvested -- 4,813 rows in August against 117,988 in July.
#
# WORSE THAN MISSING: the one daily job that does exist, the `citius blog
# refresh` scheduled task, runs export_blog_data.R and nothing else. It
# succeeds every morning and republishes the site with a fresh `generated_at`
# over an increasingly stale corpus. Its own header claims a `harvest_ok`
# banner makes failure visible, but that guard covers a FAILED harvest, not a
# never-attempted one -- so the signal that should catch staleness only fires
# on a path we never take.
#
# This script is that missing signal. It asserts freshness rather than assuming
# it, names every completed meet absent from the corpus, and exits non-zero
# when either is past tolerance so a scheduled run goes red instead of quietly
# green.
#
# READ-ONLY. It fetches nothing and mutates nothing -- deliberately. Appending
# a meet to championship_results.rds is what append_meet_to_championship_
# results.R does, and its own header explains why that must stay a deliberate
# act: "quietly adding a meet a calibration was not fitted on is how a model
# ends up evaluated on its own training data." Discovery is safe to automate;
# mutating a training input is not.
#
# Usage:
#   Rscript citiusdata/scripts/check_harvest_freshness.R
#   CITIUS_MAX_LAG_DAYS=14 Rscript ...     # tolerance before failing

suppressMessages({library(data.table); library(cli)})
D <- here::here("citiusdata", "data")
MAX_LAG <- as.integer(Sys.getenv("CITIUS_MAX_LAG_DAYS", "10"))
TODAY <- Sys.Date()
problems <- character(0)

say_lag <- function(label, latest, limit = MAX_LAG) {
  if (is.na(latest)) {
    cli_alert_warning("{label}: no date column readable")
    return(invisible(NA_integer_))
  }
  lag <- as.integer(TODAY - as.Date(latest))
  fmt <- if (lag > limit) cli_alert_danger else cli_alert_success
  fmt("{label}: latest {as.character(latest)} ({lag} day{?s} behind, limit {limit})")
  if (lag > limit) problems <<- c(problems, sprintf("%s is %d days behind", label, lag))
  invisible(lag)
}

cli_h1("Data freshness")

# The corpus is the training input; the catalogue is the tiering dimension.
# Both are read by the deployed model, so both going stale changes answers.
f_corpus <- file.path(D, "athletics_corpus.parquet")
if (file.exists(f_corpus)) {
  suppressMessages(library(arrow))
  d <- as.data.table(arrow::read_parquet(f_corpus, col_select = "date"))
  say_lag("athletics_corpus", suppressWarnings(max(d$date, na.rm = TRUE)))
  rm(d); invisible(gc(verbose = FALSE))
} else cli_alert_warning("athletics_corpus.parquet absent (not downloaded on this runner?)")

f_ctl <- file.path(D, "competition_catalogue.parquet")
if (file.exists(f_ctl)) {
  ctl <- as.data.table(arrow::read_parquet(f_ctl, col_select = c("competition_id", "last_date")))
  say_lag("competition_catalogue", suppressWarnings(max(ctl$last_date, na.rm = TRUE)), limit = MAX_LAG * 2)
} else {
  ctl <- NULL
  cli_alert_warning("competition_catalogue.parquet absent")
}

cli_h1("Completed meets missing from the corpus")

# The calendar is hand-maintained and IS in git (see citiusdata/.gitignore's
# own comment: it cannot be regenerated, because the catalogue is built from
# harvested history and structurally cannot hold a meet that has not happened).
# So it is the only source that knows a meet is due before any result exists.
cal <- fread(file.path(D, "athletics_calendar.csv"))
cal[, date_end := as.Date(date_end)]
done <- cal[!is.na(date_end) & date_end < TODAY]

if (!nrow(done)) {
  cli_alert_info("No completed meets on the calendar.")
} else if (is.null(ctl)) {
  cli_alert_warning("Cannot check membership without the catalogue.")
} else {
  have <- as.character(ctl$competition_id)
  done[, cid := as.character(wa_competition_id)]
  done[, in_catalogue := cid %in% have]
  for (i in seq_len(nrow(done))) {
    r <- done[i]
    if (isTRUE(r$in_catalogue)) {
      cli_alert_success("{r$meet_id} ({as.character(r$date_end)}): in the catalogue")
    } else {
      cli_alert_danger("{r$meet_id} ({as.character(r$date_end)}, comp {r$cid}): NOT in the catalogue")
      problems <- c(problems, sprintf("%s is missing from the catalogue", r$meet_id))
    }
  }
  # A calendar row still reading `upcoming` after the meet has ended is its own
  # bug -- it is what makes the site say a finished meet has not been forecast.
  stale_state <- done[state == "upcoming"]
  if (nrow(stale_state)) {
    cli_alert_danger("{nrow(stale_state)} completed meet{?s} still marked {.val upcoming}: {.val {stale_state$meet_id}}")
    problems <- c(problems, sprintf("%d calendar row(s) still say upcoming", nrow(stale_state)))
  }
}

cli_h1("Verdict")
if (!length(problems)) {
  cli_alert_success("All checks passed.")
  quit(status = 0)
}
cli_alert_danger("{length(problems)} problem{?s}:")
for (p in problems) cli_li(p)
# Non-zero so a scheduled workflow goes RED. The entire point of this script is
# that staleness currently produces a green run.
quit(status = 1)
