# forecast_kind: is a published card a real forecast or a rebuild after the fact?
#
# Red-team F1 (docs/reviews/OPUS-redteam-2026-10-25.md): all six athletics cards
# on the site were re-predicted on 2026-09-30, 17-51 days after their meets,
# under a calibration whose corpus contained those meets, while the site said
# every card was "locked before the first day and never rewritten". Nothing in
# the published data could tell a reader otherwise: the export overwrites the
# card's own `generated_at` with the publish time.
#
# So the kind is DERIVED FROM THE STAMPS, never from a hand-kept list:
#   "locked"        the predict run that produced the card finished before
#                   00:00 UTC on the meet's first day (calendar date_start);
#   "retrospective" anything else, including a card with no usable stamp
#                   (a forecast that cannot prove when it was made is not
#                   locked).
# The latest row stamp is used, so a card any row of which was written after
# the meet started counts as retrospective.
#
# `field_type` is a separate question (where the start list came from) and is
# left alone; a card can be locked and still have a provisional field.

FORECAST_KINDS <- c("locked", "retrospective")

forecast_kind <- function(generated_at, date_start) {
  stopifnot("date_start must be one date" = length(date_start) == 1L,
            "date_start is missing" = !is.na(date_start))
  first_day_utc <- as.POSIXct(paste(as.Date(date_start), "00:00:00"), tz = "UTC")
  st <- suppressWarnings(as.POSIXct(generated_at))
  if (!length(st) || all(is.na(st))) {
    warning("card has no generated_at stamp: forecast_kind set to 'retrospective'",
            call. = FALSE)
    return("retrospective")
  }
  attr(st, "tzone") <- "UTC"  # same instant, just silences the tzone-mismatch warning
  if (max(st, na.rm = TRUE) < first_day_utc) "locked" else "retrospective"
}

# The predict-run stamp itself, kept beside forecast_kind because the export
# writes the publish time over `generated_at`. Returned as POSIXct UTC.
forecast_generated_at <- function(generated_at) {
  st <- suppressWarnings(as.POSIXct(generated_at))
  if (!length(st) || all(is.na(st))) return(as.POSIXct(NA, tz = "UTC"))
  out <- max(st, na.rm = TRUE)
  attr(out, "tzone") <- "UTC"
  out
}
