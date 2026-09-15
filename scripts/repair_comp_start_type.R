# Restore comp_start to a real Date after the 2026-09-15 backfill turned it into
# a character column wearing a Date class.
#
# WHAT HAPPENED. fread() reads athletics_calendar.csv's date_start as IDate
# (class IDate+Date, storage INTEGER), and as.Date() on an IDate keeps integer
# storage. Only meets WITH a calendar row take that path, so competition 7212925
# produced comp_start as Date/integer while the other 3,151 backfilled meets
# produced Date/double. rbindlist() reconciled the conflict by coercing the
# whole column to character, and rbind() onto the stored table carried that
# across all 4,748,486 rows.
#
# WHY NOTHING CAUGHT IT. The class attribute stayed "Date", so is.na() returned
# FALSE on every row -- a string is not NA. Every NA-coverage gate therefore
# read comp_start as 100% populated, which looks like an improvement. The values
# print as NA and arithmetic throws "non-numeric argument to binary operator",
# which is how it finally surfaced: backtest_athletics.R takes comp_start as
# cut_date and filters history strictly before it, so the A/B aborted on its
# first meet with an error naming neither dates nor the column.
#
# The values themselves are correct ISO strings; only the storage is wrong, so
# this is a pure type conversion and not a recovery. It verifies that by
# checking no value changes meaning and that nothing becomes NA.
#
# The mapper and backfill_append.R are both fixed, so this repairs history only.
#
# Usage:
#   Rscript citiusdata/scripts/repair_comp_start_type.R      # dry run
#   CITIUS_REPAIR_GO=1 Rscript ...                           # write

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
D    <- file.path(VERSE, "citiusdata", "data")
CH_F <- file.path(D, "championship_results.rds")
GO   <- nzchar(Sys.getenv("CITIUS_REPAIR_GO", ""))
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

ch <- readRDS(CH_F); setDT(ch)
say("loaded %s rows", format(nrow(ch), big.mark = ","))

# Report EVERY column whose class disagrees with its storage, not just the one
# we know about -- the same coercion can hit any classed column.
susp <- names(ch)[vapply(names(ch), function(k) {
  x <- ch[[k]]
  (inherits(x, "Date") && !is.double(x)) || (inherits(x, "POSIXct") && !is.double(x))
}, logical(1))]
if (!length(susp)) { say("no Date column has non-double storage; nothing to repair"); quit(status = 0) }
for (k in susp)
  say("SUSPECT %s: class=%s typeof=%s, is.na()=%s (which is why coverage checks passed)",
      k, paste(class(ch[[k]]), collapse = "+"), typeof(ch[[k]]),
      format(sum(is.na(ch[[k]])), big.mark = ","))

fixed <- list()
for (k in susp) {
  raw <- unclass(ch[[k]])
  d   <- suppressWarnings(as.Date(as.character(raw)))
  n_lost <- sum(is.na(d) & !is.na(raw) & nzchar(as.character(raw)))
  say("  %s -> Date/double: %s value(s) would become NA", k, format(n_lost, big.mark = ","))
  if (n_lost > 0) cli_abort(c(
    "Converting {.field {k}} would lose {n_lost} value(s).",
    i = "That is a recovery, not a type fix. Inspect before writing."))
  fixed[[k]] <- structure(as.numeric(d), class = "Date")
  say("  %s range: %s .. %s", k,
      as.character(min(fixed[[k]], na.rm = TRUE)), as.character(max(fixed[[k]], na.rm = TRUE)))
}

if (!GO) { say("DRY RUN -- nothing written. CITIUS_REPAIR_GO=1 to convert: %s",
               paste(susp, collapse = ", ")); quit(status = 0) }

bk <- file.path(D, sprintf("championship_results.pre_typerepair_%s.rds", format(Sys.Date(), "%Y%m%d")))
if (!file.exists(bk)) { say("backing up to %s ...", basename(bk)); file.copy(CH_F, bk) }

for (k in susp) set(ch, j = k, value = fixed[[k]])
tmp <- paste0(CH_F, ".tmp"); saveRDS(ch, tmp); file.rename(tmp, CH_F)

chk <- readRDS(CH_F); setDT(chk)
for (k in susp) {
  v <- chk[[k]]
  say("VERIFIED %s: class=%s typeof=%s, %s NA, range %s .. %s", k,
      paste(class(v), collapse = "+"), typeof(v), format(sum(is.na(v)), big.mark = ","),
      as.character(min(v, na.rm = TRUE)), as.character(max(v, na.rm = TRUE)))
  if (!is.double(v)) cli_abort("{k} is still {typeof(v)} after the repair.")
  # The point of the whole exercise: it must now do arithmetic.
  invisible(v[1] - 1L)
}
say("done. DuckDB, the corpus and the stores still hold the old column -- refresh them.")
