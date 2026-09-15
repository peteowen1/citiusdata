# Restore `date` (and the `age` that depends on it) for meets the 2026-09-15
# backfill stored with no date at all.
#
# WHAT WENT WRONG. append_meet_to_championship_results.R's date fallback chain
# ended at `crow$date_start`, which comes from athletics_calendar.csv -- a
# 7-row file. For the 3,152 meets the backfill staged there is no calendar row,
# so when WA returned no per-race date and no day_date, nothing rescued it. 33
# meets (1,153 rows) landed with `date` NA on every row; no meet was partially
# dated, which is the signature of a per-meet fallback gap rather than scattered
# corruption.
#
# WHY IT MATTERED MORE THAN 0.02% SUGGESTS. backtest_athletics.R takes a meet's
# date as `cut_date` and selects history "strictly before" it. An NA cut_date
# made `date >= cut_date - HISTORY_DAYS` fail outright, so a single dateless
# meet at the head of the 900-meet pool aborted the entire A/B run on its first
# call, twice. The same NAs also drove build_competition_catalogue.R's
# min()/max() to Inf and out the other side as the year -5877641.
#
# The fix is in the append script so it cannot recur. This repairs what is
# already stored, from the staged harvest those meets were built from -- the
# API supplied comp_start on all 33; we dropped it.
#
# SCOPE IS EXACTLY THE BROKEN ROWS. It touches only meets where EVERY row is
# undated, and only their `date`/`age` columns. It never overwrites a date that
# is already present.
#
# Usage:
#   Rscript citiusdata/scripts/repair_dateless_meets.R          # dry run
#   CITIUS_REPAIR_GO=1 Rscript ...                              # write

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
D     <- file.path(VERSE, "citiusdata", "data")
STAGE <- file.path(D, "backfill")
CH_F  <- file.path(D, "championship_results.rds")
GO    <- nzchar(Sys.getenv("CITIUS_REPAIR_GO", ""))
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

.wa_date <- function(x) {
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  suppressWarnings(try(Sys.setlocale("LC_TIME", "C"), silent = TRUE))
  x <- as.character(x)
  d <- suppressWarnings(as.Date(x, format = "%d %b %Y"))
  iso <- suppressWarnings(tryCatch(as.Date(x), error = function(e) rep(as.Date(NA), length(x))))
  fifelse(is.na(d), iso, d)
}

ch <- readRDS(CH_F); setDT(ch)
say("loaded %s rows", format(nrow(ch), big.mark = ","))

per <- ch[, .(rows = .N, dated = sum(!is.na(date))), by = competition_id]
bad <- per[dated == 0]
if (!nrow(bad)) { say("no wholly-undated meets; nothing to repair"); quit(status = 0) }
say("wholly-undated meets: %d (%s rows)", nrow(bad), format(sum(bad$rows), big.mark = ","))
if (nrow(per[dated > 0 & dated < rows]))
  say("NOTE: %d meet(s) are PARTIALLY dated -- not touched by this repair",
      nrow(per[dated > 0 & dated < rows]))

fix <- data.table(competition_id = integer(0), start = as.Date(character(0)))
unresolved <- integer(0)
for (cid in bad$competition_id) {
  f <- file.path(STAGE, sprintf("comp_%s.rds", cid))
  if (!file.exists(f)) { unresolved <- c(unresolved, cid); next }
  h <- tryCatch(as.data.table(readRDS(f)), error = function(e) NULL)
  if (is.null(h) || !nrow(h) || !"comp_start" %in% names(h)) {
    unresolved <- c(unresolved, cid); next
  }
  s <- .wa_date(h$comp_start)
  s <- s[!is.na(s)]
  if (!length(s)) { unresolved <- c(unresolved, cid); next }
  fix <- rbind(fix, data.table(competition_id = as.integer(cid), start = min(s)))
}
say("recovered a start date for %d of %d meets", nrow(fix), nrow(bad))
if (length(unresolved))
  say("UNRESOLVED (left undated): %s", paste(head(unresolved, 10), collapse = ", "))
if (!nrow(fix)) { say("nothing recoverable"); quit(status = 0) }

# Sanity-bound every recovered date before writing it. A date outside the
# harvest's own span is a parse failure wearing a plausible type.
lo <- as.Date("1900-01-01"); hi <- Sys.Date() + 400L
if (nrow(fix[start < lo | start > hi])) cli_abort(c(
  "Recovered date(s) outside a plausible range.",
  x = "{.val {as.character(fix[start < lo | start > hi]$start)}}"))
say("recovered range: %s .. %s", as.character(min(fix$start)), as.character(max(fix$start)))
print(head(fix[order(start)], 10))

if (!GO) { say("DRY RUN -- nothing written. CITIUS_REPAIR_GO=1 to repair %s rows.",
               format(sum(bad[competition_id %in% fix$competition_id]$rows), big.mark = ",")); quit(status = 0) }

bk <- file.path(D, sprintf("championship_results.pre_daterepair_%s.rds", format(Sys.Date(), "%Y%m%d")))
if (!file.exists(bk)) { say("backing up to %s ...", basename(bk)); file.copy(CH_F, bk) }

before_na <- sum(is.na(ch$date))
# Only rows that are BOTH in a wholly-undated meet AND currently undated.
ch[fix, on = "competition_id", date := fifelse(is.na(date), i.start, date)]
# age depends on date; it was NA for exactly these rows and must follow.
ch[competition_id %chin% as.character(fix$competition_id) | competition_id %in% fix$competition_id,
   age := fifelse(is.na(age) & !is.na(date) & !is.na(birthdate),
                  as.numeric(date - birthdate) / 365.25, age)]
after_na <- sum(is.na(ch$date))
say("date NAs: %s -> %s (repaired %s rows)", format(before_na, big.mark = ","),
    format(after_na, big.mark = ","), format(before_na - after_na, big.mark = ","))
if (before_na - after_na <= 0) cli_abort("The repair changed nothing -- do not ship a no-op as a fix.")

tmp <- paste0(CH_F, ".tmp"); saveRDS(ch, tmp); file.rename(tmp, CH_F)
chk <- readRDS(CH_F); setDT(chk)
say("VERIFIED on re-read: %s rows, %s undated (%d wholly-undated meets remain)",
    format(nrow(chk), big.mark = ","), format(sum(is.na(chk$date)), big.mark = ","),
    nrow(chk[, .(d = sum(!is.na(date))), by = competition_id][d == 0]))
say("NOTE: DuckDB and the corpus still hold the OLD dates -- re-run")
say("      sync_duckdb_championship_results.R is NOT enough (it only ADDS meets);")
say("      these meets need build_athletics_corpus.R and a store rebuild.")
