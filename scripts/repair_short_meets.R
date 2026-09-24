# Replace meets we hold FEWER results for than World Athletics still returns.
#
# WHY. A 90-meet probe on 2026-09-15 found nine meets materially short -- the
# worst 3,681 rows stored against 5,839 available (59%), and the list includes
# two Pan American Games, the 2016 World Indoor Championships and the 2013 World
# Youth Championships. These are championship-tier meets, the ones meet_tier
# weights most and the backtest scores on, so missing half a meet is not a
# rounding issue.
#
# The cause is a harvest that stopped part-way through a multi-day meet: the
# stored rows show a DATE GAP inside the meet's own span. That signature has
# plenty of false positives (championship rest days; meets that genuinely run on
# non-consecutive days), so it selects candidates and a re-fetch confirms -- 37
# of 50 candidates turned out complete.
#
# WHY REPLACE AND NOT APPEND. backfill_append.R skips any competition already
# present, by design, so it cannot fix a meet that is present but incomplete.
# This deletes the meet's existing rows and inserts the fresh fetch.
#
# SAFETY. Backs up first; only touches competition_ids named on the command line
# or in short_meets.rds; asserts every meet ends with MORE rows than it started
# with, and rolls nothing forward if the re-fetch came back smaller.
#
# Usage:
#   Rscript citiusdata/scripts/repair_short_meets.R            # dry run
#   CITIUS_REPAIR_GO=1 Rscript ...                             # write

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
# The mapper calls citius_events(); without this it fails with "could not find
# function", once per meet, and the run exits 0 having repaired nothing.
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D  <- file.path(VERSE, "citiusdata", "data")
S  <- file.path(VERSE, "citiusdata", "scripts")
GO <- nzchar(Sys.getenv("CITIUS_REPAIR_GO", ""))
CH_F <- file.path(D, "championship_results.rds")
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

args <- commandArgs(trailingOnly = TRUE)
ids <- if (length(args)) as.integer(args) else {
  f <- file.path(D, "short_meets.rds")
  if (!file.exists(f)) cli_abort("No ids given and {.file short_meets.rds} is absent.")
  as.integer(readRDS(f))
}
say("meets to repair: %s", paste(ids, collapse = ", "))

# Reuse the mapper rather than copying it, so the two cannot drift.
#
# SAFETY of the eval() below: the text comes from a FIXED path inside this repo
# (append_meet_to_championship_results.R), never from user input, the network or
# any data file, and the line range is located by matching two literal anchors
# that must both be present or the script aborts. source()ing the file instead
# would execute the append itself, which is the one thing this script must not
# do. Same pattern as backfill_append.R:34-39.
src <- readLines(file.path(S, "append_meet_to_championship_results.R"))
i0 <- grep("^\\.map_harvest_to_championship <- function", src)
i1 <- grep("^# --- what championship_results\\.rds already holds", src)
if (!length(i0) || !length(i1)) cli_abort("Could not locate the mapper.")
eval(parse(text = paste(src[i0:(i1 - 1)], collapse = "\n")))

ch <- readRDS(CH_F); setDT(ch)
need <- names(ch)
say("loaded %s rows, %s competitions", format(nrow(ch), big.mark = ","),
    format(uniqueN(ch$competition_id), big.mark = ","))

plan <- data.table()
fresh <- list()
for (cid in ids) {
  before <- ch[competition_id == cid, .N]
  if (!before) { say("  %s not in the table -- skipping (use backfill_append)", cid); next }
  Sys.setenv(CITIUS_COMP = cid, CITIUS_MEET = sprintf("fix_%s", cid))
  suppressWarnings(system2("Rscript", c(shQuote(file.path(S, "harvest_wa_results.R"))),
                           stdout = FALSE, stderr = FALSE))
  f <- file.path(D, sprintf("fix_%s_raw_results.rds", cid))
  if (!file.exists(f)) { say("  %s: fetch produced nothing -- skipping", cid); next }
  h <- tryCatch(as.data.table(readRDS(f)), error = function(e) NULL)
  # Capture WHY a map failed. "could not map -- skipping" with no reason is the
  # same silent-failure shape this repo keeps getting caught by: the run exits
  # 0, repairs nothing, and reports it as if nothing needed repairing.
  err <- NULL
  m <- if (is.null(h) || !nrow(h)) NULL else
    tryCatch(.map_harvest_to_championship(h, cid, D),
             error = function(e) { err <<- conditionMessage(e); NULL })
  unlink(Sys.glob(file.path(D, sprintf("fix_%s_raw_*.rds", cid))))
  if (is.null(m)) {
    say("  %s: could not map the fetch -- %s", cid,
        if (is.null(err)) "fetch was empty" else gsub("\\s+", " ", err))
    next
  }
  miss <- setdiff(need, names(m))
  if (length(miss)) { say("  %s: mapped rows lack %s -- skipping", cid, paste(miss, collapse=",")); next }
  m <- m[, ..need]
  say("  %s: stored %s -> fetched %s (%+s)", cid, format(before, big.mark=","),
      format(nrow(m), big.mark=","), format(nrow(m) - before, big.mark=","))
  plan <- rbind(plan, data.table(competition_id = cid, before = before, after = nrow(m)))
  fresh[[as.character(cid)]] <- m
}
if (!nrow(plan)) { say("nothing to repair"); quit(status = 0) }

# NEVER shrink a meet. A smaller re-fetch means the source changed or the fetch
# was partial -- either way, replacing would DELETE results we hold.
shrink <- plan[after <= before]
if (nrow(shrink)) {
  print(shrink)
  say("%d meet(s) came back no larger; they are dropped from the repair.", nrow(shrink))
  for (cid in shrink$competition_id) fresh[[as.character(cid)]] <- NULL
  plan <- plan[after > before]
}
if (!nrow(plan)) { say("nothing left to repair after the shrink check"); quit(status = 0) }
say("will repair %d meet(s): %s -> %s rows (%+s)", nrow(plan),
    format(sum(plan$before), big.mark=","), format(sum(plan$after), big.mark=","),
    format(sum(plan$after) - sum(plan$before), big.mark=","))

if (!GO) { say("DRY RUN -- nothing written. CITIUS_REPAIR_GO=1 to commit."); quit(status = 0) }

bk <- file.path(D, sprintf("championship_results.pre_shortfix_%s.rds", format(Sys.Date(), "%Y%m%d")))
if (!file.exists(bk)) { say("backing up to %s ...", basename(bk)); file.copy(CH_F, bk) }

target <- nrow(ch) - sum(plan$before) + sum(plan$after)
out <- rbind(ch[!competition_id %in% plan$competition_id],
             rbindlist(fresh, use.names = TRUE), use.names = TRUE)
if (nrow(out) != target)
  cli_abort("Row count {nrow(out)} does not match the expected {target}.")
tmp <- paste0(CH_F, ".tmp"); saveRDS(out, tmp); file.rename(tmp, CH_F)

chk <- readRDS(CH_F); setDT(chk)
say("VERIFIED on re-read: %s rows, %s competitions",
    format(nrow(chk), big.mark=","), format(uniqueN(chk$competition_id), big.mark=","))
for (i in seq_len(nrow(plan))) {
  n <- chk[competition_id == plan$competition_id[i], .N]
  if (n != plan$after[i])
    cli_abort("Meet {plan$competition_id[i]} has {n} rows, expected {plan$after[i]}.")
}
say("all %d repaired meets verified at their new row counts", nrow(plan))
say("NOTE: DuckDB, the corpus and the stores are now stale -- refresh them.")
