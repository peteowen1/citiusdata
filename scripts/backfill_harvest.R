# Overnight backfill: harvest every competition we do not already have.
#
# WHY. Audited 2026-09-14: nothing fetches on a schedule, the corpus is 9 days
# behind and August 2026 holds 4,813 rows against July's 117,988. The direct
# WA route works (the community mirror has 500'd since 09-12), so the only
# thing missing was something to drive it over many meets unattended.
#
# DESIGN FOR RUNNING UNATTENDED. Every one of these exists because a long
# overnight job that dies at 3am and is discovered at 8am has wasted the night:
#
#   RESUMABLE      one staged file per competition; a completed meet is
#                  skipped on the next run, so a crash costs only the meet in
#                  flight. Re-running is always safe.
#   ISOLATED       each meet runs in its OWN Rscript subprocess. A segfault or
#                  C-level crash in one meet cannot take down the loop, which
#                  an in-process tryCatch cannot promise.
#   TIMEOUT        per-meet wall-clock cap, so one hanging request cannot
#                  consume the whole night. This is the "does not hang"
#                  guarantee, and it is enforced from outside the child.
#   MEMORY FLOOR   checks available RAM before each meet and stops cleanly if
#                  it is below the floor, rather than being OOM-killed. A
#                  3.6-hour arm died silently at 585 MB once.
#   BUDGET         a total wall-clock cap so it is finished before morning.
#   POLITE         a pause between meets; this is someone else's API.
#   JOURNAL        one line per meet to a CSV as it goes, so the morning
#                  summary is readable even if the process died.
#
# It only ever WRITES STAGED FILES. It does not append to
# championship_results.rds, rebuild the corpus, or touch any training input --
# that is backfill_append.R's job, deliberately separate, so an unattended
# fetch can never corrupt training data.
#
# Usage:
#   Rscript citiusdata/scripts/backfill_harvest.R
#   CITIUS_BF_HOURS=8        total budget (default 8)
#   CITIUS_BF_FROM=2026-01-01  earliest meet start to consider
#   CITIUS_BF_MAX=500        max meets this run
#   CITIUS_BF_MIN_FREE_MB=3000

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
D <- file.path(VERSE, "citiusdata", "data")
S <- file.path(VERSE, "citiusdata", "scripts")

HOURS       <- as.numeric(Sys.getenv("CITIUS_BF_HOURS", "8"))
FROM        <- as.Date(Sys.getenv("CITIUS_BF_FROM", "2026-01-01"))
TO          <- as.Date(Sys.getenv("CITIUS_BF_TO", as.character(Sys.Date())))
MAXN        <- as.integer(Sys.getenv("CITIUS_BF_MAX", "500"))
MIN_FREE_MB <- as.integer(Sys.getenv("CITIUS_BF_MIN_FREE_MB", "3000"))
PER_MEET_S  <- as.integer(Sys.getenv("CITIUS_BF_MEET_TIMEOUT", "300"))
# How long to tolerate a memory dip before concluding it is not a dip.
MEM_PATIENCE_S <- as.integer(Sys.getenv("CITIUS_BF_MEM_PATIENCE", "900"))
MEM_RECHECK_S  <- as.integer(Sys.getenv("CITIUS_BF_MEM_RECHECK", "30"))
PAUSE_S     <- as.numeric(Sys.getenv("CITIUS_BF_PAUSE", "1.5"))
STAGE       <- file.path(D, "backfill")
JOURNAL     <- file.path(D, "backfill_journal.csv")
dir.create(STAGE, showWarnings = FALSE, recursive = TRUE)

T0 <- Sys.time()
deadline <- T0 + HOURS * 3600
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                           sprintf(...), "\n", sep = ""); flush.console() }

free_mb <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) return(NA_real_)
  tryCatch(ps::ps_system_memory()[["avail"]] / 1024^2, error = function(e) NA_real_)
}

journal <- function(cid, name, status, rows, secs, note = "") {
  fwrite(data.table(ts = format(Sys.time()), competition_id = cid, name = name,
                    status = status, rows = rows, secs = round(secs, 1), note = note),
         JOURNAL, append = file.exists(JOURNAL))
}

# --- what we already have ----------------------------------------------------
say("loading championship_results.rds to see what is already held ...")
# Pull the ids and DROP the table immediately. Holding 4.5M rows for the rest
# of the night would sit against the memory floor below and halt the run on
# the parent's own footprint -- which is exactly what the first smoke test did.
have <- local({
  ch <- readRDS(file.path(D, "championship_results.rds"))
  ids <- unique(as.character(ch$competition_id))
  rm(ch); gc(verbose = FALSE)
  ids
})
say("already hold %s competitions (table released, %.0f MB free)",
    format(length(have), big.mark = ","), free_mb())

staged_done <- sub("^comp_", "", sub("\\.rds$", "", basename(Sys.glob(file.path(STAGE, "comp_*.rds")))))
say("already staged this/previous run: %s", format(length(staged_done), big.mark = ","))

# --- discover candidates from the WA calendar --------------------------------
# CACHE THE DISCOVERY. Paging the calendar for 2023-2026 took 28 minutes, and
# without this every restart pays it again -- which on the first overnight
# attempt would have meant 28 minutes of the budget to re-derive a list that
# had not changed. Keyed on the date range so a different range re-discovers.
CAND_F <- file.path(STAGE, sprintf("_candidates_%s_%s.rds", FROM, TO))
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
if (file.exists(CAND_F) && difftime(Sys.time(), file.info(CAND_F)$mtime, units = "hours") < 24) {
  say("reusing cached discovery %s", basename(CAND_F))
  cand <- as.data.table(readRDS(CAND_F))
} else {
  say("discovering competitions %s .. %s from the WA calendar ...", FROM, TO)
  cand <- tryCatch(as.data.table(athletics_calendar_all(start_date = FROM, end_date = TO)),
                   error = function(e) { say("calendar discovery FAILED: %s", conditionMessage(e)); NULL })
  if (!is.null(cand) && nrow(cand)) saveRDS(cand, CAND_F)
}
if (is.null(cand) || !nrow(cand)) {
  say("no candidates discovered; nothing to do")
  quit(status = 0)
}
cand[, cid := as.character(competition_id)]
# has_results is the calendar's own flag: asking for a meet with none is a
# wasted request and a spurious failure line in the journal.
if ("has_results" %in% names(cand)) cand <- cand[is.na(has_results) | has_results]
cand <- cand[!cid %in% have][!cid %in% staged_done]
cand <- unique(cand, by = "cid")
if (!is.null(cand$start_date)) setorder(cand, -start_date)   # newest first: most valuable
if (nrow(cand) > MAXN) cand <- head(cand, MAXN)
say("%s competitions to harvest this run", format(nrow(cand), big.mark = ","))
if (!nrow(cand)) quit(status = 0)

# --- harvest loop -------------------------------------------------------------
ok <- 0L; failed <- 0L; empty <- 0L
for (i in seq_len(nrow(cand))) {
  if (Sys.time() > deadline) { say("BUDGET REACHED (%.1f h) -- stopping cleanly", HOURS); break }
  # WAIT OUT A DIP, do not end the night on one.
  #
  # The first overnight attempt stopped after 28 meets at 1,405 MB free, and
  # memory was back to 5,739 MB three minutes later -- another session's job
  # fluctuates around 6.9 GB and my floor caught a trough. Halting was right
  # for "we are about to be OOM-killed" and completely wrong for "a neighbour
  # spiked for a minute", which is what it actually was.
  #
  # So: sleep and re-check, and only give up if it stays low for the whole
  # patience window. Still bounded -- a machine genuinely out of memory ends
  # the run rather than spinning until morning.
  fm <- free_mb()
  if (!is.na(fm) && fm < MIN_FREE_MB) {
    waited <- 0
    while (!is.na(fm) && fm < MIN_FREE_MB && waited < MEM_PATIENCE_S &&
           Sys.time() < deadline) {
      if (waited == 0) say("   low memory (%.0f MB) -- waiting for it to recover", fm)
      Sys.sleep(MEM_RECHECK_S); waited <- waited + MEM_RECHECK_S
      fm <- free_mb()
    }
    if (!is.na(fm) && fm < MIN_FREE_MB) {
      say("STOPPING: %.0f MB free after waiting %.0fs; the machine is genuinely short",
          fm, waited)
      journal(NA, NA, "halted_low_memory", NA, waited, sprintf("%.0f MB free", fm)); break
    }
    say("   recovered to %.0f MB after %.0fs -- continuing", fm, waited)
  }

  cid <- cand$cid[i]; nm <- cand$name[i] %||% NA_character_
  out <- file.path(STAGE, sprintf("comp_%s.rds", cid))
  t1 <- Sys.time()
  say("[%d/%d] %s -- %s", i, nrow(cand), cid, substr(nm %||% "", 1, 60))

  # SUBPROCESS, not a tryCatch: a C-level crash in one meet must not kill the
  # night. The timeout is enforced here, outside the child, so a hung HTTP
  # request cannot outlive it either.
  # system2()'s `env=` IS NOT SUPPORTED ON WINDOWS -- documented, and silently
  # ignored rather than erroring, so the child ran with no CITIUS_COMP and
  # aborted instantly with rc=5. Set them in THIS process instead; the child
  # inherits. Found by the first smoke test, which failed two meets in 0s each.
  Sys.setenv(CITIUS_COMP = cid, CITIUS_MEET = sprintf("bf_%s", cid))
  logf <- file.path(STAGE, sprintf("comp_%s.log", cid))
  rc <- tryCatch(
    system2("Rscript", c(shQuote(file.path(S, "harvest_wa_results.R"))),
            stdout = logf, stderr = logf, timeout = PER_MEET_S),
    error = function(e) 99L, warning = function(w) 98L)

  secs <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  src <- file.path(D, sprintf("bf_%s_raw_results.rds", cid))
  if (identical(as.integer(rc), 0L) && file.exists(src)) {
    n <- tryCatch(nrow(readRDS(src)), error = function(e) 0L)
    if (n > 0L) {
      file.rename(src, out); ok <- ok + 1L
      journal(cid, nm, "ok", n, secs)
      say("   ok: %s rows in %.0fs", format(n, big.mark = ","), secs)
    } else {
      unlink(src); empty <- empty + 1L
      journal(cid, nm, "empty", 0, secs)
      say("   empty")
    }
    # The sibling branches move with it, so nothing is orphaned in data/.
    for (sfx in c("startlist", "summary")) {
      f <- file.path(D, sprintf("bf_%s_raw_%s.rds", cid, sfx))
      if (file.exists(f)) file.rename(f, file.path(STAGE, sprintf("comp_%s_%s.rds", cid, sfx)))
    }
    unlink(logf)          # keep the log only for meets that failed
  } else {
    failed <- failed + 1L
    journal(cid, nm, if (identical(as.integer(rc), 124L)) "timeout" else "failed", NA, secs,
            sprintf("rc=%s", rc))
    say("   FAILED (rc=%s, %.0fs)", rc, secs)
  }
  Sys.sleep(PAUSE_S)
}

say("")
say("=== BACKFILL SUMMARY ===")
say("elapsed        %.2f h", as.numeric(difftime(Sys.time(), T0, units = "hours")))
say("harvested ok   %d", ok)
say("empty          %d", empty)
say("failed         %d", failed)
say("staged files   %d", length(Sys.glob(file.path(STAGE, "comp_*.rds"))))
say("journal        %s", JOURNAL)
say("NOTHING was appended to championship_results.rds -- run backfill_append.R for that.")
