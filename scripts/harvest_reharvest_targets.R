# Re-harvest the competitions whose age divisions are merged.
#
# WHY. 24,089 competitions were only ever seen through the CAREER route, whose
# per-athlete feeds carry no `eventName`. Without it the race key derives as
# competition|event|round|date, which cannot separate a U18 400m from a senior
# 400m at the same meet on the same day - Ypsilanti's women's 400m has Alena Riva
# at 57.92 in the senior final and Alice Bucher at 58.44 in the U18 final, and we
# stored them as one race with two winners.
#
# The COMPETITION endpoint returns those as separate event objects carrying
# eventName, and source_athletics.R already puts eventName in the key. So this is
# purely a coverage problem: fetch the competition, get a correct key.
#
# TARGETED, NOT EXHAUSTIVE. All 24,089 is about seven hours at a polite request
# rate. Only 3,274 show a detectable merge, and the top 2,000 of those carry
# 89.9% of the affected rows, so the list is ranked and worked from the top.
# build_reharvest_targets.R produces it.
#
# POLITE BY DEFAULT. The meet harvester records "sustained 429s from
# worldathletics.nimarion.de even at 0.75s spacing", so this waits a full second
# between competitions and backs off hard when it is asked to. It writes into the
# SAME cache the main harvester uses, so anything fetched here is not fetched
# again there, and a re-run skips whatever already landed.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(arrow))
source(here::here("citiusdata", "scripts", "_env.R"))
D          <- here::here("citiusdata", "data")
COMP_CACHE <- file.path(D, "ath_comp_cache")
dir.create(COMP_CACHE, recursive = TRUE, showWarnings = FALSE)
MAXN  <- .env_int("REHARVEST_MAX", "25")     # small by default: prove it works first
PAUSE <- .env_num("REHARVEST_PAUSE", "1.0")  # seconds between competitions
DURCAP <- .env_int("REHARVEST_DAYCAP", "12") # upper bound on day-pages per competition
TARGETS <- Sys.getenv("REHARVEST_TARGETS", "reharvest_targets.csv")

f <- file.path(D, TARGETS)
stopifnot("target list missing - run build_reharvest_targets.R first" = file.exists(f))
t <- fread(f)
t[, competition_id := as.character(competition_id)]
stopifnot("target list is empty" = nrow(t) > 0)
# TWO TARGET LISTS, RANKED ON DIFFERENT QUANTITIES. reharvest_targets.csv ranks by
# rows sitting in merged races; bigcomp_targets.csv ranks by how many results the
# competition holds at the endpoint. Both are "work the most valuable first", but
# naming one column after the other would be a quiet lie, so accept either and
# fail loudly if neither is present rather than silently fetching in file order.
.rank <- intersect(c("rows_in_merged_races", "rank_value"), names(t))[1]
stopifnot("target list has no ranking column (rows_in_merged_races or rank_value)" =
            !is.na(.rank))
setorderv(t, .rank, -1L)

cached <- sub("[.]rds$", "", list.files(COMP_CACHE, pattern = "[.]rds$"))
# A PERSISTENT FAILURE MUST BE REMEMBERED, or the ranked list guarantees we lead
# with it on every run. Some competitions make the upstream API return a 500 -
# 7065893, 7065899 and 7065902 are adjacent ids, so it is one broken meet series
# rather than a transient fault. Empty results were already cached for exactly
# this reason; errors were not, so a restart walked straight back into them.
# Recorded in a separate file rather than as an empty cache entry, because "we
# fetched it and it held nothing" and "we could not fetch it" are different facts.
FAILFILE <- file.path(D, "reharvest_failed.csv")
failed <- if (file.exists(FAILFILE)) as.character(fread(FAILFILE)$competition_id) else character(0)
DISPUTED0 <- file.path(D, "reharvest_disputed_empty.csv")
disputed <- if (file.exists(DISPUTED0)) as.character(fread(DISPUTED0)$competition_id) else character(0)
todo <- t[!competition_id %chin% cached & !competition_id %chin% failed &
          !competition_id %chin% disputed]
cat(sprintf("targets %s | already cached %s | remaining %s | this run %s\n",
            format(nrow(t), big.mark = ","),
            format(nrow(t) - nrow(todo), big.mark = ","),
            format(nrow(todo), big.mark = ","),
            format(min(MAXN, nrow(todo)), big.mark = ",")))
if (!nrow(todo)) { cat("nothing to do\n"); quit(status = 0) }

# How many day-pages to request. The meet harvester found 603 of 1,120 meets are
# a single day and a blanket 1:12 wasted 84% of requests, so use the span the
# corpus already knows about, plus a day of slack.
# THE SPAN CAME FROM THE CATALOGUE, and until 2026-08-20 it did not: both
# branches of the `if` this replaces set dur = 3L, so the intent in the comment
# above was never implemented. Two costs, and the second is the serious one.
# (1) 366 of them run LONGER than three days and were being silently TRUNCATED -
# we fetched days 1-3 of an eleven-day meet and cached the result as if it were
# the whole competition. That is worse than not fetching it at all, because a
# cached partial is skipped on every later run, so the missing days can never
# arrive and the bug reports success forever. Paris 2024, Rio, London 2012 and
# the 2025 World Championships were all in that state.
# (2) 32% run a SINGLE day, so a blanket 3 asked for two pages that cannot
# exist. That looks like a 30% saving and is not: the slack day below spends
# it, and the true totals are 15,431 day-pages against a blanket 14,877 - about
# 4% MORE. This is a correctness fix that costs a little time, not a speed-up,
# and saying otherwise here would be the same overclaiming the fix exists to
# undo.
cgs <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet"),
                          col_select = c("competition_id", "first_date", "last_date",
                                         "results")))
setnames(cgs, "results", "expect")   # rows the corpus already holds for this meet
cgs[, competition_id := as.character(competition_id)]
cgs[, dur := as.integer(last_date - first_date) + 1L]
# A day of slack past the catalogue span: the catalogue dates come from results
# we already hold, so a day whose results we are missing entirely cannot widen
# them - which is exactly the day this harvest exists to find.
cgs[, dur := dur + 1L]
cgs[is.na(dur) | dur < 1L, dur := 3L]
# SAY SO WHEN THE CAP BITES. Capping is truncation, and truncation cached as if
# complete is the exact bug this block exists to fix - silent is how it survived
# the first time. The longest real span today is 12 days, so the cap currently
# clips only the slack, but nothing guarantees that stays true.
.capped <- cgs[dur > DURCAP, .N]
cgs[dur > DURCAP, dur := DURCAP]
if (.capped > 0L)
  cat(sprintf("NOTE: %s competition(s) span more than the %d-day cap and will be\n",
              format(.capped, big.mark = ","), DURCAP),
      "  fetched incompletely - raise REHARVEST_DAYCAP if that matters\n", sep = "")
todo <- merge(todo, cgs[, .(competition_id, dur, expect)], by = "competition_id",
              all.x = TRUE)
todo[is.na(dur) | dur < 1L, dur := 3L]
cat(sprintf("day-pages to request: %s across %s competitions (a blanket 3 would be %s)
",
            format(sum(todo$dur), big.mark = ","), format(nrow(todo), big.mark = ","),
            format(3L * nrow(todo), big.mark = ",")))
setorderv(todo, .rank, -1L)

n_ok <- 0L; n_empty <- 0L; n_err <- 0L; got <- 0L; new_fail <- character(0)
# EMPTY-BUT-DISPUTED IS ITS OWN OUTCOME, not an error. Counting it as one made
# a batch containing nothing else look like total failure, which tripped the
# endpoint-liveness guard below and aborted the whole chain - so the T1/T2 list
# it was queued ahead of never ran. The endpoint was healthy throughout.
n_fault <- 0L; new_fault <- character(0)
for (i in seq_len(min(MAXN, nrow(todo)))) {
  cid <- todo$competition_id[i]
  r <- tryCatch(athletics_competition_results(cid, days = seq_len(todo$dur[i])),
                error = function(e) { msg <- conditionMessage(e)
                  # a 429 means slow down, not that the competition is missing
                  if (grepl("429|rate|too many", msg, ignore.case = TRUE)) {
                    cat("  rate limited - backing off 30s\n"); Sys.sleep(30) }
                  structure(list(), class = "reharvest_error", msg = msg) })
  if (inherits(r, "reharvest_error")) {
    n_err <- n_err + 1L
    # a 500 is the upstream failing to build THAT competition; it will not fix
    # itself, so record it rather than retrying it forever
    if (grepl("500|Internal Server", attr(r, "msg"))) new_fail <- c(new_fail, cid)
    cat(sprintf("  [%d/%s] %s ERROR %s\n", i, format(min(MAXN, nrow(todo))), cid,
                substr(attr(r, "msg"), 1, 60)))
  } else if (is.null(r) || !NROW(r)) {
    # AN EMPTY RESPONSE IS ONLY A FACT IF THE CORPUS AGREES. Caching empty is
    # meant to record "we fetched it and it held nothing" so it is not retried
    # forever - but the endpoint also returns empty transiently, and then the
    # cache preserves the fault as the fact, permanently and silently. On
    # 2026-08-20 a re-fetch of 7196499 (Jamaican Championships 2023) returned
    # nothing and overwrote 473 good rows, and a scan then found 47 competitions
    # cached as empty while the corpus holds 20,201 rows for them - among them
    # the 2025 World Indoor Championships and the Kenyan and South African
    # nationals. The catalogue knows how many rows we already hold, so it can
    # tell the two apart: empty from a competition the corpus credits with
    # results is a FAULT, and a fault must not be written down as an answer.
    .expect <- todo$expect[i]
    if (!is.na(.expect) && .expect > 0) {
      n_fault <- n_fault + 1L
      new_fault <- c(new_fault, cid)
      cat(sprintf("  [%d] %s EMPTY but the corpus holds %s rows - treating as a fault, not caching\n",
                  i, cid, format(.expect, big.mark = ",")))
      Sys.sleep(PAUSE)
      next
    }
    n_empty <- n_empty + 1L
    saveRDS(data.table(), file.path(COMP_CACHE, paste0(cid, ".rds")))
  } else {
    # A RE-FETCH MUST NEVER SHRINK A CACHE ENTRY. Not by half, not by one row.
    #
    # The first version of this guard compared against the CORPUS count and
    # allowed anything above 50% of it, which was a guess dressed as a rule: it
    # would have let a response carrying 51% of the rows overwrite a complete
    # one. Two different comparisons were being conflated.
    #
    #   endpoint vs CORPUS   - different sources, legitimately different. The
    #                          endpoint usually returns MORE (median 1.86x, which
    #                          is the whole premise of the big-competition list),
    #                          but 1.5% of competitions come in below the corpus
    #                          count for honest reasons. Cannot be a hard gate.
    #   endpoint vs ENDPOINT - the same source twice, and measured over 128
    #                          re-fetched competitions it is deterministic:
    #                          102 identical, 26 grew, ZERO shrank.
    #
    # So the like-for-like comparison needs no threshold at all. Keep the larger
    # of old and new; a shrink is a transport fault every time.
    .old <- file.path(COMP_CACHE, paste0(cid, ".rds"))
    .have <- if (file.exists(.old)) NROW(readRDS(.old)) else 0L
    if (NROW(r) < .have) {
      n_err <- n_err + 1L
      cat(sprintf("  [%d] %s returned %s rows but the cache already holds %s - keeping the cached copy\n",
                  i, cid, format(NROW(r), big.mark = ","), format(.have, big.mark = ",")))
      Sys.sleep(PAUSE)
      next
    }
    # A FIRST fetch well below the corpus count is worth saying out loud, but it
    # is still the only copy we have, so it is cached rather than refused.
    .expect <- todo$expect[i]
    if (.have == 0L && !is.na(.expect) && .expect > 0 && NROW(r) < .expect)
      cat(sprintf("  [%d] %s: %s rows against %s in the corpus - caching anyway, nothing to compare\n",
                  i, cid, format(NROW(r), big.mark = ","), format(.expect, big.mark = ",")))
    saveRDS(r, .old)
    n_ok <- n_ok + 1L; got <- got + NROW(r)
    if (i %% 10 == 0 || i <= 3)
      cat(sprintf("  [%d] %s -> %s rows\n", i, cid, format(NROW(r), big.mark = ",")))
  }
  Sys.sleep(PAUSE)
}
cat(sprintf("\nfetched %d competition(s), %s rows | empty %d | disputed-empty %d | errors %d\n",
            n_ok, format(got, big.mark = ","), n_empty, n_fault, n_err))
# RECORDED BEFORE THE GUARD BELOW, deliberately. A run that hits only broken
# competitions would otherwise abort at the guard without recording them, and
# the next run would lead with exactly the same ids.
#
# And only when something ELSE succeeded: a 500 while other competitions are
# fetching fine means that competition is broken upstream, while a 500 on
# everything means the endpoint is down and blacklisting the list would be
# self-inflicted damage.
# COUNT ATTEMPTS, so a FINAL stuck item can still retire itself. The condition
# below is right in spirit - do not blacklist a whole list because the endpoint
# is down - but it deadlocks at the tail: when the only competition left is a
# persistent 500, nothing else can succeed, so it is never recorded and blocks
# every subsequent run. 7158255 had to be retired by hand on 2026-08-20. An
# attempt counter settles it without weakening the endpoint-down protection:
# two independent runs failing on the same id is not a coincidence.
ATT <- file.path(D, "reharvest_attempts.csv")
if (length(new_fail)) {
  .a <- if (file.exists(ATT)) fread(ATT) else data.table(competition_id = character(0), n = integer(0))
  .a[, competition_id := as.character(competition_id)]
  .a <- rbind(.a, data.table(competition_id = new_fail, n = 1L))[, .(n = sum(n)), by = competition_id]
  fwrite(.a, ATT)
  .worn <- .a[n >= .env_int("REHARVEST_MAX_ATTEMPTS", "3"), competition_id]
  .worn <- intersect(.worn, new_fail)
  if (length(.worn) && n_ok == 0L && n_empty == 0L && n_fault == 0L) {
    fwrite(data.table(competition_id = unique(c(failed, .worn))), FAILFILE)
    cat(sprintf("recorded %d competition(s) as persistently failing after repeated attempts
",
                length(.worn)))
  }
}
if (length(new_fail) && n_ok > 0) {
  fwrite(data.table(competition_id = unique(c(failed, new_fail))), FAILFILE)
  cat(sprintf("recorded %d competition(s) as persistently failing
", length(new_fail)))
} else if (length(new_fail)) {
  cat(sprintf("%d error(s) but nothing succeeded - NOT recording them; the endpoint may be down
",
              length(new_fail)))
}

# An all-error run means the endpoint changed or we are blocked, and silently
# caching nothing would look like success on the next run.
# LIVENESS IS ABOUT WHETHER THE ENDPOINT ANSWERED, not whether we kept the
# answer. An empty-but-disputed response is still a response, and a run whose
# remaining work happens to be all disputed is not evidence the endpoint is
# down. Reading it that way aborted the chain on 2026-08-20 with the endpoint
# perfectly healthy, and cost the T1/T2 pass its whole overnight window.
# AND IT MUST BE PROPORTIONATE TO THE BATCH. "Nothing succeeded" is evidence
# about the endpoint only when enough was attempted for it to mean something.
# With one competition left, and that one a known-bad 500, this fired and
# aborted the chain - twice - while the endpoint was healthy. Worse, it is a
# deadlock: the 500 blacklist below only records when something ELSE succeeded,
# so a final stuck item can never be retired and never stops blocking.
.attempted <- min(MAXN, nrow(todo))
LIVE_MIN <- .env_int("REHARVEST_LIVENESS_MIN", "5")
if (.attempted < LIVE_MIN && n_ok == 0L && n_empty == 0L && n_fault == 0L) {
  cat(sprintf("all %d attempted failed, but that is too few to judge the endpoint on\n",
              .attempted))
} else {
  stopifnot("every request failed - check the endpoint before re-running" =
              n_ok > 0 || n_empty > 0 || n_fault > 0)
}
# A DISPUTED EMPTY MUST BE REMEMBERED, for the same reason a 500 is: otherwise
# a ranked list leads with it on every single run. Recorded in its own file
# rather than as an empty cache entry, because "the endpoint says empty and the
# corpus disagrees" is a different fact from "it holds nothing", and writing
# the second one down is what this whole change set exists to stop.
DISPUTED <- file.path(D, "reharvest_disputed_empty.csv")
if (length(new_fault)) {
  .prev <- if (file.exists(DISPUTED)) as.character(fread(DISPUTED)$competition_id) else character(0)
  fwrite(data.table(competition_id = unique(c(.prev, new_fault))), DISPUTED)
  cat(sprintf("recorded %d competition(s) as disputed-empty; delete %s to retry them\n",
              length(new_fault), basename(DISPUTED)))
}
cat("Re-run to continue; already-cached competitions are skipped.\n")
cat("Rebuild the corpus afterwards for the new keys to take effect.\n")
