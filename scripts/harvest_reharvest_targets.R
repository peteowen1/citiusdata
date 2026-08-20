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
todo <- t[!competition_id %chin% cached & !competition_id %chin% failed]
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
                          col_select = c("competition_id", "first_date", "last_date")))
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
todo <- merge(todo, cgs[, .(competition_id, dur)], by = "competition_id", all.x = TRUE)
todo[is.na(dur) | dur < 1L, dur := 3L]
cat(sprintf("day-pages to request: %s across %s competitions (a blanket 3 would be %s)
",
            format(sum(todo$dur), big.mark = ","), format(nrow(todo), big.mark = ","),
            format(3L * nrow(todo), big.mark = ",")))
setorderv(todo, .rank, -1L)

n_ok <- 0L; n_empty <- 0L; n_err <- 0L; got <- 0L; new_fail <- character(0)
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
    n_empty <- n_empty + 1L
    # cache the empty result too, so the next run does not retry it forever
    saveRDS(data.table(), file.path(COMP_CACHE, paste0(cid, ".rds")))
  } else {
    saveRDS(r, file.path(COMP_CACHE, paste0(cid, ".rds")))
    n_ok <- n_ok + 1L; got <- got + NROW(r)
    if (i %% 10 == 0 || i <= 3)
      cat(sprintf("  [%d] %s -> %s rows\n", i, cid, format(NROW(r), big.mark = ",")))
  }
  Sys.sleep(PAUSE)
}
cat(sprintf("\nfetched %d competition(s), %s rows | empty %d | errors %d\n",
            n_ok, format(got, big.mark = ","), n_empty, n_err))
# RECORDED BEFORE THE GUARD BELOW, deliberately. A run that hits only broken
# competitions would otherwise abort at the guard without recording them, and
# the next run would lead with exactly the same ids.
#
# And only when something ELSE succeeded: a 500 while other competitions are
# fetching fine means that competition is broken upstream, while a 500 on
# everything means the endpoint is down and blacklisting the list would be
# self-inflicted damage.
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
stopifnot("every request failed - check the endpoint before re-running" =
            n_ok > 0 || n_empty > 0)
cat("Re-run to continue; already-cached competitions are skipped.\n")
cat("Rebuild the corpus afterwards for the new keys to take effect.\n")
