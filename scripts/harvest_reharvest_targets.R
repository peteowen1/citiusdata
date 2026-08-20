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
source(here::here("citiusdata", "scripts", "_env.R"))
D          <- here::here("citiusdata", "data")
COMP_CACHE <- file.path(D, "ath_comp_cache")
dir.create(COMP_CACHE, recursive = TRUE, showWarnings = FALSE)
MAXN  <- .env_int("REHARVEST_MAX", "25")     # small by default: prove it works first
PAUSE <- .env_num("REHARVEST_PAUSE", "1.0")  # seconds between competitions
TARGETS <- Sys.getenv("REHARVEST_TARGETS", "reharvest_targets.csv")

f <- file.path(D, TARGETS)
stopifnot("target list missing - run build_reharvest_targets.R first" = file.exists(f))
t <- fread(f)
t[, competition_id := as.character(competition_id)]
stopifnot("target list is empty" = nrow(t) > 0,
          "target list has no ranking column" = "rows_in_merged_races" %chin% names(t))
setorder(t, -rows_in_merged_races)

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
span <- t[, .(competition_id, dur = 3L)]
if ("last" %chin% names(t)) span <- t[, .(competition_id, dur = 3L)]
todo <- merge(todo, span, by = "competition_id", all.x = TRUE)
todo[is.na(dur) | dur < 1L, dur := 3L]
setorder(todo, -rows_in_merged_races)

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
