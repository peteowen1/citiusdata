# Harvest Swim England event rankings -- the official British domestic database.
#
# WHY: World Aquatics cannot represent home-nations swimmers. England, Scotland
# and Wales all compete internationally as GBR, and Jersey/Guernsey/Isle of Man
# are not World Aquatics members at all, so a Commonwealth Games entrant who has
# never swum a sanctioned international meet is invisible to it. That is the
# whole reason 95 Glasgow swimmers have no prior form in our corpus.
#
# WHAT THIS IS NOT: these are ranked lists, i.e. each swimmer's BEST time for a
# given event/pool/year. They are truncated at the good end by construction, so
# they support ability estimation but NOT variance estimation. Every row carries
# is_best = TRUE so calibration can exclude them explicitly.
#
# SHAPE: one job per (nationality, pool, sex, year, stroke), paging adaptively
# until a short page or MAX_PAGES. Jobs run in parallel; paging within a job is
# serial because each page depends on the previous one's length.
#
# Usage:
#   Rscript scripts/harvest_swimengland_rankings.R
#   CITIUS_WORKERS=9 CITIUS_MAX_PAGES=10 CITIUS_YEARS=2010:2026 Rscript ...
VERSE <- here::here()
suppressMessages({library(citius); library(data.table); library(parallel)})
OUT <- file.path(VERSE, "citiusdata", "data")
CACHE <- file.path(OUT, "se_rankings_cache")
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
say <- function(...) cat(sprintf(...), "\n", sep = "")

WORKERS   <- as.integer(Sys.getenv("CITIUS_WORKERS", "16"))
MAX_PAGES <- as.integer(Sys.getenv("CITIUS_MAX_PAGES", "10"))
# Accepts "2010:2026" or "2014,2016,2020". Parsed explicitly rather than with
# eval(parse()) -- this is a scheduled script and an env var is not a place to
# accept arbitrary code, however local the caller.
yr_env <- Sys.getenv("CITIUS_YEARS", "2010:2026")
YEARS <- if (grepl("^\\s*\\d{4}\\s*:\\s*\\d{4}\\s*$", yr_env)) {
  b <- as.integer(strsplit(gsub("\\s", "", yr_env), ":")[[1]])
  seq.int(b[1], b[2])
} else {
  as.integer(strsplit(gsub("\\s", "", yr_env), ",")[[1]])
}
if (anyNA(YEARS) || !length(YEARS)) stop("CITIUS_YEARS must be 'a:b' or 'a,b,c'")

# Measured on REALISTIC deep jobs: 16 workers is the sweet spot (1.46x over 6);
# 20 buys only 4% more for 25% more load. An earlier tuning run on single-page
# jobs said 9, which was wrong because page 1 costs the server 0.096s while a
# StartNumber=501 page costs ~2.9s -- the deep pages are the actual work, and
# they are what the sweep is mostly made of. Row counts were identical at every
# worker setting, so nothing is being shed. RecordsToView is capped at 100 by
# the server however much is requested, so paging is the only way to go deeper.
#
# A cached job records whatever MAX_PAGES was in force when it ran. Re-running
# with a larger MAX_PAGES will NOT deepen existing cache entries -- clear the
# cache (or the affected keys) if the depth changes.
strokes <- swimengland_strokes()$stroke_code
# 'A' (British) excludes the Crown Dependencies -- Jersey returns its own list --
# so they are swept separately. They are tiny, which is why the cost is trivial.
nats <- c("X", "J", "G", "I")

grid <- CJ(nat = nats, pool = c("L", "S"), sex = c("M", "F"),
           year = YEARS, stroke = strokes)
grid[, key := sprintf("%s_%s_%s_%s_%s", nat, pool, sex, year, stroke)]
done <- sub("\\.rds$", "", list.files(CACHE))
todo <- grid[!key %in% done]
say("grid: %s jobs | already cached: %s | to fetch: %s",
    format(nrow(grid), big.mark = ","), format(length(done), big.mark = ","),
    format(nrow(todo), big.mark = ","))
if (!nrow(todo)) { say("nothing to do"); quit(save = "no") }

# Every value a worker needs travels INSIDE the job. Referencing an outer object
# from a PSOCK worker fails silently and the run reports success having written
# nothing -- which is exactly what happened on the last parallel harvest here.
jobs <- lapply(seq_len(nrow(todo)), function(i) list(
  nat = todo$nat[i], pool = todo$pool[i], sex = todo$sex[i],
  year = todo$year[i], stroke = todo$stroke[i], key = todo$key[i],
  cache = CACHE, max_pages = MAX_PAGES))

fetch_one <- function(j) {
  out <- list()
  for (p in seq_len(j$max_pages)) {
    r <- tryCatch(citius::swimengland_rankings(
      j$stroke, j$pool, j$sex, j$year, j$nat,
      start = 1L + (p - 1L) * 100L, n = 100L), error = function(e) NULL)
    if (is.null(r) || !nrow(r)) break
    out[[length(out) + 1L]] <- r
    # A short page means the list is exhausted. Most (year, event) combinations
    # are shallow -- 2010 has 99 ranked swimmers where 2026 has thousands -- so
    # stopping early is where most of the saving comes from.
    if (nrow(r) < 100L) break
  }
  res <- if (length(out)) data.table::rbindlist(out, fill = TRUE) else data.table::data.table()
  saveRDS(res, file.path(j$cache, paste0(j$key, ".rds")))
  nrow(res)
}

say("fetching on %d worker%s, up to %d page%s per job ...", WORKERS,
    if (WORKERS == 1) "" else "s", MAX_PAGES, if (MAX_PAGES == 1) "" else "s")
cl <- makeCluster(WORKERS)
on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
invisible(clusterEvalQ(cl, suppressMessages(library(citius))))

t0 <- Sys.time()
CHUNK <- 200L
chunks <- split(jobs, ceiling(seq_along(jobs) / CHUNK))
total <- 0L
for (i in seq_along(chunks)) {
  n <- tryCatch(unlist(parLapply(cl, chunks[[i]], fetch_one)),
                error = function(e) { say("  chunk %d FAILED: %s", i, conditionMessage(e)); NULL })
  # A chunk that dies must be visible. The last parallel harvest swallowed
  # worker errors and reported success having written zero files.
  if (is.null(n)) next
  total <- total + sum(n)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  frac <- i / length(chunks)
  say("  chunk %d/%d | %s rows so far | %.0fs elapsed | ~%.0fs remaining | %.2f req-equiv/s",
      i, length(chunks), format(total, big.mark = ","), el,
      el / frac - el, sum(lengths(chunks[seq_len(i)])) / el)
}
stopCluster(cl)
say("\nfetched %s rows in %.1f min", format(total, big.mark = ","),
    as.numeric(difftime(Sys.time(), t0, units = "mins")))
say("assemble with:  Rscript scripts/assemble_swimengland.R")
