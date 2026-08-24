# The large competitions we have never fetched from the competition endpoint.
#
# WHY THIS IS A DIFFERENT LIST FROM reharvest_targets.csv. That one ranks
# competitions by DETECTABLE MERGES - races where a shared key put two age
# divisions together. It is a good criterion and it barely touches the big
# meets: of the 5,359 competitions holding 200+ results, exactly 27 are on it.
#
# THE PATTERN THAT MOTIVATES THIS. Meet-name coverage runs BACKWARDS in
# competition size - 100% named at 50 results or fewer, 4.7% at 200+. That is
# not a data-quality gradient, it is two different acquisition routes. Holding
# size constant, competitions fetched from the COMPETITION endpoint are named
# 100% of the time in every band, and unfetched large ones 4.5-6.0%:
#
#   band        unfetched  fetched
#   <=50           100.0%    100%
#   51-200          81.1%    100%
#   201-1000         4.5%    100%
#   1000+            6.0%    100%
#
# The name travels with the endpoint. The per-athlete CAREER route never carried
# one, and the big meets are exactly the ones our per-athlete sweep assembled
# piecemeal. So this is pure coverage, and fetching fixes it outright.
#
# NAMES ARE NOT THE ONLY PRIZE, OR EVEN THE MAIN ONE. The same response carries
# `eventName`, which is what separates a senior 400m from a U18 400m under one
# key, and it returns the FULL field rather than the athletes our sweep happened
# to include - competition 7085681 holds 290 rows in the corpus and 1,438 at the
# endpoint. Expect a corpus vintage change, not a patch.
#
# ORDERED RECENCY-FIRST, not by size. A 2026 meet feeds the ratings that are
# published now; a 2019 one mostly feeds history. Within a year, bigger first.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D      <- here::here("citiusdata", "data")
MINRES <- .env_int("BIGCOMP_MIN_RESULTS", "200")
OUTF   <- Sys.getenv("BIGCOMP_OUT", "bigcomp_targets.csv")

cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet"),
                         col_select = c("competition_id", "comp_name", "results", "year",
                                        "first_date", "last_date")))
cg[, competition_id := as.character(competition_id)]
stopifnot("catalogue is empty or missing its results column" =
            nrow(cg) > 1000 && is.numeric(cg$results))

cached <- sub("[.]rds$", "", list.files(file.path(D, "ath_comp_cache"), pattern = "[.]rds$"))
cg[, `:=`(named   = !is.na(comp_name) & nzchar(comp_name),
          fetched = competition_id %chin% cached)]

# Anything already on the merge list is excluded rather than duplicated: the two
# harvests share one cache, so a competition on both would simply be fetched by
# whichever ran first, and carrying it twice makes the size estimate a lie.
# `$missing_column` ON A data.table IS NULL, NOT AN ERROR, and as.character(NULL)
# is character(0) - so a renamed column here would empty the exclusion list in
# total silence, which the comment above calls out as making the size estimate a
# lie. Read the column by name and fail if it is gone.
.ids <- function(path) {
  if (!file.exists(path)) return(character(0))
  z <- fread(path)
  stopifnot("target list has no competition_id column" =
              "competition_id" %chin% names(z))
  as.character(z$competition_id)
}
tf <- file.path(D, "reharvest_targets.csv")
already <- .ids(tf)

# And a competition that failed persistently upstream stays failed. The merge
# harvester records these because a ranked list otherwise leads with them on
# every single run.
ff <- file.path(D, "reharvest_failed.csv")
failed <- .ids(ff)

t <- cg[results >= MINRES & fetched == FALSE &
        !competition_id %chin% already & !competition_id %chin% failed]
stopifnot("no large unfetched competitions - has this already been run?" = nrow(t) > 0)

# rank_value, not rows_in_merged_races: this list is ranked on a different
# quantity and naming the column after the other one would be a quiet lie.
t[, rank_value := results]
setorder(t, -year, -results)

# THE DAY-PAGE COUNT IS THE HONEST COST, not competitions x 3. The sibling
# harvester stopped assuming three day-pages per competition on 2026-08-20
# precisely because it was wrong in both directions - 32% of these run a single
# day and 366 run longer than three - so repeating that assumption here would
# rebuild the same lie one file over. Same span, slack and cap it now uses.
t[, days := as.integer(last_date - first_date) + 1L + 1L]
t[is.na(days) | days < 1L, days := 3L]
t[days > 12L, days := 12L]

f <- file.path(D, OUTF)
fwrite(t[, .(competition_id, rank_value, results, year, named)], f)
# `results` IS THE CORPUS COUNT, NOT THE ENDPOINT COUNT. build_competition_
# catalogue.R sets `results = .N` over championship_results.rds - rows we already
# hold. For THIS population that makes it a FLOOR on what the harvest returns
# rather than an estimate of it: competition 7085681 holds 290 corpus rows
# against 1,438 at the endpoint, and that undercounting is the entire reason
# these competitions are on the list. Calling it "results at the endpoint" would
# state the opposite of what the header two screens up establishes.
cat(sprintf("wrote %s: %s competitions, holding %s corpus rows today\n", basename(f),
            format(nrow(t), big.mark = ","), format(sum(t$results), big.mark = ",")))
cat("  (a FLOOR on what the endpoint returns - these are exactly the competitions\n")
cat("   the per-athlete route undercounted, so expect meaningfully more)\n")
cat(sprintf("of those, %s already carry a meet name (%.1f%%)\n",
            format(sum(t$named), big.mark = ","), 100 * mean(t$named)))
cat(sprintf("day-pages to request: %s (a blanket 3 would be %s)\n",
            format(sum(t$days), big.mark = ","), format(3L * nrow(t), big.mark = ",")))
cat(sprintf("at roughly 4s per day-page that is about %.1f hours\n", sum(t$days) * 4 / 3600))
cat("\nby year, in the order they will be fetched:\n")
print(t[, .(competitions = .N, results = sum(results)), by = year][order(-year)])
cat("\nHarvest with:\n")
cat(sprintf("  REHARVEST_TARGETS=%s REHARVEST_MAX=<n> Rscript citiusdata/scripts/harvest_reharvest_targets.R\n",
            OUTF))
cat("Rebuild the corpus afterwards, then follow docs/plans/post-reharvest-runbook.md.\n")
