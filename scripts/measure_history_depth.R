# Baseline: how much history does each athlete actually have?
#
# WHY THIS EXISTS. The case for harvesting ~16,000 small domestic meets is NOT
# the meets -- form_ratings.R keeps only T1/T2 and inner-joins, so a T3 club
# fixture is invisible to the model. The case is ATHLETE HISTORY: more races per
# athlete raises w_total and better-evidences each ability, which is what
# estimate_ability() consumes. That is a claim, and it should be checked rather
# than assumed.
#
# Run this BEFORE the append and again after. Taking the "before" afterwards is
# impossible, and comparing two differently-written queries would measure the
# queries. Same script both times, output stamped and kept.
#
# Runs entirely as DuckDB aggregates -- no table is materialised in R -- so it
# is safe to run alongside a harvest.
#
# Usage:
#   Rscript citiusdata/scripts/measure_history_depth.R [label]

VERSE <- here::here()
suppressMessages({library(data.table)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D <- file.path(VERSE, "citiusdata", "data")
label <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(label)) label <- format(Sys.time(), "%Y%m%d_%H%M")

q <- function(sql) as.data.table(with_citius_db_connection(
  function(cn) DBI::dbGetQuery(cn, sql), read_only = TRUE))

cat(sprintf("=== history depth, label '%s' ===\n\n", label))

tot <- q("SELECT COUNT(*) n_rows, COUNT(DISTINCT athlete_id) athletes,
                 COUNT(DISTINCT competition_id) comps
          FROM championship_results")
cat(sprintf("rows %s | athletes %s | competitions %s\n",
            format(tot$n_rows, big.mark=","), format(tot$athletes, big.mark=","),
            format(tot$comps, big.mark=",")))

# Races per athlete, all time and in the window the model actually reads.
for (lbl in c("all time", "last 12 years", "last 2 years")) {
  where <- switch(lbl,
    "all time"      = "",
    "last 12 years" = "WHERE date >= CURRENT_DATE - INTERVAL 4380 DAY",
    "last 2 years"  = "WHERE date >= CURRENT_DATE - INTERVAL 730 DAY")
  r <- q(sprintf(
    "SELECT median(n) med, quantile_cont(n, 0.25) q25, quantile_cont(n, 0.75) q75,
            quantile_cont(n, 0.90) q90, AVG(n) mean, COUNT(*) athletes
     FROM (SELECT athlete_id, COUNT(*) n FROM championship_results %s
           GROUP BY athlete_id)", where))
  cat(sprintf("\nraces per athlete, %-14s: median %.0f | IQR %.0f-%.0f | p90 %.0f | mean %.1f | n=%s\n",
              lbl, r$med, r$q25, r$q75, r$q90, r$mean, format(r$athletes, big.mark=",")))
}

# The share of athletes thin enough that ability is mostly prior, not evidence.
thin <- q("SELECT
    SUM(CASE WHEN n = 1 THEN 1 ELSE 0 END) n1,
    SUM(CASE WHEN n <= 2 THEN 1 ELSE 0 END) n2,
    SUM(CASE WHEN n <= 4 THEN 1 ELSE 0 END) n4, COUNT(*) tot
  FROM (SELECT athlete_id, COUNT(*) n FROM championship_results
        WHERE date >= CURRENT_DATE - INTERVAL 4380 DAY GROUP BY athlete_id)")
cat(sprintf("\nthin athletes in the 12-year window (ability mostly prior, not evidence):\n"))
cat(sprintf("  exactly 1 race : %s (%.1f%%)\n", format(thin$n1, big.mark=","), 100*thin$n1/thin$tot))
cat(sprintf("  <= 2 races     : %s (%.1f%%)\n", format(thin$n2, big.mark=","), 100*thin$n2/thin$tot))
cat(sprintf("  <= 4 races     : %s (%.1f%%)\n", format(thin$n4, big.mark=","), 100*thin$n4/thin$tot))

# Per athlete-EVENT is what estimate_ability() actually groups by.
ae <- q("SELECT median(n) med, quantile_cont(n, 0.90) q90, AVG(n) mean, COUNT(*) pairs
         FROM (SELECT athlete_id, event_id, COUNT(*) n FROM championship_results
               WHERE date >= CURRENT_DATE - INTERVAL 4380 DAY
               GROUP BY athlete_id, event_id)")
cat(sprintf("\nraces per athlete-EVENT (what estimate_ability groups by):\n"))
cat(sprintf("  median %.0f | p90 %.0f | mean %.2f | pairs %s\n",
            ae$med, ae$q90, ae$mean, format(ae$pairs, big.mark=",")))

out <- file.path(D, sprintf("history_depth_%s.rds", label))
saveRDS(list(label = label, when = Sys.time(), totals = tot, thin = thin, athlete_event = ae), out)
cat(sprintf("\nwrote %s\n", basename(out)))
