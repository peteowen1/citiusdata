# Does the career sweep actually thicken the histories the model rates on?
#
# This is the test the whole sweep exists to pass. ~2M new results only matter
# if they raise races per ATHLETE-EVENT, which is what estimate_ability() groups
# by and what w_total is computed from. "More data" is not the claim; "more
# evidence per rated ability" is.
#
# COMPARES LIKE WITH LIKE. It measures only athletes we have actually fetched a
# career for, and compares their depth in championship_results alone against
# championship_results PLUS the career rows. Comparing swept athletes against
# the whole population would measure who was selected, not what was gained --
# the cohort was chosen because they are elite, so they are deeper to begin with.
#
# Reads wa_careers.parquet, so run assemble_wa_careers.R first. Safe to run
# mid-sweep for an early read: it reports how many athletes it is based on.
#
# Usage: Rscript citiusdata/scripts/measure_career_gain.R

VERSE <- here::here()
suppressMessages({library(data.table); library(arrow); library(cli)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                          sprintf(...), "\n", sep = ""); flush.console() }

f <- file.path(D, "wa_careers.parquet")
if (!file.exists(f)) cli_abort("No {.file wa_careers.parquet}; run assemble_wa_careers.R first.")
ca <- as.data.table(read_parquet(f, col_select = c("athlete_id","event_id","date")))
ca <- ca[!is.na(event_id) & !is.na(date)]
ids <- unique(as.character(ca$athlete_id))
say("career rows: %s across %s athletes", format(nrow(ca), big.mark=","), format(length(ids), big.mark=","))

cut <- Sys.Date() - 4380
ca <- ca[date >= cut]
say("inside the 12-year window: %s rows", format(nrow(ca), big.mark=","))

# The SAME athletes, as championship_results alone sees them.
ch <- as.data.table(with_citius_db_connection(function(cn) DBI::dbGetQuery(cn, sprintf(
  "SELECT athlete_id, event_id FROM championship_results
   WHERE date >= DATE '%s' AND event_id IS NOT NULL AND athlete_id IN (%s)",
  as.character(cut), paste(sprintf("'%s'", ids), collapse=","))), read_only = TRUE))
say("competition rows for the same athletes: %s", format(nrow(ch), big.mark=","))

pair <- function(x, lbl) {
  p <- x[, .N, by = .(athlete_id, event_id)]
  data.table(basis = lbl, pairs = nrow(p), median = median(p$N), mean = round(mean(p$N), 2),
             p90 = as.numeric(quantile(p$N, 0.9)),
             thin_le2 = round(100 * mean(p$N <= 2), 1))
}
a <- pair(ch, "competition only")
both <- rbind(ch[, .(athlete_id = as.character(athlete_id), event_id)],
              ca[, .(athlete_id = as.character(athlete_id), event_id)])
b <- pair(both, "competition + career")

say("")
say("=== races per ATHLETE-EVENT, 12-year window, SAME athletes ===")
say("(higher median/mean is better; thin_le2 = %% of pairs with <=2 races, lower is better)")
print(rbind(a, b))
say("")
say("median  %.0f -> %.0f", a$median, b$median)
say("mean    %.2f -> %.2f  (%+.0f%%)", a$mean, b$mean, 100*(b$mean-a$mean)/a$mean)
say("thin    %.1f%% -> %.1f%% of pairs on <=2 races", a$thin_le2, b$thin_le2)
say("pairs   %s -> %s  (%+s new athlete-event pairs the model can rate)",
    format(a$pairs, big.mark=","), format(b$pairs, big.mark=","),
    format(b$pairs - a$pairs, big.mark=","))

# A gain that is all in events we never rate is not a gain.
new_pairs <- fsetdiff(unique(both[, .(athlete_id, event_id)]),
                      unique(ch[, .(athlete_id = as.character(athlete_id), event_id)]))
say("")
say("of the %s NEW athlete-event pairs, top events:", format(nrow(new_pairs), big.mark=","))
print(head(new_pairs[, .N, by = event_id][order(-N)], 8))
