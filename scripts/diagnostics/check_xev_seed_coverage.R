# What limits cross-event seeding to one sibling per athlete?
#
# The seeding fired on 24,720 athlete-events at a mean of 1.0 siblings each.
# One is suspiciously round: 589 event pairs clear the 0.30 correlation gate, so
# an athlete who has raced two or three correlated events should be drawing on
# all of them. Either debutants genuinely have only one prior event, or a join
# is dropping the rest - and those want opposite responses.
#
# Four candidate limits, each of which would show up differently below:
#   1. debutants really do have one prior event - nothing to fix
#   2. the 0.30 correlation gate is too strict for the pairs they actually hold
#   3. the similarity table does not COVER their pairs at all - it holds 589
#      pairs out of 85*84/2 = 3,570 possible, so most pairs simply are not in it
#   4. the join drops rows for a mechanical reason (id type, missing mean)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D <- here::here("citiusdata", "data")

sm <- setDT(read_parquet(file.path(D, "event_similarity_spec.parquet")))
simcol <- intersect(c("cor_use", "cor_shrunk", "cor"), names(sm))[1]
cat(sprintf("similarity table: %s pairs, correlation column `%s`\n",
            format(nrow(sm), big.mark = ","), simcol))
sim <- rbindlist(list(sm[, .(event_id = e1, sib = e2, cr = get(simcol))],
                      sm[, .(event_id = e2, sib = e1, cr = get(simcol))]))
cat(sprintf("mirrored: %s directed pairs | clearing 0.30: %s | 0.20: %s | 0.10: %s\n",
            format(nrow(sim), big.mark = ","),
            format(sim[cr >= 0.30, .N], big.mark = ","),
            format(sim[cr >= 0.20, .N], big.mark = ","),
            format(sim[cr >= 0.10, .N], big.mark = ",")))
cat(sprintf("events with at least one sibling at 0.30: %s of %s in the table\n",
            format(sim[cr >= 0.30, uniqueN(event_id)], big.mark = ","),
            format(uniqueN(sim$event_id), big.mark = ",")))

# how many siblings does each event have, at each gate?
cat("\n=== siblings per event, by gate ===\n")
print(rbindlist(lapply(c(0.30, 0.20, 0.10), function(g)
  data.table(gate = g,
             events_with_any = sim[cr >= g, uniqueN(event_id)],
             median_siblings = sim[cr >= g, .N, by = event_id][, stats::median(N)],
             max_siblings    = sim[cr >= g, .N, by = event_id][, max(N)]))))

# --- what do actual debutants hold? -----------------------------------------
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                        col_select = c("athlete_id","event_id","date","seen")))
h <- h[is.finite(as.numeric(date))]
setorder(h, athlete_id, date)
# first appearance per athlete-event, and every OTHER event they raced before it
fd <- h[, .(first_date = min(date)), by = .(athlete_id, event_id)]
cold <- fd[h[seen == FALSE, .(athlete_id, event_id)], on = .(athlete_id, event_id),
           nomatch = NULL]
cold <- unique(cold)
cat(sprintf("\ncold athlete-events: %s\n", format(nrow(cold), big.mark = ",")))

prior <- h[cold[, .(athlete_id, target = event_id, first_date)],
           on = .(athlete_id), allow.cartesian = TRUE, nomatch = NULL]
prior <- unique(prior[date < first_date & event_id != target,
                      .(athlete_id, target, prior_event = event_id)])
cat(sprintf("cold athlete-events with ANY prior other event: %s\n",
            format(uniqueN(prior[, .(athlete_id, target)]), big.mark = ",")))
cat("\n=== how many distinct prior events do they hold? ===\n")
pc <- prior[, .(n_prior_events = uniqueN(prior_event)), by = .(athlete_id, target)]
print(pc[, .(athlete_events = .N), by = n_prior_events][order(n_prior_events)][1:6])

# --- of those prior events, how many are IN the similarity table at each gate?
cat("\n=== of their prior events, how many survive each gate? ===\n")
for (g in c(0.30, 0.20, 0.10)) {
  s <- sim[cr >= g]
  j <- merge(prior, s, by.x = c("target", "prior_event"),
             by.y = c("event_id", "sib"))
  cat(sprintf("  gate %.2f: %s athlete-events keep at least one sibling (mean %.2f siblings)\n",
              g, format(uniqueN(j[, .(athlete_id, target)]), big.mark = ","),
              if (nrow(j)) j[, .N, by = .(athlete_id, target)][, mean(N)] else 0))
}
cat("\nIf the count barely moves as the gate loosens, the limit is COVERAGE -\n")
cat("the similarity table simply does not contain their pairs - and loosening\n")
cat("the gate will not help. If it rises sharply, the gate is the limit.\n")

# --- which pairs are missing entirely? --------------------------------------
inany <- merge(prior, unique(sim[, .(event_id, sib)]),
               by.x = c("target", "prior_event"), by.y = c("event_id", "sib"))
cat(sprintf("\nprior-event pairs present in the similarity table AT ALL: %s of %s (%.1f%%)\n",
            format(nrow(unique(inany[, .(target, prior_event)])), big.mark = ","),
            format(nrow(unique(prior[, .(target, prior_event)])), big.mark = ","),
            100 * nrow(unique(inany[, .(target, prior_event)])) /
                  max(nrow(unique(prior[, .(target, prior_event)])), 1)))
cat("\nThe most common prior-event pairs that are NOT in the table:\n")
miss <- prior[!inany, on = .(target, prior_event)]
print(miss[, .N, by = .(target, prior_event)][order(-N)][1:10])
