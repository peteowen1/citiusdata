# DID THE REPLACEMENT-LEVEL DEBUT PRIOR HELP, AND DID IT HELP WHERE IT SHOULD?
#
# The prior only touches athletes with no rating in the event, so a pooled
# concordance figure dilutes it across the 83% of rows it cannot reach. Score by
# how much of each pair is cold instead.
#
# WHAT SHOULD MOVE, WRITTEN DOWN BEFORE LOOKING:
#
#   one side cold   SHOULD IMPROVE. The debutant was seeded 1.55 sd too high, so
#                   they were predicted to beat established athletes far too
#                   often. Currently 53.48%, barely above a coin flip. This is
#                   the whole point of the change.
#
#   both sides cold SHOULD NOT MOVE, and must not be read as a failure. Two
#                   debutants in the same race share an event, a tier and a
#                   year, so they receive the IDENTICAL prior under every option
#                   here and the pair ties by construction - which is exactly
#                   why it scores 50.00 now. Separating them needs athlete-level
#                   information the prior does not have. If this number DOES
#                   move, something is wrong: it would mean the prior varies
#                   within a single race, which none of these options should do.
#
#   neither cold    SHOULD BARELY MOVE. These athletes have their own ratings.
#                   A small drift is legitimate, because an athlete's rating
#                   history begins at their debut and a different starting point
#                   propagates - but a large move here means the change is
#                   reaching further than intended.
#
# Stating the expected pattern first is the point: a fix that improves the
# headline while moving the wrong slice is not the fix working.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
BASE <- Sys.getenv("DP_BASE", "dp_id")
ARMS <- strsplit(Sys.getenv("DP_ARMS", "dp_rep,dp_tier"), ",")[[1]]
YR   <- .env_int("DP_FROM_YEAR", "2021")

pairs_for <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) { cat(sprintf("MISSING history for '%s'\n", tag)); return(NULL) }
  h <- setDT(read_parquet(f, col_select = c("race_key","date","athlete_id",
                                            "r_pre","r_use","place","seen")))
  if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
  h[!is.finite(r_use), r_use := r_pre]
  h <- h[is.finite(r_use) & is.finite(place) & place > 0 & year(as.Date(date)) >= YR]
  a <- h[, .(rid = .GRP, i = seq_len(.N), place, r_use, seen, athlete_id), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  won <- m$place.x < m$place.y
  m[, c := fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won))]
  # COLD COUNT FROM THE BASE ARM ONLY, applied to every arm. `seen` can itself
  # differ between arms, and letting each arm define its own slices would move
  # the population and the model at the same time - the two things this
  # comparison exists to keep apart.
  m[, ncold := (!seen.x) + (!seen.y)]
  m[, pid := paste(race_key.x, pmin(athlete_id.x, athlete_id.y),
                   pmax(athlete_id.x, athlete_id.y), sep = "|")]
  unique(m[, .(pid, c, ncold)], by = "pid")
}

b <- pairs_for(BASE)
stopifnot("base arm has no pairs" = !is.null(b) && nrow(b) > 0)
cat(sprintf("base %s: %s pairs, %.3f overall\n", BASE,
            format(nrow(b), big.mark = ","), 100 * mean(b$c)))
cat("\nbase by cold slice:\n")
print(b[, .(pairs = .N, conc = round(100 * mean(c), 3)), by = ncold][order(ncold)])

out <- list()
for (tg in ARMS) {
  x <- pairs_for(tg)
  if (is.null(x)) next
  j <- merge(x[, .(pid, c_arm = c)], b, by = "pid")
  cat(sprintf("\n=== %s against %s, %s common pairs ===\n", tg, BASE,
              format(nrow(j), big.mark = ",")))
  r <- j[, .(pairs = .N,
             base = round(100 * mean(c), 3),
             arm  = round(100 * mean(c_arm), 3),
             delta = round(100 * (mean(c_arm) - mean(c)), 3),
             floor = round(100 * sqrt(0.25 / .N), 3)), by = ncold][order(ncold)]
  r[, ratio := round(delta / floor, 2)]
  r[, slice := c("neither cold", "one side cold", "both cold")[ncold + 1L]]
  print(r[, .(slice, pairs, base, arm, delta, floor, ratio)])
  tot <- j[, .(pairs = .N, base = round(100 * mean(c), 3),
               arm = round(100 * mean(c_arm), 3),
               delta = round(100 * (mean(c_arm) - mean(c)), 3),
               floor = round(100 * sqrt(0.25 / .N), 3))]
  cat(sprintf("pooled: %s pairs | %.3f -> %.3f | %+.3f (floor %.3f, %.1fx)\n",
              format(tot$pairs, big.mark = ","), tot$base, tot$arm, tot$delta,
              tot$floor, tot$delta / tot$floor))
  # THE PREDICTION, CHECKED. both-cold moving is a red flag, not a bonus.
  bc <- r[ncold == 2]
  if (nrow(bc) && abs(bc$ratio) > 2)
    cat(sprintf("  RED FLAG: both-cold moved %+.3f (%.1f floors). Two debutants in\n",
                bc$delta, bc$ratio),
        "  one race should receive the identical prior and tie - a move here\n",
        "  means the prior varies within a race, which none of these options\n",
        "  should do. Investigate before reading anything else in this table.\n", sep = "")
  out[[length(out) + 1L]] <- cbind(arm = tg, r)
}
stopifnot("no arm scored" = length(out) > 0)
f <- file.path(D, "debut_prior_scores.json")
writeLines(jsonlite::toJSON(rbindlist(out), dataframe = "rows",
                            auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
