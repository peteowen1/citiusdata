# WHERE DOES THE EXTRA DECADE ACTUALLY COST ANYTHING?
#
# The from2010 arm scored -0.350 against from2020 at T2. Two objections to
# reading that as "the earlier data is bad", and this script tests both.
#
# 1. IS THE COMPARISON SCORED ON PRE-2020 RACES? If it were, the result would be
#    close to circular: an arm that never saw 2010-2019 cannot be asked to order
#    2010-2019 races, so any such comparison measures population, not model.
#    The scorer filters to 2021+ before pairing, but that is a claim about code -
#    print the actual year range of the scored pairs instead.
#
# 2. IS THE LOSS CONCENTRATED IN ATHLETES WHO HAVE PRE-2020 HISTORY? This is the
#    real test of mechanism. The engine's decay and k parameters were TUNED on a
#    six-year window. Handed a sixteen-year one, they may simply forget too
#    slowly - in which case the loss is an artefact of stale hyperparameters,
#    not evidence that the older data is worthless, and the honest conclusion is
#    "does not help AT CURRENT SETTINGS" rather than "reject the data".
#
#    If the loss sits in athletes carrying pre-2020 history, that is under-decay
#    and the fix is a re-tune, not a rejection. If it is spread evenly across
#    athletes who have no pre-2020 races at all, under-decay cannot explain it
#    and something more global is going on.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
TIER <- Sys.getenv("TIER_ONLY", "T2_strong")
YR   <- .env_int("ARMS_FROM_YEAR", "2021")
BAR  <- "|"

c0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
c0[, competition_id := as.character(competition_id)]
keep <- c0[meet_tier == TIER, unique(competition_id)]

load_arm <- function(tag) {
  h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", tag)),
                          col_select = c("race_key","date","athlete_id",
                                         "r_pre","r_use","place")))
  if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
  h[!is.finite(r_use), r_use := r_pre]
  h[is.finite(r_use) & is.finite(place) & place > 0][, date := as.Date(date)][]
}

a10 <- load_arm("from2010")
a20 <- load_arm("from2020")
cat(sprintf("from2010 history spans %s..%s | from2020 spans %s..%s\n",
            min(a10$date), max(a10$date), min(a20$date), max(a20$date)))

# WHO CARRIES PRE-2020 HISTORY? Taken from the from2010 arm, which is the only
# one that can see it. An athlete with no pre-2020 race is one for whom the two
# arms hold literally the same evidence.
pre <- a10[date < as.Date("2020-01-01"), .(pre_races = .N), by = athlete_id]
cat(sprintf("athletes with pre-2020 races in the from2010 corpus: %s\n",
            format(nrow(pre), big.mark = ",")))

pairset <- function(h) {
  h <- h[competition_id_ok & year(date) >= YR]
  a <- h[, .(rid = .GRP, i = seq_len(.N), place, r_use, athlete_id, date), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  won <- m$place.x < m$place.y
  m[, c := fifelse(r_use.x == r_use.y, 0.5, as.numeric((r_use.x > r_use.y) == won))]
  m[, pid := paste(race_key.x, pmin(athlete_id.x, athlete_id.y),
                   pmax(athlete_id.x, athlete_id.y), sep = "|")]
  unique(m[, .(pid, c, yr = year(date.x), athlete_id.x, athlete_id.y)], by = "pid")
}
for (nm in c("a10", "a20")) {
  x <- get(nm)
  x[, competition_id_ok := tstrsplit(race_key, BAR, fixed = TRUE, keep = 1L)[[1]] %chin% keep]
  assign(nm, x)
}
p10 <- pairset(a10); p20 <- pairset(a20)
j <- merge(p10[, .(pid, c10 = c, yr, athlete_id.x, athlete_id.y)],
           p20[, .(pid, c20 = c)], by = "pid")
cat(sprintf("\ncommon pairs: %s\n", format(nrow(j), big.mark = ",")))

# OBJECTION 1: what years are actually being scored?
cat("\n=== year range of the scored pairs (both arms, same pairs) ===\n")
print(j[, .(pairs = .N, from2020 = round(100 * mean(c20), 3),
            from2010 = round(100 * mean(c10), 3),
            delta = round(100 * (mean(c10) - mean(c20)), 3)), by = yr][order(yr)])
cat("If every row here is 2021 or later, the comparison never scores a race the\n")
cat("from2020 arm could not have seen, and the circularity objection does not\n")
cat("apply to it.\n")

# OBJECTION 2: is the loss where under-decay would put it?
has_pre <- pre$athlete_id
j[, npre := (athlete_id.x %chin% has_pre) + (athlete_id.y %chin% has_pre)]
cat("\n=== by how many of the pair carry pre-2020 history ===\n")
r <- j[, .(pairs = .N, from2020 = round(100 * mean(c20), 3),
           from2010 = round(100 * mean(c10), 3),
           delta = round(100 * (mean(c10) - mean(c20)), 3),
           floor = round(100 * sqrt(0.25 / .N), 3)), by = npre][order(npre)]
r[, ratio := round(delta / floor, 2)]
print(r)
cat("\nnpre = 0 means NEITHER athlete has a pre-2020 race, so both arms hold the\n")
cat("same evidence about them and the delta there should be ~0. If it is not,\n")
cat("the extra decade is changing something global rather than just carrying\n")
cat("stale form forward, and under-decay is not the explanation.\n")
