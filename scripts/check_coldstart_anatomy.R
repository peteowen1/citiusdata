# What ARE the cold-start comparisons, and how many are genuinely cold?
#
# THE GAP. The engine's own note: cold starts are 28.7% of the scored metric at
# 52.94%, while every other depth band sits at 74-77%. A coin flip on more than
# a quarter of all comparisons, and by far the largest remaining lever - every
# parameter swept in this session moved things by hundredths of a point, while
# this is twenty points on a quarter of the data.
#
# BEFORE PROPOSING A FIX, SPLIT THE POPULATION. "Cold start" is not one thing,
# and the four cases below want completely different responses:
#
#   1. GENUINELY NEW - no prior result anywhere, in this event or any other.
#      Nothing can be done from history; only entry-list data (seed marks,
#      qualifying standards) would help, and we do not hold it.
#   2. NEW TO THIS EVENT, KNOWN ELSEWHERE - has raced a correlated event. The
#      cross-event blend is supposed to cover this. Does it reach them?
#   3. KNOWN, BUT NOT MATCHED - we hold prior results under a name or id that
#      did not join. That is a data defect, not a modelling one, and it is the
#      cheapest possible fix.
#   4. SEEDED - the seeding step gave them a debut rating from the careers
#      store. Are seeded debutants scoring better than unseeded ones? If not,
#      seeding is not working and that is worth knowing before extending it.
#
# The split decides everything. If most cold starts are case 1, the 52.94% is a
# floor imposed by the sport and the lever is smaller than it looks. If many are
# case 2 or 3, it is addressable now.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
SEAL <- .env_int("COLD_SEALED", "2026")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
# DO NOT FILTER seen == TRUE HERE. `seen` is FALSE on an athlete's first race in
# an event, and those rows ARE the cold starts - the first version of this script
# filtered them out at load and then reported zero cold rows, which is a
# self-inflicted empty set rather than a finding. check_cold_coverage.R defines
# the population as `seen == FALSE`, and this must match it or the two scripts
# describe different things.
h <- h[is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
h[, yr := year(date)]
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)

# A row is COLD if the athlete carried no evidence into it.
# COLD = the athlete had not been seen in this event before this race. That is
# the engine's own definition and the one check_cold_coverage.R counts.
h[, cold := seen == FALSE]
cat(sprintf("sealed %d: %s scored rows, %s cold (%.1f%%)\n", SEAL,
            format(h[yr == SEAL, .N], big.mark = ","),
            format(h[yr == SEAL & cold == TRUE, .N], big.mark = ","),
            100 * h[yr == SEAL, mean(cold)]))

# --- CASE 2 and 3: do we hold ANY prior result for this athlete? -------------
# Their own history in the engine, and their history in the wider careers store
# which the seeding step reads.
setorder(h, athlete_id, date, race_key)
h[, prior_any_event := shift(seq_len(.N)) , by = athlete_id]
h[, has_prior_other := !is.na(prior_any_event) & prior_any_event > 0]

cs <- h[yr == SEAL & cold == TRUE]
cat("\n=== what the cold rows actually are ===\n")
cs[, kind := fifelse(has_prior_other, "raced a DIFFERENT event before",
                     "no prior scored race anywhere")]
print(cs[, .(rows = .N, pct = round(100 * .N / nrow(cs), 1)), by = kind][order(-rows)])

# --- do they carry a seeded rating? -----------------------------------------
if ("seeded" %chin% names(h)) {
  cat("\n=== seeded versus not, among cold rows ===\n")
  print(cs[, .(rows = .N, pct = round(100 * .N / nrow(cs), 1)), by = seeded][order(-rows)])
} else cat("\nNOTE: the history carries no `seeded` flag, so case 4 cannot be split here.\n")

# --- HOW DO THEY SCORE? pairs where at least one side is cold ---------------
score_pairs <- function(d, lbl) {
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, cold, has_prior_other), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (nrow(m) < 500) return(NULL)
  dd <- m$r_use.x - m$r_use.y
  m[, cw := as.numeric((dd > 0) == (place.x < place.y))][dd == 0, cw := 0.5]
  m[, involves_cold := cold.x | cold.y]
  m[, both_cold := cold.x & cold.y]
  rbind(
    data.table(slice = paste(lbl, "- neither side cold"),
               pairs = m[involves_cold == FALSE, .N], conc = round(100 * m[involves_cold == FALSE, mean(cw)], 2)),
    data.table(slice = paste(lbl, "- exactly one side cold"),
               pairs = m[involves_cold == TRUE & both_cold == FALSE, .N],
               conc = round(100 * m[involves_cold == TRUE & both_cold == FALSE, mean(cw)], 2)),
    data.table(slice = paste(lbl, "- BOTH sides cold"),
               pairs = m[both_cold == TRUE, .N], conc = round(100 * m[both_cold == TRUE, mean(cw)], 2)))
}
cat("\n=== concordance by how much of the pair is cold ===\n")
sc <- score_pairs(h[yr == SEAL], sprintf("%d", SEAL))
sc[, floor := round(100 * sqrt(0.25 / pairs), 3)]
print(sc)
cat("\nBoth-cold pairs are two athletes the model knows nothing about, so 50% is\n")
cat("the honest expectation there and nothing will move it. One-side-cold is\n")
cat("where a better debut rating can actually pay.\n")

# --- and does having raced ANOTHER event help, among the cold? --------------
cat("\n=== among one-side-cold pairs, does the cold athlete's other-event history help? ===\n")
a <- h[yr == SEAL, .(rid = .GRP, i = seq_len(.N), place, r_use, cold, has_prior_other), by = race_key]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
m <- m[(cold.x | cold.y) & !(cold.x & cold.y)]
dd <- m$r_use.x - m$r_use.y
m[, cw := as.numeric((dd > 0) == (place.x < place.y))][dd == 0, cw := 0.5]
# the cold side's own prior-elsewhere flag
m[, cold_has_other := fifelse(cold.x, has_prior_other.x, has_prior_other.y)]
print(m[, .(pairs = .N, conc = round(100 * mean(cw), 2),
            floor = round(100 * sqrt(0.25 / .N), 3)), by = cold_has_other][order(-pairs)])
cat("\nIf the two rows differ, the cross-event blend is already earning something\n")
cat("for debutants who have raced elsewhere - and the gap is what extending it\n")
cat("could be worth. If they are the same, it is not reaching them.\n")

f <- file.path(OUT, "coldstart_anatomy.json")
writeLines(jsonlite::toJSON(list(tag = TAG, sealed = SEAL, by_pair = sc),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
