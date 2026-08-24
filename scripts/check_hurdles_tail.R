# Do the hurdles have a fat tail of technical disasters, and is that why a
# season best beats the rating there?
#
# WHAT THE SLICING FOUND. The hurdles deficit is not evenly spread. Against
# season best, sealed 2026:
#   by depth   the model is still NEGATIVE at 8-15 effective races (-1.64) and
#              only turns positive at 16+ (+2.02). In the SPRINTS the same curve
#              turns positive at 8-15 (+2.31). Same shape, shifted right - the
#              hurdles need roughly twice the evidence before the rating is
#              worth more than a max.
#   by round   semi-finals -3.80 on 3,671 pairs against a 0.83 floor, heats
#              -0.99, finals -0.21 (inside its floor). The loss is in the
#              qualifying rounds, not the ones that decide medals.
#
# THE HYPOTHESIS BOTH POINT AT. A season best is a MAXIMUM, so it is immune to
# bad days by construction. A rating is an average, so it is not. Hitting a
# barrier costs a hurdler a large, sudden amount of time and says nothing about
# their form - and there is no equivalent event in a flat sprint, where a bad
# day is a slightly bad day. If the hurdles carry a fatter LEFT tail (slow
# outliers) than the sprints, then a max is structurally advantaged there, the
# rating needs more races to average the disasters out, and both slices above
# follow without needing a second explanation.
#
# WHAT WOULD REFUTE IT: symmetric tails, or a left tail no fatter than the
# sprints'. Then the deficit is about something else - pacing in qualifying,
# lane draw in the 400mH - and a robustness fix would be aimed at nothing.
#
# This measures the tail. It does not fix anything.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
MINR <- .env_int("TAIL_MIN_RACES", "6")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("event_id","athlete_id","date","perf","r_pre","n_eff","rc")))
h <- h[is.finite(perf) & is.finite(r_pre)]
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, family)]
h <- merge(h, reg, by = "event_id", all.x = TRUE)
h <- h[!is.na(family)]

# WITHIN-ATHLETE, so ability drops out entirely and what is left is the
# race-to-race variation the rating has to average over. Demeaning per
# athlete-event is the same device the wind fit uses.
h[, nr := .N, by = .(athlete_id, event_id)]
h <- h[nr >= MINR]
h[, resid := perf - mean(perf), by = .(athlete_id, event_id)]
h[, z := resid / stats::sd(resid), by = .(athlete_id, event_id)]
h <- h[is.finite(z)]
cat(sprintf("%s athlete-events with %d+ races, %s rows\n",
            format(uniqueN(h[, .(athlete_id, event_id)]), big.mark = ","), MINR,
            format(nrow(h), big.mark = ",")))
stopifnot("not enough rows to characterise a tail" = nrow(h) > 50000)

# perf is oriented so HIGHER IS BETTER in every event, so a negative z is a bad
# day in the sprints and in the throws alike. No orientation handling needed.
tail_stats <- function(d) data.table(
  rows          = nrow(d),
  sd_within     = round(stats::sd(d$resid), 5),
  skew          = round(mean(d$z^3), 3),                       # negative = fat LEFT tail
  bad_2sd       = round(100 * mean(d$z < -2), 2),
  good_2sd      = round(100 * mean(d$z >  2), 2),
  bad_3sd       = round(100 * mean(d$z < -3), 3),
  good_3sd      = round(100 * mean(d$z >  3), 3),
  worst_z       = round(min(d$z), 2))

cat("\n=== within-athlete tail shape, by family ===\n")
fs <- h[, tail_stats(.SD), by = family]
fs[, bad_minus_good_2sd := round(bad_2sd - good_2sd, 2)]
setorder(fs, -bad_minus_good_2sd)
print(fs)
cat("\nskew < 0 and bad_2sd > good_2sd both mean a fat LEFT tail: more disasters\n")
cat("than equivalent triumphs. A season best is a MAX, so it ignores that tail\n")
cat("entirely, which is exactly the advantage being measured.\n")

cat("\n=== the same, per hurdles event, against its flat equivalent ===\n")
PAIRS <- list(c("110 Metres Hurdles","100 Metres"), c("100 Metres Hurdles","100 Metres"),
              c("60 Metres Hurdles","60 Metres"),   c("400 Metres Hurdles","400 Metres"))
cmp <- rbindlist(lapply(PAIRS, function(p) {
  a <- h[discipline == p[1]]; b <- h[discipline == p[2]]
  if (!nrow(a) || !nrow(b)) return(NULL)
  cbind(data.table(hurdle = p[1], flat = p[2]),
        data.table(h_skew = round(mean(a$z^3), 3), f_skew = round(mean(b$z^3), 3),
                   h_bad2 = round(100*mean(a$z < -2), 2), f_bad2 = round(100*mean(b$z < -2), 2),
                   h_sd = round(stats::sd(a$resid), 5), f_sd = round(stats::sd(b$resid), 5)))
}), fill = TRUE)
print(cmp)
cat("\nSame athletes, same tracks, same era - the hurdle version differs from the\n")
cat("flat version only by the barriers.\n")

cat("\n=== is the tail worse in qualifying rounds? ===\n")
h[, rnd := fifelse(grepl("final", rc, ignore.case = TRUE) &
                   !grepl("semi", rc, ignore.case = TRUE), "final",
                   fifelse(grepl("semi", rc, ignore.case = TRUE), "semi", "heat"))]
print(h[family %chin% c("hurdles","sprint"), tail_stats(.SD), by = .(family, rnd)][order(family, rnd)])

f <- file.path(OUT, "hurdles_tail.json")
writeLines(jsonlite::toJSON(list(tag = TAG, by_family = fs, hurdle_vs_flat = cmp),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
