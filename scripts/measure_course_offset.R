# Measure the short-course / long-course offset, per event, from data.
#
# WHY IT MATTERS: the Swim England corpus is 53% short course. Short course is
# roughly 5% faster, and measured swimming sigma_within is 0.73% -- so pooling
# the two courses blind injects a systematic error SEVEN TIMES the entire
# within-athlete spread. It would swamp every ability estimate.
#
# WHY NOT A PUBLISHED CONVERSION: swimmingresults.org supplies a "Converted to
# LC" column, but that is THEIR model. Using it imports someone else's
# assumptions into a package whose rule is that every number affecting an answer
# is estimated from our own data.
#
# DESIGN: within athlete, within event, within season.
#   - within-athlete removes ability, which is the big confound: better swimmers
#     race long course more often, so a cross-sectional comparison would measure
#     who swims where rather than what the pool does.
#   - within-season removes form drift and ageing across a career.
# The offset is then the mean paired difference on the oriented log scale, so it
# reads directly as a proportional advantage.
#
# Usage:  Rscript scripts/measure_course_offset.R
VERSE <- here::here()
suppressMessages({library(citius); library(data.table)})
D <- file.path(VERSE, "citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

se <- setDT(readRDS(file.path(D, "swimengland_rankings.rds")))
se <- se[!is.na(event_id) & !is.na(mark) & mark > 0]
# to_perf() takes the ORIENTATION, not the event id -- passing the id silently
# coerces to NA and every performance comes back NA, which shows up as "zero
# paired observations" rather than as an error.
se[citius_events(), on = "event_id", orientation := i.orientation]
stopifnot(!anyNA(se$orientation))
se[, perf := to_perf(mark, orientation)]
stopifnot(!anyNA(se$perf))

# One best per athlete-event-season-course, which is what a ranked list already
# is; this guards against any duplication from the overlapping nationality
# sweeps.
b <- se[, .(perf = max(perf)), by = .(tiref, event_id, season, course)]
w <- dcast(b, tiref + event_id + season ~ course, value.var = "perf")
w <- w[!is.na(LCM) & !is.na(SCM)]
say("paired athlete-event-season observations with BOTH courses: %s",
    format(nrow(w), big.mark = ","))
say("distinct swimmers contributing: %s", format(uniqueN(w$tiref), big.mark = ","))

w[, diff := SCM - LCM]          # oriented log scale: positive = short course faster
ev <- w[, .(n = .N, swimmers = uniqueN(tiref),
            offset = mean(diff), sd = sd(diff)), by = event_id]
ev[, se_ := sd / sqrt(n)]
ev[, `:=`(lo = offset - 1.96 * se_, hi = offset + 1.96 * se_)]
ev[, pct := 100 * (exp(offset) - 1)]
setorder(ev, -n)

say("\n=== per-event offset (oriented log scale; %% = short-course advantage) ===")
print(ev[n >= 30, .(event_id, n, swimmers, offset = round(offset, 4),
                    pct = round(pct, 2), lo = round(lo, 4), hi = round(hi, 4))],
      nrows = 40)

pooled <- w[, .(n = .N, offset = mean(diff), sd = sd(diff))]
say("\npooled offset: %.4f (%.2f%% faster short course) over %s pairs",
    pooled$offset, 100 * (exp(pooled$offset) - 1), format(pooled$n, big.mark = ","))
say("measured sigma_within for swimming is ~0.0073, so this is ~%.0fx that.",
    abs(pooled$offset) / 0.0073)

# Does the advantage scale with the number of turns? A 50m short-course race has
# one turn where the long-course has none; a 1500m has 59 against 29. If the
# effect is really about turns, longer events should gain more -- a check that
# the estimate is measuring physics rather than noise.
ev[, dist := suppressWarnings(as.integer(gsub("^SW-([0-9]+)m.*$", "\\1", event_id)))]
say("\n=== does the advantage grow with race distance (i.e. with turns)? ===")
d <- ev[n >= 30 & !is.na(dist), .(events = .N, mean_pct = round(mean(pct), 2)),
        by = dist][order(dist)]
print(d)
if (nrow(d) > 2) say("  Spearman(distance, advantage) = %.2f",
                     cor(d$dist, d$mean_pct, method = "spearman"))

saveRDS(ev, file.path(D, "course_offset.rds"))
say("\nwrote course_offset.rds")
