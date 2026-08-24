# Why does a 3:27.79 runner carry a 3:33.83 rating and rank ~21st?
#
# Not a cross-event question - the blend cannot touch him (n_eff 11.05, past the
# XB_MAXN cutoff). Something in his own 1500m history is holding the rating six
# seconds below his best, so this traces every race and shows what each one did.
#
# The suspicion is the peak-versus-average balance for an athlete with a
# catastrophe in their history: the rating tracks average form, and his average
# includes the 2025 Tokyo final where he fell and ran 4:11.23. SEQ_HUBER=3 caps
# a surprise beyond 3 of his own sd, so the fall is clipped rather than removed
# - this shows whether the clip is doing enough.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
WHO <- Sys.getenv("TRACE_NAME", "Josh Kerr")
EV  <- Sys.getenv("TRACE_EVENT", "AT-1500Metres-M")

d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
me <- d[athlete_name %like% WHO]
stopifnot("athlete not found in the display table" = nrow(me) > 0)
cat(sprintf("=== %s, as published ===\n", WHO))
print(me[, .(event_id, rk, R = round(R, 4), pred_mark = round(pred_mark, 2),
             raw_mark = round(raw_mark, 2), n_eff = round(n_eff, 2), last)])
aid <- unique(me$athlete_id)
stopifnot("expected a single athlete id" = length(aid) == 1)

# The history to trace and the published table to look the athlete up in are
# separate tags: a fresh experimental arm has a history but no display table.
HTAG <- Sys.getenv("TRACE_TAG", "final")
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", HTAG))))
cat(sprintf("
tracing history arm: %s
", HTAG))
k <- h[athlete_id == aid]
cat(sprintf("\nraces in the corpus: %d across %d events\n", nrow(k), uniqueN(k$event_id)))
print(k[, .N, by = event_id][order(-N)])

# seconds from the rating scale, so the numbers are readable as times
secs <- function(p) exp(-p)
fmt <- function(s) sprintf("%d:%05.2f", floor(s / 60), s %% 60)

e <- k[event_id == EV][order(date)]
stopifnot("no races in that event" = nrow(e) > 0)
# The engine feeds (perf - r_pre) - shock to the update, NOT perf - r_pre. This
# script printed the raw deviation and called it "surprise" until 2026-08-18,
# when reading it that way produced a wrong diagnosis of Almgren's 10,000m: his
# 28:53 win showed -0.0412, of which the shared race shock accounted for part.
# The shock cannot be reconstructed after the fact - it is a trimmed mean over
# ESTABLISHED athletes scaled by their share of the field - so the engine now
# stores it. Older histories lack the column; say so rather than fall back to
# the raw deviation under a label that implies otherwise.
HAS <- all(c("shock", "surprise", "k") %in% names(h))
if (!HAS) cat("
NOTE: this history predates stored shock/surprise/k, so only the
",
              "      RAW deviation (perf - r_pre) can be shown. It is not what
",
              "      moved the rating. Re-run the arm to get exact numbers.
", sep = "")
e[, `:=`(raw_dev = perf - r_pre,
         mark = secs(perf), rating_before = secs(r_pre))]
cat(sprintf("\n=== every %s race, oldest first ===\n", EV))
cat("surprise is (perf - r_pre) on the rating scale: negative = ran worse than\n")
cat("the rating expected. mark and rating are the same numbers as times.\n\n")
if (HAS) {
  # rating_moved is what this race actually did: k x net surprise, on the
  # rating scale. That is the only column that explains a ranking.
  e[, rating_moved := k * surprise]
  print(e[, .(date, rc, place, mark = fmt(mark), rating_before = fmt(rating_before),
              raw_dev = round(raw_dev, 4), shock = round(shock, 4),
              surprise = round(surprise, 4), k = round(k, 3),
              rating_moved = round(rating_moved, 4), n_eff = round(n_eff, 1))])
} else {
  print(e[, .(date, rc, place, mark = fmt(mark), rating_before = fmt(rating_before),
              raw_dev = round(raw_dev, 4), n_eff = round(n_eff, 1))])
}

cat("\n=== the races that cost him most ===\n")
print(head(e[order(surprise), .(date, rc, place, mark = fmt(mark),
                                expected = fmt(rating_before),
                                surprise = round(surprise, 4))], 8))

sd_own <- stats::sd(e$surprise, na.rm = TRUE)
cat(sprintf("\nhis own surprise sd: %.4f | HUBER=3 clips beyond %.4f\n",
            sd_own, 3 * sd_own))
cl <- e[abs(surprise) > 3 * sd_own]
cat(sprintf("races clipped by the Huber limit: %d of %d\n", nrow(cl), nrow(e)))
if (nrow(cl)) print(cl[, .(date, place, mark = fmt(mark), surprise = round(surprise, 4))])

cat("\n=== peak versus average ===\n")
cat(sprintf("best mark in the corpus : %s\n", fmt(min(e$mark))))
cat(sprintf("median mark             : %s\n", fmt(stats::median(e$mark))))
cat(sprintf("mean of his last 5      : %s\n", fmt(mean(tail(e$mark, 5)))))
cat(sprintf("carried rating          : %s\n", fmt(secs(me[event_id == EV, R]))))
cat("\nIf the carried rating sits near the MEDIAN rather than near the BEST,\n")
cat("the ranking is working as designed and the design is the question -\n")
cat("a championship ranking arguably wants who can run fast on the day.\n")
