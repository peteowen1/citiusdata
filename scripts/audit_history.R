# Two sweeps for the class of bug found on 2026-07-31, when a Commonwealth
# entrant reached the published card as second favourite on a predicted 10.97
# because one impossible 17.33 s mark inflated his sigma twelvefold.
#
# The point is that neither sweep needs to know about that athlete. Case-by-case
# inspection found him by luck; these find the class.
#
#   SWEEP 1  Is this file clean? Compares every history source against what
#            flag_implausible() would remove. A non-zero count means something
#            downstream is reading marks the cleaner would have dropped -- which
#            is exactly what happened: predict_glasgow_pretournament.R read the
#            raw .rds and filtered only by date.
#
#   SWEEP 2  Athlete-relative contamination. flag_implausible() is a Hampel
#            filter per EVENT, so it catches 17.33 in a 100m. It cannot catch a
#            jogged 11.5 -- an ordinary mark for a slow sprinter and a
#            non-performance for a 10.0 one. Detection has to be relative to the
#            athlete's own distribution, and it has to use the GOOD side as the
#            reference because the bad side is what is contaminated.
#
# Usage:  Rscript scripts/audit_history.R
#         CITIUS_AUDIT_EVENTS="AT-100Metres-M,AT-PoleVault-M" Rscript ...
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius"))
library(data.table)
OUT <- "C:/dev/citiusverse/citiusdata/data"

EV <- Sys.getenv("CITIUS_AUDIT_EVENTS", "")
EVENTS <- if (nzchar(EV)) trimws(strsplit(EV, ",")[[1]]) else NULL

cat("==============================================================\n")
cat("SWEEP 1: would flag_implausible() remove anything still in use?\n")
cat("==============================================================\n")
sources <- c("championship_results.rds", "athletics_corpus.rds",
             "athletics_history.rds", "swimming_history_full.rds")
for (f in sources) {
  p <- file.path(OUT, f)
  if (!file.exists(p)) { cat(sprintf("%-32s (absent)\n", f)); next }
  x <- setDT(readRDS(p))
  if (!is.null(EVENTS)) x <- x[event_id %in% EVENTS]
  if (!nrow(x) || !"perf" %in% names(x)) { cat(sprintf("%-32s (no perf)\n", f)); next }
  na0 <- sum(is.na(x$perf))
  y <- flag_implausible(copy(x))
  removed <- sum(is.na(y$perf)) - na0
  cat(sprintf("%-32s %10s rows | flag_implausible would drop %5d (%.3f%%)\n",
              f, format(nrow(x), big.mark = ","), removed, 100 * removed / nrow(x)))
  rm(x, y); invisible(gc())
}
for (s in c("athletics_store", "athletics_corpus_store")) {
  d <- file.path(OUT, s)
  if (!dir.exists(d)) { cat(sprintf("%-32s (absent)\n", s)); next }
  x <- setDT(read_results_store(d, events = EVENTS))
  na0 <- sum(is.na(x$perf))
  y <- flag_implausible(copy(x))
  cat(sprintf("%-32s %10s rows | flag_implausible would drop %5d  <- should be 0\n",
              s, format(nrow(x), big.mark = ","), sum(is.na(y$perf)) - na0))
  rm(x, y); invisible(gc())
}

cat("\n==============================================================\n")
cat("SWEEP 2: marks far below the ATHLETE'S own level\n")
cat("==============================================================\n")
cat("Reference is each athlete's own good-side spread, so a contaminated\n")
cat("history cannot set its own threshold. Athletes with 12+ marks only.\n\n")
STORE <- file.path(OUT, "athletics_corpus_store")
x <- setDT(read_results_store(STORE, events = EVENTS))[!is.na(perf)]
x[, .k := .N, by = .(athlete_id, event_id)]
x <- x[.k >= 12L]
x[, dev := perf - median(perf), by = .(athlete_id, event_id)]
x[, up := sqrt(mean(dev[dev > 0]^2)), by = .(athlete_id, event_id)]
x <- x[is.finite(up) & up > 0]
x[, z := dev / up]

by_ev <- x[, .(marks = .N, athletes = uniqueN(athlete_id),
               beyond_4 = 100 * mean(z < -4), beyond_6 = 100 * mean(z < -6),
               worst_z = round(min(z), 1)), by = event_id]
setorder(by_ev, -beyond_6)
cat("worst events by severe-contamination rate:\n")
print(head(by_ev[marks >= 2000], 15))

cat("\nathletes carrying the most severe marks (z < -6):\n")
bad <- x[z < -6, .(n_bad = .N, worst = round(min(z), 1),
                   worst_mark = mark[which.min(z)]), by = .(athlete_id, event_id)]
setorder(bad, -n_bad, worst)
print(head(bad, 15))

cat(sprintf("\ntotal marks beyond -6 good-side sds: %s of %s (%.3f%%)\n",
            format(sum(x$z < -6), big.mark = ","), format(nrow(x), big.mark = ","),
            100 * mean(x$z < -6)))
cat("\nThese are candidates for the no-mark channel rather than the performance\n")
cat("channel: a jog-through, an injury or a foul-out is a DNF that happens to\n")
cat("have a time attached, and foul_rate already ranks those last.\n")
