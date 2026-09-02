# Are some athletes genuinely BETTER INDOORS than their rating implies?
#
# This is the one untested feature that passes the test the last four ideas
# failed. Cross-event, momentum, per-event k and head-to-head all re-derived
# signal from races the update had already processed, and all were worth
# nothing. An indoor offset is different in kind: a rating is ONE number
# averaging an athlete's indoor and outdoor form, so it structurally CANNOT
# represent "1% better indoors". The model has never been able to absorb it.
#
# And unlike wind or altitude it does not cancel: indoor is shared by the field,
# but an athlete's SENSITIVITY to it is not, and sensitivity is the only thing
# that can reorder a race.
#
# Three questions, in order - the second is the one that decides it:
#   1. Is there a POPULATION indoor bias? (expect ~0: shared effects are
#      absorbed by the race shock, which is the whole point of S)
#   2. Is the per-athlete effect IDENTIFIABLE - does an athlete's indoor edge in
#      one half of their races predict it in the other? Without that it is noise
#      and estimating it would just fit noise, exactly as the per-athlete bias
#      panel found for dominant athletes.
#   3. If it is real, how big, and what could it be worth?
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
# indoor is not in the history; join it from the corpus
ev <- setdiff(sub("^event_id=", "", list.dirs(file.path(D, "athletics_corpus_store"),
              recursive = FALSE, full.names = FALSE)), "__unmatched__")
ind <- rbindlist(lapply(ev, function(EV) {
  f <- file.path(D, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(setDT(read_parquet(f, col_select = c("athlete_id","race_key","indoor"))),
                error = function(e) NULL)
  if (is.null(x)) return(NULL)
  x[, athlete_id := as.character(athlete_id)][]
}), fill = TRUE)
ind <- unique(ind[!is.na(indoor)])
h[, athlete_id := as.character(athlete_id)]
h <- merge(h, ind, by = c("athlete_id", "race_key"), all.x = TRUE)
h <- h[!is.na(indoor)]
h[, resid := perf - r_pre]
cat(sprintf("athlete-races with an indoor flag: %s (%.1f%% indoor)\n",
            format(nrow(h), big.mark = ","), 100 * mean(h$indoor)))

cat("\n=== 1. POPULATION bias (expect ~0 - shared effects are absorbed) ===\n")
print(h[, .(races = .N, bias_pct = round(100 * mean(resid), 4),
            sd_pct = round(100 * stats::sd(resid), 3)), by = indoor])

cat("\n=== 2. IS THE PER-ATHLETE EFFECT IDENTIFIABLE? ===\n")
# an athlete's indoor edge = mean residual indoors minus mean residual outdoors.
# Split each athlete's races in half by date and ask whether the first half's
# edge predicts the second's. No persistence => nothing to estimate.
setorder(h, athlete_id, event_id, date)
h[, half := fifelse(seq_len(.N) <= .N / 2, 1L, 2L), by = .(athlete_id, event_id)]
ae <- h[, .(n_in = sum(indoor), n_out = sum(!indoor)), by = .(athlete_id, event_id)]
ok <- ae[n_in >= 3 & n_out >= 3]
cat(sprintf("athlete-events with >=3 races both indoors and out: %s\n",
            format(nrow(ok), big.mark = ",")))
e <- merge(h, ok[, .(athlete_id, event_id)], by = c("athlete_id", "event_id"))
edge <- e[, .(edge = mean(resid[indoor]) - mean(resid[!indoor]),
              n_in = sum(indoor), n_out = sum(!indoor)),
          by = .(athlete_id, event_id, half)]
edge <- edge[is.finite(edge) & n_in >= 2 & n_out >= 2]
w <- dcast(edge, athlete_id + event_id ~ half, value.var = "edge")
setnames(w, c("1", "2"), c("h1", "h2"), skip_absent = TRUE)
w <- w[is.finite(h1) & is.finite(h2)]
cat(sprintf("athlete-events measurable in BOTH halves: %s\n", format(nrow(w), big.mark = ",")))
if (nrow(w) >= 30) {
  ct <- stats::cor.test(w$h1, w$h2)
  cat(sprintf("split-half correlation of the indoor edge: %.3f (95%% CI %.3f to %.3f, p = %.3g)\n",
              ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
  cat(sprintf("sd of the edge: %.3f%% of a mark\n", 100 * stats::sd(w$h1)))
  cat("\nA correlation near 0 means the edge is noise and there is nothing to fit.\n")
  cat("Positive and significant means some athletes really are indoor specialists.\n")
} else cat("too few athlete-events to test - this is the answer, not a step toward one\n")

cat("\n=== 3. size, if it is real ===\n")
cat("Residual sd is ~3%% of a mark. Removing an independent component of spread s\n")
cat("improves the error metric by roughly s^2/2sigma^2 - QUADRATIC, so an edge of\n")
cat("0.3%% against a 3%% residual is worth about 0.5%% of the error, not 10%%.\n")
