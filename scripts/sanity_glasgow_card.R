# Sanity checks on the published Glasgow card, run before anyone reads it.
#
# The failure this exists for: an athlete reached the card as second favourite in
# the 100m on a predicted 10.97s, because his own inconsistency (a 17.33 in his
# history) gave him a huge sigma and a race is decided by the minimum. Fixed in
# ce4881e; these assertions are what would have caught it without a human
# noticing, and what will catch the next one.
#
# Anchors are written down BEFORE the numbers are looked at:
#   1. p_gold must rank with predicted performance within every event.
#   2. Nobody may be a top-3 favourite while typically finishing outside the
#      top 10 -- that combination IS the paid-for-uncertainty signature.
#   3. No predicted mark may be implausible for its event.
#   4. Every event must have a field and probabilities that sum to about 1.
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
pre <- setDT(readRDS(file.path(D, "glasgow2026_pretournament.rds")))
reg <- as.data.table(citius_events())[, .(event_id, orientation, family)]
pre <- merge(pre, reg, by = "event_id", all.x = TRUE)
pre[, perf_pred := orientation * log(median_mark)]
fails <- 0L
say <- function(ok, msg) { if (!ok) fails <<- fails + 1L
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", msg)) }

cat("card generated:", as.character(unique(pre$generated_at))[1],
    "| cutoff:", as.character(unique(pre$cutoff))[1], "\n")
cat(sprintf("events: %d | athletes: %s\n\n", uniqueN(pre$event_id),
            format(nrow(pre), big.mark = ",")))

cat("1. p_gold ranks with predicted performance\n")
ag <- pre[is.finite(perf_pred), .(n = .N,
      rho = if (.N > 2) suppressWarnings(cor(p_gold, perf_pred, method = "spearman")) else NA_real_),
      by = event_id][!is.na(rho)]
say(all(ag$rho > 0.4), sprintf("min rho %.3f over %d events (worst: %s)",
    min(ag$rho), nrow(ag), ag[which.min(rho)]$event_id))
say(median(ag$rho) > 0.8, sprintf("median rho %.3f", median(ag$rho)))

cat("\n2. nobody is paid for their own uncertainty\n")
pre[, favrank := frank(-p_gold, ties.method = "first"), by = event_id]
bad <- pre[favrank <= 3 & median_rank > 10 & p_gold > 0.08]
say(nrow(bad) == 0, sprintf("%d top-3 favourites with median finish outside the top 10 and p_gold > 8%%", nrow(bad)))
if (nrow(bad)) print(bad[order(-p_gold), .(event_id, athlete_name, p_gold = round(p_gold,3),
                                           median_rank, median_mark = round(median_mark,2))])

cat("\n3. predicted marks are physically plausible\n")
lim <- data.table(
  event_id = c("AT-100Metres-M","AT-100Metres-W","AT-Mile-M","AT-LongJump-M","AT-ShotPut-W",
               "SW-100mFreestyle-M","SW-50mFreestyle-M"),
  lo = c(9.4, 10.2, 210, 7.0, 14.0, 46.0, 20.5),
  hi = c(10.8, 11.8, 260, 9.0, 21.0, 53.0, 24.0))
# Applied to the TOP 5 only -- those are what appears on the card. A Commonwealth
# entry list legitimately includes a 12.5s women's 100m from a small federation,
# so banding the whole field flags real athletes and says nothing about the model.
chk <- merge(pre[favrank <= 5, .(event_id, athlete_name, median_mark)], lim, by = "event_id")
oob <- chk[is.finite(median_mark) & (median_mark < lo | median_mark > hi)]
say(nrow(oob) == 0, sprintf("%d predicted marks outside a plausible band for their event", nrow(oob)))
if (nrow(oob)) print(head(oob[order(event_id)], 12))

cat("\n4. every event has a coherent field\n")
ev <- pre[, .(n = .N, psum = sum(p_gold), top = max(p_gold)), by = event_id]
say(all(ev$n >= 3), sprintf("smallest field %d (%s)", min(ev$n), ev[which.min(n)]$event_id))
say(all(abs(ev$psum - 1) < 0.02), sprintf("gold probabilities sum to 1 (worst deviation %.4f)",
                                          max(abs(ev$psum - 1))))
say(all(ev$top < 0.99), sprintf("no event is a certainty (max favourite %.3f in %s)",
                                max(ev$top), ev[which.max(top)]$event_id))

cat("\n5. the specific case this file exists for\n")
v <- pre[grepl("VENCATASAMY", athlete_name, ignore.case = TRUE)]
if (nrow(v)) {
  say(all(v$favrank > 5), sprintf("Vencatasamy is our #%s on a %.2f (was #2 at 19.2%% on a 10.97)",
                                  paste(v$favrank, collapse=","), v$median_mark[1]))
} else cat("  (not in the field)\n")

cat(sprintf("\n%s: %d check%s failed\n", if (fails == 0L) "ALL CLEAR" else "PROBLEMS",
            fails, if (fails == 1L) "" else "s"))
if (fails > 0L) quit(status = 1)
