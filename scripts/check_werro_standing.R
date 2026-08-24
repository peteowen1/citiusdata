# Two questions:
# 1. Was Werro a top-3 athlete at END-2025? If not, the model was not
#    "under-rating an elite" — it was tracking a genuine improvement, with the
#    lag any recency-weighted estimator has.
# 2. Pete: would the GOOD DAY number be the better point prediction for finals?
#    Test it head to head against TYPICAL on 2026 finals.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG))))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
d <- setDT(read_parquet(file.path(OUT, sprintf("form_display_%s.parquet", TAG))))
nm <- unique(d[, .(athlete_id, athlete_name)])

first26 <- h[year(date) == 2026][order(date), .SD[1], by = .(athlete_id, event_id)]
first26 <- first26[n_eff >= 3, .(athlete_id, event_id, r_end2025 = r_pre)]
setorder(first26, event_id, -r_end2025)
first26[, standing := seq_len(.N), by = event_id]
w <- merge(first26[event_id == "AT-800Metres-W"], nm, by = "athlete_id")
setorder(w, standing)
cat("800m W standing as at END-2025 (top 8), before any 2026 result:\n")
# seq_len, not 1:8. On an empty or short `w` this indexed past the end and
# printed fully-formed rows of NA - eight of them - which read as real output.
stopifnot("the 800m W merge produced no rows" = nrow(w) > 0)
for (i in seq_len(min(8L, nrow(w)))) cat(sprintf("  %d. %-24s %s\n", w$standing[i],
    substr(ifelse(is.na(w$athlete_name[i]),"?",w$athlete_name[i]),1,24),
    sprintf("%d:%05.2f", floor(exp(-w$r_end2025[i])/60), exp(-w$r_end2025[i]) %% 60)))
ws <- w[grepl("Werro", athlete_name)]$standing
cat(sprintf("\nWerro was ranked %s at end-2025.\n", if (length(ws)) ws else "unranked"))

cat("\n--- 2. TYPICAL vs GOOD DAY as a point prediction, 2026 finals ---\n")
# reconstruct both from the same offset/spread the display uses
fitset <- h[seen == TRUE & rc == "final" & date < as.Date("2025-01-01")]
fitset[, z := (perf - r_pre)/sqrt(v_pre)]
ZS <- unname(stats::quantile(fitset$z[is.finite(fitset$z)], .90) -
             stats::quantile(fitset$z[is.finite(fitset$z)], .50))
off <- fitset[, .(offset = mean(perf - r_pre), n = .N), by = event_id]
pool <- fitset[, mean(perf - r_pre)]
off[n < 200, offset := pool]
v <- h[year(date) == 2026 & rc == "final" & is.finite(v_pre) & v_pre > 0]
v <- merge(v, off[, .(event_id, offset)], by = "event_id", all.x = TRUE)
v[is.na(offset), offset := pool]
v[, typical := r_pre + offset]
v[, goodday := r_pre + offset + ZS*sqrt(v_pre)]
sc <- function(x, lab, sub) data.table(pred = lab, n = sum(sub),
        mean_abs_err_pct = round(100*mean(abs(x[sub])), 3),
        bias_pct = round(100*mean(x[sub]), 3))
allf <- rep(TRUE, nrow(v))
v <- merge(v, first26[, .(athlete_id, event_id, standing)], by = c("athlete_id","event_id"), all.x = TRUE)
t3 <- !is.na(v$standing) & v$standing <= 3
cat("\nALL 2026 finals:\n")
print(rbind(sc(v$perf - v$typical, "typical", allf), sc(v$perf - v$goodday, "good day", allf)))
cat("\nTOP-3-at-end-2025 athletes only (the ones a preview features):\n")
print(rbind(sc(v$perf - v$typical, "typical", t3), sc(v$perf - v$goodday, "good day", t3)))
cat("\nLower mean_abs_err is the better point prediction. bias near 0 is honest.\n")
