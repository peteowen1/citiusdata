# Why is the race shock ~0 in races that were obviously slow?
#
# Almgren's six 10,000m races carry shocks of 0.0019, 0.0000, 0.0000, -0.0051,
# 0.0000, 0.0000 - so a 28:53 win registered as a 4.1% personal failure with the
# race conditions contributing nothing. If a tactical race is not recognised as
# slow, every athlete in it is charged for the pace.
#
# HISTORICAL NOTE, 2026-08-19: this script diagnosed the pre-fix behaviour and
# the fix has since shipped. form_ratings.R now defaults to SEQ_SHOCK_W=kappa,
# weighting the shock m/(m+K) with K=2 and a floor of 2 established athletes.
# Run it with SEQ_SHOCK_W=share to reproduce what is described below.
#
# THE SUSPECTED MECHANISM (as it was). form_ratings.R scaled the shock by the ESTABLISHED
# athletes' share of the field:  S = trimmed_mean(surprise | established) *
# (n_established / n_field). The intent is honest - a shock estimated from two
# known athletes in a field of thirty should not be trusted at full strength -
# but the scaling is applied to the ESTIMATE rather than to confidence in it, so
# a perfectly well-estimated shock is still divided by the share. In a field
# that is 20% established, a real 5% slow race is recorded as 1% slow and the
# other 4% is charged to the athletes.
#
# This script tests that against the stored shock rather than assuming it.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "base3")
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","date","event_id","r_pre","perf",
                                       "seen","shock","surprise","k","place")))
stopifnot("this history predates stored shock - re-run the arm" = "shock" %in% names(h))
h <- h[is.finite(r_pre) & is.finite(perf) & is.finite(shock)]
cat(sprintf("%s: %s athlete-races | %s races\n", TAG,
            format(nrow(h), big.mark = ","), format(uniqueN(h$race_key), big.mark = ",")))

h[, raw := perf - r_pre]
r <- h[, .(n = .N, n_seen = sum(seen), shock = shock[1],
           trimmed_all  = mean(raw, trim = 0.20),
           trimmed_seen = if (sum(seen) >= 3) mean(raw[seen == TRUE], trim = 0.20) else NA_real_),
       by = race_key]
r[, share := n_seen / n]

cat("\n=== is the shock just the trimmed mean times the established share? ===\n")
r[, predicted := trimmed_seen * share]
ok <- r[is.finite(predicted) & is.finite(shock)]
cat(sprintf("races testable: %s | correlation(shock, trimmed_seen x share): %.4f\n",
            format(nrow(ok), big.mark = ","), stats::cor(ok$shock, ok$predicted)))
cat(sprintf("correlation(shock, trimmed_seen alone):                 %.4f\n",
            stats::cor(ok$shock, ok$trimmed_seen)))
cat("If the first is far higher than the second, the share scaling is what is\n")
cat("shrinking the shock, and it is shrinking the ESTIMATE not the confidence.\n")

cat("\n=== how much shock survives, by how much of the field is established ===\n")
r[, sb := cut(share, c(-Inf, .1, .25, .5, .75, Inf),
              labels = c("<10%", "10-25%", "25-50%", "50-75%", ">75%"))]
print(r[is.finite(trimmed_seen), .(races = .N,
        median_field = as.numeric(median(n)),
        trimmed_seen = round(mean(abs(trimmed_seen)), 4),
        shock_kept   = round(mean(abs(shock)), 4),
        kept_pct     = round(100 * mean(abs(shock)) / mean(abs(trimmed_seen)), 1)),
        by = sb][order(sb)])
cat("kept_pct is the fraction of the measured race effect the engine actually\n")
cat("applies. Everything not applied is charged to the athletes as form.\n")

cat("\n=== how often is the shock effectively zero? ===\n")
cat(sprintf("races with |shock| < 0.001: %s of %s (%.1f%%)\n",
            format(sum(abs(r$shock) < 0.001), big.mark = ","),
            format(nrow(r), big.mark = ","),
            100 * mean(abs(r$shock) < 0.001)))
cat(sprintf("  of those, races whose trimmed_seen exceeds 0.01 (a real 1%%+ effect): %s\n",
            format(sum(abs(r$shock) < 0.001 & abs(r$trimmed_seen) > 0.01, na.rm = TRUE),
                   big.mark = ",")))

cat("\n=== the biggest missed race effects ===\n")
r[, missed := abs(trimmed_seen) - abs(shock)]
setorder(r, -missed)
print(head(r[is.finite(missed), .(race_key = substr(race_key, 1, 34), n, n_seen,
                                  share = round(share, 2),
                                  trimmed_seen = round(trimmed_seen, 4),
                                  shock = round(shock, 4))], 10))
