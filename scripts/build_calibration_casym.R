# Calibration for the `casym` arm: csigma, plus an asymmetric performance draw.
#
# Performance is not symmetric around an athlete's own level. Measured within
# athlete on the corpus, the bad-side spread is 1.36x (high jump) to 1.81x (pole
# vault) the good-side spread, and the events where it is worst are exactly the
# ones you can fail out of. The simulator draws a symmetric t, so the GOOD tail
# it grants is 12-39% wider than anything the athlete has ever produced -- and a
# race is decided by the best draw, so the surplus becomes win probability.
#
# This arm reshapes the draw and DELIBERATELY does not rescale it: the mean is
# corrected so `median_mark` cannot move. Pre-registered as a placings-only
# change. If marks move, the arm is confounded, not successful.
#
# It is NOT the fix for contaminated individuals -- an event-level ratio gives
# every athlete the same haircut, so an athlete whose sigma is 6.6x too large
# because of one corrupt mark stays 6.6x too large. That is a separate, later
# arm (per-athlete robust dispersion). Kept separate on purpose: one variable.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
BASE <- Sys.getenv("CITIUS_ASYM_BASE", "calibration_corpus_csigma.rds")

x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))[!is.na(perf)]
x <- flag_implausible(x)
cat(sprintf("corpus: %s marks\n", format(nrow(x), big.mark = ",")))

t0 <- Sys.time()
asym <- fit_asymmetry(x)
cat(sprintf("fitted %d event%s in %.1f min\n", nrow(asym),
            if (nrow(asym) == 1) "" else "s",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

setorder(asym, -n)
cat("\n=== most asymmetric events with their own fit ===\n")
print(head(asym[source == "event"][order(-r_dn / r_up),
                                   .(event_id, r_up = round(r_up, 3), r_dn = round(r_dn, 3),
                                     ratio = round(r_dn / r_up, 2), n_ath, n)], 12))
cat("\n=== least asymmetric ===\n")
print(head(asym[source == "event"][order(r_dn / r_up),
                                   .(event_id, r_up = round(r_up, 3), r_dn = round(r_dn, 3),
                                     ratio = round(r_dn / r_up, 2), n_ath, n)], 6))
cat("\nby family:\n")
print(asym[, .(events = .N, r_up = round(median(r_up), 3), r_dn = round(median(r_dn), 3),
               ratio = round(median(r_dn / r_up), 2)), by = family][order(-ratio)])
cat("\nfell back to family:", sum(asym$source == "family"), "of", nrow(asym), "events\n")

saveRDS(asym, file.path(OUT, "asymmetry.rds"))
cal <- readRDS(file.path(OUT, BASE))
cal$asymmetry <- asym
saveRDS(cal, file.path(OUT, "calibration_corpus_casym.rds"))
cat("\nwrote asymmetry.rds and calibration_corpus_casym.rds (base:", BASE, ")\n")
