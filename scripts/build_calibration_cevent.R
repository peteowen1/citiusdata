# Calibration for the `cevent` arm: the deployed csigma calibration with round
# and tier offsets refitted at EVENT grain instead of family.
#
# The defect this targets is measurable and specific. On T1 finals with data
# richness held fixed, the model beats a last-five baseline by 10.8% in the 100m
# and LOSES to it by 6.4% in the 400m and 7.4% in the 400m hurdles; throws lose
# by 4.2%. In both cases our ability estimate correlates with the truth WORSE
# than a plain average of the athlete's last five marks -- 0.595 against 0.648
# for the 400m group, 0.694 against 0.726 for throws -- which means something we
# do to the data is destroying ordering information the raw average keeps.
#
# Round and tier adjustment is the suspect, because it is fitted per family and
# `sprint` contains both the 100m and the 400m. Per-family cannot represent that
# split at any sample size.
#
# Built by refitting only the context block on top of the csigma calibration, so
# this arm differs from the reference in exactly one thing. Rebuilding from
# scratch would also pick up a fresh wind fit and a fresh sigma_context and the
# comparison would no longer be single-variable -- which is the confound that
# made six arms look like six findings this morning.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

cal <- readRDS(file.path(OUT, "calibration_corpus_csigma.rds"))
x <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))

ctx <- estimate_context_effects(x, per_family = TRUE, per_event = TRUE)
cal$round_family <- ctx$round_family
cal$tier_family  <- ctx$tier_family
cal$round_event  <- ctx$round_event
cal$tier_event   <- ctx$tier_event

re <- as.data.table(ctx$round_event)
te <- as.data.table(ctx$tier_event)
cat(sprintf("\nper-event cells: round %d, tier %d\n", nrow(re), nrow(te)))
if (nrow(re)) cat(sprintf("round shrink k = %s\n", format(re$shrink_k[1])))
if (nrow(te)) cat(sprintf("tier  shrink k = %s\n", format(te$shrink_k[1])))

# THE ANCHOR. The whole rationale is that the 100m and the 400m need different
# heat offsets, so if they come out the same the refit has not done the thing it
# exists to do and the arm is not worth the hours. Reported either way rather
# than asserted, because a null result here is information, not a failure.
if (nrow(re)) {
  h <- re[round_class == "heat" & event_id %in%
          c("AT-100Metres-M", "AT-400Metres-M", "AT-400MetresHurdles-M",
            "AT-200Metres-M", "AT-DiscusThrow-M", "AT-ShotPut-M")]
  if (nrow(h)) {
    cat("\nheat offsets, sprint family and throws (the split under test):\n")
    print(h[, .(event_id, offset = round(offset, 5), raw = round(raw, 5), n)])
    sp <- h[event_id %in% c("AT-100Metres-M", "AT-400Metres-M")]
    if (nrow(sp) == 2L) {
      gap <- abs(diff(sp$offset))
      cat(sprintf("\n100m vs 400m heat offset gap: %.5f (%.3f%% of a mark)\n",
                  gap, 100 * gap))
      if (gap < 1e-4) cli::cli_alert_warning(
        "The two are effectively identical; per-event has nothing to add here.")
    }
  }
}

saveRDS(cal, file.path(OUT, "calibration_corpus_cevent.rds"))
cat("\nwrote calibration_corpus_cevent.rds\n")
