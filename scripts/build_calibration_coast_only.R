# A SINGLE-VARIABLE coasting arm.
#
# build_calibration_coasting.R cannot answer "is the coasting trait worth it",
# because it changes two things: it refits the base calibration on `meet_tier`
# AND attaches the trait (see its own line 1). The -0.67% gold Brier recorded on
# 2026-08-01 came from that arm, and ticket 15 later found the package reads
# `calibration$coasting_trait` nowhere at all -- so whatever that arm measured,
# it was not this feature.
#
# This script takes an EXISTING calibration file unchanged and attaches only the
# trait, fitted on the same corpus that calibration was fitted on. Nothing else
# moves, so a backtest against the base file isolates the trait.
#
#   CITIUS_COAST_BASE  calibration to start from (default: this morning's control
#                      arm, which reproduces the deployed configuration exactly --
#                      same centre, same max_iter, same delta 1.66e-04 -- and
#                      already has a scored backtest in backtest_ctrA.rds)
#   CITIUS_COAST_OUT   output file
#   CITIUS_COAST_MIN_HEATS / CITIUS_COAST_SHRINK_K  fitter settings
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

BASE <- Sys.getenv("CITIUS_COAST_BASE", "calibration_corpus_csigma_ctrA.rds")
DEST <- Sys.getenv("CITIUS_COAST_OUT", "calibration_corpus_csigma_coast.rds")
MIN_HEATS <- as.integer(Sys.getenv("CITIUS_COAST_MIN_HEATS", "2"))
SHRINK_K <- as.numeric(Sys.getenv("CITIUS_COAST_SHRINK_K", "5.0"))

base_path <- file.path(OUT, BASE)
if (!file.exists(base_path)) stop("base calibration not found: ", base_path)
cal <- readRDS(base_path)
say("base: ", BASE, " | converged=", cal$converged, " sweeps=", cal$sweeps)
if (!is.null(cal$coasting_trait)) stop("base already carries a coasting trait; pick a clean base")

# Fit on the corpus the base was fitted on, so the trait is not measured on a
# different vintage from the calibration it is attached to -- the exact confound
# score_arm.R's vintage guard exists to catch.
src <- if (!is.null(cal$provenance$input)) cal$provenance$input else "athletics_corpus.rds"
say("fitting trait on ", src, " (from the base's own provenance stamp)")
x <- setDT(readRDS(file.path(OUT, src)))[!is.na(date)]
clean <- flag_implausible(x); rm(x); invisible(gc())

ct <- as.data.table(fit_coasting_trait(clean, min_heats = MIN_HEATS, shrink_k = SHRINK_K))
if (!nrow(ct)) stop("fit_coasting_trait returned nothing; refusing to write an inert arm")
say(sprintf("fitted for %s athletes (min_heats=%d, shrink_k=%.1f)",
            format(nrow(ct), big.mark = ","), MIN_HEATS, SHRINK_K))

# The pooled heat offset is what estimate_ability() already removes. The trait
# only earns its place through the spread AROUND it, so report both -- a trait
# whose spread is small next to the offset cannot reorder anybody.
ph <- NA_real_
rt <- cal$round
if (!is.null(rt) && "round_class" %in% names(rt)) ph <- rt$offset[match("heat", rt$round_class)]
say(sprintf("pooled heat offset: %s", signif(ph, 3)))
say(sprintf("trait: mean %s  sd %s  5-95%% %s .. %s",
            signif(mean(ct$coasting_trait), 3), signif(sd(ct$coasting_trait), 3),
            signif(quantile(ct$coasting_trait, .05), 3),
            signif(quantile(ct$coasting_trait, .95), 3)))
if (is.finite(ph)) {
  ex <- ct$coasting_trait - ph
  say(sprintf("EXCESS over the pooled offset (what actually gets applied): mean %s sd %s",
              signif(mean(ex), 3), signif(sd(ex), 3)))
}

cal$coasting_trait <- ct
saveRDS(cal, file.path(OUT, DEST))
say("wrote ", DEST)
say("NEXT: backtest it against ", BASE, " -- the control is already scored as backtest_ctrA.rds")
