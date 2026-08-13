# LEAKAGE CONTROL for the coasting arm.
#
# `coasting_trait` is a PER-ATHLETE coefficient, and it was fitted on the same
# corpus the backtest uses as history -- which contains the scored races. An
# athlete's own jogged heats therefore helped fit the trait that then adjusts
# that athlete's history in the races being scored. Population-level offsets
# leak weakly through this channel; a per-athlete term leaks hard, and the
# shared-corpus control does not cover it. backtest_athletics.R:64-68 already
# documents the same channel for `sensitivity`.
#
# The test excludes the SCORED MEETS specifically rather than applying a date
# cutoff. A cutoff at the start of the scored window (2016-05-06) would also
# strip every athlete who debuted after it, so a shrinking effect could not be
# told apart from lost coverage. Excluding the meets keeps the athlete pool and
# the time span, and removes only the races whose outcomes are being predicted.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

BASE <- Sys.getenv("CITIUS_COAST_BASE", "calibration_corpus_csigma_ctrA.rds")
DEST <- Sys.getenv("CITIUS_COAST_OUT", "calibration_corpus_csigma_coastnl.rds")
SCORED <- Sys.getenv("CITIUS_COAST_SCORED", "backtest_coast.rds")

cal <- readRDS(file.path(OUT, BASE))
if (!is.null(cal$coasting_trait)) stop("base already carries a trait; pick a clean base")

# Which meets were scored? Take them from the backtest artefact itself rather
# than re-deriving the meet selection, so this cannot drift from what was run.
bt <- readRDS(file.path(OUT, SCORED))
ids <- unique(as.character(as.data.table(bt$outcomes)$race_id))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
scored_comp <- unique(as.character(ch[as.character(race_key) %in% ids, competition_id]))
scored_comp <- scored_comp[!is.na(scored_comp)]
say(sprintf("scored races: %s across %s competitions",
            format(length(ids), big.mark = ","), format(length(scored_comp), big.mark = ",")))
if (!length(scored_comp)) stop("could not resolve scored competitions; refusing to claim a leakage control")

src <- if (!is.null(cal$provenance$input)) cal$provenance$input else "athletics_corpus.rds"
x <- setDT(readRDS(file.path(OUT, src)))[!is.na(date)]
x[, competition_id := as.character(competition_id)]

# The corpus and championship_results use different race_key SCHEMES (0 of
# 509,650 shared, see the Werro incident), so verify the competition_id spaces
# DO meet before trusting an exclusion built from one against the other. A
# zero-overlap exclusion would remove nothing and quietly report "no leakage".
hit <- sum(x$competition_id %in% scored_comp)
say(sprintf("corpus rows inside scored competitions: %s (%.2f%%)",
            format(hit, big.mark = ","), 100 * hit / nrow(x)))
if (hit == 0) stop("competition_id spaces do not meet -- the exclusion would be a no-op")

x <- x[!(competition_id %in% scored_comp)]
say(sprintf("corpus after exclusion: %s rows", format(nrow(x), big.mark = ",")))
clean <- flag_implausible(x); rm(x); invisible(gc())

ct <- as.data.table(fit_coasting_trait(clean, min_heats = 2L, shrink_k = 5.0))
if (!nrow(ct)) stop("no trait fitted after exclusion")
say(sprintf("fitted for %s athletes (leaky arm had 89,839)", format(nrow(ct), big.mark = ",")))
ph <- cal$round$offset[match("heat", cal$round$round_class)]
say(sprintf("pooled heat offset %s | trait mean %s sd %s",
            signif(ph, 3), signif(mean(ct$coasting_trait), 3), signif(sd(ct$coasting_trait), 3)))

cal$coasting_trait <- ct
saveRDS(cal, file.path(OUT, DEST))
say("wrote ", DEST)
