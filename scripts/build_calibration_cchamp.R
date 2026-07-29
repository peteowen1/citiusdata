# Calibration for the `cchamp` arm: corpus + wind (both confirmed) + the
# championship effect.
#
# A global championship final differs from another top-tier final, and the
# existing offsets cannot say so -- top-tier final is the zero-adjustment
# reference. Measured within top-tier finals so round and tier are held
# constant: road -2.42%, throw +1.70%, sign flipping by family. Validated out of
# sample at -12.8% relative RMSE on 2020+ championship finals.
#
# Deliberately does NOT carry per-family round or tier offsets: both were
# refuted by backtest (cstack, cround), so this arm differs from cwind in
# exactly one thing.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

cal <- readRDS(file.path(OUT, "calibration_corpus_w.rds"))
x <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))
ce <- fit_championship_effect(x)
cal$championship <- ce
stopifnot(is.null(cal$round_family), is.null(cal$tier_family))
print(ce[order(offset), .(family, n, pct = round(100 * (exp(offset) - 1), 3))])
saveRDS(cal, file.path(OUT, "calibration_corpus_cchamp.rds"))
cat("\nwrote calibration_corpus_cchamp.rds\n")
