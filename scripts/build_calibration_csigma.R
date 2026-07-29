# Calibration for the `csigma` arm: corpus + wind (both confirmed) + sigma
# rescaled to the forecast context.
#
# sigma_within is fitted across the pooled history, but every forecast targets a
# top-tier final. Measured championship/pooled spread ratios track the model's
# dispersion error closely (cor 0.80 across families): throw 0.681 against a
# measured sd(z) of 0.698, road 1.141 against 1.142. This is a PLACINGS lever --
# too-wide spread flattens medal probabilities, too-narrow sharpens them --
# unlike the location fixes that dominated the week.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

cal <- readRDS(file.path(OUT, "calibration_corpus_w.rds"))
x <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))
sc <- fit_sigma_context(x)
cal$sigma_context <- sc
print(sc[order(ratio), .(family, n_champ, ratio = round(ratio, 3))])
saveRDS(cal, file.path(OUT, "calibration_corpus_csigma.rds"))
cat("\nwrote calibration_corpus_csigma.rds\n")
