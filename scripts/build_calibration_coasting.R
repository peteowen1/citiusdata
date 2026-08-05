# Calibration for the `coasting` arm: tier offsets fitted on meet_tier plus athlete-specific heat coasting traits.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
x[, competition_id := as.character(competition_id)]
cat_tbl[, competition_id := as.character(competition_id)]
x <- merge(x, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)

cov <- 100 * mean(!is.na(x$meet_tier))
say(sprintf("meet_tier attached to %.1f%% of corpus rows", cov))
stopifnot(cov > 50)

say("calibrating base on meet_tier ...")
clean <- flag_implausible(x)
cal <- calibrate(clean, min_races = 30L)

say("fitting athlete coasting traits ...")
ct <- fit_coasting_trait(clean, min_heats = 2L, shrink_k = 5.0)
cal$coasting_trait <- ct
say(sprintf("fitted coasting trait for %d athletes", nrow(ct)))
if (nrow(ct)) {
  print(head(ct[order(coasting_trait)]))
}

w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) NULL)
if (!is.null(w) && nrow(w)) { cal$wind <- w; say("wind fitted on ", nrow(w), " events") }
cal$sigma_context <- fit_sigma_context(clean)

saveRDS(cal, file.path(OUT, "calibration_corpus_coasting.rds"))
say("wrote calibration_corpus_coasting.rds")
