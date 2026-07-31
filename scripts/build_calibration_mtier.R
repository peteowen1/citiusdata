# Calibration for the `mtier` arm: tier offsets FITTED on the catalogue's
# meet_tier instead of the feed's `tier`.
#
# The feed's tier is per-RESULT and varies within a single meet -- the 2025
# Weltklasse Zurich carries A, DF, F and GW across its own results -- and it
# labels Diamond League "low", so the strongest fields in the sport get adjusted
# UPWARD by 1.69%. The catalogue's meet_tier is per-competition and
# anchor-guarded.
#
# WHY THIS SCRIPT EXISTS RATHER THAN JUST SETTING THE FLAG. `estimate_ability()`
# has been able to APPLY meet_tier for weeks, behind CITIUS_BT_MEET_TIER. But the
# offsets it applies were always FITTED from the feed's tier, so switching the
# flag on looked up a value estimated for "the feed says low" and applied it to
# "the catalogue says T3". Those are different populations, and the error runs
# the wrong way: the feed's "low" bucket contains Diamond League, so its fitted
# penalty is far too small, and applying it to genuine development meets
# under-corrects them. That mismatch is why the `meettier` arm measured only
# -0.10% on marks -- it was measuring the half-wiring, not the fix.
#
# So the corpus is given meet_tier BEFORE calibrate() runs, and the arm sets the
# flag too. Both halves then use .tier_class_of() and cannot diverge.
#
# Usage:  Rscript scripts/build_calibration_mtier.R
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
print(x[, .N, by = meet_tier][order(-N)])
# A silent join failure here would leave every meet_tier NA, the helper would
# fall back to the feed tier throughout, and the arm would come back a dead heat
# that looks like a null result rather than a broken one.
stopifnot(cov > 50)

say("calibrating on meet_tier ...")
clean <- flag_implausible(x)
cal <- calibrate(clean, min_races = 30L)

say("tier offsets, fitted on meet_tier:")
print(as.data.table(cal$tier))
say("(compare calibration_corpus_csigma.rds, fitted on the feed tier)")
old <- tryCatch(as.data.table(readRDS(file.path(OUT, "calibration_corpus_csigma.rds"))$tier),
                error = function(e) NULL)
if (!is.null(old)) print(old)

# Match the rest of the deployed chain exactly, so the ONLY difference from
# csigma is which label the tier offsets were fitted on.
w <- tryCatch(as.data.table(fit_wind_effect(clean)), error = function(e) NULL)
if (!is.null(w) && nrow(w)) { cal$wind <- w; say("wind fitted on ", nrow(w), " events") }
cal$sigma_context <- fit_sigma_context(clean)

saveRDS(cal, file.path(OUT, "calibration_corpus_mtier.rds"))
say("wrote calibration_corpus_mtier.rds")
