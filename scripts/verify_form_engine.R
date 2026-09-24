# Regression for form_ratings.R: the engine must reproduce a known arm exactly.
#
# The model is sequential and deterministic, so identical is the right bar — any
# change to processing order or arithmetic moves these digits. This is what
# licensed every optimisation this engine has had (split() -> boundary scan,
# CJ() -> integer vectors, unique() hoisted): each was required to leave the
# output untouched, and each did.
#
# Usage:  Rscript citiusdata/scripts/verify_form_engine.R
# from the VERSE ROOT (here::here anchors at the nearest git repo).
suppressMessages(library(data.table))
ROOT <- here::here()
OUT  <- file.path(ROOT, "citiusdata", "data")
TMP  <- file.path(tempdir(), "form_verify")
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)

# The reference arm: engine DEFAULTS at cap 12, which is the shipping config.
# Update these two numbers deliberately, never to make a run pass.
# 2026-09-20: reset from 69.127 / 69.387 (2026-08-15) after the script was
# found not to have run since the similarity gate landed; the new values are
# today's engine defaults on the v12 adjusted_marks.parquet.
EXPECT <- c(conc25 = 72.308, conc26 = 71.112)
TOL <- 1e-3   # the reference is quoted to 3dp, so compare at that precision

# The engine reads event_similarity_spec.parquet from FORM_OUT and refuses to
# start without it (SEQ_XB_MINCOR gate, added after this script was written),
# so the scratch dir needs a copy. Broken silently from that day to 2026-09-20.
file.copy(file.path(OUT, "event_similarity_spec.parquet"),
          file.path(TMP, "event_similarity_spec.parquet"), overwrite = TRUE)
e <- new.env()
Sys.setenv(SEQ_TAG = "verify", FORM_OUT = TMP)
sys.source(file.path(ROOT, "citiusdata", "scripts", "form_ratings.R"), envir = e)

got <- c(conc25 = e$res$conc25, conc26 = e$res$conc26)
cat("\n================ REGRESSION ================\n")
for (n in names(EXPECT))
  cat(sprintf("%-7s expected %.3f  got %.3f  delta %+.4f\n",
              n, EXPECT[[n]], got[[n]], got[[n]] - EXPECT[[n]]))
if (max(abs(got - EXPECT)) < TOL) {
  cat("PASS - the engine reproduces the reference arm\n")
} else {
  cat("FAIL - the engine no longer reproduces the reference arm.\n")
  cat("       If the change was intentional, update EXPECT and say why in the\n")
  cat("       commit. If it was not, this is the regression it exists to catch.\n")
  quit(status = 1)
}
